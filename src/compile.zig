const std = @import("std");

const scn = @import("scanner.zig");
const Scanner = scn.Scanner;
const Token = scn.Token;
const TokenType = scn.TokenType;

const chnk = @import("chunk.zig");
const Chunk = chnk.Chunk;
const OpCode = chnk.OpCode;

const disassemble_chunk = @import("debug.zig").disassemble_chunk;

const Value = @import("value.zig").Value;

const obj = @import("object.zig");
const Obj = obj.Obj;
const alloc_string = obj.alloc_string;

const ObjStringHashMap = @import("vm.zig").ObjStringHashMap;

const compile_err = error.CompileFailed;
const local_not_found = error.LocalNotFound;

const PLACEHOLDER_BYTE: u8 = 0xff;

const Precedence = enum(u8) {
    none,
    assignment, // =
    _or, // or
    _and, // and
    equality, // == !=
    comparison, // < > <= >=
    term, // + -
    factor, // * /
    unary, // ! -
    call, // . ()
    primary,
};

const ParseFn = fn (compiler: *Compiler, can_assign: bool) void;

const ParseRule = struct {
    prefix: ?ParseFn,
    infix: ?ParseFn,
    precedence: Precedence,
};

// order here must match order in scanner.zig TokenType so that indexing on the enum value works
const RULES = [_]ParseRule{
    // Single-character tokens.
    .{ .prefix = Compiler.grouping, .infix = null, .precedence = .none }, // left_paren,
    .{ .prefix = null, .infix = null, .precedence = .none }, // right_paren,
    .{ .prefix = null, .infix = null, .precedence = .none }, // left_brace,
    .{ .prefix = null, .infix = null, .precedence = .none }, // right_brace,
    .{ .prefix = null, .infix = null, .precedence = .none }, // comma,
    .{ .prefix = null, .infix = null, .precedence = .none }, // dot,
    .{ .prefix = Compiler.unary, .infix = Compiler.binary, .precedence = .term }, // minus,
    .{ .prefix = null, .infix = Compiler.binary, .precedence = .term }, // plus,
    .{ .prefix = null, .infix = null, .precedence = .none }, // semicolon,
    .{ .prefix = null, .infix = Compiler.binary, .precedence = .factor }, // slash,
    .{ .prefix = null, .infix = Compiler.binary, .precedence = .factor }, // star,
    // One or two character tokens.
    .{ .prefix = Compiler.unary, .infix = null, .precedence = .none }, // bang,
    .{ .prefix = null, .infix = Compiler.binary, .precedence = .equality }, // bang_equal,
    .{ .prefix = null, .infix = null, .precedence = .none }, // equal,
    .{ .prefix = null, .infix = Compiler.binary, .precedence = .equality }, // equal_equal,
    .{ .prefix = null, .infix = Compiler.binary, .precedence = .comparison }, // greater,
    .{ .prefix = null, .infix = Compiler.binary, .precedence = .comparison }, // greater_equal,
    .{ .prefix = null, .infix = Compiler.binary, .precedence = .comparison }, // less,
    .{ .prefix = null, .infix = Compiler.binary, .precedence = .comparison }, // less_equal,
    // Literals.
    .{ .prefix = Compiler.variable, .infix = null, .precedence = .none }, // identifier,
    .{ .prefix = Compiler.string, .infix = null, .precedence = .none }, // string,
    .{ .prefix = Compiler.number, .infix = null, .precedence = .none }, // number,
    // Keywords.
    .{ .prefix = null, .infix = null, .precedence = .none }, // logical_and,
    .{ .prefix = null, .infix = null, .precedence = .none }, // class,
    .{ .prefix = null, .infix = null, .precedence = .none }, // cf_else,
    .{ .prefix = Compiler.literal, .infix = null, .precedence = .none }, // logical_false,
    .{ .prefix = null, .infix = null, .precedence = .none }, // cf_for,
    .{ .prefix = null, .infix = null, .precedence = .none }, // fun,
    .{ .prefix = null, .infix = null, .precedence = .none }, // cf_if,
    .{ .prefix = Compiler.literal, .infix = null, .precedence = .none }, // nil,
    .{ .prefix = null, .infix = null, .precedence = .none }, // logical_or,
    .{ .prefix = null, .infix = null, .precedence = .none }, // print,
    .{ .prefix = null, .infix = null, .precedence = .none }, // cf_return,
    .{ .prefix = null, .infix = null, .precedence = .none }, // super,
    .{ .prefix = null, .infix = null, .precedence = .none }, // this,
    .{ .prefix = Compiler.literal, .infix = null, .precedence = .none }, // logical_true,
    .{ .prefix = null, .infix = null, .precedence = .none }, // cf_var,
    .{ .prefix = null, .infix = null, .precedence = .none }, // cf_while,
    //
    .{ .prefix = null, .infix = null, .precedence = .none }, // scan_error,
    .{ .prefix = null, .infix = null, .precedence = .none }, // eof,
};

fn get_rule(t: TokenType) *const ParseRule {
    return &RULES[@intCast(usize, @enumToInt(t))];
}

pub fn compile(
    source: []const u8,
    chunk: *Chunk,
    allocator: std.mem.Allocator,
    debug: bool,
    objects: *?*Obj,
    strings: *ObjStringHashMap,
) !void {
    var scanner = Scanner.init(source);
    var parser = Parser.init(&scanner);
    parser.advance();

    var compiler = Compiler{
        .current_chunk = chunk,
        .parser = &parser,
        .allocator = allocator,
        .debug = debug,
        .objects = objects,
        .strings = strings,
    };

    try compiler.compile();
}

const Local = struct {
    token: Token,
    depth: ?u8 = null,
};

pub const Compiler = struct {
    current_chunk: *Chunk,
    parser: *Parser,
    allocator: std.mem.Allocator,
    debug: bool,
    objects: *?*Obj,
    strings: *ObjStringHashMap,

    locals: [std.math.maxInt(u8)]Local = undefined,
    local_count: u8 = 0,
    scope_depth: u8 = 0,

    pub fn compile(self: *Compiler) !void {
        while (!self.parser.match(TokenType.eof)) {
            self.declaration();
        }
        self.end();

        if (self.parser.had_error) return compile_err;
    }

    fn parse_precedence(self: *Compiler, precedence: Precedence) void {
        self.parser.advance();
        const prefix_rule = get_rule(self.parser.previous.t).prefix orelse {
            self.parser.error_at_previous("expect expression");
            return;
        };

        const precedence_int = @enumToInt(precedence);
        const can_assign = precedence_int <= @enumToInt(Precedence.assignment);
        prefix_rule(self, can_assign);

        while (precedence_int <= @enumToInt(get_rule(self.parser.current.t).precedence)) {
            self.parser.advance();
            // TODO: unclear if this translation is correct
            const infix_rule = get_rule(self.parser.previous.t).infix orelse {
                return;
            };
            infix_rule(self, can_assign);
        }

        if (can_assign and self.parser.match(TokenType.equal)) {
            self.parser.error_at_previous("invalid assignment target");
        }
    }

    fn declaration(self: *Compiler) void {
        if (self.parser.match(TokenType.cf_var)) {
            self.var_declaration();
        } else {
            self.statement();
        }

        if (self.parser.panic_mode) {
            self.parser.synchronize();
        }
    }

    fn statement(self: *Compiler) void {
        if (self.parser.match(TokenType.print)) {
            self.print_statement();
        } else if (self.parser.match(TokenType.cf_if)) {
            self.if_statement();
        } else if (self.parser.match(TokenType.left_brace)) {
            self.begin_scope();
            self.block();
            self.end_scope();
        } else {
            self.expression_statement();
        }
    }

    fn expression(self: *Compiler) void {
        self.parse_precedence(Precedence.assignment);
    }

    fn block(self: *Compiler) void {
        while (!self.parser.check(TokenType.right_brace) and !self.parser.check(TokenType.eof)) {
            self.declaration();
        }

        self.parser.consume(TokenType.right_brace, "expect '}' at end of block");
    }

    fn number(self: *Compiler, _: bool) void {
        // TODO: error handling
        const value = std.fmt.parseFloat(
            f64,
            self.parser.previous.start[0..self.parser.previous.length],
        ) catch 0;
        self.emit_constant(Value.number(value));
    }

    fn string(self: *Compiler, _: bool) void {
        var object = alloc_string(
            self.strings,
            self.parser.previous.start[1 .. self.parser.previous.length - 1],
            self.allocator,
        );
        self.emit_constant(Value.obj(object, self.objects));
    }

    fn variable(self: *Compiler, can_assign: bool) void {
        self.named_variable(self.parser.previous, can_assign);
    }

    fn named_variable(self: *Compiler, token: Token, can_assign: bool) void {
        var get_op = OpCode.get_local;
        var set_op = OpCode.set_local;
        var arg = self.resolve_local(token) catch blk: {
            get_op = OpCode.get_global;
            set_op = OpCode.set_global;
            break :blk self.identifier_constant(token);
        };

        if (can_assign and self.parser.match(TokenType.equal)) {
            self.expression();
            self.emit_compound(set_op, arg);
        } else {
            self.emit_compound(get_op, arg);
        }
    }

    fn literal(self: *Compiler, _: bool) void {
        switch (self.parser.previous.t) {
            .logical_false => self.emit_opcode(OpCode._false),
            .logical_true => self.emit_opcode(OpCode._true),
            .nil => self.emit_opcode(OpCode.nil),
            else => unreachable,
        }
    }

    fn grouping(self: *Compiler, _: bool) void {
        self.expression();
        self.parser.consume(TokenType.right_paren, "expect ')' after expression");
    }

    fn unary(self: *Compiler, _: bool) void {
        const operator_type = self.parser.previous.t;

        self.parse_precedence(Precedence.unary);

        switch (operator_type) {
            .bang => self.emit_opcode(OpCode.not),
            .minus => self.emit_opcode(OpCode.negate),
            else => return,
        }
    }

    fn binary(self: *Compiler, _: bool) void {
        const operator_type = self.parser.previous.t;
        const rule = get_rule(operator_type);
        self.parse_precedence(@intToEnum(Precedence, @enumToInt(rule.precedence) + 1));

        switch (operator_type) {
            .plus => self.emit_opcode(OpCode.add),
            .minus => self.emit_opcode(OpCode.subtract),
            .star => self.emit_opcode(OpCode.multiply),
            .slash => self.emit_opcode(OpCode.divide),
            .bang_equal => self.emit_opcodes(.{ OpCode.equal, OpCode.not }),
            .equal_equal => self.emit_opcode(OpCode.equal),
            .less => self.emit_opcode(OpCode.less),
            .less_equal => self.emit_opcodes(.{ OpCode.greater, OpCode.not }),
            .greater => self.emit_opcode(OpCode.greater),
            .greater_equal => self.emit_opcodes(.{ OpCode.less, OpCode.not }),
            else => return,
        }
    }

    // Statement compilations helper methods
    fn print_statement(self: *Compiler) void {
        self.expression();
        self.parser.consume(TokenType.semicolon, "expect ';' after value");
        self.emit_opcode(OpCode.print);
    }

    fn if_statement(self: *Compiler) void {
        self.parser.consume(TokenType.left_paren, "expect '(' after if");
        self.expression();
        self.parser.consume(TokenType.right_paren, "expect ')' after if condition");

        const then_jump = self.emit_jump(OpCode.jump_if_false);
        self.emit_opcode(OpCode.pop);

        self.statement();
        const else_jump = self.emit_jump(OpCode.jump);
        self.emit_opcode(OpCode.pop);

        self.patch_jump(then_jump);

        if (self.parser.match(TokenType.cf_else)) {
            self.statement();
        }

        self.patch_jump(else_jump);
    }

    fn expression_statement(self: *Compiler) void {
        self.expression();
        self.parser.consume(TokenType.semicolon, "expect ';' after expression");
        self.emit_opcode(OpCode.pop);
    }

    fn var_declaration(self: *Compiler) void {
        const global = self.parse_variable("expect variable name");

        if (self.parser.match(TokenType.equal)) {
            self.expression();
        } else {
            self.emit_opcode(OpCode.nil);
        }

        self.parser.consume(TokenType.semicolon, "expect ';' after declaration");

        self.define_variable(global);
    }

    fn parse_variable(self: *Compiler, message: []const u8) u8 {
        self.parser.consume(TokenType.identifier, message);
        self.declare_variable();
        if (self.scope_depth > 0) return 0;
        return self.identifier_constant(self.parser.previous);
    }

    fn define_variable(self: *Compiler, index: u8) void {
        if (self.scope_depth > 0) {
            self.mark_initialized();
            return;
        }
        self.emit_compound(OpCode.define_global, index);
    }

    fn identifier_constant(self: *Compiler, token: Token) u8 {
        return self.make_constant(Value.obj(alloc_string(
            self.strings,
            token.start[0..token.length],
            self.allocator,
        ), self.objects));
    }

    fn declare_variable(self: *Compiler) void {
        if (self.scope_depth == 0) return;

        const token = self.parser.previous;
        if (self.local_count > 0) {
            var i = self.local_count - 1;
            while (i >= 0) : (i -= 1) {
                var local: *Local = &self.locals[i];
                if (local.depth != null and local.depth.? < self.scope_depth) break;

                if (token.equals(local.token)) {
                    self.parser.error_at_previous("local variable already delcared");
                }
            }
        }
        self.add_local(token);
    }

    fn add_local(self: *Compiler, token: Token) void {
        if (self.local_count > std.math.maxInt(u8)) {
            self.parser.error_at_previous("too many local variables");
            return;
        }
        var local: *Local = &self.locals[self.local_count];
        self.local_count += 1;
        local.* = .{ .token = token };
    }

    fn mark_initialized(self: *Compiler) void {
        var local: *Local = &self.locals[self.local_count - 1];
        local.depth = self.scope_depth;
    }

    fn resolve_local(self: *Compiler, token: Token) !u8 {
        if (self.local_count == 0) return local_not_found;
        var i = self.local_count - 1;
        while (i >= 0) : (i -= 1) {
            if (token.equals(self.locals[i].token)) {
                if (self.locals[i].depth == null) {
                    self.parser.error_at_previous("can't read local variable in its own initializer");
                }
                return i;
            }
        }

        return local_not_found;
    }

    // Writing Byte Code to chunk helper methods
    fn emit_byte(self: *Compiler, byte: u8) void {
        self.current_chunk.write(byte, self.parser.current.line);
    }

    fn emit_opcode(self: *Compiler, op: OpCode) void {
        self.emit_byte(@enumToInt(op));
    }

    fn emit_opcodes(self: *Compiler, ops: [2]OpCode) void {
        for (ops) |op| {
            self.emit_opcode(op);
        }
    }

    fn emit_compound(self: *Compiler, op: OpCode, byte: u8) void {
        self.emit_opcode(op);
        self.emit_byte(byte);
    }

    fn emit_constant(self: *Compiler, value: Value) void {
        self.emit_compound(OpCode.constant, self.make_constant(value));
    }

    fn make_constant(self: *Compiler, value: Value) u8 {
        const constant_idx = self.current_chunk.add_constant(value);
        if (constant_idx > std.math.maxInt(u8)) {
            self.parser.error_at_previous("too many constants in one chunk");
            return 0;
        }

        return @intCast(u8, constant_idx);
    }

    fn emit_jump(self: *Compiler, op: OpCode) usize {
        self.emit_opcode(op);
        self.emit_byte(PLACEHOLDER_BYTE);
        self.emit_byte(PLACEHOLDER_BYTE);
        return self.current_chunk.code.items.len - 2;
    }

    fn patch_jump(self: *Compiler, offset: usize) void {
        // account for bytes containing the jump value
        const jump = self.current_chunk.code.items.len - (offset + 2);

        if (jump > std.math.maxInt(u16)) {
            self.parser.error_at_previous("too much code to jump over");
        }

        self.current_chunk.code.items[offset] = @intCast(u8, jump >> 8) & PLACEHOLDER_BYTE;
        self.current_chunk.code.items[offset + 1] = @intCast(u8, jump) & PLACEHOLDER_BYTE;
    }

    // helpers for handling scope
    fn begin_scope(self: *Compiler) void {
        self.scope_depth += 1;
    }

    fn end_scope(self: *Compiler) void {
        self.scope_depth -= 1;

        while (self.local_count > 0 and self.locals[self.local_count - 1].depth.? > self.scope_depth) {
            self.emit_opcode(OpCode.pop);
            self.local_count -= 1;
        }
    }

    fn end(self: *Compiler) void {
        self.emit_opcode(OpCode._return);
        if (self.debug and !self.parser.had_error) {
            disassemble_chunk(self.current_chunk, "code");
        }
    }
};

const Parser = struct {
    scanner: *Scanner,
    current: Token,
    previous: Token,
    had_error: bool = false,
    panic_mode: bool = false,

    pub fn init(scanner: *Scanner) Parser {
        return Parser{
            .scanner = scanner,
            .current = scanner.error_token("null token"),
            .previous = scanner.error_token("null token"),
        };
    }

    pub fn advance(self: *Parser) void {
        self.previous = self.current;
        while (true) {
            self.current = self.scanner.scan_token();
            if (self.current.t != TokenType.scan_error) break;

            self.error_at_current(null);
        }
    }

    pub fn consume(self: *Parser, t: TokenType, message: []const u8) void {
        if (self.check(t)) {
            self.advance();
            return;
        }

        self.error_at_current(message);
    }

    pub fn match(self: *Parser, t: TokenType) bool {
        if (!self.check(t)) return false;

        self.advance();
        return true;
    }

    fn check(self: *Parser, t: TokenType) bool {
        return self.current.t == t;
    }

    fn synchronize(self: *Parser) void {
        self.panic_mode = false;

        while (self.current.t != TokenType.eof) : (self.advance()) {
            if (self.previous.t == TokenType.semicolon) return;
            switch (self.current.t) {
                .class => return,
                .fun => return,
                .cf_var => return,
                .cf_for => return,
                .cf_if => return,
                .cf_while => return,
                .print => return,
                .cf_return => return,
                else => {},
            }
        }
    }

    fn error_at_current(self: *Parser, message: ?[]const u8) void {
        self.error_at(self.current, message);
    }

    fn error_at_previous(self: *Parser, message: ?[]const u8) void {
        self.error_at(self.previous, message);
    }

    // TODO (WASM): don't print the errors, but bundle/pass up somehow
    fn error_at(self: *Parser, token: Token, message: ?[]const u8) void {
        if (self.panic_mode) return;

        self.panic_mode = true;

        std.debug.print("[line {d}] Error", .{token.line});

        switch (token.t) {
            .eof => std.debug.print(" at end", .{}),
            .scan_error => {},
            else => std.debug.print(" at {s}", .{token.start[0..token.length]}),
        }

        // if there is no message, use the token content, which in the case of scan_error token's
        // is actually the message itself
        const message_str: []const u8 = message orelse token.start[0..token.length];
        std.debug.print(": {s}\n", .{message_str});
        self.had_error = true;
    }
};
