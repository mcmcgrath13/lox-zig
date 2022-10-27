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

const ParseFn = fn (compiler: *Compiler) void;

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
    .{ .prefix = null, .infix = null, .precedence = .none }, // identifier,
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
    objects: ?*Obj,
    strings: *ObjStringHashMap,
) !?*Obj {
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

    return compiler.compile();
}

pub const Compiler = struct {
    current_chunk: *Chunk,
    parser: *Parser,
    allocator: std.mem.Allocator,
    debug: bool,
    objects: ?*Obj,
    strings: *ObjStringHashMap,

    pub fn compile(self: *Compiler) !?*Obj {
        while (!self.parser.match(TokenType.eof)) {
            self.declaration();
        }
        self.end();

        if (self.parser.had_error) return compile_err;

        return self.objects;
    }

    fn parse_precedence(self: *Compiler, precedence: Precedence) void {
        self.parser.advance();
        const prefix_rule = get_rule(self.parser.previous.t).prefix orelse {
            self.parser.error_at_previous("expect expression");
            return;
        };

        prefix_rule(self);

        while (@enumToInt(precedence) <= @enumToInt(get_rule(self.parser.current.t).precedence)) {
            self.parser.advance();
            // TODO: unclear if this translation is correct
            const infix_rule = get_rule(self.parser.previous.t).infix orelse {
                return;
            };
            infix_rule(self);
        }
    }

    fn declaration(self: *Compiler) void {
        self.statement();
    }

    fn statement(self: *Compiler) void {
        if (self.parser.match(TokenType.print)) {
            self.print_statement();
        } else {
            self.expression_statement();
        }
    }

    fn expression(self: *Compiler) void {
        self.parse_precedence(Precedence.assignment);
    }

    fn number(self: *Compiler) void {
        // TODO: error handling
        const value = std.fmt.parseFloat(
            f64,
            self.parser.previous.start[0..self.parser.previous.length],
        ) catch 0;
        self.emit_constant(Value.number(value));
    }

    fn string(self: *Compiler) void {
        var object = alloc_string(
            self.strings,
            self.parser.previous.start[1 .. self.parser.previous.length - 1],
            self.allocator,
        );
        self.emit_constant(Value.obj(object, &self.objects));
    }

    fn literal(self: *Compiler) void {
        switch (self.parser.previous.t) {
            .logical_false => self.emit_opcode(OpCode._false),
            .logical_true => self.emit_opcode(OpCode._true),
            .nil => self.emit_opcode(OpCode.nil),
            else => unreachable,
        }
    }

    fn grouping(self: *Compiler) void {
        self.expression();
        self.parser.consume(TokenType.right_paren, "expect ')' after expression");
    }

    fn unary(self: *Compiler) void {
        const operator_type = self.parser.previous.t;

        self.parse_precedence(Precedence.unary);

        switch (operator_type) {
            .bang => self.emit_opcode(OpCode.not),
            .minus => self.emit_opcode(OpCode.negate),
            else => return,
        }
    }

    fn binary(self: *Compiler) void {
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

    fn expression_statement(self: *Compiler) void {
        self.expression();
        self.parser.consume(TokenType.semicolon, "expect ';' after expression");
        self.emit_opcode(OpCode.pop);
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

    fn emit_constant(self: *Compiler, value: Value) void {
        self.emit_opcode(OpCode.constant);
        self.emit_byte(self.make_constant(value));
    }

    fn make_constant(self: *Compiler, value: Value) u8 {
        const constant_idx = self.current_chunk.add_constant(value);
        if (constant_idx > std.math.maxInt(u8)) {
            self.parser.error_at_previous("too many constants in one chunk");
            return 0;
        }

        return @intCast(u8, constant_idx);
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
        std.debug.print(": {s}", .{message_str});
        self.had_error = true;
    }
};
