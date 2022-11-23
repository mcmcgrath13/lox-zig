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
const new_function = obj.new_function;
const new_string = obj.new_string;

const ObjStringHashMap = @import("vm.zig").ObjStringHashMap;

const compile_err = error.CompileFailed;

const NotFoundError = error{
    local,
    upvalue,
};

const HIGH_BYTE: u8 = 0xff;

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
    .{ .prefix = Compiler.grouping, .infix = Compiler.call, .precedence = .call }, // left_paren,
    .{ .prefix = null, .infix = null, .precedence = .none }, // right_paren,
    .{ .prefix = null, .infix = null, .precedence = .none }, // left_brace,
    .{ .prefix = null, .infix = null, .precedence = .none }, // right_brace,
    .{ .prefix = null, .infix = null, .precedence = .none }, // comma,
    .{ .prefix = null, .infix = Compiler.dot, .precedence = .call }, // dot,
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
    .{ .prefix = null, .infix = Compiler.logical_and, .precedence = ._and }, // logical_and,
    .{ .prefix = null, .infix = null, .precedence = .none }, // class,
    .{ .prefix = null, .infix = null, .precedence = .none }, // cf_else,
    .{ .prefix = Compiler.literal, .infix = null, .precedence = .none }, // logical_false,
    .{ .prefix = null, .infix = null, .precedence = .none }, // cf_for,
    .{ .prefix = null, .infix = null, .precedence = .none }, // fun,
    .{ .prefix = null, .infix = null, .precedence = .none }, // cf_if,
    .{ .prefix = Compiler.literal, .infix = null, .precedence = .none }, // nil,
    .{ .prefix = null, .infix = Compiler.logical_or, .precedence = ._or }, // logical_or,
    .{ .prefix = null, .infix = null, .precedence = .none }, // print,
    .{ .prefix = null, .infix = null, .precedence = .none }, // cf_return,
    .{ .prefix = Compiler.super, .infix = null, .precedence = .none }, // super,
    .{ .prefix = Compiler.this, .infix = null, .precedence = .none }, // this,
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
    allocator: std.mem.Allocator,
    debug: bool,
    objects: *?*Obj,
    strings: *ObjStringHashMap,
) !*Obj {
    var scanner = Scanner.init(source);
    var parser = Parser.init(&scanner);
    parser.advance();

    var compiler = Compiler.init(
        FunctionType.script,
        &parser,
        allocator,
        debug,
        objects,
        strings,
    );

    return try compiler.compile();
}

const Local = struct {
    token: Token,
    depth: ?u8 = null,
    is_captured: bool = false,
};

const UpValue = struct {
    is_local: bool,
    index: u8,
};

const FunctionType = enum {
    function,
    initializer,
    method,
    script,
};

const ClassCompiler = struct {
    enclosing: ?*ClassCompiler,
    has_superclass: bool = false,
};

pub const Compiler = struct {
    function: *Obj,
    function_type: FunctionType,

    parser: *Parser,

    allocator: std.mem.Allocator,

    debug: bool,

    objects: *?*Obj,
    strings: *ObjStringHashMap,

    locals: [std.math.maxInt(u8)]Local = undefined,
    local_count: u8 = 0,
    scope_depth: u8 = 0,

    upvalues: [std.math.maxInt(u8)]UpValue = undefined,

    enclosing: ?*Compiler = null,

    current_class: ?*ClassCompiler = null,

    pub fn init(
        function_type: FunctionType,
        parser: *Parser,
        allocator: std.mem.Allocator,
        debug: bool,
        objects: *?*Obj,
        strings: *ObjStringHashMap,
    ) Compiler {
        var function_obj = new_function(objects, allocator);

        var compiler = Compiler{
            .function = function_obj,
            .function_type = function_type,
            .parser = parser,
            .allocator = allocator,
            .debug = debug,
            .objects = objects,
            .strings = strings,
        };

        // reserve the first local slot for compiler use
        if (function_type != .function) {
            compiler.add_local(Token.synthetic(TokenType.this, "this"));
        } else {
            compiler.add_local(Token.synthetic(TokenType.nil, ""));
        }
        compiler.locals[0].depth = 0;
        compiler.mark_initialized();

        if (function_type != FunctionType.script) {
            function_obj.as_function().name = new_string(
                strings,
                parser.previous.start[0..parser.previous.length],
                objects,
                allocator,
                false,
            ).as_string();
        }

        return compiler;
    }

    fn init_with(
        self: *Compiler,
        function_type: FunctionType,
    ) Compiler {
        var comp = Compiler.init(
            function_type,
            self.parser,
            self.allocator,
            self.debug,
            self.objects,
            self.strings,
        );
        comp.enclosing = self;
        comp.current_class = self.current_class;
        return comp;
    }

    pub fn compile(self: *Compiler) !*Obj {
        while (!self.parser.match(TokenType.eof)) {
            self.declaration();
        }

        if (self.parser.had_error) return compile_err;

        return self.end();
    }

    pub fn current_chunk(self: *Compiler) *Chunk {
        return &self.function.as_function().chunk;
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
        if (self.parser.match(TokenType.class)) {
            self.class_declaration();
        } else if (self.parser.match(TokenType.fun)) {
            self.fun_declaration();
        } else if (self.parser.match(TokenType.cf_var)) {
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
        } else if (self.parser.match(TokenType.cf_return)) {
            self.return_statement();
        } else if (self.parser.match(TokenType.cf_if)) {
            self.if_statement();
        } else if (self.parser.match(TokenType.cf_while)) {
            self.while_statement();
        } else if (self.parser.match(TokenType.cf_for)) {
            self.for_statement();
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

        self.parser.consume(
            TokenType.right_brace,
            "expect '}' at end of block",
        );
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
        var object = new_string(
            self.strings,
            self.parser.previous.start[1 .. self.parser.previous.length - 1],
            self.objects,
            self.allocator,
            false,
        );
        self.emit_constant(Value.obj(object));
    }

    fn variable(self: *Compiler, can_assign: bool) void {
        self.named_variable(self.parser.previous, can_assign);
    }

    fn this(self: *Compiler, _: bool) void {
        if (self.current_class == null) {
            self.parser.error_at_previous("can't use 'this' outside of a class");
            return;
        }
        self.variable(false);
    }

    fn super(self: *Compiler, _: bool) void {
        if (self.current_class == null) {
            self.parser.error_at_previous("can't use 'super' outside of a class");
        } else if (!self.current_class.?.has_superclass) {
            self.parser.error_at_previous("can't use 'super' in a class without a superclass");
        }

        self.parser.consume(TokenType.dot, "expect '.' after 'super'");
        self.parser.consume(TokenType.identifier, "expect superclass method name");
        const name_idx = self.identifier_constant(self.parser.previous);

        self.named_variable(Token.synthetic(TokenType.this, "this"), false);
        if (self.parser.match(TokenType.left_paren)) {
            const arg_count = self.argument_list();
            self.named_variable(Token.synthetic(TokenType.super, "super"), false);
            self.emit_compound(OpCode.super_invoke, name_idx);
            self.emit_byte(arg_count);
        } else {
            self.named_variable(Token.synthetic(TokenType.super, "super"), false);
            self.emit_compound(OpCode.get_super, name_idx);
        }
    }

    fn named_variable(self: *Compiler, token: Token, can_assign: bool) void {
        var get_op = OpCode.get_local;
        var set_op = OpCode.set_local;
        var arg = self.resolve_local(token) catch blk: {
            get_op = OpCode.get_upvalue;
            set_op = OpCode.set_upvalue;
            break :blk self.resolve_upvalue(token) catch inner: {
                get_op = OpCode.get_global;
                set_op = OpCode.set_global;
                break :inner self.identifier_constant(token);
            };
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
        self.parser.consume(
            TokenType.right_paren,
            "expect ')' after expression",
        );
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
        self.parse_precedence(@intToEnum(
            Precedence,
            @enumToInt(rule.precedence) + 1,
        ));

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

    fn call(self: *Compiler, _: bool) void {
        const arg_count = self.argument_list();
        self.emit_compound(OpCode.call, arg_count);
    }

    fn dot(self: *Compiler, can_assign: bool) void {
        self.parser.consume(TokenType.identifier, "expect identifier after '.'");
        const name = self.identifier_constant(self.parser.previous);

        if (can_assign and self.parser.match(TokenType.equal)) {
            self.expression();
            self.emit_compound(OpCode.set_property, name);
        } else if (self.parser.match(TokenType.left_paren)) {
            const arg_count = self.argument_list();
            self.emit_compound(OpCode.invoke, name);
            self.emit_byte(arg_count);
        } else {
            self.emit_compound(OpCode.get_property, name);
        }
    }

    fn logical_and(self: *Compiler, _: bool) void {
        const end_jump = self.emit_jump(OpCode.jump_if_false);
        self.emit_opcode(OpCode.pop);

        self.parse_precedence(Precedence._and);

        self.patch_jump(end_jump);
    }

    fn logical_or(self: *Compiler, _: bool) void {
        const else_jump = self.emit_jump(OpCode.jump_if_false);
        const end_jump = self.emit_jump(OpCode.jump);

        self.patch_jump(else_jump);
        self.emit_opcode(OpCode.pop);

        self.parse_precedence(Precedence._or);
        self.patch_jump(end_jump);
    }

    // Statement compilations helper methods
    fn print_statement(self: *Compiler) void {
        self.expression();
        self.parser.consume(TokenType.semicolon, "expect ';' after value");
        self.emit_opcode(OpCode.print);
    }

    fn return_statement(self: *Compiler) void {
        if (self.function_type == .script) {
            self.parser.error_at_previous("can't return from top-level code");
        }

        if (self.parser.match(TokenType.semicolon)) {
            self.emit_return();
        } else {
            if (self.function_type == .initializer) {
                self.parser.error_at_previous("can't return a value from an initializer");
            }
            self.expression();
            self.parser.consume(TokenType.semicolon, "expect ';' after return");
            self.emit_opcode(OpCode._return);
        }
    }

    fn if_statement(self: *Compiler) void {
        self.parser.consume(TokenType.left_paren, "expect '(' after if");
        self.expression();
        self.parser.consume(
            TokenType.right_paren,
            "expect ')' after if condition",
        );

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

    fn while_statement(self: *Compiler) void {
        const loop_start = self.current_chunk().length();

        self.parser.consume(TokenType.left_paren, "expect '(' after while");
        self.expression();
        self.parser.consume(
            TokenType.right_paren,
            "expect ')' after while condition",
        );

        const end_jump = self.emit_jump(OpCode.jump_if_false);
        self.emit_opcode(OpCode.pop);
        self.statement();
        self.emit_loop(loop_start);

        self.patch_jump(end_jump);
        self.emit_opcode(OpCode.pop);
    }

    fn for_statement(self: *Compiler) void {
        self.begin_scope();

        self.parser.consume(TokenType.left_paren, "expect '(' after for");
        if (self.parser.match(TokenType.semicolon)) {} else if (self.parser.match(TokenType.cf_var)) {
            self.var_declaration();
        } else {
            self.expression_statement();
        }

        var loop_start = self.current_chunk().length();
        var exit_jump: ?usize = null;

        if (!self.parser.match(TokenType.semicolon)) {
            self.expression();
            self.parser.consume(
                TokenType.semicolon,
                "expect ';' after for condition",
            );

            exit_jump = self.emit_jump(OpCode.jump_if_false);
            self.emit_opcode(OpCode.pop);
        }

        if (!self.parser.match(TokenType.right_paren)) {
            const body_jump = self.emit_jump(OpCode.jump);
            const incr_start = self.current_chunk().length();
            self.expression();
            self.emit_opcode(OpCode.pop);
            self.parser.consume(
                TokenType.right_paren,
                "expect ')' after while increment",
            );

            self.emit_loop(loop_start);
            loop_start = incr_start;
            self.patch_jump(body_jump);
        }

        self.statement();
        self.emit_loop(loop_start);

        if (exit_jump) |jump| {
            self.patch_jump(jump);
            self.emit_opcode(OpCode.pop);
        }

        self.end_scope();
    }

    fn expression_statement(self: *Compiler) void {
        self.expression();
        self.parser.consume(TokenType.semicolon, "expect ';' after expression");
        self.emit_opcode(OpCode.pop);
    }

    fn fun_declaration(self: *Compiler) void {
        const global = self.parse_variable("expect function name");
        self.mark_initialized();
        self.fun(FunctionType.function);
        self.define_variable(global);
    }

    fn fun(self: *Compiler, function_type: FunctionType) void {
        var function_compiler = self.init_with(function_type);
        function_compiler.begin_scope();
        var obj_function = function_compiler.function.as_function();

        function_compiler.parser.consume(
            TokenType.left_paren,
            "expect '(' after function name",
        );
        if (!self.parser.check(TokenType.right_paren)) {
            while (true) {
                obj_function.arity += 1;
                if (obj_function.arity > std.math.maxInt(u8)) {
                    self.parser.error_at_current("can't have more than 255 parameters");
                }
                const constant_idx = function_compiler.parse_variable("expect parameter name");
                function_compiler.define_variable(constant_idx);

                if (!self.parser.match(TokenType.comma)) break;
            }
        }

        function_compiler.parser.consume(
            TokenType.right_paren,
            "expect ')' after function args",
        );
        function_compiler.parser.consume(
            TokenType.left_brace,
            "expect '{' after function declaration",
        );
        function_compiler.block();

        var function_obj = function_compiler.end();
        self.emit_compound(
            OpCode.closure,
            self.make_constant(Value.obj(function_obj)),
        );

        var i: u8 = 0;
        while (i < function_obj.as_function().upvalue_count) : (i += 1) {
            self.emit_byte(if (function_compiler.upvalues[i].is_local) 1 else 0);
            self.emit_byte(function_compiler.upvalues[i].index);
        }
    }

    fn class_declaration(self: *Compiler) void {
        self.parser.consume(TokenType.identifier, "expect class name");
        const name = self.parser.previous;
        const name_index = self.identifier_constant(name);
        self.declare_variable();

        self.emit_compound(OpCode.class, name_index);
        self.define_variable(name_index);

        var class_compiler = ClassCompiler{ .enclosing = self.current_class };
        self.current_class = &class_compiler;

        if (self.parser.match(TokenType.less)) {
            self.parser.consume(TokenType.identifier, "expect superclass name");
            self.variable(false);

            if (name.equals(self.parser.previous)) {
                self.parser.error_at_previous("class can't inherit from itself");
            }

            self.begin_scope();
            self.add_local(Token.synthetic(TokenType.super, "super"));
            self.define_variable(0);

            self.named_variable(name, false);
            self.emit_opcode(OpCode.inherit);
            class_compiler.has_superclass = true;
        }

        self.named_variable(name, false);

        self.parser.consume(TokenType.left_brace, "expect '{' before class body");
        while (!self.parser.check(TokenType.right_brace) and !self.parser.check(TokenType.eof)) {
            self.method();
        }
        self.parser.consume(TokenType.right_brace, "expect '}' after class bod");
        self.emit_opcode(OpCode.pop);

        if (class_compiler.has_superclass) {
            self.end_scope();
        }

        self.current_class = self.current_class.?.enclosing;
    }

    fn method(self: *Compiler) void {
        self.parser.consume(TokenType.identifier, "expect method name");
        const name_index = self.identifier_constant(self.parser.previous);

        const fun_type = if (self.parser.previous.length == 4 and std.mem.eql(u8, self.parser.previous.start[0..4], "init")) FunctionType.initializer else FunctionType.method;
        self.fun(fun_type);

        self.emit_compound(OpCode.method, name_index);
    }

    fn var_declaration(self: *Compiler) void {
        const global = self.parse_variable("expect variable name");

        if (self.parser.match(TokenType.equal)) {
            self.expression();
        } else {
            self.emit_opcode(OpCode.nil);
        }

        self.parser.consume(
            TokenType.semicolon,
            "expect ';' after declaration",
        );

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

    fn argument_list(self: *Compiler) u8 {
        var count: u8 = 0;
        if (!self.parser.check(TokenType.right_paren)) {
            while (true) {
                if (count == std.math.maxInt(u8)) {
                    self.parser.error_at_previous("can't have more than 255 arguments");
                    break;
                }
                count += 1;
                self.expression();

                if (!self.parser.match(TokenType.comma)) break;
            }
        }
        self.parser.consume(
            TokenType.right_paren,
            "expect ')' after argument list",
        );

        return count;
    }

    fn identifier_constant(self: *Compiler, token: Token) u8 {
        return self.make_constant(Value.obj(new_string(
            self.strings,
            token.start[0..token.length],
            self.objects,
            self.allocator,
            false,
        )));
    }

    fn declare_variable(self: *Compiler) void {
        if (self.scope_depth == 0) return;

        const token = self.parser.previous;
        if (self.local_count > 0) {
            var i = self.local_count;
            while (i > 0) : (i -= 1) {
                var local: *Local = &self.locals[i - 1];
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
        if (self.scope_depth == 0) return;
        var local: *Local = &self.locals[self.local_count - 1];
        local.depth = self.scope_depth;
    }

    fn resolve_local(self: *Compiler, token: Token) !u8 {
        var i = self.local_count;
        while (i > 0) : (i -= 1) {
            const local_idx = i - 1;
            if (token.equals(self.locals[local_idx].token)) {
                if (self.locals[local_idx].depth == null) {
                    self.parser.error_at_previous("can't read local variable in its own initializer");
                }
                return local_idx;
            }
        }

        return NotFoundError.local;
    }

    fn resolve_upvalue(self: *Compiler, token: Token) NotFoundError!u8 {
        if (self.enclosing == null) return NotFoundError.upvalue;

        const local_idx = self.enclosing.?.resolve_local(token) catch {
            const upvalue = try self.enclosing.?.resolve_upvalue(token);
            return self.add_upvalue(upvalue, false);
        };

        self.enclosing.?.locals[local_idx].is_captured = true;
        return self.add_upvalue(local_idx, true);
    }

    fn add_upvalue(self: *Compiler, index: u8, is_local: bool) u8 {
        const upvalue_count = self.function.as_function().upvalue_count;
        var i: u8 = 0;
        while (i < upvalue_count) : (i += 1) {
            var upvalue = &self.upvalues[i];
            if (upvalue.index == index and upvalue.is_local == is_local) {
                return i;
            }
        }

        if (i == std.math.maxInt(u8)) {
            self.parser.error_at_previous("too many closure variables in function");
            return 0;
        }

        self.upvalues[upvalue_count].is_local = is_local;
        self.upvalues[upvalue_count].index = index;
        self.function.as_function().upvalue_count += 1;
        return upvalue_count;
    }

    // Writing Byte Code to chunk helper methods
    fn emit_byte(self: *Compiler, byte: u8) void {
        self.current_chunk().write(byte, self.parser.current.line);
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
        const constant_idx = self.current_chunk().add_constant(value);
        if (constant_idx > std.math.maxInt(u8)) {
            self.parser.error_at_previous("too many constants in one chunk");
            return 0;
        }

        return @intCast(u8, constant_idx);
    }

    fn emit_jump(self: *Compiler, op: OpCode) usize {
        self.emit_opcode(op);
        self.emit_byte(HIGH_BYTE);
        self.emit_byte(HIGH_BYTE);
        return self.current_chunk().length() - 2;
    }

    fn patch_jump(self: *Compiler, offset: usize) void {
        // account for bytes containing the jump value
        const jump = self.current_chunk().length() - (offset + 2);

        if (jump > std.math.maxInt(u16)) {
            self.parser.error_at_previous("too much code to jump over");
        }

        self.current_chunk().code.items[offset] = @intCast(u8, jump >> 8) & HIGH_BYTE;
        self.current_chunk().code.items[offset + 1] = @intCast(u8, jump) & HIGH_BYTE;
    }

    fn emit_loop(self: *Compiler, loop_start: usize) void {
        self.emit_opcode(OpCode.loop);

        const jump = self.current_chunk().length() - loop_start + 2;
        if (jump > std.math.maxInt(u16)) {
            self.parser.error_at_previous("too much code in loop");
        }

        self.emit_byte(@intCast(u8, jump >> 8) & HIGH_BYTE);
        self.emit_byte(@intCast(u8, jump) & HIGH_BYTE);
    }

    fn emit_return(self: *Compiler) void {
        if (self.function_type == FunctionType.initializer) {
            self.emit_compound(OpCode.get_local, 0);
        } else {
            self.emit_opcode(OpCode.nil);
        }
        self.emit_opcode(OpCode._return);
    }

    // helpers for handling scope
    fn begin_scope(self: *Compiler) void {
        self.scope_depth += 1;
    }

    fn end_scope(self: *Compiler) void {
        self.scope_depth -= 1;

        var local = self.locals[self.local_count - 1];
        while (self.local_count > 0 and local.depth != null and local.depth.? > self.scope_depth) : (local = self.locals[self.local_count - 1]) {
            if (local.is_captured) {
                self.emit_opcode(OpCode.close_upvalue);
            } else {
                self.emit_opcode(OpCode.pop);
            }
            self.local_count -= 1;
        }
    }

    fn end(self: *Compiler) *Obj {
        self.emit_return();
        if (self.debug and !self.parser.had_error) {
            disassemble_chunk(self.current_chunk(), self.function.as_function());
        }

        return self.function;
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
