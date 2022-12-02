const std = @import("std");
const HashMap = std.HashMap;

const chnk = @import("chunk.zig");
const Chunk = chnk.Chunk;
const OpCode = chnk.OpCode;

const vlu = @import("value.zig");
const Value = vlu.Value;

const compile = @import("compile.zig").compile;

const GCAllocator = @import("gc.zig").GCAllocator;

const disassemble_instruction = @import("debug.zig").disassemble_instruction;

const obj = @import("object.zig");
const Obj = obj.Obj;
const new_string = obj.new_string;
const ObjString = obj.ObjString;
const ObjStringContext = obj.ObjStringContext;
const new_native = obj.new_native;
const NativeFn = obj.NativeFn;
const ObjClosure = obj.ObjClosure;
const new_closure = obj.new_closure;
const ObjUpValue = obj.ObjUpValue;
const new_upvalue = obj.new_upvalue;
const ObjClass = obj.ObjClass;
const new_class = obj.new_class;
const new_instance = obj.new_instance;
const new_bound_method = obj.new_bound_method;

const common = @import("common.zig");

const log = std.log.scoped(.runtime);

const FRAME_MAX = 64;
const STACK_MAX = FRAME_MAX * std.math.maxInt(u8);

pub const ObjStringHashMap = HashMap(
    *ObjString,
    void,
    ObjStringContext,
    std.hash_map.default_max_load_percentage,
);

pub const VariableHashMap = std.AutoHashMap(*ObjString, Value);

pub const InterpretError = error{
    compile,
    runtime,
};

pub const Options = struct {
    debug_runtime: bool = false,
    debug_compiler: bool = false,
    debug_stress_gc: bool = false,
    debug_gc: bool = false,

    pub fn init_all() Options {
        return Options{
            .debug_runtime = true,
            .debug_compiler = true,
            .debug_stress_gc = true,
            .debug_gc = true,
        };
    }
};

const CallFrame = struct {
    closure: *ObjClosure,
    ip: [*]u8,
    slots_start: [*]Value,

    fn init(closure: *ObjClosure, slots_start: [*]Value) CallFrame {
        return .{
            .closure = closure,
            .ip = closure.function.chunk.code.items.ptr,
            .slots_start = slots_start,
        };
    }

    fn read_byte(self: *CallFrame) u8 {
        const byte = self.ip[0];
        self.ip += 1;
        return byte;
    }

    fn read_constant(self: *CallFrame) Value {
        return self.closure.function.chunk.constants.items[self.read_byte()];
    }

    fn read_short(self: *CallFrame) u16 {
        return common.read_short(.{ self.read_byte(), self.read_byte() });
    }

    fn read_string(self: *CallFrame) *ObjString {
        return self.read_constant().as_obj().as_string();
    }
};

extern fn wasm_print(msg_ptr: [*]const u8, msg_len: usize) void;

pub const VM = struct {
    frames: [FRAME_MAX]CallFrame = undefined,
    frame_count: u8 = 0,

    allocator: std.mem.Allocator,

    stack: []Value,
    stack_top: [*]Value,

    objects: ?*Obj = null,
    open_upvalues: ?*ObjUpValue = null,
    strings: ObjStringHashMap,
    init_string: ?*ObjString = null,

    globals: VariableHashMap,

    options: Options,

    compiling: bool = false,

    pub fn init(options: Options, gc: *GCAllocator) VM {
        const gc_allocator = gc.allocator();
        const strings = ObjStringHashMap.init(gc_allocator);
        const globals = VariableHashMap.init(gc_allocator);
        const stack = common.alloc_or_die(gc_allocator, Value, STACK_MAX);
        var vm = VM{
            .options = options,
            .allocator = gc_allocator,
            .strings = strings,
            .globals = globals,
            .stack = stack,
            .stack_top = stack.ptr,
        };
        vm.define_native("clock", clock_native);
        vm.init_string = new_string(&vm.strings, "init", &vm.objects, vm.allocator, false).as_string();
        return vm;
    }

    pub fn deinit(self: *VM) void {
        self.free_objects();
        self.strings.deinit();
        self.globals.deinit();
        self.allocator.free(self.stack);
    }

    pub fn free_objects(self: *VM) void {
        var object: ?*Obj = self.objects;
        while (object) |o| {
            const next: ?*Obj = o.next;
            o.deinit(self.allocator);
            self.allocator.destroy(o);
            object = next;
        }
    }

    fn reset_stack(self: *VM) void {
        self.stack_top = self.stack.ptr;
    }

    fn current_frame(self: *VM) *CallFrame {
        return &self.frames[self.frame_count - 1];
    }

    fn push(self: *VM, value: Value) void {
        self.stack_top.* = value;
        self.stack_top += 1;
    }

    fn pop(self: *VM) Value {
        self.stack_top -= 1;
        return self.stack_top[0];
    }

    fn peek(self: *VM, distance: usize) Value {
        return (self.stack_top - 1 - distance)[0];
    }

    fn call(self: *VM, closure: *ObjClosure, arg_count: u8) !void {
        if (arg_count != closure.function.arity) {
            self.runtime_error(
                "expected {d} arguments, but got {d}\n",
                .{ closure.function.arity, arg_count },
            );
        }

        if (self.frame_count == FRAME_MAX) {
            self.runtime_error("stack overflow\n", .{});
            return InterpretError.runtime;
        }

        var frame = &self.frames[self.frame_count];
        self.frame_count += 1;

        frame.* = CallFrame.init(closure, self.stack_top - arg_count - 1);
    }

    pub fn interpret(self: *VM, source: []const u8) InterpretError!void {
        self.compiling = true;
        var function_obj = compile(
            source,
            self.allocator,
            self.options.debug_compiler,
            &self.objects,
            &self.strings,
        ) catch {
            return InterpretError.compile;
        };
        self.compiling = false;

        self.push(Value.obj(function_obj));
        var closure_obj = new_closure(
            function_obj.as_function(),
            &self.objects,
            self.allocator,
        );
        _ = self.pop();
        self.push(Value.obj(closure_obj));
        try self.call(closure_obj.as_closure(), 0);

        return self.run();
    }

    fn run(self: *VM) InterpretError!void {
        var frame = self.current_frame();

        while (true) {
            if (self.options.debug_runtime) {
                // print stack
                log.debug("          ", .{});
                for (self.stack) |value, i| {
                    if (@ptrToInt(self.stack.ptr + i) == @ptrToInt(self.stack_top)) {
                        break;
                    }
                    log.debug("[{}]", .{value});
                }
                log.debug("\n", .{});

                _ = disassemble_instruction(&frame.closure.function.chunk, @ptrToInt(frame.ip) - @ptrToInt(frame.closure.function.chunk.code.items.ptr));
            }
            const instruction = frame.read_byte();
            try switch (@intToEnum(OpCode, instruction)) {
                .constant => {
                    const constant = frame.read_constant();
                    self.push(constant);
                },
                .define_global => {
                    var name = frame.read_string();
                    self.globals.put(name, self.peek(0)) catch {
                        @panic("Out of memory!\n");
                    };
                    _ = self.pop();
                },
                .get_global => {
                    var name = frame.read_string();
                    if (self.globals.get(name)) |value| {
                        self.push(value);
                    } else {
                        self.runtime_error("undefined variable '{s}'\n", .{name.data});
                        return InterpretError.runtime;
                    }
                },
                .set_global => {
                    var name = frame.read_string();
                    if (self.globals.getPtr(name)) |value_ptr| {
                        value_ptr.* = self.peek(0);
                    } else {
                        self.runtime_error("can't assign to undefined variable '{s}'\n", .{name.data});
                        return InterpretError.runtime;
                    }
                },
                .get_local => {
                    const idx = frame.read_byte();
                    self.push(frame.slots_start[idx]);
                },
                .set_local => {
                    const idx = frame.read_byte();
                    frame.slots_start[idx] = self.peek(0);
                },
                .get_property => {
                    if (!self.peek(0).is_instance()) {
                        self.runtime_error("only class instances have properties\n", .{});
                        return InterpretError.runtime;
                    }

                    var instance = self.peek(0).as_obj().as_instance();
                    var name = frame.read_string();

                    if (instance.fields.get(name)) |value| {
                        _ = self.pop();
                        self.push(value);
                    } else {
                        try self.bind_method(instance.class, name);
                    }
                },
                .set_property => {
                    if (!self.peek(1).is_instance()) {
                        self.runtime_error("only class instances have properties\n", .{});
                        return InterpretError.runtime;
                    }

                    var instance = self.peek(1).as_obj().as_instance();
                    var name = frame.read_string();

                    instance.fields.put(name, self.peek(0)) catch {
                        self.runtime_error("out of memory\n", .{});
                        return InterpretError.runtime;
                    };

                    var value = self.pop();
                    _ = self.pop();
                    self.push(value);
                },
                .get_upvalue => {
                    const slot = frame.read_byte();
                    self.push(frame.closure.upvalues[slot].?.location.*);
                },
                .set_upvalue => {
                    const slot = frame.read_byte();
                    frame.closure.upvalues[slot].?.location.* = self.peek(0);
                },
                .close_upvalue => {
                    self.close_upvalues(self.stack_top - 1);
                    _ = self.pop();
                },
                .get_super => {
                    const method_name = frame.read_string();
                    const superclass = self.pop().as_obj().as_class();
                    try self.bind_method(superclass, method_name);
                },
                .jump_if_false => {
                    const jump = frame.read_short();
                    if (self.peek(0).is_falsey()) {
                        frame.ip += jump;
                    }
                },
                .jump => {
                    const jump = frame.read_short();
                    frame.ip += jump;
                },
                .loop => {
                    const jump = frame.read_short();
                    frame.ip -= jump;
                },
                ._return => {
                    const result = self.pop();
                    self.close_upvalues(frame.slots_start);
                    self.frame_count -= 1;
                    if (self.frame_count == 0) {
                        _ = self.pop();
                        return;
                    }

                    self.stack_top = frame.slots_start;
                    self.push(result);
                    frame = self.current_frame();
                },
                .print => {
                    if (@hasField(std.os.system, "fd_t")) {
                        std.io.getStdOut().writer().print("{}\n", .{self.pop()}) catch {
                            return InterpretError.runtime;
                        }; 
                    } else {
                        // WASM
                        var str = std.fmt.allocPrint(self.allocator, "{}\n", .{self.pop()}) catch {
                            return InterpretError.runtime;
                        };
                        defer self.allocator.free(str);
                        wasm_print(str.ptr, str.len);
                    }
                                       
                },
                .pop => {
                    _ = self.pop();
                },
                .call => {
                    const arg_count = frame.read_byte();
                    try self.call_value(self.peek(arg_count), arg_count);
                    frame = self.current_frame();
                },
                .closure => {
                    var fun = frame.read_constant().as_obj().as_function();
                    var closure = new_closure(fun, &self.objects, self.allocator);
                    self.push(Value.obj(closure));
                    for (closure.as_closure().upvalues) |*upvalue| {
                        const is_local = frame.read_byte();
                        const index = frame.read_byte();
                        if (is_local == 1) {
                            upvalue.* = self.capture_upvalue(&frame.slots_start[index]);
                        } else {
                            upvalue.* = frame.closure.upvalues[index];
                        }
                    }
                },
                .class => {
                    var name = frame.read_string();
                    self.push(Value.obj(new_class(name, &self.objects, self.allocator)));
                },
                .method => {
                    const name = frame.read_string();
                    const method = self.peek(0);
                    var class = self.peek(1).as_obj().as_class();
                    class.methods.put(name, method) catch {
                        self.runtime_error("out of memory\n", .{});
                    };
                    _ = self.pop();
                },
                .invoke => {
                    const name = frame.read_string();
                    const arg_count = frame.read_byte();
                    try (self.invoke(name, arg_count));
                    frame = &self.frames[self.frame_count - 1];
                },
                .super_invoke => {
                    const method_name = frame.read_string();
                    const arg_count = frame.read_byte();
                    const superclass = self.pop().as_obj().as_class();
                    try self.invoke_from_class(superclass, method_name, arg_count);
                    frame = &self.frames[self.frame_count - 1];
                },
                .inherit => {
                    const superclass = self.peek(1);
                    if (!superclass.is_class()) {
                        self.runtime_error("superclass must be a class\n", .{});
                        return InterpretError.runtime;
                    }
                    var subclass = self.peek(0).as_obj().as_class();
                    subclass.inherit(superclass.as_obj().as_class());
                    _ = self.pop();
                },

                // literals
                .nil => self.push(Value.nil()),
                ._true => self.push(Value.boolean(true)),
                ._false => self.push(Value.boolean(false)),

                // unary
                .not => self.push(Value.boolean(self.pop().is_falsey())),
                .negate => {
                    if (!self.peek(0).is_number()) {
                        self.runtime_error("operand must be a number: found {s}\n", .{self.peek(0)});
                        return InterpretError.runtime;
                    }
                    self.push(Value.number(-1 * self.pop().as_number()));
                },

                //binary
                .add => {
                    if (self.peek(0).is_string() and self.peek(1).is_string()) {
                        try self.concatenate();
                    } else {
                        // TODO: fix error message
                        try self.binary_op(f64, Value.number, add);
                    }
                },
                .divide => try self.binary_op(f64, Value.number, divide),
                .multiply => try self.binary_op(f64, Value.number, multiply),
                .subtract => try self.binary_op(f64, Value.number, subtract),
                .equal => {
                    const right: Value = self.pop();
                    const left: Value = self.pop();
                    self.push(Value.boolean(left.equals(right)));
                },
                .less => self.binary_op(bool, Value.boolean, less),
                .greater => self.binary_op(bool, Value.boolean, greater),

                // fallthrough
                _ => return InterpretError.runtime,
            };
        }
    }

    fn binary_op(
        self: *VM,
        comptime R: type,
        value_type: (fn (R) Value),
        op: (fn (f64, f64) R),
    ) InterpretError!void {
        if (!self.peek(0).is_number() or !self.peek(1).is_number()) {
            self.runtime_error(
                "operands must be numbers, found: {s} {s}\n",
                .{ self.peek(0), self.peek(1) },
            );
            return InterpretError.runtime;
        }
        const right = self.pop().as_number();
        const left = self.pop().as_number();
        self.push(value_type(op(left, right)));
    }

    fn concatenate(self: *VM) InterpretError!void {
        const b: []const u8 = self.peek(0).as_obj().as_string().data;
        const a: []const u8 = self.peek(1).as_obj().as_string().data;
        const data = std.mem.concat(self.allocator, u8, &[_][]const u8{ a, b }) catch {
            self.runtime_error("out of memory\n", .{});
            return InterpretError.runtime;
        };
        _ = self.pop();
        _ = self.pop();
        self.push(
            Value.obj(new_string(
                &self.strings,
                data,
                &self.objects,
                self.allocator,
                true,
            )),
        );
    }

    fn call_value(
        self: *VM,
        callee: Value,
        arg_count: u8,
    ) !void {
        if (callee.is_obj()) {
            var obj_val = callee.as_obj();
            switch (obj_val.t) {
                .closure => {
                    return self.call(obj_val.as_closure(), arg_count);
                },
                .native => {
                    const native = obj_val.as_native();
                    const result = native.function(
                        arg_count,
                        self.stack_top - arg_count,
                    );
                    self.stack_top -= arg_count + 1;
                    self.push(result);
                    return;
                },
                .class => {
                    var class = obj_val.as_class();
                    (self.stack_top - arg_count - 1).* = Value.obj(new_instance(class, &self.objects, self.allocator));
                    if (class.methods.get(self.init_string.?)) |initializer| {
                        return self.call(initializer.as_obj().as_closure(), arg_count);
                    } else if (arg_count > 0) {
                        self.runtime_error("expected 0 arguments, but got {d}\n", .{arg_count});
                        return InterpretError.runtime;
                    }
                    return;
                },
                .bound_method => {
                    var bound = obj_val.as_bound_method();
                    (self.stack_top - arg_count - 1).* = bound.receiver;
                    return self.call(bound.method, arg_count);
                },
                else => {},
            }
        }
        self.runtime_error("can only call functions and classes\n", .{});
        return InterpretError.runtime;
    }

    fn bind_method(self: *VM, class: *ObjClass, name: *ObjString) !void {
        if (class.methods.get(name)) |method| {
            var bound_method = new_bound_method(
                self.peek(0),
                method.as_obj().as_closure(),
                &self.objects,
                self.allocator,
            );

            _ = self.pop();
            self.push(Value.obj(bound_method));
        } else {
            self.runtime_error("undefined property: '{s}'", .{name});
            return InterpretError.runtime;
        }
    }

    fn invoke(self: *VM, name: *ObjString, arg_count: u8) !void {
        var receiver = self.peek(arg_count);
        if (!receiver.is_instance()) {
            self.runtime_error("only instances have methods\n", .{});
            return InterpretError.runtime;
        }
        var instance = receiver.as_obj().as_instance();
        if (instance.fields.get(name)) |value| {
            (self.stack_top - arg_count - 1).* = value;
            return self.call_value(value, arg_count);
        }
        try self.invoke_from_class(instance.class, name, arg_count);
    }

    fn invoke_from_class(
        self: *VM,
        class: *ObjClass,
        name: *ObjString,
        arg_count: u8,
    ) !void {
        if (class.methods.get(name)) |method| {
            try self.call(method.as_obj().as_closure(), arg_count);
        } else {
            self.runtime_error("undefined property {s}\n", .{name});
            return InterpretError.runtime;
        }
    }

    fn capture_upvalue(self: *VM, value: *Value) *ObjUpValue {
        var prev: ?*ObjUpValue = null;
        var upvalue = self.open_upvalues;
        while (upvalue != null and @ptrToInt(upvalue.?.location) > @ptrToInt(value)) {
            prev = upvalue;
            upvalue = upvalue.?.next;
        }
        if (upvalue != null and upvalue.?.location == value) {
            return upvalue.?;
        }

        var created_upvalue = new_upvalue(value, &self.objects, self.allocator).as_upvalue();
        created_upvalue.next = upvalue;
        if (prev) |uv| {
            uv.next = created_upvalue;
        } else {
            self.open_upvalues = created_upvalue;
        }

        return created_upvalue;
    }

    fn close_upvalues(self: *VM, last: [*]Value) void {
        while (self.open_upvalues != null and @ptrToInt(self.open_upvalues.?.location) >= @ptrToInt(last)) {
            var upvalue = self.open_upvalues.?;
            upvalue.closed = upvalue.location.*;
            upvalue.location = &upvalue.closed;
            self.open_upvalues = upvalue.next;
        }
    }

    fn define_native(
        self: *VM,
        name: []const u8,
        function: NativeFn,
    ) void {
        self.push(Value.obj(new_string(
            &self.strings,
            name,
            &self.objects,
            self.allocator,
            false,
        )));
        self.push(Value.obj(new_native(
            function,
            &self.objects,
            self.allocator,
        )));
        self.globals.put(self.peek(1).as_obj().as_string(), self.peek(0)) catch {
            @panic("Out of memory!\n");
        };
        _ = self.pop();
        _ = self.pop();
    }

    fn runtime_error(
        self: *VM,
        comptime format: []const u8,
        args: anytype,
    ) void {
        log.err(format, args);

        var i = self.frame_count;
        while (i > 0) : (i -= 1) {
            const frame = self.frames[i - 1];
            const chunk = frame.closure.function.chunk;
            const instruction: usize = @ptrToInt(frame.ip) - @ptrToInt(chunk.code.items.ptr) - 1;
            const line: usize = chunk.lines.items[instruction];
            log.err("[line {d}] in {}\n", .{ line, frame.closure });
        }

        self.reset_stack();
    }
};

// TODO: handle math errors (overflow, div by zero, etc)
fn add(left: f64, right: f64) f64 {
    return left + right;
}

fn subtract(left: f64, right: f64) f64 {
    return left - right;
}

fn multiply(left: f64, right: f64) f64 {
    return left * right;
}

fn divide(left: f64, right: f64) f64 {
    return left / right;
}

fn less(left: f64, right: f64) bool {
    return left < right;
}

fn greater(left: f64, right: f64) bool {
    return left > right;
}

// ========= NATIVE FUNCTIONS =========
fn clock_native(arg_count: u8, args: [*]Value) Value {
    _ = arg_count;
    _ = args;
    if (@hasField(std.os.system, "timespec")) {
        return Value.number(@intToFloat(f64, std.time.timestamp()));
    } else {
        // TODO: what to do when there isn't an os clock?
        return Value.number(0);
    }
    
}
