const std = @import("std");
const HashMap = std.HashMap;

const chnk = @import("chunk.zig");
const Chunk = chnk.Chunk;
const OpCode = chnk.OpCode;

const vlu = @import("value.zig");
const Value = vlu.Value;

const compile = @import("compile.zig").compile;

const disassemble_instruction = @import("debug.zig").disassemble_instruction;

const obj = @import("object.zig");
const Obj = obj.Obj;
const ObjFunction = obj.ObjFunction;
const new_string = obj.new_string;
const ObjString = obj.ObjString;
const ObjStringContext = obj.ObjStringContext;

const common = @import("common.zig");

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

const CallFrame = struct {
    function: *ObjFunction,
    ip: [*]u8,
    slots: [*]Value,

    fn init(function: *ObjFunction, slots: [*]Value) CallFrame {
        return .{ .function = function, .ip = function.chunk.code.items.ptr, .slots = slots };
    }

    fn read_byte(self: *CallFrame) u8 {
        const byte = self.ip[0];
        self.ip = self.ip + 1;
        return byte;
    }

    fn read_constant(self: *CallFrame) Value {
        return self.function.chunk.constants.values.items[self.read_byte()];
    }

    fn read_short(self: *CallFrame) u16 {
        return common.read_short(.{ self.read_byte(), self.read_byte() });
    }
};

pub const VM = struct {
    frames: [FRAME_MAX]CallFrame = undefined,
    frame_count: u8 = 0,

    allocator: std.mem.Allocator,

    // TODO: revisit if this should really use pointer math
    stack: [STACK_MAX]Value = undefined,
    stack_top: usize = 0,

    objects: ?*Obj = null,
    strings: ObjStringHashMap,

    globals: VariableHashMap,

    debug: bool = false,

    pub fn init(debug: bool, allocator: std.mem.Allocator) VM {
        const strings = ObjStringHashMap.init(allocator);
        const globals = VariableHashMap.init(allocator);
        return VM{
            .debug = debug,
            .allocator = allocator,
            .strings = strings,
            .globals = globals,
        };
    }

    pub fn deinit(self: *VM) void {
        self.free_objects();
        self.strings.deinit();
        self.globals.deinit();
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
        self.stack_top = 0;
    }

    fn push(self: *VM, value: Value) void {
        self.stack[self.stack_top] = value;
        self.stack_top += 1;
    }

    fn pop(self: *VM) Value {
        self.stack_top -= 1;
        return self.stack[self.stack_top];
    }

    fn peek(self: *VM, distance: usize) Value {
        return self.stack[self.stack_top - 1 - distance];
    }

    pub fn interpret(self: *VM, source: []const u8) InterpretError!void {
        var function_obj = compile(
            source,
            self.allocator,
            self.debug,
            &self.objects,
            &self.strings,
        ) catch {
            return InterpretError.compile;
        };

        self.push(Value.obj(function_obj));

        var frame = &self.frames[self.frame_count];
        self.frame_count += 1;

        frame.* = CallFrame.init(function_obj.as_function(), &self.stack);

        return self.run();
    }

    fn run(self: *VM) InterpretError!void {
        var frame = &self.frames[self.frame_count - 1];

        while (true) {
            if (self.debug) {
                // print stack
                std.debug.print("          ", .{});
                for (self.stack) |value, i| {
                    if (i == self.stack_top) {
                        break;
                    }
                    std.debug.print("[{}]", .{value});
                }
                std.debug.print("\n", .{});

                _ = disassemble_instruction(&frame.function.chunk, @ptrToInt(frame.ip) - @ptrToInt(frame.function.chunk.code.items.ptr));
            }
            const instruction = frame.read_byte();
            try switch (@intToEnum(OpCode, instruction)) {
                .constant => {
                    const constant = frame.read_constant();
                    self.push(constant);
                },
                .define_global => {
                    var name = frame.read_constant().as_obj().as_string();
                    self.globals.put(name, self.peek(0)) catch {
                        std.debug.print("Out of memory!\n", .{});
                        std.process.exit(1);
                    };
                    _ = self.pop();
                },
                .get_global => {
                    var name = frame.read_constant().as_obj().as_string();
                    if (self.globals.get(name)) |value| {
                        self.push(value);
                    } else {
                        self.runtime_error(frame, "undefined variable '{s}'", .{name.data});
                        return InterpretError.runtime;
                    }
                },
                .set_global => {
                    var name = frame.read_constant().as_obj().as_string();
                    if (self.globals.getPtr(name)) |value_ptr| {
                        value_ptr.* = self.peek(0);
                    } else {
                        self.runtime_error(frame, "can't assign to undefined variable '{s}'", .{name.data});
                        return InterpretError.runtime;
                    }
                },
                .get_local => {
                    const idx = frame.read_byte();
                    self.push(frame.slots[idx]);
                },
                .set_local => {
                    const idx = frame.read_byte();
                    frame.slots[idx] = self.peek(0);
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
                    return;
                },
                .print => {
                    std.debug.print("{}\n", .{self.pop()});
                },
                .pop => {
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
                        self.runtime_error(frame, "operand must be a number: found {s}", .{self.peek(0)});
                        return InterpretError.runtime;
                    }
                    self.push(Value.number(-1 * self.pop().as_number()));
                },

                //binary
                .add => {
                    if (self.peek(0).is_string() and self.peek(1).is_string()) {
                        try self.concatenate(frame);
                    } else {
                        // TODO: fix error message
                        try self.binary_op(frame, f64, Value.number, add);
                    }
                },
                .divide => try self.binary_op(frame, f64, Value.number, divide),
                .multiply => try self.binary_op(frame, f64, Value.number, multiply),
                .subtract => try self.binary_op(frame, f64, Value.number, subtract),
                .equal => {
                    const right: Value = self.pop();
                    const left: Value = self.pop();
                    self.push(Value.boolean(left.equals(right)));
                },
                .less => self.binary_op(frame, bool, Value.boolean, less),
                .greater => self.binary_op(frame, bool, Value.boolean, greater),

                // fallthrough
                _ => return InterpretError.runtime,
            };
        }
    }

    fn binary_op(
        self: *VM,
        frame: *CallFrame,
        comptime R: type,
        value_type: (fn (R) Value),
        op: (fn (f64, f64) R),
    ) InterpretError!void {
        if (!self.peek(0).is_number() or !self.peek(1).is_number()) {
            self.runtime_error(
                frame,
                "operands must be numbers, found: {s} {s}",
                .{ self.peek(0), self.peek(1) },
            );
            return InterpretError.runtime;
        }
        const right = self.pop().as_number();
        const left = self.pop().as_number();
        self.push(value_type(op(left, right)));
    }

    fn concatenate(self: *VM, frame: *CallFrame) InterpretError!void {
        const b: []const u8 = self.pop().as_obj().as_string().data;
        const a: []const u8 = self.pop().as_obj().as_string().data;
        const data = std.mem.concat(self.allocator, u8, &[_][]const u8{ a, b }) catch {
            self.runtime_error(frame, "out of memory", .{});
            return InterpretError.runtime;
        };
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

    fn runtime_error(
        self: *VM,
        frame: *CallFrame,
        comptime format: []const u8,
        args: anytype,
    ) void {
        std.debug.print(format, args);

        const instruction: usize = @ptrToInt(frame.ip) - @ptrToInt(frame.function.chunk.code.items.ptr) - 1;
        const line: usize = frame.function.chunk.lines.items[instruction];
        std.debug.print("[line {d}] in script\n", .{line});
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
