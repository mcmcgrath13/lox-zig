const std = @import("std");
const HashMap = std.HashMap;

const chnk = @import("chunk.zig");
const Chunk = chnk.Chunk;
const OpCode = chnk.OpCode;

const vlu = @import("value.zig");
const Value = vlu.Value;
const print_value = vlu.print_value;

const compile = @import("compile.zig").compile;

const disassemble_instruction = @import("debug.zig").disassemble_instruction;

const obj = @import("object.zig");
const Obj = obj.Obj;
const take_string = obj.take_string;
const ObjString = obj.ObjString;
const ObjStringContext = obj.ObjStringContext;

const STACK_MAX = 256;

pub const ObjStringHashMap = HashMap(
    *ObjString,
    void,
    ObjStringContext,
    std.hash_map.default_max_load_percentage,
);

pub const InterpretError = error{
    compile,
    runtime,
};

pub const VM = struct {
    chunk: ?*Chunk = null,
    ip: ?[*]u8 = null,
    allocator: std.mem.Allocator,

    // TODO: revisit if this should really use pointer math
    stack: [STACK_MAX]Value = undefined,
    stack_top: usize = 0,

    objects: ?*Obj = null,
    strings: ObjStringHashMap,

    debug: bool = false,

    pub fn init(debug: bool, allocator: std.mem.Allocator) VM {
        const strings = ObjStringHashMap.init(allocator);
        return VM{
            .debug = debug,
            .allocator = allocator,
            .strings = strings,
        };
    }

    pub fn deinit(self: *VM) void {
        self.free_objects();
        self.strings.deinit();
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
        var chunk = Chunk.init(self.allocator);
        defer chunk.deinit();

        self.objects = compile(
            source,
            &chunk,
            self.allocator,
            self.debug,
            self.objects,
            &self.strings,
        ) catch {
            return InterpretError.compile;
        };
        self.chunk = &chunk;
        self.ip = chunk.code.items.ptr;

        return self.run();
    }

    fn run(self: *VM) InterpretError!void {
        while (true) {
            if (self.debug) {
                // print stack
                std.debug.print("          ", .{});
                for (self.stack) |value, i| {
                    if (i == self.stack_top) {
                        break;
                    }
                    std.debug.print("[", .{});
                    print_value(value);
                    std.debug.print("]", .{});
                }
                std.debug.print("\n", .{});

                _ = disassemble_instruction(self.chunk.?, @ptrToInt(self.ip.?) - @ptrToInt(self.chunk.?.code.items.ptr));
            }
            const instruction = self.read_byte();
            try switch (@intToEnum(OpCode, instruction)) {
                .constant => {
                    const constant = self.read_constant();
                    self.push(constant);
                },
                ._return => {
                    print_value(self.pop());
                    std.debug.print("\n", .{});
                    return;
                },

                // literals
                .nil => self.push(Value.nil()),
                ._true => self.push(Value.boolean(true)),
                ._false => self.push(Value.boolean(false)),

                // unary
                .not => self.push(Value.boolean(self.pop().is_falsey())),
                .negate => {
                    if (!self.peek(0).is_number()) {
                        self.runtime_error(.{"operand must be a number"});
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

    fn read_byte(self: *VM) u8 {
        const byte = self.ip.?[0];
        self.ip = self.ip.? + 1;
        return byte;
    }

    fn read_constant(self: *VM) Value {
        return self.chunk.?.constants.values.items[@intCast(usize, self.read_byte())];
    }

    fn binary_op(
        self: *VM,
        comptime R: type,
        value_type: (fn (R) Value),
        op: (fn (f64, f64) R),
    ) InterpretError!void {
        if (!self.peek(0).is_number() or !self.peek(1).is_number()) {
            self.runtime_error(.{"operands must be numbers"});
            return InterpretError.runtime;
        }
        const right = self.pop().as_number();
        const left = self.pop().as_number();
        self.push(value_type(op(left, right)));
    }

    fn concatenate(self: *VM) InterpretError!void {
        const b: []const u8 = self.pop().as_obj().as_string();
        const a: []const u8 = self.pop().as_obj().as_string();
        const data = std.mem.concat(self.allocator, u8, &[_][]const u8{ a, b }) catch {
            self.runtime_error(.{"out of memory"});
            return InterpretError.runtime;
        };
        self.push(Value.obj(take_string(&self.strings, data, self.allocator), &self.objects));
    }

    fn runtime_error(self: *VM, args: anytype) void {
        std.debug.print("{s}\n", args);

        const instruction: usize = @ptrToInt(self.ip.?) - @ptrToInt(self.chunk.?.code.items.ptr) - 1;
        const line: usize = self.chunk.?.lines.items[instruction];
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
