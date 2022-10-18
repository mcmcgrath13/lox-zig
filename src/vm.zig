const std = @import("std");

const chnk = @import("chunk.zig");
const Chunk = chnk.Chunk;
const OpCode = chnk.OpCode;

const vlu = @import("value.zig");
const Value = vlu.Value;
const print_value = vlu.print_value;

const compile = @import("compile.zig").compile;

const disassemble_instruction = @import("debug.zig").disassemble_instruction;

const STACK_MAX = 256;

const InterpretResult = enum {
    ok,
    compile_error,
    runtime_error,
};

pub const VM = struct {
    chunk: ?*Chunk = null,
    ip: ?[*]u8 = null,

    // TODO: revisit if this should really use pointer math
    stack: [STACK_MAX]Value = undefined,
    stack_top: usize = 0,

    debug: bool = false,

    pub fn init(debug: bool) VM { //allocator: std.mem.Allocator
        return VM{
            .debug = debug,
        };
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

    pub fn deinit() void { // self: *VM
    }

    pub fn interpret(source: []const u8) InterpretResult {
        compile(source);
        return InterpretResult.ok;
        // self.chunk = chunk;
        // self.ip = chunk.code.items.ptr;
        // return self.run();
    }

    fn run(self: *VM) InterpretResult {
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
            switch (@intToEnum(OpCode, instruction)) {
                .op_constant => {
                    const constant = self.read_constant();
                    self.push(constant);
                },
                .op_return => {
                    print_value(self.pop());
                    std.debug.print("\n", .{});
                    return InterpretResult.ok;
                },

                // unary
                .op_negate => {
                    self.push(-1 * self.pop());
                },

                //binary
                .op_add => self.binary_op(add),
                .op_divide => self.binary_op(divide),
                .op_multiply => self.binary_op(multiply),
                .op_subtract => self.binary_op(subtract),

                // fallthrough
                _ => return InterpretResult.compile_error,
            }
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

    fn binary_op(self: *VM, op: (fn (Value, Value) Value)) void {
        const right = self.pop();
        const left = self.pop();
        self.push(op(left, right));
    }
};

// TODO: handle math errors (overflow, div by zero, etc)
fn add(left: Value, right: Value) Value {
    return left + right;
}

fn subtract(left: Value, right: Value) Value {
    return left - right;
}

fn multiply(left: Value, right: Value) Value {
    return left * right;
}

fn divide(left: Value, right: Value) Value {
    return left / right;
}
