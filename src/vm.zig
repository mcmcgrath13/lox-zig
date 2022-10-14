const std = @import("std");

const chnk = @import("chunk.zig");
const Chunk = chnk.Chunk;
const OpCode = chnk.OpCode;

const vlu = @import("value.zig");
const Value = vlu.Value;
const print_value = vlu.print_value;

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

    pub fn interpret(self: *VM, chunk: *Chunk) InterpretResult {
        self.chunk = chunk;
        self.ip = chunk.code.items.ptr;
        return self.run();
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
                .op_return => {
                    print_value(self.pop());
                    std.debug.print("\n", .{});
                    return InterpretResult.ok;
                },
                .op_constant => {
                    const constant = self.read_constant();
                    self.push(constant);
                },
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
};
