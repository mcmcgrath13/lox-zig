// this file is the entrypoint for the executable zig binary
const std = @import("std");

const chunk = @import("chunk.zig");
const VM = @import("vm.zig").VM;

pub fn main() void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    const allocator = gpa.allocator();
    defer {
        const leaked = gpa.deinit();
        if (leaked) std.testing.expect(false) catch @panic("TEST FAIL"); //fail test; can't try in defer as defer is executed after we return
    }

    var vm = VM.init(true);
    // defer vm.deinit();

    var c = chunk.Chunk.init(allocator);
    defer c.deinit();

    const constant_idx = c.add_constant(1.2);
    c.write(@enumToInt(chunk.OpCode.op_constant), 1);
    c.write(constant_idx, 1);

    c.write(@enumToInt(chunk.OpCode.op_return), 1);

    _ = vm.interpret(&c);
}
