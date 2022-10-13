// this file is the entrypoint for the executable zig binary
const std = @import("std");

const chunk = @import("chunk.zig");
const debug = @import("debug.zig");

pub fn main() void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    const allocator = gpa.allocator();
    defer {
        const leaked = gpa.deinit();
        if (leaked) std.testing.expect(false) catch @panic("TEST FAIL"); //fail test; can't try in defer as defer is executed after we return
    }

    var c = chunk.Chunk.init(allocator);
    defer c.deinit();
    c.write(@enumToInt(chunk.OpCode.op_return), 1);

    const constant_idx = c.add_constant(1.2);
    c.write(@enumToInt(chunk.OpCode.op_constant), 1);
    c.write(constant_idx, 1);

    debug.disassemble_chunk(&c, "test chunk");
}
