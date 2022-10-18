const std = @import("std");
const chunk = @import("chunk.zig");
const Chunk = chunk.Chunk;
const OpCode = chunk.OpCode;
const print_value = @import("value.zig").print_value;

pub fn disassemble_chunk(c: *Chunk, name: []const u8) void {
    std.debug.print("==={s}===\n", .{name});

    var offset: usize = 0;
    while (offset < c.length()) {
        offset = disassemble_instruction(c, offset);
    }
}

pub fn disassemble_instruction(c: *Chunk, offset: usize) usize {
    std.debug.print("{d:0>4} ", .{offset});

    // print line number
    if ((offset > 0) and (c.lines.items[offset] == c.lines.items[offset - 1])) {
        std.debug.print("   | ", .{});
    } else {
        std.debug.print("{d: >4} ", .{c.lines.items[offset]});
    }

    const instruction_byte = c.code.items[offset];
    return switch (@intToEnum(OpCode, instruction_byte)) {
        .op_constant => constant_instruction("OP_CONSTANT", c, offset),
        .op_return => simple_instruction("OP_RETURN", offset),

        // unary
        .op_negate => simple_instruction("OP_NEGATE", offset),

        // binary
        .op_add => simple_instruction("OP_ADD", offset),
        .op_divide => simple_instruction("OP_DIVIDE", offset),
        .op_multiply => simple_instruction("OP_MULTIPLY", offset),
        .op_subtract => simple_instruction("OP_SUBTRACT", offset),

        // fallthrough
        _ => simple_instruction("UH OHHHHHH", offset),
    };
}

pub fn simple_instruction(name: []const u8, offset: usize) usize {
    std.debug.print("{s}\n", .{name});
    return offset + 1;
}

pub fn constant_instruction(name: []const u8, c: *Chunk, offset: usize) usize {
    const constant_idx: usize = c.code.items[offset + 1];
    std.debug.print("{s: <16} {d: >4} ", .{ name, constant_idx });
    print_value(c.constants.values.items[constant_idx]);
    std.debug.print("\n", .{});
    return offset + 2;
}
