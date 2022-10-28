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
        .constant => constant_instruction("CONSTANT", c, offset),
        .define_global => constant_instruction("DEFINE_GLOBAL", c, offset),
        .get_global => constant_instruction("GET_GLOBAL", c, offset),
        .set_global => constant_instruction("SET_GLOBAL", c, offset),
        ._return => simple_instruction("RETURN", offset),
        .print => simple_instruction("PRINT", offset),
        .pop => simple_instruction("POP", offset),

        // literals
        .nil => simple_instruction("NIL", offset),
        ._false => simple_instruction("FALSE", offset),
        ._true => simple_instruction("TRUE", offset),

        // unary
        .not => simple_instruction("NOT", offset),
        .negate => simple_instruction("NEGATE", offset),

        // binary
        .add => simple_instruction("ADD", offset),
        .divide => simple_instruction("DIVIDE", offset),
        .multiply => simple_instruction("MULTIPLY", offset),
        .subtract => simple_instruction("SUBTRACT", offset),
        .equal => simple_instruction("EQUAL", offset),
        .less => simple_instruction("LESS", offset),
        .greater => simple_instruction("GREATER", offset),

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
