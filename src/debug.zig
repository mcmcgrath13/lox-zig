const std = @import("std");
const chunk = @import("chunk.zig");
const Chunk = chunk.Chunk;
const OpCode = chunk.OpCode;

const read_short = @import("common.zig").read_short;

pub fn disassemble_chunk(c: *Chunk, name: anytype) void {
    std.log.debug("==={s}===\n", .{name});

    var offset: usize = 0;
    while (offset < c.length()) {
        offset = disassemble_instruction(c, offset);
    }
}

pub fn disassemble_instruction(c: *Chunk, offset: usize) usize {
    std.log.debug("{d:0>4} ", .{offset});

    // print line number
    if ((offset > 0) and (c.lines.items[offset] == c.lines.items[offset - 1])) {
        std.log.debug("   | ", .{});
    } else {
        std.log.debug("{d: >4} ", .{c.lines.items[offset]});
    }

    const instruction_byte = c.code.items[offset];
    return switch (@intToEnum(OpCode, instruction_byte)) {
        .constant => constant_instruction("CONSTANT", c, offset),
        .define_global => constant_instruction("DEFINE_GLOBAL", c, offset),
        .get_global => constant_instruction("GET_GLOBAL", c, offset),
        .set_global => constant_instruction("SET_GLOBAL", c, offset),
        .get_local => byte_instruction("GET_LOCAL", c, offset),
        .set_local => byte_instruction("SET_LOCAL", c, offset),
        .get_property => constant_instruction("GET_PROPERTY", c, offset),
        .set_property => constant_instruction("SET_PROPERTY", c, offset),
        .get_upvalue => byte_instruction("GET_UPVALUE", c, offset),
        .set_upvalue => byte_instruction("SET_UPVALUE", c, offset),
        .close_upvalue => simple_instruction("CLOSE_UPVALUE", offset),
        .get_super => constant_instruction("GET_SUPER", c, offset),
        .jump_if_false => jump_instruction("JUMP_IF_FALSE", true, c, offset),
        .jump => jump_instruction("JUMP", true, c, offset),
        .loop => jump_instruction("LOOP", false, c, offset),
        ._return => simple_instruction("RETURN", offset),
        .print => simple_instruction("PRINT", offset),
        .pop => simple_instruction("POP", offset),
        .call => byte_instruction("CALL", c, offset),
        .closure => closure_instruction("CLOSURE", c, offset),
        .class => constant_instruction("CLASS", c, offset),
        .method => constant_instruction("METHOD", c, offset),
        .invoke => invoke_instruction("INVOKE", c, offset),
        .super_invoke => invoke_instruction("SUPER_INVOKE", c, offset),
        .inherit => simple_instruction("INHERIT", offset),

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
    std.log.debug("{s}\n", .{name});
    return offset + 1;
}

pub fn constant_instruction(name: []const u8, c: *Chunk, offset: usize) usize {
    const constant_idx: usize = c.code.items[offset + 1];
    std.log.debug(
        "{s: <16} {d: >4} '{}'\n",
        .{ name, constant_idx, c.constants.items[constant_idx] },
    );
    return offset + 2;
}

pub fn closure_instruction(
    name: []const u8,
    c: *Chunk,
    original_offset: usize,
) usize {
    var offset = original_offset + 1;
    const constant_idx: usize = c.code.items[offset];
    std.log.debug(
        "{s: <16} {d: >4} '{}'\n",
        .{ name, constant_idx, c.constants.items[constant_idx] },
    );
    offset += 1;

    var function = c.constants.items[constant_idx].as_obj().as_function();
    var i: u8 = 0;
    while (i < function.upvalue_count) : (i += 1) {
        const is_local = if (c.code.items[offset] == 1) "local" else "upvalue";
        const index = c.code.items[offset + 1];
        std.log.debug(
            "{d:0>4}      |                     {s} {d}\n",
            .{ offset, is_local, index },
        );
        offset += 2;
    }

    return offset;
}

fn byte_instruction(name: []const u8, c: *Chunk, offset: usize) usize {
    const byte_idx: usize = c.code.items[offset + 1];
    std.log.debug("{s: <16} {d: >4}\n", .{ name, byte_idx });
    return offset + 2;
}

fn jump_instruction(
    name: []const u8,
    positive_jump: bool,
    c: *Chunk,
    offset: usize,
) usize {
    const jump_size = read_short(.{ c.code.items[offset + 1], c.code.items[offset + 2] });
    const sign: i32 = if (positive_jump) 1 else -1;
    std.log.debug("{s: <16} {d: >4} -> {d}\n", .{ name, offset, @intCast(i32, offset + 3) + sign * jump_size });
    return offset + 3;
}

fn invoke_instruction(
    name: []const u8,
    c: *Chunk,
    offset: usize,
) usize {
    const constant_idx: usize = c.code.items[offset + 1];
    const arg_count: usize = c.code.items[offset + 2];
    std.log.debug(
        "{s: <16} ({d} args) {d: >4} '{s}'\n",
        .{ name, arg_count, constant_idx, c.constants.items[constant_idx] },
    );
    return offset + 3;
}
