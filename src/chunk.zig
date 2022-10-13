const std = @import("std");
const ArrayList = std.ArrayList;

const common = @import("common.zig");

const ValueArray = @import("value.zig").ValueArray;
const Value = @import("value.zig").Value;

pub const OpCode = enum(u8) { op_return, op_constant, _ };

// this should maybe be ArrayList instead
pub const Chunk = struct {
    code: ArrayList(u8),
    lines: ArrayList(usize),
    constants: ValueArray,

    pub fn init(allocator: std.mem.Allocator) Chunk {
        return Chunk{
            .code = ArrayList(u8).init(allocator),
            .lines = ArrayList(usize).init(allocator),
            .constants = ValueArray.init(allocator),
        };
    }

    pub fn deinit(self: *Chunk) void {
        self.code.deinit();
        self.lines.deinit();
        self.constants.deinit();
    }

    // if we're out of memory, bail out
    pub fn write(self: *Chunk, byte: anytype, line: usize) void {
        common.write_or_die(u8, &self.code, @intCast(u8, byte));
        common.write_or_die(usize, &self.lines, line);
    }

    pub fn add_constant(self: *Chunk, value: Value) usize {
        self.constants.write(value);
        return self.constants.length() - 1;
    }

    pub fn length(self: *Chunk) usize {
        return self.code.items.len;
    }
};
