const std = @import("std");
const ArrayList = std.ArrayList;

const common = @import("common.zig");

pub const Value = f64;

pub const ValueArray = struct {
    values: ArrayList(Value),

    pub fn init(allocator: std.mem.Allocator) ValueArray {
        return ValueArray{ .values = ArrayList(Value).init(allocator) };
    }

    pub fn deinit(self: *ValueArray) void {
        self.values.deinit();
    }

    // if we're out of memory, bail out
    pub fn write(self: *ValueArray, value: Value) void {
        common.write_or_die(Value, &self.values, value);
    }

    pub fn length(self: *ValueArray) usize {
        return self.values.items.len;
    }
};

pub fn print_value(value: Value) void {
    std.debug.print("'{d}'", .{value});
}
