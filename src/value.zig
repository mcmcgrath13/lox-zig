const std = @import("std");
const ArrayList = std.ArrayList;

const common = @import("common.zig");

pub const Value = union(enum) {
    val_bool: bool,
    val_nil: void,
    val_number: f64,

    pub fn number(val: f64) Value {
        return Value{ .val_number = val };
    }

    pub fn boolean(val: bool) Value {
        return Value{ .val_bool = val };
    }

    pub fn nil() Value {
        return Value.val_nil;
    }

    pub fn as_number(self: Value) f64 {
        return switch (self) {
            .val_number => self.val_number,
            else => unreachable,
        };
    }

    pub fn as_bool(self: Value) bool {
        return switch (self) {
            .val_bool => self.val_bool,
            else => unreachable,
        };
    }

    pub fn is_number(self: Value) bool {
        return switch (self) {
            .val_number => true,
            else => false,
        };
    }

    pub fn is_bool(self: Value) bool {
        return switch (self) {
            .val_bool => true,
            else => false,
        };
    }

    pub fn is_nil(self: Value) bool {
        return switch (self) {
            .val_nil => true,
            else => false,
        };
    }
};

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
    switch (value) {
        .val_bool => std.debug.print("'{any}'", .{value.as_bool()}),
        .val_nil => std.debug.print("'nil'", .{}),
        .val_number => std.debug.print("'{d}'", .{value.as_number()}),
    }
}
