const std = @import("std");
const ArrayList = std.ArrayList;

const common = @import("common.zig");
const obj = @import("object.zig");
const Obj = obj.Obj;

pub const Value = union(enum) {
    val_boolean: bool,
    val_nil: void,
    val_number: f64,
    val_obj: *Obj,

    pub fn number(val: f64) Value {
        return Value{ .val_number = val };
    }

    pub fn boolean(val: bool) Value {
        return Value{ .val_boolean = val };
    }

    pub fn nil() Value {
        return Value.val_nil;
    }

    pub fn obj(val: *Obj) Value {
        return Value{ .val_obj = val };
    }

    pub fn as_number(self: Value) f64 {
        return switch (self) {
            .val_number => self.val_number,
            else => unreachable,
        };
    }

    pub fn as_boolean(self: Value) bool {
        return switch (self) {
            .val_boolean => self.val_boolean,
            else => unreachable,
        };
    }

    pub fn as_obj(self: Value) *Obj {
        return switch (self) {
            .val_obj => self.val_obj,
            else => unreachable,
        };
    }

    fn is(self: Value, comptime field: [:0]const u8) bool {
        return std.mem.eql(u8, @tagName(self), field);
    }

    pub fn is_number(self: Value) bool {
        return self.is("val_number");
    }

    pub fn is_boolean(self: Value) bool {
        return self.is("val_boolean");
    }

    pub fn is_nil(self: Value) bool {
        return self.is("val_nil");
    }

    pub fn is_obj(self: Value) bool {
        return self.is("val_obj");
    }

    fn is_obj_type(self: Value, comptime field: [:0]const u8) bool {
        if (self.is_obj()) {
            return std.mem.eql(u8, @tagName(self.as_obj().t), field);
        }

        return false;
    }

    pub fn is_string(self: Value) bool {
        return self.is_obj_type("string");
    }

    pub fn is_function(self: Value) bool {
        return self.is_obj_type("function");
    }

    pub fn is_native(self: Value) bool {
        return self.is_obj_type("native");
    }

    pub fn is_closure(self: Value) bool {
        return self.is_obj_type("closure");
    }

    pub fn is_upvalue(self: Value) bool {
        return self.is_obj_type("upvalue");
    }

    pub fn is_class(self: Value) bool {
        return self.is_obj_type("class");
    }

    pub fn is_instance(self: Value) bool {
        return self.is_obj_type("instance");
    }

    pub fn is_bound_method(self: Value) bool {
        return self.is_obj_type("bound_method");
    }

    pub fn is_falsey(self: Value) bool {
        return self.is_nil() or (self.is_boolean() and !self.as_boolean());
    }

    pub fn equals(self: Value, other: Value) bool {
        switch (self) {
            .val_obj => {
                switch (other) {
                    .val_obj => {
                        return self.as_obj().equals(other.as_obj());
                    },
                    else => return false,
                }
            },
            else => {
                return std.meta.eql(self, other);
            },
        }
    }

    pub fn format(
        self: Value,
        comptime fmt: []const u8,
        options: std.fmt.FormatOptions,
        writer: anytype,
    ) !void {
        _ = fmt;
        _ = options;

        switch (self) {
            .val_boolean => try writer.print("{}", .{self.as_boolean()}),
            .val_nil => try writer.print("nil", .{}),
            .val_number => try writer.print("{d}", .{self.as_number()}),
            .val_obj => try writer.print("{}", .{self.as_obj()}),
        }
    }
};

pub const ValueArray = struct {
    values: ArrayList(Value),

    pub fn init(allocator: std.mem.Allocator) ValueArray {
        var arr = ArrayList(Value).init(allocator);
        return ValueArray{ .values = arr };
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
