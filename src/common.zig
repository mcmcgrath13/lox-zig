const std = @import("std");
const ArrayList = std.ArrayList;
const exit = std.process.exit;

pub fn write_or_die(comptime T: type, list: *ArrayList(T), value: T) void {
    list.append(value) catch {
        std.debug.print("Out of memory\n", .{});
        exit(1);
    };
}

pub fn alloc_or_die(allocator: std.mem.Allocator, comptime T: type, len: usize) []T {
    return allocator.alloc(T, len) catch {
        std.debug.print("Out of memory\n", .{});
        std.process.exit(1);
    };
}

pub fn create_or_die(allocator: std.mem.Allocator, comptime T: type) *T {
    return allocator.create(T) catch {
        std.debug.print("Out of memory\n", .{});
        std.process.exit(1);
    };
}

pub fn read_short(bytes: [2]u8) u16 {
    return @intCast(u16, bytes[0]) << 8 | bytes[1];
}
