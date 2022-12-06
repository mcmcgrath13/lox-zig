const std = @import("std");
const ArrayList = std.ArrayList;

pub fn write_or_die(comptime T: type, list: *ArrayList(T), value: T) void {
    list.append(value) catch {
        @panic("Out of memory\n");
    };
}

pub fn alloc_or_die(allocator: std.mem.Allocator, comptime T: type, len: usize) []T {
    return allocator.alloc(T, len) catch {
        @panic("Out of memory\n");
    };
}

pub fn alloc_aligned_or_die(allocator: std.mem.Allocator, comptime alignment: u29, len: usize) []align(alignment) u8 {
    return allocator.alignedAlloc(u8, alignment, len) catch {
        @panic("Out of memory\n");
    };
}

pub fn create_or_die(allocator: std.mem.Allocator, comptime T: type) *T {
    return allocator.create(T) catch {
        @panic("Out of memory\n");
    };
}

pub fn read_short(bytes: [2]u8) u16 {
    return @intCast(u16, bytes[0]) << 8 | bytes[1];
}
