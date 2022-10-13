const std = @import("std");
const ArrayList = std.ArrayList;
const exit = std.process.exit;

pub fn write_or_die(comptime T: type, list: *ArrayList(T), value: anytype) void {
    list.append(value) catch {
        std.debug.print("Out of memory\n", .{});
        exit(1);
    };
}
