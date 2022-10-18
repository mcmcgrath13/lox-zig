const std = @import("std");

const scn = @import("scanner.zig");
const Scanner = scn.Scanner;
const TokenType = scn.TokenType;

pub fn compile(source: []const u8) void {
    var scanner = Scanner.init(source);
    var line: usize = 0;
    var count: usize = 0;
    while (count < 10) : (count += 1) {
        const token = scanner.scan_token();
        if (token.line != line) {
            std.debug.print("{d} ", .{token.line});
            line = token.line;
        } else {
            std.debug.print("   | ", .{});
        }
        std.debug.print("{d: >2} '{s}'\n", .{ token.t, token.start[0..token.length] });

        if (token.t == TokenType.eof) break;
    }
}
