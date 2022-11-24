// this file is the entrypoint for the executable zig binary
const std = @import("std");

const lox = @import("lox.zig");
const InterpretError = lox.InterpretError;

// Define root.log to override the std implementation
pub fn log(
    comptime _: std.log.Level,
    comptime _: @TypeOf(.EnumLiteral),
    comptime format: []const u8,
    args: anytype,
) void {
    // Print the message to stderr, silently ignoring any errors
    std.debug.getStderrMutex().lock();
    defer std.debug.getStderrMutex().unlock();
    const stderr = std.io.getStdErr().writer();
    nosuspend stderr.print(format, args) catch return;
}

pub fn main() anyerror!void {
    var gpa = std.heap.GeneralPurposeAllocator(.{
        .retain_metadata = true,
        .never_unmap = true,
        // .verbose_log = true,
    }){};
    const allocator = gpa.allocator();
    defer {
        const leaked = gpa.deinit();
        if (leaked) std.testing.expect(false) catch @panic("TEST FAIL"); //fail test; can't try in defer as defer is executed after we return
    }

    const args = try std.process.argsAlloc(allocator);
    defer std.process.argsFree(allocator, args);

    // TODO: put debug bool behind an args flag
    var options = lox.Options.init_all();
    options = lox.Options{};
    options.debug_stress_gc = false;
    options.debug_gc = false;
    var vm = lox.Lox.init(options, allocator);
    defer vm.deinit();

    switch (args.len) {
        1 => try repl(&vm),
        2 => run_file(&vm, allocator, args[1]),
        else => {
            std.log.err("Usage: lox [path]\n", .{});
            std.process.exit(64);
        },
    }
}

fn next_line(reader: anytype, buffer: []u8) ?[]const u8 {
    var line = (reader.readUntilDelimiterOrEof(
        buffer,
        '\n',
    ) catch null) orelse return null;
    // trim annoying windows-only carriage return character
    if (@import("builtin").os.tag == .windows) {
        return std.mem.trimRight(u8, line, "\r");
    } else {
        return line;
    }
}

fn repl(vm: *lox.Lox) !void {
    const stdout = std.io.getStdOut().writer();
    const stdin = std.io.getStdIn();

    var buffer: [1024]u8 = undefined;

    while (true) {
        try stdout.print("\n>", .{});

        // break if no input is passed (ctrl+d)
        const input = (next_line(stdin.reader(), &buffer)) orelse {
            try stdout.print("\n", .{});
            break;
        };

        try stdout.print("{s} ({d})\n", .{ input, input.len });

        // clear the buffer maybe?

        vm.interpret(input) catch {
            std.log.err("ERROR", .{});
        };
    }
}

fn run_file(vm: *lox.Lox, allocator: std.mem.Allocator, path: []u8) void {
    const file = std.fs.cwd().openFile(path, .{}) catch {
        std.log.err("Could not open file", .{});
        std.process.exit(74);
    };
    defer file.close();

    const contents = file.reader().readAllAlloc(
        allocator,
        std.math.maxInt(usize),
    ) catch {
        std.log.err("Could not read file", .{});
        std.process.exit(74);
    };
    defer allocator.free(contents);

    vm.interpret(contents) catch |e| {
        switch (e) {
            InterpretError.compile => std.process.exit(65),
            InterpretError.runtime => std.process.exit(70),
        }
    };
}
