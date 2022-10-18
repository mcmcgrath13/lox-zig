// this file is the entrypoint for the executable zig binary
const std = @import("std");

const lox = @import("lox");
const InterpretResult = lox.InterpretResult;

pub fn main() anyerror!void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    const allocator = gpa.allocator();
    defer {
        const leaked = gpa.deinit();
        if (leaked) std.testing.expect(false) catch @panic("TEST FAIL"); //fail test; can't try in defer as defer is executed after we return
    }

    const args = try std.process.argsAlloc(allocator);
    defer std.process.argsFree(allocator, args);

    // var vm = lox.VM.init(true);

    switch (args.len) {
        1 => repl(),
        2 => run_file(allocator, args[1]),
        else => {
            std.debug.print("Usage: lox [path]\n", .{});
            std.process.exit(64);
        },
    }

    // defer vm.deinit();

    //
    // _ = vm.interpret(&c);
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

fn repl() void {
    const stdout = std.io.getStdOut();
    const stdin = std.io.getStdIn();

    var buffer: [1024]u8 = undefined;

    while (true) {
        stdout.writeAll("\n>") catch {
            std.debug.print("couldn't write", .{});
        };

        const input = (next_line(stdin.reader(), &buffer)).?;

        std.debug.print("{s} ({d})", .{ input, input.len });

        // clear the buffer maybe?

        _ = lox.VM.interpret(input);
    }
}

fn run_file(allocator: std.mem.Allocator, path: []u8) void {
    const file = std.fs.cwd().openFile(path, .{}) catch {
        std.debug.print("Could not open file", .{});
        std.process.exit(74);
    };
    defer file.close();

    const contents = file.reader().readAllAlloc(
        allocator,
        std.math.maxInt(usize),
    ) catch {
        std.debug.print("Could not read file", .{});
        std.process.exit(74);
    };
    defer allocator.free(contents);

    const result = lox.VM.interpret(contents);

    switch (result) {
        .ok => std.process.exit(0),
        .compile_error => std.process.exit(65),
        .runtime_error => std.process.exit(70),
    }
}

// TODO: make this a test, somewhere
// var c = lox.chunk.Chunk.init(allocator);
// defer c.deinit();
//
// c.write(@enumToInt(lox.chunk.OpCode.op_constant), 1);
// c.write(c.add_constant(1.2), 1);
//
// c.write(@enumToInt(lox.chunk.OpCode.op_constant), 2);
// c.write(c.add_constant(3.4), 2);
//
// c.write(@enumToInt(lox.chunk.OpCode.op_add), 3);
//
// c.write(@enumToInt(lox.chunk.OpCode.op_constant), 4);
// c.write(c.add_constant(5.6), 4);
//
// c.write(@enumToInt(lox.chunk.OpCode.op_divide), 5);
//
// c.write(@enumToInt(lox.chunk.OpCode.op_negate), 6);
//
// c.write(@enumToInt(lox.chunk.OpCode.op_return), 7);
