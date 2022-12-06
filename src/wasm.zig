// this file is the entrypoint for the wasm library
const std = @import("std");

const lox = @import("lox.zig");

extern "zig" fn wasm_print(msg_ptr: [*]const u8, msg_len: usize) void;

// Define root.log to override the std implementation
pub fn log(
    comptime _: std.log.Level,
    comptime _: @TypeOf(.EnumLiteral),
    comptime fmt: []const u8,
    value: anytype,
) void {
    // Print the message to stderr, silently ignoring any errors
    // std.debug.getStderrMutex().lock();
    // defer std.debug.getStderrMutex().unlock();
    // const stderr = std.io.getStdErr().writer();
    // nosuspend stderr.print(format, args) catch return;
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

    var str = std.fmt.allocPrint(allocator, fmt, value) catch return;
    defer allocator.free(str);
    wasm_print(str.ptr, str.len);
}

export fn run(src: [*:0]const u8, src_len: usize) void {
    // TODO change this to the wee allocator?
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

    // TODO: put debug bool behind an args flag
    var options = lox.Options{};
    var vm = lox.Lox.init(options, allocator);
    defer vm.deinit();
    
    const code: []const u8 = src[0..src_len];
    vm.interpret(code) catch {
        wasm_print("uh oh", 5);
    };
}

export fn _wasm_alloc(len: usize) u32 {
    var buf = std.heap.page_allocator.alloc(u8, len) catch return 0;
    return @ptrToInt(buf.ptr);
}