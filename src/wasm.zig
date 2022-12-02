// this file is the entrypoint for the wasm library
const std = @import("std");

const lox = @import("lox.zig");

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
    vm.interpret(code) catch {};
}