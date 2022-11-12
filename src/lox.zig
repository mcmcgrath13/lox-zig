// this file is the entrypoint for the lox-zig library
const std = @import("std");

const vml = @import("vm.zig");
pub const VM = vml.VM;
pub const Options = vml.Options;
pub const InterpretError = vml.InterpretError;
const GCAllocator = @import("gc.zig").GCAllocator;

pub const Lox = struct {
    vm: ?VM = null,
    options: Options,
    gc: GCAllocator,

    pub fn init(options: Options, allocator: std.mem.Allocator) Lox {
        var gc = GCAllocator.init(
            allocator,
            options.debug_stress_gc,
            options.debug_gc,
        );
        return Lox{ .gc = gc, .options = options };
    }

    pub fn deinit(self: *Lox) void {
        if (self.vm) |*vm| vm.deinit();
        self.gc.deinit();
    }

    pub fn interpret(self: *Lox, source: []const u8) InterpretError!void {
        if (self.vm == null) {
            self.vm = VM.init(self.options, &self.gc);
            self.gc.vm = &self.vm.?;
        }
        return self.vm.?.interpret(source);
    }
};
