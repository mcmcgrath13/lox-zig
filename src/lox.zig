// this file is the entrypoint for the lox-zig library
pub const chunk = @import("chunk.zig");
pub const VM = @import("vm.zig").VM;
pub const InterpretResult = @import("vm.zig").InterpretResult;
