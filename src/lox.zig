// this file is the entrypoint for the lox-zig library
const vm = @import("vm.zig");
pub const VM = vm.VM;
pub const Options = vm.Options;
pub const InterpretError = vm.InterpretError;
