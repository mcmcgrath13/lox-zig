const std = @import("std");

pub fn build(b: *std.build.Builder) !void {
    const target = b.standardTargetOptions(.{});
    const mode = b.standardReleaseOptions();
    
    const wasm_lib = b.option(bool, "wasm", "build as a library for wasm") orelse false;

    const value_union = b.option(bool, "value-union", "use union values instead of packed values") orelse false;
    
    const exe_options = b.addOptions();
    exe_options.addOption(bool, "value_union", value_union);

    if (wasm_lib) {
        return build_wasm(b, exe_options);
    }

    const exe = b.addExecutable("lox", "src/runner.zig");
    exe.setTarget(target);
    exe.setBuildMode(mode);
    exe.addOptions("build_options", exe_options);
    exe.install();

    const run_cmd = exe.run();
    run_cmd.step.dependOn(b.getInstallStep());
    if (b.args) |args| {
        run_cmd.addArgs(args);
    }

    const run_step = b.step("run", "Run the app");
    run_step.dependOn(&run_cmd.step);


    
}

fn build_wasm(b: *std.build.Builder, options: *std.build.OptionsStep) !void {
    // Standard release options allow the person running `zig build` to select
    // between Debug, ReleaseSafe, ReleaseFast, and ReleaseSmall.
    const mode = b.standardReleaseOptions();
    
    const lib = b.addSharedLibrary("lox", "src/wasm.zig", .unversioned);
    lib.addSystemIncludeDir("src");
    lib.setTarget(try std.zig.CrossTarget.parse(.{.arch_os_abi = "wasm32-freestanding"}));
    lib.addOptions("build_options", options);
    lib.setBuildMode(mode);
    lib.install();
}
