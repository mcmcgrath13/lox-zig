const std = @import("std");

pub fn build(b: *std.build.Builder) void {
    const target = b.standardTargetOptions(.{});
    const mode = b.standardReleaseOptions();

    const value_union = b.option(bool, "value-union", "use union values instead of packed values") orelse false;
    
    const exe_options = b.addOptions();
    exe_options.addOption(bool, "value_union", value_union);

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


    // Standard release options allow the person running `zig build` to select
    // between Debug, ReleaseSafe, ReleaseFast, and ReleaseSmall.
    // const mode = b.standardReleaseOptions();
    //
    // const lib = b.addStaticLibrary("lox-zig", "src/main.zig");
    // lib.setBuildMode(mode);
    // lib.install();
    //
    // const main_tests = b.addTest("src/main.zig");
    // main_tests.setBuildMode(mode);
    //
    // const test_step = b.step("test", "Run library tests");
    // test_step.dependOn(&main_tests.step);
}
