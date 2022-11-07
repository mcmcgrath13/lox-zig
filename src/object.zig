const std = @import("std");

const common = @import("common.zig");

const Chunk = @import("chunk.zig").Chunk;

const Value = @import("value.zig").Value;

const ObjStringHashMap = @import("vm.zig").ObjStringHashMap;

pub const ObjType = union(enum) {
    string: *ObjString,
    function: *ObjFunction,
    native: *ObjNative,
    closure: *ObjClosure,

    pub fn string(obj: *ObjString) ObjType {
        return ObjType{ .string = obj };
    }

    pub fn function(obj: *ObjFunction) ObjType {
        return ObjType{ .function = obj };
    }

    pub fn native(obj: *ObjNative) ObjType {
        return ObjType{ .native = obj };
    }

    pub fn closure(obj: *ObjClosure) ObjType {
        return ObjType{ .closure = obj };
    }

    pub fn deinit(self: *ObjType, allocator: std.mem.Allocator) void {
        switch (self.*) {
            .string => free_objstr(self.string, allocator),
            .function => {
                if (self.function.name) |name| free_objstr(name, allocator);
                self.function.deinit(allocator);
                allocator.destroy(self.function);
            },
            .native => {
                allocator.destroy(self.native);
            },
            .closure => {
                allocator.destroy(self.closure);
            },
        }
    }
};

fn free_objstr(objstr: *ObjString, allocator: std.mem.Allocator) void {
    objstr.refs -= 1;
    if (objstr.refs == 0) {
        objstr.deinit(allocator);
        allocator.destroy(objstr);
    }
}

pub const Obj = struct {
    t: ObjType,
    next: ?*Obj = null,

    pub fn init(t: ObjType) Obj {
        return Obj{ .t = t };
    }

    pub fn deinit(self: *Obj, allocator: std.mem.Allocator) void {
        self.t.deinit(allocator);
    }

    fn update_next(obj: *Obj, head: *?*Obj) void {
        if (head.*) |o| {
            obj.next = o;
        }
        head.* = obj;
    }

    pub fn equals(self: *Obj, other: *Obj) bool {
        return std.meta.eql(self.t, other.t);
    }

    pub fn as_string(self: *Obj) *ObjString {
        switch (self.t) {
            .string => return self.t.string,
            else => unreachable,
        }
    }

    pub fn as_function(self: *Obj) *ObjFunction {
        switch (self.t) {
            .function => return self.t.function,
            else => unreachable,
        }
    }

    pub fn as_native(self: *Obj) *ObjNative {
        switch (self.t) {
            .native => return self.t.native,
            else => unreachable,
        }
    }

    pub fn as_closure(self: *Obj) *ObjClosure {
        switch (self.t) {
            .closure => return self.t.closure,
            else => unreachable,
        }
    }

    pub fn format(
        self: Obj,
        comptime fmt: []const u8,
        options: std.fmt.FormatOptions,
        writer: anytype,
    ) !void {
        _ = fmt;
        _ = options;

        switch (self.t) {
            .string => try writer.print("\"{s}\"", .{self.t.string}),
            .function => try writer.print("{}", .{self.t.function}),
            .native => try writer.print("{}", .{self.t.native}),
            .closure => try writer.print("{}", .{self.t.closure}),
        }
    }
};

pub fn alloc_obj(
    objt: ObjType,
    objects: *?*Obj,
    allocator: std.mem.Allocator,
) *Obj {
    var obj = common.create_or_die(allocator, Obj);
    obj.* = Obj.init(objt);
    obj.update_next(objects);
    return obj;
}

// ======== OBJ STRING ==========
pub const ObjString = struct {
    data: []const u8,
    refs: usize = 1,

    pub fn init(string: []const u8, allocator: std.mem.Allocator) ObjString {
        const data = common.alloc_or_die(allocator, u8, string.len);
        std.mem.copy(u8, data, string);
        return ObjString{ .data = data };
    }

    pub fn take(data: []const u8) ObjString {
        return ObjString{ .data = data };
    }

    pub fn deinit(self: *ObjString, allocator: std.mem.Allocator) void {
        allocator.free(self.data);
    }

    pub fn format(
        self: ObjString,
        comptime fmt: []const u8,
        options: std.fmt.FormatOptions,
        writer: anytype,
    ) !void {
        _ = fmt;
        _ = options;

        try writer.print("{s}", .{self.data});
    }
};

// Context for hash map - can't use default string one as we want to get back
// the ObjString pointer, this uses the same methods, but defers to the data
// for equality and hashing
pub const ObjStringContext = struct {
    pub fn hash(self: @This(), s: *ObjString) u64 {
        _ = self;
        return std.hash_map.hashString(s.data);
    }
    pub fn eql(self: @This(), a: *ObjString, b: *ObjString) bool {
        _ = self;
        return std.hash_map.eqlString(a.data, b.data);
    }
};

fn get_or_put_interned_string(
    strings: *ObjStringHashMap,
    new_objstr: *ObjString,
    allocator: std.mem.Allocator,
) *ObjString {
    // new_objstr is a placeholder for the results of the if
    var objstr: *ObjString = new_objstr;
    if (strings.getKey(new_objstr)) |interned_objstr| {
        objstr = interned_objstr;
        objstr.refs += 1;
        new_objstr.deinit(allocator);
    } else {
        objstr = common.create_or_die(allocator, ObjString);
        objstr.* = new_objstr.*;
        strings.put(objstr, {}) catch {
            std.debug.print("Out of memory\n", .{});
            std.process.exit(1);
        };
    }

    return objstr;
}

pub fn new_string(
    strings: *ObjStringHashMap,
    string: []const u8,
    objects: *?*Obj,
    allocator: std.mem.Allocator,
    take: bool,
) *Obj {
    var objstr = if (take) take_string(strings, string, allocator) else alloc_string(strings, string, allocator);
    var objt = ObjType.string(objstr);
    return alloc_obj(objt, objects, allocator);
}

pub fn alloc_string(
    strings: *ObjStringHashMap,
    string: []const u8,
    allocator: std.mem.Allocator,
) *ObjString {
    var new_objstr = ObjString.init(string, allocator);
    return get_or_put_interned_string(strings, &new_objstr, allocator);
}

pub fn take_string(
    strings: *ObjStringHashMap,
    data: []const u8,
    allocator: std.mem.Allocator,
) *ObjString {
    var new_objstr = ObjString.take(data);
    return get_or_put_interned_string(strings, &new_objstr, allocator);
}

// ========= OBJ FUNCTION =======
pub const ObjFunction = struct {
    arity: u8 = 0,
    chunk: Chunk,
    name: ?*ObjString = null,

    pub fn init(
        allocator: std.mem.Allocator,
    ) ObjFunction {
        var chunk = Chunk.init(allocator);
        return .{ .chunk = chunk };
    }

    pub fn deinit(self: *ObjFunction, _: std.mem.Allocator) void {
        self.chunk.deinit();
    }

    pub fn format(
        self: ObjFunction,
        comptime fmt: []const u8,
        options: std.fmt.FormatOptions,
        writer: anytype,
    ) !void {
        _ = fmt;
        _ = options;

        if (self.name) |name| {
            try writer.print("<fn {s}>", .{name});
        } else {
            try writer.print("<script>", .{});
        }
    }
};

pub fn new_function(
    objects: *?*Obj,
    allocator: std.mem.Allocator,
) *Obj {
    var objfunc = common.create_or_die(allocator, ObjFunction);
    objfunc.* = ObjFunction.init(allocator);
    var objt = ObjType.function(objfunc);
    return alloc_obj(objt, objects, allocator);
}

// =========== OBJ CLOSURE ============

pub const ObjClosure = struct {
    function: *ObjFunction,

    pub fn init(function: *ObjFunction) ObjClosure {
        return ObjClosure{ .function = function };
    }

    pub fn format(
        self: ObjClosure,
        comptime fmt: []const u8,
        options: std.fmt.FormatOptions,
        writer: anytype,
    ) !void {
        _ = fmt;
        _ = options;

        try writer.print("{}", .{self.function});
    }
};

pub fn new_closure(
    function: *ObjFunction,
    objects: *?*Obj,
    allocator: std.mem.Allocator,
) *Obj {
    var objclosure = common.create_or_die(allocator, ObjClosure);
    objclosure.* = ObjClosure.init(function);
    var objt = ObjType.closure(objclosure);
    return alloc_obj(objt, objects, allocator);
}

// ========= OBJ NATIVE =======
pub const NativeFn = fn (arg_count: u8, args: [*]Value) Value;

pub const ObjNative = struct {
    function: NativeFn,

    pub fn init(function: NativeFn) ObjNative {
        return ObjNative{ .function = function };
    }

    pub fn format(
        self: ObjNative,
        comptime fmt: []const u8,
        options: std.fmt.FormatOptions,
        writer: anytype,
    ) !void {
        _ = self;
        _ = fmt;
        _ = options;

        try writer.print("<native fn>", .{});
    }
};

pub fn new_native(
    function: NativeFn,
    objects: *?*Obj,
    allocator: std.mem.Allocator,
) *Obj {
    var objnative = common.create_or_die(allocator, ObjNative);
    objnative.* = ObjNative.init(function);
    var objt = ObjType.native(objnative);
    return alloc_obj(objt, objects, allocator);
}
