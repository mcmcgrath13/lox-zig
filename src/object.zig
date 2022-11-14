const std = @import("std");

const common = @import("common.zig");

const Chunk = @import("chunk.zig").Chunk;

const Value = @import("value.zig").Value;

const vm = @import("vm.zig");
const ObjStringHashMap = vm.ObjStringHashMap;
const VariableHashMap = vm.VariableHashMap;

pub const ObjType = union(enum) {
    string: *ObjString,
    function: *ObjFunction,
    native: *ObjNative,
    closure: *ObjClosure,
    upvalue: *ObjUpValue,
    class: *ObjClass,
    instance: *ObjInstance,

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

    pub fn upvalue(obj: *ObjUpValue) ObjType {
        return ObjType{ .upvalue = obj };
    }

    pub fn class(obj: *ObjClass) ObjType {
        return ObjType{ .class = obj };
    }

    pub fn instance(obj: *ObjInstance) ObjType {
        return ObjType{ .instance = obj };
    }

    pub fn deinit(self: *ObjType, allocator: std.mem.Allocator) void {
        switch (self.*) {
            .string => {
                self.string.deinit();
                allocator.destroy(self.string);
            },
            .function => {
                self.function.deinit();
                allocator.destroy(self.function);
            },
            .native => {
                allocator.destroy(self.native);
            },
            .closure => {
                self.closure.deinit();
                allocator.destroy(self.closure);
            },
            .upvalue => {
                allocator.destroy(self.upvalue);
            },
            .class => {
                allocator.destroy(self.class);
            },
            .instance => {
                self.instance.deinit();
                allocator.destroy(self.instance);
            },
        }
    }
};

pub const Obj = struct {
    t: ObjType,
    is_marked: bool = false,
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

    pub fn as_upvalue(self: *Obj) *ObjUpValue {
        switch (self.t) {
            .upvalue => return self.t.upvalue,
            else => unreachable,
        }
    }

    pub fn as_class(self: *Obj) *ObjClass {
        switch (self.t) {
            .class => return self.t.class,
            else => unreachable,
        }
    }

    pub fn as_instance(self: *Obj) *ObjInstance {
        switch (self.t) {
            .instance => return self.t.instance,
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
            .upvalue => try writer.print("{}", .{self.t.upvalue}),
            .class => try writer.print("{}", .{self.t.class}),
            .instance => try writer.print("{}", .{self.t.instance}),
        }
    }
};

fn alloc_obj(
    objt: ObjType,
    objects: *?*Obj,
    allocator: std.mem.Allocator,
) *Obj {
    var obj = common.create_or_die(allocator, Obj);
    obj.* = Obj.init(objt);
    obj.update_next(objects);

    // set the header on objt payload
    const tag_obj = std.meta.activeTag(objt);
    const info = @typeInfo(ObjType).Union;
    const UnionTag = info.tag_type.?;

    inline for (info.fields) |field_info| {
        if (@field(UnionTag, field_info.name) == tag_obj) {
            const field = @field(objt, field_info.name);
            field.header = obj;
            break;
        }
    }

    return obj;
}

// ======== OBJ STRING ==========
pub const ObjString = struct {
    header: ?*Obj = null,

    data: []const u8,

    allocator: std.mem.Allocator,

    pub fn init(string: []const u8, allocator: std.mem.Allocator, take: bool) ObjString {
        const data = if (!take) ifb: {
            const data = common.alloc_or_die(allocator, u8, string.len);
            std.mem.copy(u8, data, string);
            break :ifb data;
        } else elseb: {
            break :elseb string;
        };

        return ObjString{ .data = data, .allocator = allocator };
    }

    pub fn deinit(self: *ObjString) void {
        self.allocator.free(self.data);
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
    objects: *?*Obj,
    allocator: std.mem.Allocator,
) *Obj {
    // new_objstr is a placeholder for the results of the if
    var objstr: *ObjString = new_objstr;
    if (strings.getKey(new_objstr)) |interned_objstr| {
        objstr = interned_objstr;
        new_objstr.deinit();
    } else {
        objstr = common.create_or_die(allocator, ObjString);
        objstr.* = new_objstr.*;
        strings.put(objstr, {}) catch {
            std.debug.print("Out of memory\n", .{});
            std.process.exit(1);
        };
    }

    if (objstr.header) |obj| {
        return obj;
    } else {
        var objt = ObjType.string(objstr);
        return alloc_obj(objt, objects, allocator);
    }
}

pub fn new_string(
    strings: *ObjStringHashMap,
    string: []const u8,
    objects: *?*Obj,
    allocator: std.mem.Allocator,
    take: bool,
) *Obj {
    var new_objstr = ObjString.init(string, allocator, take);
    return get_or_put_interned_string(strings, &new_objstr, objects, allocator);
}

// ========= OBJ FUNCTION =======
pub const ObjFunction = struct {
    header: ?*Obj = null,

    arity: u8 = 0,
    upvalue_count: u8 = 0,
    chunk: Chunk,
    name: ?*ObjString = null,

    pub fn init(
        allocator: std.mem.Allocator,
    ) ObjFunction {
        var chunk = Chunk.init(allocator);
        return .{ .chunk = chunk };
    }

    pub fn deinit(self: *ObjFunction) void {
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
    header: ?*Obj = null,

    function: *ObjFunction,
    upvalues: []?*ObjUpValue,

    allocator: std.mem.Allocator,

    pub fn init(
        function: *ObjFunction,
        allocator: std.mem.Allocator,
    ) ObjClosure {
        var upvalues = common.alloc_or_die(
            allocator,
            ?*ObjUpValue,
            function.upvalue_count,
        );
        std.mem.set(?*ObjUpValue, upvalues, null);

        return ObjClosure{
            .function = function,
            .upvalues = upvalues,
            .allocator = allocator,
        };
    }

    pub fn deinit(self: *ObjClosure) void {
        self.allocator.free(self.upvalues);
    }

    pub fn format(
        self: ObjClosure,
        comptime fmt: []const u8,
        options: std.fmt.FormatOptions,
        writer: anytype,
    ) !void {
        _ = fmt;
        _ = options;

        try writer.print("{} (closure)", .{self.function});
    }
};

pub fn new_closure(
    function: *ObjFunction,
    objects: *?*Obj,
    allocator: std.mem.Allocator,
) *Obj {
    var objclosure = common.create_or_die(allocator, ObjClosure);
    objclosure.* = ObjClosure.init(function, allocator);
    var objt = ObjType.closure(objclosure);
    return alloc_obj(objt, objects, allocator);
}

// ============ OBJ UPVALUE ============
pub const ObjUpValue = struct {
    header: ?*Obj = null,

    location: *Value,
    closed: Value = Value.nil(),
    next: ?*ObjUpValue = null,

    pub fn init(location: *Value) ObjUpValue {
        return ObjUpValue{ .location = location };
    }

    pub fn format(
        self: ObjUpValue,
        comptime fmt: []const u8,
        options: std.fmt.FormatOptions,
        writer: anytype,
    ) !void {
        _ = fmt;
        _ = options;

        try writer.print("upvalue: {}", .{self.location});
    }
};

pub fn new_upvalue(
    location: *Value,
    objects: *?*Obj,
    allocator: std.mem.Allocator,
) *Obj {
    var objupvalue = common.create_or_die(allocator, ObjUpValue);
    objupvalue.* = ObjUpValue.init(location);
    var objt = ObjType.upvalue(objupvalue);
    return alloc_obj(objt, objects, allocator);
}

// ========= OBJ NATIVE =======
pub const NativeFn = fn (arg_count: u8, args: [*]Value) Value;

pub const ObjNative = struct {
    header: ?*Obj = null,

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

// ============ OBJ CLASS ============

pub const ObjClass = struct {
    header: ?*Obj = null,

    name: *ObjString,

    pub fn init(name: *ObjString) ObjClass {
        return ObjClass{ .name = name };
    }

    pub fn format(
        self: ObjClass,
        comptime fmt: []const u8,
        options: std.fmt.FormatOptions,
        writer: anytype,
    ) !void {
        _ = fmt;
        _ = options;

        try writer.print("{s}", .{self.name});
    }
};

pub fn new_class(
    name: *ObjString,
    objects: *?*Obj,
    allocator: std.mem.Allocator,
) *Obj {
    var objclass = common.create_or_die(allocator, ObjClass);
    objclass.* = ObjClass.init(name);
    var objt = ObjType.class(objclass);
    return alloc_obj(objt, objects, allocator);
}

// ============ OBJ INSTANCE ============

pub const ObjInstance = struct {
    header: ?*Obj = null,

    class: *ObjClass,
    fields: VariableHashMap,

    pub fn init(class: *ObjClass, allocator: std.mem.Allocator) ObjInstance {
        return ObjInstance{
            .class = class,
            .fields = VariableHashMap.init(allocator),
        };
    }

    pub fn deinit(self: *ObjInstance) void {
        self.fields.deinit();
    }

    pub fn format(
        self: ObjInstance,
        comptime fmt: []const u8,
        options: std.fmt.FormatOptions,
        writer: anytype,
    ) !void {
        _ = fmt;
        _ = options;

        try writer.print("{s} instance", .{self.class});
    }
};

pub fn new_instance(
    class: *ObjClass,
    objects: *?*Obj,
    allocator: std.mem.Allocator,
) *Obj {
    var objinstance = common.create_or_die(allocator, ObjInstance);
    objinstance.* = ObjInstance.init(class, allocator);
    var objt = ObjType.instance(objinstance);
    return alloc_obj(objt, objects, allocator);
}
