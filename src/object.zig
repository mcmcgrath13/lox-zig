const std = @import("std");

const common = @import("common.zig");

const Chunk = @import("chunk.zig").Chunk;

const Value = @import("value.zig").Value;

const vm = @import("vm.zig");
const ObjStringHashMap = vm.ObjStringHashMap;
const VariableHashMap = vm.VariableHashMap;

const ObjType = enum {
    string,
    function,
    native,
    closure,
    upvalue,
    class,
    instance,
    bound_method,
};

pub const Obj = struct {
    t: ObjType,
    is_marked: bool = false,
    next: ?*Obj = null,

    pub fn init(t: ObjType) Obj {
        return Obj{ .t = t };
    }

    pub fn deinit(self: *Obj, allocator: std.mem.Allocator) void {
        switch (self.t) {
            .string => {
                self.as_string().deinit(allocator);
            },
            .function => {
                self.as_function().deinit();
            },
            .closure => {
                self.as_closure().deinit(allocator);
            },
            .class => {
                self.as_class().deinit();
            },
            .instance => {
                self.as_instance().deinit();
            },
            else => {},
        }
    }

    fn update_next(obj: *Obj, head: *?*Obj) void {
        if (head.*) |o| {
            obj.next = o;
        }
        head.* = obj;
    }

    pub fn equals(self: *Obj, other: *Obj) bool {
        return self == other;
    }

    fn cast(self: *Obj, comptime T: type) *T {
        var ptr = @ptrCast([*]Obj, self);
        return @ptrCast(*T, @alignCast(@alignOf(T), ptr + 1));
    }

    pub fn as_string(self: *Obj) *ObjString {
        return self.cast(ObjString);
    }

    pub fn as_function(self: *Obj) *ObjFunction {
        return self.cast(ObjFunction);
    }

    pub fn as_native(self: *Obj) *ObjNative {
        return self.cast(ObjNative);
    }

    pub fn as_closure(self: *Obj) *ObjClosure {
        return self.cast(ObjClosure);
    }

    pub fn as_upvalue(self: *Obj) *ObjUpValue {
        return self.cast(ObjUpValue);
    }

    pub fn as_class(self: *Obj) *ObjClass {
        return self.cast(ObjClass);
    }

    pub fn as_instance(self: *Obj) *ObjInstance {
        return self.cast(ObjInstance);
    }

    pub fn as_bound_method(self: *Obj) *ObjBoundMethod {
        return self.cast(ObjBoundMethod);
    }

    pub fn print(
        self: *Obj,
        writer: anytype,
    ) !void {
        switch (self.t) {
            .string => try writer.print("{s}", .{self.as_string()}),
            .function => try writer.print("{}", .{self.as_function()}),
            .native => try writer.print("{}", .{self.as_native()}),
            .closure => try writer.print("{}", .{self.as_closure()}),
            .upvalue => try writer.print("{}", .{self.as_upvalue()}),
            .class => try writer.print("{}", .{self.as_class()}),
            .instance => try writer.print("{}", .{self.as_instance()}),
            .bound_method => try writer.print("{}", .{self.as_bound_method()}),
        }
    }
};

fn alloc_obj(
    comptime T: type,
    objt: T,
    obj_type: ObjType,
    objects: *?*Obj,
    allocator: std.mem.Allocator,
) *Obj {
    var memory = common.alloc_aligned_or_die(allocator, @maximum(@alignOf(Obj), @alignOf(T)), @sizeOf(Obj) + @sizeOf(T));
    var ptr = memory.ptr;

    var obj_ptr = @ptrCast(*Obj, ptr);
    obj_ptr.* = Obj.init(obj_type);
    obj_ptr.update_next(objects);

    var objt_ptr = @ptrCast(*T, ptr + @sizeOf(Obj));
    objt_ptr.* = objt;

    return obj_ptr;
}

pub fn get_obj(comptime T: type, objt: *T) *Obj {
    var ptr = @ptrCast([*]Obj, objt);
    return @ptrCast(*Obj, ptr - 1);
}

// ======== OBJ STRING ==========
pub const ObjString = struct {
    data: []const u8,

    pub fn init(string: []const u8, allocator: std.mem.Allocator, take: bool) ObjString {
        const data = if (!take) ifb: {
            const data = common.alloc_or_die(allocator, u8, string.len);
            std.mem.copy(u8, data, string);
            break :ifb data;
        } else elseb: {
            break :elseb string;
        };

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
    objects: *?*Obj,
    allocator: std.mem.Allocator,
) *Obj {
    if (strings.getKey(new_objstr)) |interned_objstr| {
        var objstr = interned_objstr;
        new_objstr.deinit(allocator);
        return get_obj(ObjString, objstr);
    } else {
        var obj = alloc_obj(ObjString, new_objstr.*, .string, objects, allocator);
        var objstr = obj.as_string();
        strings.put(objstr, {}) catch {
            @panic("Out of memory\n");
        };
        return obj;
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
    return alloc_obj(
        ObjFunction,
        ObjFunction.init(allocator),
        .function,
        objects,
        allocator,
    );
}

// =========== OBJ CLOSURE ============

pub const ObjClosure = struct {
    function: *ObjFunction,
    upvalues: []?*ObjUpValue,

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
        };
    }

    pub fn deinit(self: *ObjClosure, allocator: std.mem.Allocator) void {
        allocator.free(self.upvalues);
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
    return alloc_obj(
        ObjClosure,
        ObjClosure.init(function, allocator),
        .closure,
        objects,
        allocator,
    );
}

// ============ OBJ UPVALUE ============
pub const ObjUpValue = struct {
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
    return alloc_obj(ObjUpValue, ObjUpValue.init(location), .upvalue, objects, allocator);
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
    return alloc_obj(ObjNative, ObjNative.init(function), .native, objects, allocator);
}

// ============ OBJ CLASS ============

pub const ObjClass = struct {
    name: *ObjString,
    methods: VariableHashMap,

    pub fn init(name: *ObjString, allocator: std.mem.Allocator) ObjClass {
        return ObjClass{ .name = name, .methods = VariableHashMap.init(allocator) };
    }

    pub fn deinit(self: *ObjClass) void {
        self.methods.deinit();
    }

    pub fn inherit(self: *ObjClass, super: *ObjClass) void {
        var new_methods = super.methods.clone() catch {
            @panic("out of memory\n");
        };
        self.methods.deinit();
        self.methods = new_methods;
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
    return alloc_obj(ObjClass, ObjClass.init(name, allocator), .class, objects, allocator);
}

// ============ OBJ INSTANCE ============

pub const ObjInstance = struct {
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
    return alloc_obj(
        ObjInstance,
        ObjInstance.init(class, allocator),
        .instance,
        objects,
        allocator,
    );
}

// ============ OBJ BOUND METHOD ============

pub const ObjBoundMethod = struct {
    receiver: Value,
    method: *ObjClosure,

    pub fn init(receiver: Value, method: *ObjClosure) ObjBoundMethod {
        return ObjBoundMethod{
            .receiver = receiver,
            .method = method,
        };
    }

    pub fn format(
        self: ObjBoundMethod,
        comptime fmt: []const u8,
        options: std.fmt.FormatOptions,
        writer: anytype,
    ) !void {
        _ = fmt;
        _ = options;

        try writer.print("{s}", .{self.method});
    }
};

pub fn new_bound_method(
    receiver: Value,
    method: *ObjClosure,
    objects: *?*Obj,
    allocator: std.mem.Allocator,
) *Obj {
    return alloc_obj(
        ObjBoundMethod,
        ObjBoundMethod.init(receiver, method),
        .bound_method,
        objects,
        allocator,
    );
}
