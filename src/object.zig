const std = @import("std");

const common = @import("common.zig");

const Chunk = @import("chunk.zig").Chunk;

const ObjStringHashMap = @import("vm.zig").ObjStringHashMap;

pub const ObjType = union(enum) {
    string: *ObjString,
    function: *ObjFunction,

    pub fn string(obj: *ObjString) ObjType {
        return ObjType{ .string = obj };
    }

    pub fn function(obj: *ObjFunction) ObjType {
        return ObjType{ .function = obj };
    }

    pub fn deinit(self: *ObjType, allocator: std.mem.Allocator) void {
        switch (self.*) {
            .string => {
                self.string.refs -= 1;
                if (self.string.refs == 0) {
                    self.string.deinit(allocator);
                    allocator.destroy(self.string);
                }
            },
            .function => {
                self.function.deinit(allocator);
                allocator.destroy(self.function);
            },
        }
    }
};

pub const Obj = struct {
    t: ObjType,
    next: ?*Obj = null,

    pub fn init(t: ObjType) Obj {
        return Obj{ .t = t };
    }

    pub fn deinit(self: *Obj, allocator: std.mem.Allocator) void {
        self.t.deinit(allocator);
    }

    pub fn equals(self: *Obj, other: *Obj) bool {
        switch (self.t) {
            .string => switch (other.t) {
                .string => {
                    return self.t.string == other.t.string;
                },
                else => return false,
            },
            .function => switch (other.t) {
                .function => {
                    return self.t.function == other.t.function;
                },
                else => return false,
            },
        }
    }

    pub fn as_string(self: *Obj) *ObjString {
        switch (self.t) {
            .string => return self.t.string,
            else => unreachable,
        }
    }
};

pub fn alloc_obj(objt: ObjType, allocator: std.mem.Allocator) *Obj {
    var obj = common.create_or_die(allocator, Obj);
    obj.* = Obj.init(objt);
    return obj;
}

pub fn print_obj(obj: *Obj) void {
    switch (obj.t) {
        .string => std.debug.print("'{s}'", .{obj.t.string.data}),
        .function => std.debug.print("<fn {s}>", .{obj.t.function.name.data}),
    }
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

pub fn new_string(strings: *ObjStringHashMap, string: []const u8, allocator: std.mem.Allocator, take: bool) *Obj {
    var objstr = if (take) take_string(strings, string, allocator) else alloc_string(strings, string, allocator);
    var objt = ObjType.string(objstr);
    return alloc_obj(objt, allocator);
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
const ObjFunction = struct {
    arity: u8,
    chunk: Chunk,
    name: *ObjString,

    pub fn init(
        strings: *ObjStringHashMap,
        name: []const u8,
        arity: u8,
        allocator: std.mem.Allocator,
    ) ObjFunction {
        const chunk = Chunk.init(allocator);
        const name_objstr = alloc_string(strings, name, allocator);
        return .{ .arity = arity, .chunk = chunk, .name = name_objstr };
    }

    pub fn deinit(self: *ObjFunction, _: std.mem.Allocator) void {
        self.chunk.deinit();
    }
};

pub fn new_function(
    strings: *ObjStringHashMap,
    name: []const u8,
    arity: u8,
    allocator: std.mem.Allocator,
) *Obj {
    var objfunc = common.create_or_die(ObjFunction, allocator);
    objfunc.* = ObjFunction.init(strings, name, arity, allocator);
    var objt = ObjType.function(objfunc);
    return alloc_obj(objt, allocator);
}
