const std = @import("std");

const common = @import("common.zig");

const ObjStringHashMap = @import("vm.zig").ObjStringHashMap;

pub const ObjType = union(enum) {
    string: *ObjString,

    pub fn string(obj: *ObjString) ObjType {
        return ObjType{ .string = obj };
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
            },
        }
    }

    pub fn as_string(self: *Obj) *ObjString {
        switch (self.t) {
            .string => return self.t.string,
        }
    }
};

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

pub fn alloc_obj(objt: ObjType, allocator: std.mem.Allocator) *Obj {
    var obj = common.create_or_die(allocator, Obj);
    obj.* = Obj.init(objt);
    return obj;
}

fn get_or_put_interned_string(
    strings: *ObjStringHashMap,
    new_objstr: *ObjString,
    allocator: std.mem.Allocator,
) *Obj {
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

    var objt = ObjType.string(objstr);

    return alloc_obj(objt, allocator);
}

pub fn alloc_string(
    strings: *ObjStringHashMap,
    string: []const u8,
    allocator: std.mem.Allocator,
) *Obj {
    var new_objstr = ObjString.init(string, allocator);
    return get_or_put_interned_string(strings, &new_objstr, allocator);
}

pub fn take_string(
    strings: *ObjStringHashMap,
    data: []const u8,
    allocator: std.mem.Allocator,
) *Obj {
    var new_objstr = ObjString.take(data);
    return get_or_put_interned_string(strings, &new_objstr, allocator);
}

pub fn print_obj(obj: *Obj) void {
    switch (obj.t) {
        .string => std.debug.print("'{s}'", .{obj.t.string.data}),
    }
}
