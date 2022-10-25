const std = @import("std");

const common = @import("common.zig");

pub const ObjType = union(enum) {
    string: *ObjString,

    pub fn string(obj: *ObjString) ObjType {
        return ObjType{ .string = obj };
    }

    pub fn deinit(self: *ObjType, allocator: std.mem.Allocator) void {
        switch (self.*) {
            .string => {
                self.string.deinit(allocator);
                allocator.destroy(self.string);
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

    pub fn as_string(self: *Obj) []const u8 {
        switch (self.t) {
            .string => return self.t.string.data,
        }
    }
};

pub const ObjString = struct {
    data: []const u8,

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

pub fn alloc_obj(objt: ObjType, allocator: std.mem.Allocator) *Obj {
    var obj = common.create_or_die(allocator, Obj);
    obj.* = Obj.init(objt);
    return obj;
}

pub fn alloc_string(string: []const u8, allocator: std.mem.Allocator) *Obj {
    var objstr = common.create_or_die(allocator, ObjString);
    objstr.* = ObjString.init(string, allocator);

    var objt = ObjType.string(objstr);

    return alloc_obj(objt, allocator);
}

pub fn take_string(data: []const u8, allocator: std.mem.Allocator) *Obj {
    var objstr = common.create_or_die(allocator, ObjString);
    objstr.* = ObjString.take(data);

    var objt = ObjType.string(objstr);

    return alloc_obj(objt, allocator);
}

pub fn print_obj(obj: *Obj) void {
    switch (obj.t) {
        .string => std.debug.print("'{s}'", .{obj.t.string.data}),
    }
}
