const std = @import("std");
const Allocator = std.mem.Allocator;

pub const GCAllocator = struct {
    backing_allocator: Allocator,

    debug_stress: bool,
    debug_log: bool,

    pub fn init(
        backing_allocator: Allocator,
        debug_stress: bool,
        debug_log: bool,
    ) GCAllocator {
        return GCAllocator{
            .backing_allocator = backing_allocator,
            .debug_stress = debug_stress,
            .debug_log = debug_log,
        };
    }

    pub fn allocator(self: *GCAllocator) Allocator {
        return Allocator.init(self, alloc, resize, free);
    }

    fn collect_garbage(self: *GCAllocator) void {
        if (self.debug_log) {
            std.debug.print("-- gc begin \n", .{});
        }

        if (self.debug_log) {
            std.debug.print("-- gc end \n", .{});
        }
    }

    fn mark_roots(self: *GCAllocator) void {
        _ = self;
        // todo
    }

    fn alloc(
        self: *GCAllocator,
        len: usize,
        ptr_align: u29,
        len_align: u29,
        ret_addr: usize,
    ) Allocator.Error![]u8 {
        if (self.debug_stress) self.collect_garbage();
        const res = try self.backing_allocator.rawAlloc(len, ptr_align, len_align, ret_addr);
        if (self.debug_log) {
            std.debug.print("{*} allocate {d}\n", .{ res, len });
        }
        return res;
    }

    fn resize(
        self: *GCAllocator,
        old_mem: []u8,
        old_align: u29,
        new_size: usize,
        len_align: u29,
        ret_addr: usize,
    ) ?usize {
        if (self.debug_stress and new_size > old_mem.len) self.collect_garbage();
        const res = self.backing_allocator.rawResize(old_mem, old_align, new_size, len_align, ret_addr);
        if (self.debug_log and res != null) {
            std.debug.print("{*} resized from {d} to {d}\n", .{
                old_mem,
                old_mem.len,
                new_size,
            });
        }
        return res;
    }

    fn free(
        self: *GCAllocator,
        old_mem: []u8,
        old_align: u29,
        ret_addr: usize,
    ) void {
        self.backing_allocator.rawFree(old_mem, old_align, ret_addr);
        if (self.debug_log) {
            std.debug.print("{*} freed {d}\n", .{ old_mem, old_mem.len });
        }
    }
};
