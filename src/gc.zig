const std = @import("std");
const Allocator = std.mem.Allocator;
const ArrayList = std.ArrayList;

const VM = @import("vm.zig").VM;

const vlu = @import("value.zig");
const Value = vlu.Value;
const ValueArray = vlu.ValueArray;

const Obj = @import("object.zig").Obj;

const common = @import("common.zig");

const GC_HEAP_GROW_FACTOR = 2;

pub const GCAllocator = struct {
    backing_allocator: Allocator,
    vm: ?*VM = null,

    debug_stress: bool,
    debug_log: bool,

    gray_stack: ArrayList(*Obj),

    bytes_allocated: usize = 0,
    next_gc: usize = 124,

    pub fn init(
        backing_allocator: Allocator,
        debug_stress: bool,
        debug_log: bool,
    ) GCAllocator {
        return GCAllocator{
            .backing_allocator = backing_allocator,
            .debug_stress = debug_stress,
            .debug_log = debug_log,
            .gray_stack = ArrayList(*Obj).init(backing_allocator),
        };
    }

    pub fn deinit(self: *GCAllocator) void {
        self.gray_stack.deinit();
    }

    pub fn allocator(self: *GCAllocator) Allocator {
        return Allocator.init(self, alloc, resize, free);
    }

    fn collect_garbage(self: *GCAllocator) void {
        if (self.vm == null or self.vm.?.compiling) return;

        if (self.debug_log) {
            std.debug.print("-- gc begin \n", .{});
        }
        const before = self.bytes_allocated;

        self.mark_roots();
        self.trace_references();
        self.remove_weak_refs();
        self.sweep();

        self.next_gc = self.bytes_allocated * GC_HEAP_GROW_FACTOR;

        if (self.debug_log) {
            std.debug.print("-- gc end \n", .{});
            std.debug.print("collected {d} bytes (from {d} to {d}) - next garbage collection at {d}\n", .{ before - self.bytes_allocated, before, self.bytes_allocated, self.next_gc });
        }
    }

    fn mark_roots(self: *GCAllocator) void {
        // the stack
        for (self.vm.?.stack) |*slot, index| {
            if (index == self.vm.?.stack_top) break;

            self.mark_value(slot);
        }

        self.mark_globals();
        self.mark_frames();
        self.mark_upvalues();
    }

    fn mark_value(self: *GCAllocator, value: *Value) void {
        if (value.is_obj()) self.mark_object(value.as_obj());
    }

    fn mark_object(self: *GCAllocator, obj: *Obj) void {
        if (obj.is_marked) return;

        obj.is_marked = true;
        common.write_or_die(*Obj, &self.gray_stack, obj);
        if (self.debug_log) {
            std.debug.print("{*} mark {any}\n", .{ obj, obj });
        }
    }

    fn mark_globals(self: *GCAllocator) void {
        var it = self.vm.?.globals.iterator();
        while (it.next()) |entry| {
            self.mark_object(entry.key_ptr.*.header.?);
            self.mark_value(entry.value_ptr);
        }
    }

    fn mark_frames(self: *GCAllocator) void {
        for (self.vm.?.frames) |frame, index| {
            if (index == self.vm.?.frame_count) break;

            self.mark_object(frame.closure.header.?);
        }
    }

    fn mark_upvalues(self: *GCAllocator) void {
        var upvalue = self.vm.?.open_upvalues;
        while (upvalue) |uv| {
            self.mark_object(uv.header.?);
            upvalue = uv.next;
        }
    }

    fn mark_array(self: *GCAllocator, array: *ValueArray) void {
        for (array.values.items) |*item| {
            self.mark_value(item);
        }
    }

    fn trace_references(self: *GCAllocator) void {
        while (self.gray_stack.items.len > 0) {
            var obj = self.gray_stack.pop();
            self.blacken_object(obj);
        }
    }

    fn blacken_object(self: *GCAllocator, obj: *Obj) void {
        if (self.debug_log) {
            std.debug.print("{*} blacken {any}\n", .{ obj, obj });
        }

        switch (obj.t) {
            .upvalue => {
                self.mark_value(&obj.as_upvalue().closed);
            },
            .function => {
                var function = obj.as_function();
                if (function.name) |name| {
                    self.mark_object(name.header.?);
                }
                self.mark_array(&function.chunk.constants);
            },
            .closure => {
                var closure = obj.as_closure();
                self.mark_object(closure.header.?);
                self.mark_object(closure.function.header.?);
                for (closure.upvalues) |upvalue| {
                    if (upvalue) |uv| {
                        self.mark_object(uv.header.?);
                    }
                }
            },
            else => {},
        }
    }

    fn remove_weak_refs(self: *GCAllocator) void {
        var strings = self.vm.?.strings;
        var iter = strings.keyIterator();
        while (iter.next()) |key| {
            // it's possible an ObjString has been interned, but its header not allocated yet
            if (key.*.header != null and !key.*.header.?.is_marked) {
                if (self.debug_log) {
                    std.debug.print("removing interned string {s}\n", .{key.*});
                }
                _ = strings.remove(key.*);
            }
        }
    }

    fn sweep(self: *GCAllocator) void {
        const gc_allocator = self.allocator();
        var previous: ?*Obj = null;
        var current = self.vm.?.objects;
        while (current) |object| {
            if (object.is_marked) {
                object.is_marked = false;
                previous = object;
                current = object.next;
            } else {
                var unreached = object;
                current = object.next;
                if (previous) |prev| {
                    prev.next = current;
                } else {
                    self.vm.?.objects = current;
                }

                unreached.deinit(gc_allocator);
                gc_allocator.destroy(unreached);
            }
        }
    }

    fn alloc(
        self: *GCAllocator,
        len: usize,
        ptr_align: u29,
        len_align: u29,
        ret_addr: usize,
    ) Allocator.Error![]u8 {
        if (self.debug_stress) self.collect_garbage();
        if (self.bytes_allocated > self.next_gc) self.collect_garbage();
        const res = try self.backing_allocator.rawAlloc(len, ptr_align, len_align, ret_addr);
        self.bytes_allocated += len;
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
        if (self.bytes_allocated > self.next_gc) self.collect_garbage();
        const res = self.backing_allocator.rawResize(old_mem, old_align, new_size, len_align, ret_addr);
        self.bytes_allocated += new_size - old_mem.len;
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
        self.bytes_allocated -= old_mem.len;
        if (self.debug_log) {
            std.debug.print("{*} freed {d}\n", .{ old_mem, old_mem.len });
        }
    }
};