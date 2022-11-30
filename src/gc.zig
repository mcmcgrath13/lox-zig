const std = @import("std");
const Allocator = std.mem.Allocator;
const ArrayList = std.ArrayList;

const VM = @import("vm.zig").VM;
const VariableHashMap = @import("vm.zig").VariableHashMap;

const vlu = @import("value.zig");
const Value = vlu.Value;

const Chunk = @import("chunk.zig").Chunk;

const ob = @import("object.zig");
const Obj = ob.Obj;
const ObjString = ob.ObjString;
const ObjClosure = ob.ObjClosure;
const ObjFunction = ob.ObjFunction;
const ObjClass = ob.ObjClass;
const ObjUpValue = ob.ObjUpValue;
const get_obj = ob.get_obj;

const common = @import("common.zig");

const log = std.log.scoped(.gc);

const GC_HEAP_GROW_FACTOR = 2;

pub const GCAllocator = struct {
    backing_allocator: Allocator,
    vm: ?*VM = null,

    debug_stress: bool,
    debug_log: bool,

    gray_stack: ArrayList(*Obj),

    bytes_allocated: usize = 0,
    next_gc: usize = 1024 * 1024,

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
            log.debug("-- gc begin \n", .{});
        }
        const before = self.bytes_allocated;

        self.mark_roots();
        self.trace_references();
        self.remove_weak_refs();
        self.sweep();

        self.next_gc = self.bytes_allocated * GC_HEAP_GROW_FACTOR;

        if (self.debug_log) {
            log.debug("-- gc end \n", .{});
            log.debug("collected {d} bytes (from {d} to {d}) - next garbage collection at {d}\n", .{ before - self.bytes_allocated, before, self.bytes_allocated, self.next_gc });
        }
    }

    fn mark_roots(self: *GCAllocator) void {
        // the stack
        for (self.vm.?.stack) |*slot| {
            if (@ptrToInt(slot) == @ptrToInt(self.vm.?.stack_top)) break;

            self.mark_value(slot);
        }

        self.mark_globals();
        self.mark_frames();
        self.mark_upvalues();
        if (self.vm.?.init_string) |s| {
            self.mark_object(get_obj(ObjString, s));
        }
    }

    fn mark_value(self: *GCAllocator, value: *Value) void {
        if (value.is_obj()) self.mark_object(value.as_obj());
    }

    fn mark_object(self: *GCAllocator, obj: *Obj) void {
        if (obj.is_marked) return;

        obj.is_marked = true;
        common.write_or_die(*Obj, &self.gray_stack, obj);
        if (self.debug_log) {
            log.debug("{*} mark {any}\n", .{ obj, Value.obj(obj) });
        }
    }

    fn mark_globals(self: *GCAllocator) void {
        self.mark_table(&self.vm.?.globals);
    }

    fn mark_table(self: *GCAllocator, table: *VariableHashMap) void {
        var it = table.iterator();
        while (it.next()) |entry| {
            self.mark_object(get_obj(ObjString, entry.key_ptr.*));
            self.mark_value(entry.value_ptr);
        }
    }

    fn mark_frames(self: *GCAllocator) void {
        for (self.vm.?.frames) |frame, index| {
            if (index == self.vm.?.frame_count) break;

            self.mark_object(get_obj(ObjClosure, frame.closure));
        }
    }

    fn mark_upvalues(self: *GCAllocator) void {
        var upvalue = self.vm.?.open_upvalues;
        while (upvalue) |uv| {
            self.mark_object(get_obj(ObjUpValue, uv));
            upvalue = uv.next;
        }
    }

    fn mark_constants(self: *GCAllocator, chunk: *Chunk) void {
        for (chunk.constants.items) |*item| {
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
            log.debug("{*} blacken {any}\n", .{ obj, Value.obj(obj) });
        }

        switch (obj.t) {
            .upvalue => {
                self.mark_value(&obj.as_upvalue().closed);
            },
            .function => {
                var function = obj.as_function();
                if (function.name) |name| {
                    self.mark_object(get_obj(ObjString, name));
                }
                self.mark_constants(&function.chunk);
            },
            .closure => {
                var closure = obj.as_closure();
                self.mark_object(get_obj(ObjFunction, closure.function));
                for (closure.upvalues) |upvalue| {
                    if (upvalue) |uv| {
                        self.mark_object(get_obj(ObjUpValue, uv));
                    }
                }
            },
            .class => {
                var class = obj.as_class();
                self.mark_object(get_obj(ObjString, class.name));
                self.mark_table(&class.methods);
            },
            .instance => {
                var instance = obj.as_instance();
                self.mark_object(get_obj(ObjClass, instance.class));
                self.mark_table(&instance.fields);
            },
            .bound_method => {
                var bound_method = obj.as_bound_method();
                self.mark_value(&bound_method.receiver);
                self.mark_object(get_obj(ObjClosure, bound_method.method));
            },
            else => {},
        }
    }

    fn remove_weak_refs(self: *GCAllocator) void {
        var strings = self.vm.?.strings;
        var iter = strings.keyIterator();
        while (iter.next()) |key| {
            if (!get_obj(ObjString, key.*).is_marked) {
                if (self.debug_log) {
                    log.debug("removing interned string {s}\n", .{key.*});
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
            log.debug("{*} allocate {d}\n", .{ res, len });
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
            log.debug("{*} resized from {d} to {d}\n", .{
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
            log.debug("{*} freed {d}\n", .{ old_mem, old_mem.len });
        }
    }
};
