const std = @import("std");
const ArrayListUnmanaged = std.ArrayListUnmanaged;

const Value = @import("value.zig").Value;

pub const OpCode = enum(u8) {
    constant,
    define_global,
    get_global,
    set_global,
    get_local,
    set_local,
    get_property,
    set_property,
    get_upvalue,
    set_upvalue,
    close_upvalue,
    get_super,
    jump_if_false,
    jump,
    loop,
    _return,
    print,
    pop,
    call,
    closure,
    class,
    method,
    invoke,
    super_invoke,
    inherit,

    // literals
    nil,
    _true,
    _false,

    // unary
    negate,
    not,

    // binary
    add,
    divide,
    multiply,
    subtract,
    equal,
    less,
    greater,

    // fallthrough - allows error handling/default case
    _,
};

// this should maybe be ArrayList instead
pub const Chunk = struct {
    code: ArrayListUnmanaged(u8) = ArrayListUnmanaged(u8){},
    lines: ArrayListUnmanaged(usize) = ArrayListUnmanaged(usize){},
    constants: ArrayListUnmanaged(Value) = ArrayListUnmanaged(Value){},

    allocator: std.mem.Allocator,

    pub fn init(allocator: std.mem.Allocator) Chunk {
        return Chunk{
            .allocator = allocator,
        };
    }

    pub fn deinit(self: *Chunk) void {
        self.code.deinit(self.allocator);
        self.lines.deinit(self.allocator);
        self.constants.deinit(self.allocator);
    }

    // if we're out of memory, bail out
    pub fn write(self: *Chunk, byte: u8, line: usize) void {
        self.write_or_die(u8, &self.code, byte);
        self.write_or_die(usize, &self.lines, line);
    }

    pub fn add_constant(self: *Chunk, value: Value) usize {
        self.write_or_die(Value, &self.constants, value);
        return self.constants.items.len - 1;
    }

    pub fn length(self: *Chunk) usize {
        return self.code.items.len;
    }

    fn write_or_die(
        self: *Chunk,
        comptime T: type,
        list: *ArrayListUnmanaged(T),
        value: T,
    ) void {
        list.append(self.allocator, value) catch {
            std.debug.print("Out of memory\n", .{});
            std.process.exit(1);
        };
    }
};
