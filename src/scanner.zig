const std = @import("std");

pub const Scanner = struct {
    // TODO: should these be sentinal terminated?
    start: [*]const u8,
    current: [*]const u8,
    line: usize = 1,
    end: [*]const u8,

    pub fn init(source: []const u8) Scanner {
        return Scanner{ .start = source.ptr, .current = source.ptr, .end = source.ptr + source.len };
    }

    pub fn scan_token(self: *Scanner) Token {
        self.skip_whitespace();
        self.start = self.current;

        if (self.is_at_end()) {
            return self.make_token(TokenType.eof);
        }

        const c = self.advance();

        if (is_digit(c)) return self.number();
        if (is_alpha(c)) return self.identifier();

        return switch (c) {
            '(' => self.make_token(TokenType.left_paren),
            ')' => self.make_token(TokenType.right_paren),
            '{' => self.make_token(TokenType.left_brace),
            '}' => self.make_token(TokenType.right_brace),
            ';' => self.make_token(TokenType.semicolon),
            ',' => self.make_token(TokenType.comma),
            '.' => self.make_token(TokenType.dot),
            '-' => self.make_token(TokenType.minus),
            '+' => self.make_token(TokenType.plus),
            '/' => self.make_token(TokenType.slash),
            '*' => self.make_token(TokenType.star),
            '!' => self.make_token(if (self.match('=')) TokenType.bang_equal else TokenType.bang),
            '=' => self.make_token(if (self.match('=')) TokenType.equal_equal else TokenType.equal),
            '<' => self.make_token(if (self.match('=')) TokenType.less_equal else TokenType.less),
            '>' => self.make_token(if (self.match('=')) TokenType.greater_equal else TokenType.greater),
            '"' => self.string(),
            else => self.error_token("unexpected character"),
        };
    }

    fn is_at_end(self: *Scanner) bool {
        return @ptrToInt(self.current) >= @ptrToInt(self.end);
    }

    fn peek(self: *Scanner) u8 {
        return self.current[0];
    }

    fn peek_next(self: *Scanner) u8 {
        if (self.is_at_end()) {
            return 0;
        }
        return self.current[1];
    }

    fn advance(self: *Scanner) u8 {
        const byte = self.peek();
        self.current += 1;
        return byte;
    }

    fn match(self: *Scanner, char: u8) bool {
        if (self.is_at_end()) {
            return false;
        }
        if (self.peek() == char) {
            self.current += 1;
            return true;
        }
        return false;
    }

    fn skip_whitespace(self: *Scanner) void {
        while (true) {
            _ = switch (self.peek()) {
                ' ' => self.advance(),
                '\t' => self.advance(),
                '\r' => self.advance(),
                '\n' => {
                    self.line += 1;
                    _ = self.advance();
                },
                '/' => {
                    if (self.peek_next() == '/') {
                        // a comment goes to the end of the line
                        while (self.peek() != '\n' and !self.is_at_end()) : (_ = self.advance()) {}
                    } else {
                        return;
                    }
                },
                else => return,
            };
        }
    }

    fn string(self: *Scanner) Token {
        while (self.peek() != '"' and !self.is_at_end()) : (_ = self.advance()) {
            if (self.peek() == '\n') {
                self.line += 1;
            }
        }

        if (self.is_at_end()) {
            return self.error_token("unterminated string");
        }

        // consume closing quote
        _ = self.advance();
        return self.make_token(TokenType.string);
    }

    fn number(self: *Scanner) Token {
        while (is_digit(self.peek())) : (_ = self.advance()) {}

        if (self.peek() == '.') {
            _ = self.advance();
            while (is_digit(self.peek())) : (_ = self.advance()) {}
        }

        return self.make_token(TokenType.number);
    }

    fn identifier(self: *Scanner) Token {
        while (is_alpha(self.peek()) or is_digit(self.peek())) : (_ = self.advance()) {}
        return self.make_token(self.identifier_type());
    }

    fn identifier_type(self: *Scanner) TokenType {
        return switch (self.start[0]) {
            'a' => self.check_keyword(1, 2, "nd", TokenType.logical_and),
            'c' => self.check_keyword(1, 4, "lass", TokenType.class),
            'e' => self.check_keyword(1, 3, "lse", TokenType.cf_else),
            'f' => blk: {
                if (@ptrToInt(self.current) - @ptrToInt(self.start) > 1) {
                    break :blk switch (self.start[1]) {
                        'a' => self.check_keyword(2, 3, "lse", TokenType.logical_false),
                        'o' => self.check_keyword(2, 1, "r", TokenType.cf_for),
                        'u' => self.check_keyword(2, 1, "n", TokenType.fun),
                        else => TokenType.identifier,
                    };
                } else {
                    break :blk TokenType.identifier;
                }
            },
            'i' => self.check_keyword(1, 1, "f", TokenType.cf_if),
            'n' => self.check_keyword(1, 2, "il", TokenType.nil),
            'o' => self.check_keyword(1, 1, "r", TokenType.logical_or),
            'p' => self.check_keyword(1, 4, "rint", TokenType.print),
            'r' => self.check_keyword(1, 5, "eturn", TokenType.cf_return),
            's' => self.check_keyword(1, 4, "uper", TokenType.super),
            't' => blk: {
                if (@ptrToInt(self.current) - @ptrToInt(self.start) > 1) {
                    break :blk switch (self.start[1]) {
                        'h' => self.check_keyword(2, 2, "is", TokenType.this),
                        'r' => self.check_keyword(2, 2, "ue", TokenType.logical_true),
                        else => TokenType.identifier,
                    };
                } else {
                    break :blk TokenType.identifier;
                }
            },
            'v' => self.check_keyword(1, 2, "ar", TokenType.cf_var),
            'w' => self.check_keyword(1, 4, "hile", TokenType.cf_while),
            else => TokenType.identifier,
        };
    }

    fn check_keyword(self: *Scanner, start: usize, length: usize, rest: []const u8, t: TokenType) TokenType {
        if (@ptrToInt(self.current) - @ptrToInt(self.start) == start + length and std.mem.eql(u8, self.start[start .. start + length], rest)) {
            return t;
        }

        return TokenType.identifier;
    }

    fn make_token(self: *Scanner, t: TokenType) Token {
        return Token{
            .t = t,
            .start = self.start,
            .length = @ptrToInt(self.current) - @ptrToInt(self.start),
            .line = self.line,
        };
    }

    pub fn error_token(self: *Scanner, message: []const u8) Token {
        return Token{
            .t = TokenType.scan_error,
            .start = message.ptr,
            .length = message.len,
            .line = self.line,
        };
    }
};

fn is_digit(c: u8) bool {
    return c >= '0' and c <= '9';
}

fn is_alpha(c: u8) bool {
    return (c >= 'a' and c <= 'z') or (c >= 'A' and c <= 'Z') or c == '_';
}

pub const Token = struct {
    t: TokenType,
    start: [*]const u8,
    length: usize,
    line: usize,

    pub fn equals(self: Token, other: Token) bool {
        return std.mem.eql(
            u8,
            self.start[0..self.length],
            other.start[0..other.length],
        );
    }

    pub fn synthetic(t: TokenType, name: []const u8) Token {
        return Token{
            .t = t,
            .start = name.ptr,
            .length = name.len,
            .line = 0,
        };
    }
};

pub const TokenType = enum(u8) {
    // Single-character tokens.
    left_paren,
    right_paren,
    left_brace,
    right_brace,
    comma,
    dot,
    minus,
    plus,
    semicolon,
    slash,
    star,
    // One or two character tokens.
    bang,
    bang_equal,
    equal,
    equal_equal,
    greater,
    greater_equal,
    less,
    less_equal,
    // Literals.
    identifier,
    string,
    number,
    // Keywords.
    logical_and,
    class,
    cf_else,
    logical_false,
    cf_for,
    fun,
    cf_if,
    nil,
    logical_or,
    print,
    cf_return,
    super,
    this,
    logical_true,
    cf_var,
    cf_while,

    scan_error,
    eof,
};
