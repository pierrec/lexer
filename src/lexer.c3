/**
 * @require Token.kindof == ENUM && $typefrom(Token.inner).kindof == UNSIGNED_INT
 * @require $checks(((Token)0).token)
 * @require $checks(((Comment)0).start) && $checks(((Comment)0).end)
 **/
module lexer(<Token, Comment>);
import std::io;
import trie;

enum Kind : char
{
    UNKNOWN,
    UINT,
    STRING,
    RUNE,
    TOKEN,
    IDENTIFIER,
}

fault LexerError
{
    UNTERMINATED_STRING,
    UNTERMINATED_RUNE,
    UNTERMINATED_COMMENT,
}

def TokenTrie = Trie(<Token, ushort>);

def Ident = fn bool (usz index, char c);

struct Lexer
{
    Allocator* allocator;
    Stream reader;
    char[] buf;
    TokenTrie tokens;
    Ident ident;
    bool peeked;
    Kind kind;
    uint line;
    uint column;
    uint span;
    union {
        ulong x;       // UINT
        String string; // STRING, IDENTIFIER
        char rune;     // RUNE
        Token token;   // TOKEN
    }
}

fn void! Lexer.init(&self, Stream reader, Ident ident, Allocator* using = mem::heap())
{
    TokenTrie trie;
    trie.init(.using = using);
    ushort max_token;
    foreach (i, tok : Token.values)
    {
        String name = tok.token;
        assert(name.len > 0 && name.len <= ushort.max);
        trie.set(name, (Token)i)!;
        max_token = max(max_token, (ushort)name.len);
    }
    foreach (tok : Comment.values)
    {
        String end = tok.end;
        assert(end.len > 0 && end.len <= ushort.max);
        max_token = max(max_token, (ushort)end.len);
    }
    char* buf = using.alloc(max_token)!;
    *self = { .allocator = using, .reader = reader, .buf = buf[:max_token], .tokens = trie, .ident = ident };
}

fn void! Lexer.free(&self)
{
    self.allocator.free(self.buf)!;
    *self = {};
}

fn Kind! Lexer.next(&self)
{
    if (self.peeked)
    {
        self.peeked = false;
        return self.kind;
    }
    char c = self.reader.read_byte()!;
    switch
    {
        case c.is_digit():
            self.reader.pushback_byte()!!;
            self.x = self.parse_uint()!;
            return UINT;
        case c == '"':
            self.string = self.parse_string(c)!;
            return STRING;
        case c == '\'':
            self.rune = self.parse_rune()!;
            return RUNE;
    }
    if (try tok = self.parse_token(c))
    {
        foreach (comment : Comment.values)
        {
            if (comment.start == tok)
            {
                self.parse_comment(comment.end)!;
                break;
            }
        }
        self.token = tok;
        return TOKEN;
    }
    if (try tok = self.parse_ident(c))
    {
        self.string = tok;
        return IDENTIFIER;
    }
    return UNKNOWN;
}

fn Kind! Lexer.peek(&self)
{
    if (!self.peeked)
    {
        self.kind = self.next()!;
        self.peeked = true;
    }
    return self.kind;
}

fn Token! Lexer.expect(&self, anyfault err, Token... tokens)
{
    Kind kind = self.next()!;
    if (kind == TOKEN)
    {
        foreach (tok : tokens)
        {
            if (tok == self.token) return tok;
        }
    }
    return err?;
}

fn void! Lexer.skip_whitespace(&self) @private
{
    self.span = 0;
    while (true)
    {
        char c = self.reader.read_byte()!;
        if (!c.is_space())
        {
            self.reader.pushback_byte()!!;
            return;
        }
        if (c == '\n')
        {
            self.line++;
            self.column = 0;
        }
        else
        {
            self.column++;
        }
    }
}

fn ulong! Lexer.parse_uint(&self) @private
{
    char c = self.reader.read_byte()!;
    if ZERO: (c == '0')
    {
        char! cc = self.reader.read_byte();
        if (catch err = cc)
        {
            case IoError.EOF:
                break ZERO;
            default:
                return err?;
        }
        switch (cc)
        {
            case 'x':
                self.span = 2;
                return self.parse_hexadecimal()!;
            case 'b':
                self.span = 2;
                return self.parse_binary()!;
            case 'o':
                self.span = 2;
                return self.parse_octal()!;
        }
        // The next pushback restores this character, making
        // the first 0 ignored.
    }
    self.reader.pushback_byte()!!;
    self.span = 0;
    return self.parse_decimal()!;
}

fn ulong! Lexer.parse_decimal(&self) @private
{
    return @parse_uint(self; char c)
    {
        if (c.is_digit())
        {
            self.x = self.x * 10 + (ulong)(c - '0');
        }
    };
}

fn ulong! Lexer.parse_hexadecimal(&self) @private
{
    return @parse_uint(self; char c)
    {
        switch
        {
            case c.is_digit():
                self.x = self.x * 10 + (ulong)(c - '0');
            case c.is_xdigit():
                c -= c < 'A' ? 'a' : 'A';
                self.x = self.x * 16 + (ulong)c;
        }
    };
}

fn ulong! Lexer.parse_binary(&self) @private
{
    return @parse_uint(self; char c)
    {
        if (c.is_digit())
        {
            self.x = self.x * 2 + (ulong)(c - '0');
        }
    };
}

fn ulong! Lexer.parse_octal(&self) @private
{
    return @parse_uint(self; char c)
    {
        if (c.is_odigit())
        {
            self.x = self.x * 8 + (ulong)(c - '0');
        }
    };
}

macro @parse_uint(self; @body(c)) @private
{
    uint column = self.column;
    while LOOP: (true)
    {
        char! c = self.reader.read_byte();
        if (catch err = c)
        {
            case IoError.EOF:
                break LOOP;
            default:
                return err?;
        }
        self.column++;
        if (c == '_') continue;
        ulong xx = self.x;
        @body(c);
        if (xx > self.x) return NumberConversion.INTEGER_OVERFLOW?;
        if (xx == self.x)
        {
            self.column--;
            self.reader.pushback_byte()!!;
            break;
        }
    }
    self.span = self.column - column;
}

fn String! Lexer.parse_string(&self, char quote) @private
{
    char c = self.read_char_for_string()!;
    if (c == quote) return "";
    DString str;
    str.init(8, self.allocator);
    while (true)
    {
        if (c == '\\')
        {
            c = self.read_char_for_string()!;
            switch (c)
            {
                case 'a':
                case 'b':
                case '\\':
                case '\n':
                case '\f':
                case '\r':
                case '\v':
                case '"':
                case '\'':
                    break;
                default: c = '\\';
            }
        }
        str.append_char(c);
        c = self.read_char_for_string()!;
        if (c == quote)
        {
            self.span = (uint)str.len();
            self.column += self.span + 2;
            return str.as_str();
        }
    }
}

fn char! Lexer.parse_rune(&self) @private
{
    char x = self.reader.read_byte()!;
    char c = self.reader.read_byte()!;
    if (c != '\'') return LexerError.UNTERMINATED_RUNE?;
    return x;
}

macro char! Lexer.read_char_for_string(&self) @private
{
    char! c = self.reader.read_byte();
    if (catch err = c)
    {
        case IoError.EOF:
            return LexerError.UNTERMINATED_STRING?;
        default:
            return err?;
    }
    return c;
}

fn Token! Lexer.parse_token(&self, char cc) @private
{
    self.buf[0] = cc;
    usz n = self.reader.read(self.buf[1..])!;
    defer self.unread(n);
    Token tok = self.tokens.get_best(self.buf[:n + 1])!;
    n = self.buf.len - tok.token.len;
    return tok;
}

fn void! Lexer.parse_comment(&self, String end) @private
{
    // Find the end token and accumulate the data in between.
    DString acc;
    acc.init(8, self.allocator);
    char[] buf = self.buf[:end.len];
    while (true)
    {
        if (catch err = self.reader.read_all(buf))
        {
            case IoError.UNEXPECTED_EOF:
            case IoError.EOF:
                return LexerError.UNTERMINATED_COMMENT?;
            default:
                return err?;
        }
        if (end == (String)buf)
        {
            self.string = acc.as_str();
            return;
        }
        acc.append_char(buf[0]);
        self.unread(buf.len - 1);
    }
}

macro Lexer.unread(&self, n) @private
{
    switch
    {
        case n == 1:
            self.reader.pushback_byte()!!;
        case n > 1:
            if (SeekStreamFn func = self.reader.fns.seek_fn)
            {
		        func(self.reader, -n, CURSOR)!!;
		        break;
            }
            for (; n > 0; n--) self.reader.pushback_byte()!!;
    }
}

fn String! Lexer.parse_ident(&self, char cc) @private
{
    DString str;
    str.init(8, self.allocator);
    str.append_char(cc);
    while (true)
    {
        char! c = self.reader.read_byte();
        if (catch err = c)
        {
            case IoError.EOF:
                return str.as_str();
            default:
                return err?;
        }
        if (!self.ident(str.len(), c)) return str.as_str();
        str.append_char(c);
    }
}