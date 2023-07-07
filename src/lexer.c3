module lexer;
import std::io;
import trie;

enum Kind
{
    UNKNOWN,
    UINT,
    STRING,
    RUNE,
    COMMENT,
    IDENTIFIER,
}

fault LexerError
{
    UNTERMINATED_STRING,
    UNTERMINATED_RUNE,
}

def Trie = trie::Trie(<usz, ushort>);

struct Lexer
{
    Allocator* allocator;
    ByteReader* buf;
    Trie tokens;
    String path;
    bool peeked;
    uint line;
    uint column;
    uint span;
    union {
        ulong x;
        String string;
        char rune;
    }
}

fn void! Lexer.init(&self, ByteReader *buf, String[] tokens, String path = "", Allocator* using = mem::heap())
{
    Trie trie;
    foreach (i, tok : tokens)
    {
        trie.set(tok, i)!;
    }
    *self = { .allocator = using, .buf = buf, .tokens = trie, .path = path };
}

fn Kind! Lexer.next(&self)
{
    char c = self.buf.read_byte()!;
    switch
    {
        case c.is_digit():
            self.buf.pushback_byte()!!;
            self.x = self.parse_uint()!;
            return UINT;
        case c == '"':
            self.string = self.parse_string(c)!;
            return STRING;
        case c == '\'':
            self.rune = self.parse_rune()!;
            return RUNE;
    }


    return UNKNOWN;
}

fn void! Lexer.skip_whitespace(&self)
{
    self.span = 0;
    while (true)
    {
        char c = self.buf.read_byte()!;
        if (!c.is_space())
        {
            self.buf.pushback_byte()!!;
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

fn ulong! Lexer.parse_uint(&self)
{
    ulong x;
    char c = self.buf.read_byte()!;
    if (c == '0')
    {
        c = self.buf.read_byte()!;
        switch (c)
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
    }
    self.span = 0;
    return self.parse_decimal()!;
}

fn ulong! Lexer.parse_decimal(&self)
{
    return @parse_uint(self, ulong; ulong x, char c, bool ok)
    {
        if (c.is_digit())
        {
            x = x * 10 + (ulong)(c - '0');
            ok = true;
        }
    };
}

fn ulong! Lexer.parse_hexadecimal(&self)
{
    return @parse_uint(self, ulong; ulong x, char c, bool ok)
    {
        switch
        {
            case c.is_digit():
                x = x * 10 + (ulong)(c - '0');
                ok = true;
            case c.is_xdigit():
                c -= c < 'A' ? 'a' : 'A';
                x = x * 16 + (ulong)c;
                ok = true;
        }
    };
}

fn ulong! Lexer.parse_binary(&self)
{
    return @parse_uint(self, ulong; ulong x, char c, bool ok)
    {
        if (c.is_digit())
        {
            x = x * 2 + (ulong)(c - '0');
            ok = true;
        }
    };
}

fn ulong! Lexer.parse_octal(&self)
{
    return @parse_uint(self, ulong; ulong x, char c, bool ok)
    {
        if (c.is_odigit())
        {
            x = x * 8 + (ulong)(c - '0');
            ok = true;
        }
    };
}

macro @parse_uint(self, $Type; @body(x, c, ok))
{
    $Type x;
    uint column = self.column;
    while (true)
    {
        char! c = self.buf.read_byte();
        if (catch err = c)
        {
            case IoError.EOF:
                break;
            default:
                return err?;
        }
        else
        {
            self.column++;
            if (c == '_') continue;
            $Type xx = x;
            bool ok;
            @body(x, c, ok);
            if (xx > x) return NumberConversion.INTEGER_OVERFLOW?;
            if (!ok)
            {
                self.column--;
                self.buf.pushback_byte()!!;
                break;
            }
        }
    }
    self.span = self.column - column;
    return x;
}

fn String! Lexer.parse_string(&self, char quote)
{
    DString str;
    str.init(8, self.allocator);
    char c = self.read_char_for_string()!;
    if (c == quote) return "";
    char prev;
    while (true)
    {
        c = self.read_char_for_string()!;
        if (prev != '\\')
        {
            str.append_char(prev);
            if (c == quote)
            {
                self.span = (uint)str.len();
                self.column += self.span + 2;
                return str.as_str();
            }
            prev = c;
            continue;
        }
        prev = c;
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
        str.append_char(c);
    }
}

fn char! Lexer.parse_rune(&self)
{
    char x = self.buf.read_byte()!;
    char c = self.buf.read_byte()!;
    if (c != '\'') return LexerError.UNTERMINATED_RUNE?;
    return x;
}

macro char! Lexer.read_char_for_string(&self)
{
    char! c = self.buf.read_byte();
    if (catch err = c)
    {
        case IoError.EOF:
            return LexerError.UNTERMINATED_STRING?;
        default:
            return err?;
    }
    return c;
}