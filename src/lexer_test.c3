module lexer_test;
import lexer;
import std::io;

def Lexer = Lexer(<Token, Comment>);
def Kind = Kind(<Token, Comment>);

enum Token : char (String token)
{
    KEYWORD1 ("keyword1"),
    KEYWORD2 ("keyword2"),
    SINGLE   ("//"),
    MULTI    ("/*"),
}

enum Comment : char (Token start, String end)
{
    SINGLE (SINGLE, "\n"),
    MULTI  (MULTI, "*/"),
}

fn bool is_ident_char(usz i, char c)
{
    return (i == 0 && c.is_alpha()) || (i > 0 && c.is_alnum());
}

struct UintTest
{
    String in;
    ulong out;
}

fn void! lex_uint() @test
{
    UintTest[] tcases = {
        {"0", 0},
        {"123", 123},
        {"01", 1},
        {"0x1", 1},
        {"0b10", 2},
    };
    foreach (tc : tcases)
    {
        ByteReader br;
        br.init((char[])tc.in);
        Lexer lex;
        lex.init(br.as_stream(), &is_ident_char)!;

        Kind kind = lex.next()!;
        assert(kind == UINT, "got %s; want %s", kind, Kind.UINT);
        assert(lex.x == tc.out, "got %d; want %d", lex.x, tc.out);
    }
}

struct StringTest
{
    String in;
    String out;
}

fn void! lex_string() @test
{
    StringTest[] tcases = {
        {`""`, ""},
        {`"a"`, "a"},
        {`"abc"`, "abc"},
        {`"a\"bc"`, "a\"bc"},
    };
    foreach (tc : tcases)
    {
        ByteReader br;
        br.init((char[])tc.in);
        Lexer lex;
        lex.init(br.as_stream(), &is_ident_char)!;

        Kind kind = lex.next()!;
        assert(kind == STRING, "got %s; want %s", kind, Kind.STRING);
        assert(lex.string == tc.out, "got %s; want %s", lex.string, tc.out);
    }
}

struct RuneTest
{
    String in;
    char out;
}

fn void! lex_rune() @test
{
    RuneTest[] tcases = {
        {"'a'", 'a'},
        {"'0'", '0'},
    };
    foreach (tc : tcases)
    {
        ByteReader br;
        br.init((char[])tc.in);
        Lexer lex;
        lex.init(br.as_stream(), &is_ident_char)!;

        Kind kind = lex.next()!;
        assert(kind == RUNE, "got %s; want %s", kind, Kind.RUNE);
        assert(lex.rune == tc.out, "got %s; want %s", lex.rune, tc.out);
    }
}

struct TokenTest
{
    String in;
    Token out;
}

fn void! lex_token() @test
{
    TokenTest[] tcases = {
        {"keyword1", KEYWORD1},
        {"keyword2", KEYWORD2},
    };
    foreach (tc : tcases)
    {
        ByteReader br;
        br.init((char[])tc.in);
        Lexer lex;
        lex.init(br.as_stream(), &is_ident_char)!;

        Kind kind = lex.next()!;
        assert(kind == TOKEN, "got %s; want %s", kind, Kind.TOKEN);
        assert(lex.token == tc.out, "got %s; want %s", lex.token, tc.out);
    }
}

struct IdentifierTest
{
    String in;
    String out;
}

fn void! lex_identifier() @test
{
    IdentifierTest[] tcases = {
        {"key0", "key0"},
    };
    foreach (tc : tcases)
    {
        ByteReader br;
        br.init((char[])tc.in);
        Lexer lex;
        lex.init(br.as_stream(), &is_ident_char)!;

        Kind kind = lex.next()!;
        assert(kind == IDENTIFIER, "got %s; want %s", kind, Kind.IDENTIFIER);
        assert(lex.string == tc.out, "got %s; want %s", lex.string, tc.out);
    }
}