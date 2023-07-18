module lexer_test;
import lexer;
import std::io;

def Lexer = Lexer(<Token, Comment>);

enum Token : char (String token)
{
    KEYWORD1 ("keword1"),
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
    UintTest[] tcases = {};
    foreach (tc : tcases)
    {
        ByteReader br;
        br.init((char[])tc.in);
        Lexer lex;
        lex.init(br.as_stream(), &is_ident_char)!;
    }
}