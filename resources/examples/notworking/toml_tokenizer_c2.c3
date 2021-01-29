// This is the toml_tokenizer.c2 changed to c3 to compare
module toml;
import stdio;
import string;
import ctype;
import stdlib;

const uint MaxText = 1024;

enum TokenKind : char (string name)
{
    WORD("word"),
    TEXT("text"),
    NUMBER("number"),
    KW_TRUE("true"),
    KW_FALSE("false"),
    LBRACE("["),
    LBRACE2("[[")
    RBRACE("]"),
    RBRACE2("]]"),
    EQUALS("="),
    DOT("."),
    COMMA(","),
    EOF("eof"),
    ERROR("error"),
}

func void Location.init(Location* l, uint line = 0, uint col = 0)
{
    l.line = line;
    l.column = col;
}

func string Location.str(Location* l)
{
    static char[32] msg;
    sprintf(msg, "line %u:%u", l.line, l.column);
    return msg;
}

struct Token
{
    Location loc;
    TokenKind kind;
    // TODO union?
    union
    {
        string text;
        uint number;
    }
}

func void Token.init(Token* t)
{
    t.loc.init(0, 0);
    t.kind = TokenKind.EOF;
    t.text = nil;
    t.number = 0;
}

func void Token.clear(Token* t)
{
    t.text = nil;
    t.number = 0;
}

func void Token.setLocation(Token* t, Location l)
{
    t.loc = l;
}

func bool Token.is(Token* t, TokenKind k)
{
    return t.kind == k;
}

func bool Token.isNot(Token* t, TokenKind k)
{
    return t.kind != k;
}

func string Token.getName(Token* t)
{
    return t.kind.name;
}

struct Tokenizer
{
    char* dataStart;
    char* current;
    Location loc;
    char[MaxText] text;
    Token nextToken;
    bool haveNext;
}

func void Tokenizer.init(Tokenizer* t, char* input)
{
    t.dataStart = input;
    t.current = input;
    t.loc.init(1, 1);
    t.haveNext = false;
    t.text[0] = 0;
}

error LexedEOF;
error LexError
{
    string error_message;
}

func void! Tokenizer.lex(Tokenizer* t, Token* result)
{
    if (t.haveNext)
    {
       // Q: ptr assign or copy?
       *result = t.nextToken;
       t.haveNext = false;
       return;
    }
    result.clear();
    while (1)
    {
        switch (t.current[0])
        {
            case 0:
                return! LexedEOF();
            case '#':
                if (t.loc.column != 1)
                {
                    sprintf(t.text, "unexpected '#' after line start at %s", t.loc.str());
                    return! LexError(t.text);
                }
                t.parseComment();
            case ' ':
            case '\t':
                t.advance(1);
            case '\n':
                t.current++;
                t.loc.line++;
                t.loc.column = 1;
            case '=':
                result.loc = t.loc;
                result.kind = EQUALS;
                t.advance(1);
                return;
            case '.':
                result.loc = t.loc;
                result.kind = DOT;
                t.advance(1);
                return;
            case ',':
                result.loc = t.loc;
                result.kind = COMMA;
                t.advance(1);
                return;
            case '[':
                result.loc = t.loc;
                if (t.current[1] == '[')
                {
                    t.advance(2);
                    result.kind = LBRACE2;
                }
                else
                {
                    t.advance(1);
                    result.kind = LBRACE;
                }
                return;
            case ']':
                result.loc = t.loc;
                if (t.current[1] == ']')
                {
                    t.advance(2);
                    result.kind = RBRACE2;
                }
                else
                {
                    t.advance(1);
                    result.kind = RBRACE;
                }
                return;
            case '"':
                if (t.current[1] == '"' && t.current[2] == '"')
                {
                    t.parseMultiText(result);
                }
                else
                {
                    t.parseText(result);
                }
                return;
            default:
                // key or number
                result.loc = t.loc;
                if (isdigit(t.current[0]))
                {
                    t.parseNumber(result);
                    return;
                }
                if (t.current[0] == 'f' && strncmp("false", t.current, 5) == 0) {
                    t.advance(5);
                    result.number = 0;
                    result.kind = KW_FALSE;
                    return;
                }
                if (t.current[0] == 't' && strncmp("true", t.current, 4) == 0) {
                    t.advance(4);
                    result.number = 1;
                    result.kind = KW_TRUE;
                    return;
                }
                if (isalpha(t.current[0]))
                {
                    t.parseKey(result);
                    return;
                }
                sprintf(t.text, "unexpected char '%c' at %s", t.current[0], t.loc.str());
                return LexError(t.text);
        }
    }
}

func Token*! Tokenizer.lookahead(Tokenizer* t)
{
    if (!t.haveNext)
    {
        try t.lex(&t.nextToken);
        t.haveNext = true;
    }
    return &t.nextToken;
}

func void Tokenizer.advance(Tokenizer* t, uint amount)
{
    t.loc.column += amount;
    t.current += amount;
}

func void Tokenizer.parseComment(Tokenizer* t)
{
    while (1)
    {
        switch (t.current[0])
        {
            case 0:
                return;
            case '\n':
                t.current++;
                t.loc.line++;
                t.loc.column = 1;
                return;
            default:
                t.current++;
                t.loc.column++;
                break;
        }
    }
}

func void Tokenizer.parseText(Tokenizer* t, Token* result)
{
    // TODO handle literal strings ' .. ' -> no escaping
    // TODO handle escape chars for normal strings " .. \" \r \n "
    t.advance(1);
    result.loc = t.loc;
    const char* start = t.current;
    while (t.current[0] && t.current[0] != '"') t.current++;

    uint len = cast(t.current - start as uint);
    // assert(len < MaxText);
    memcpy(t.text as start, len);
    t.text[len] = 0;
    result.kind = TokenKind.Text;
    result.text = t.text;
    t.loc.column += len;
    t.advance(1);
}

func void! Tokenizer.parseMultiText(Tokenizer* t, Token* result)
{
    t.advance(3);
    if (t.current[0] == '\n')
    {
        t.current++;
        t.loc.line++;
        t.loc.column = 1;
    }
    result.loc = t.loc;
    char* start = t.current;
    while (1)
    {
        if (t.current[0] == 0)
        {
            sprintf(t.text, "missing end \"\"\" %s", t.loc.str());
            return! LexError(t.text);
        }
        if (t.current[0] == '\n')
        {
            t.loc.line++;
            t.loc.column = 1;
        }
        else
        {
            t.loc.column++;
        }
        if (t.current[0] == '"' && t.current[1] == '"' && t.current[2] == '"') break;
        t.current++;
    }

    uint len = uint(t.current - start);
    // assert(len < MaxText);
    memcpy(t.text, start, len);
    t.text[len] = 0;
    result.kind = TokenKind.Text;
    result.text = t.text;
    t.advance(3);
}

func void Tokenizer.parseNumber(Tokenizer* t, Token* result)
{
    // TODO handle prefix +/-
    // handle hexadecimal/ocal/binary number
    // handle '_', like 1_000_000

    uint number = uint(atoi(t.current));
    result.kind = TokenKind.Number;
    result.number = number;
    while (t.current[0] && isdigit(t.current[0]))
    {
        t.current++;
        t.loc.column++;
    }
}

func bool isKeyChar(u8 c)
{
    if (c >= 128) return true;
    if (isalpha(c)) return true;
    if (isdigit(c)) return true;
    if (c == '_' || c == '-') return true;
    return false;
}

func void Tokenizer.parseKey(Tokenizer* t, Token* result)
{
    char* start = t.current;
    while (t.current[0] && isKeyChar(cast(t.current[0] as char))) t.current++;

    uint len = cast(t.current - start as uint);
    // assert(len < MaxText);
    memcpy(t.text, start, len);
    t.text[len] = 0;
    result.kind = TokenKind.Word;
    result.text = t.text;
    t.loc.column += len;
}
