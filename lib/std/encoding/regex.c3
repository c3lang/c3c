<* POSIX Regex engine based on musl libc (parallel TNFA scheme).

 **Usage:**
 ```
   // Compile a pattern (returns Regex?)
   Regex re = regex::compile("(\\w+)@(\\w+)", EXTENDED)!!;
   defer re.free();

   // Execute matching & handle faults using error_string
   Match[]? res = re.exec("user@domain.com", 3);
   if (catch err = res) {
       io::printfn("Error: %s", regex::error_string(err));
       return;
   }
   io::printfn("User: %s, Domain: %s", res[1], res[2]);
 ```

**Supported:**
  - POSIX Extended (ERE) and Basic (BRE) regular expressions.
  - Capture groups and submatch extraction.
  - BRE backreferences (\1 to \9) with backtracking execution fallback.
  - Word boundaries (\b, \B, \<, \>) and shorthands (\w, \W, \s, \S, \d, \D).
  - Control escapes (\t, \n, \r, \f, \a, \e) and hex escapes (\xHH, \x{HHHH}).
  - Multi-byte UTF-8 character matching.

**Not supported:**
  - Non-capturing groups (?:...), atomic groups (?>...), or branch resets.
  - Lookarounds, possessive quantifiers, or PCRE anchors (\A, \z, \Z, \G).
  - Inline flags ((?i), (?m), etc.) or inline comments ((?#...)).
  - Named capture groups ((?P<name>...)) or named backreferences (\k<name>).
  - Unicode character properties (\p{...}, \P{...}).
  - POSIX collation elements [[.col.]] or equivalence classes [[=eq=]].

*>
module std::encoding::regex;

import std::io;

// API

struct Regex
{
	<* The number of subexpressions in regex *>
	sz    re_nsub;
	<* internal TNFA pointer *>
	Tnfa* __tnfa;
}

<*
 Compile a POSIX regex pattern into a `Regex`.

 @param pattern  : "The regex pattern string."
 @param cflags   : "Compile flags: EXTENDED | ICASE | NOSUB | NEWLINE."
 @param allocator : "Allocator used to persist the compiled regex. Defaults to the heap allocator."
 @return "The compiled `Regex`, or a compile-error fault (BADPAT, ECOLLATE, ECTYPE, EESCAPE, ESUBREG, EBRACK, EPAREN, EBRACE, BADBR, ERANGE, OUT_OF_MEMORY, BADRPT)."
*>
fn Regex? compile(String pattern, CompileFlags cflags = {}, Allocator allocator = mem)
{
	Regex preg;
	re_compile(&preg, pattern, cflags, allocator)!;
	return preg;
}


struct Match (Printable)
{
	String input;
	RegMatch match;
}

<*
 Execute a compiled regex against a string.

 Returns a slice of `Match` structures allocated from the temp allocator, one entry per
 capture group (index 0 is the whole match). Throws `NOMATCH` if the pattern
 does not match anywhere in the string, or `OUT_OF_MEMORY` on allocation failure.

 @param str    : "The input string to search."
 @param nmatch : "Maximum number of capture groups to return (including group 0)."
 @param eflags : "Execute flags: NOTBOL and NOTEOL."
 @return "A temp-allocated slice of `Match[nmatch]`."
*>
fn Match[]? Regex.exec(&self, String str, sz nmatch = 1, ExecFlags eflags = {})
{
	RegMatch* matches = mem::talloc_array(RegMatch, nmatch);
	re_exec(self, str, nmatch, matches, eflags)!;
	Match[] res = mem::talloc_array(Match, nmatch);
	for (sz i = 0; i < nmatch; i++)
	{
		res[i] = { .input = str, .match.rm_so = matches[i].rm_so, .match.rm_eo = matches[i].rm_eo };
	}
	return res;
}

<*
 Free all resources held by a compiled regex.
*>
fn void Regex.free(&self) => re_free(self);

<*
 Returns a human-readable description for a given regex fault.
*>
fn String error_string(fault err)
{
	switch (err)
	{
		case NOMATCH:        return "No match";
		case BADPAT:         return "Invalid regexp";
		case ECOLLATE:       return "Unknown collating element";
		case ECTYPE:         return "Unknown character class name";
		case EESCAPE:        return "Trailing backslash";
		case ESUBREG:        return "Invalid back reference";
		case EBRACK:         return "Missing ']'";
		case EPAREN:         return "Missing ')'";
		case EBRACE:         return "Missing '}'";
		case BADBR:          return "Invalid contents of {}";
		case ERANGE:         return "Invalid character range";
		case OUT_OF_MEMORY:  return "Out of memory";
		case BADRPT:         return "Repetition not preceded by valid expression";
		default:             return "Unknown error";
	}
}


// ===============================================================
//  Public API types

const int RE_DUP_MAX = 255;
const int CHAR_MAX_CODEPOINT = 0x10ffff;

// Compile flags (cflags)
constdef CompileFlags : int
{
	EXTENDED = 1 << 0,
	ICASE    = 1 << 1,
	NOSUB    = 1 << 2,
	NEWLINE  = 1 << 3,
}

// Execute flags (eflags)
constdef ExecFlags : int
{
	NOTBOL = 1 << 0,
	NOTEOL = 1 << 1,
}



faultdef
	OK,
	NOMATCH,
	BADPAT,
	ECOLLATE,
	ECTYPE,
	EESCAPE,
	ESUBREG,
	EBRACK,
	EPAREN,
	EBRACE,
	BADBR,
	ERANGE,
	OUT_OF_MEMORY,
	BADRPT;

alias RegOff @private = int;

struct RegMatch
{
	RegOff rm_so;
	RegOff rm_eo;
}

fn sz? Match.to_format(&self, Formatter* formatter) @dynamic => formatter.print(self.str_view());

<* Extract the matched substring as a String view. *>
fn String Match.str_view(&self) @inline
{
	if (self.match.rm_so < 0 || self.match.rm_eo < 0) return "";
	return self.input[self.match.rm_so : (self.match.rm_eo - self.match.rm_so)];
}

fn bool Match.eq(&self, String other) @operator(==) @inline => self.str_view() == other;


// ============================================================
//  Internal: AST node types
// ============================================================

const int LITERAL    @private = 0;
const int CATENATION @private = 1;
const int ITERATION  @private = 2;
const int UNION      @private = 3;

// Special code_min sentinel values for literal nodes
const long EMPTY      @private = -1;
const long ASSERTION  @private = -2;
const long TAG        @private = -3;
const long BACKREF    @private = -4;

// Assertion bits
const int ASSERT_AT_BOL         @private = 1;
const int ASSERT_AT_EOL         @private = 2;
const int ASSERT_CHAR_CLASS     @private = 4;
const int ASSERT_CHAR_CLASS_NEG @private = 8;
const int ASSERT_AT_BOW         @private = 16;
const int ASSERT_AT_EOW         @private = 32;
const int ASSERT_AT_WB          @private = 64;
const int ASSERT_AT_WB_NEG      @private = 128;
const int ASSERT_BACKREF        @private = 256;

const int TRE_TAG_MINIMIZE @private = 0;
const int TRE_TAG_MAXIMIZE @private = 1;

// ============================================================
//  Internal: TNFA structures
// ============================================================

typedef StateId @local @constinit = inline int;
typedef TagId @local @constinit = inline int;

struct TnfaTransition @local
{
	int              code_min;
	int              code_max;
	TnfaTransition*  state;
	StateId          state_id;
	TagId*           tags;
	int              assertions;
	int              u_class_or_backref;  // wctype_t or backref index
	int*             neg_classes;
}

struct SubmatchData @local
{
	TagId  so_tag;
	TagId  eo_tag;
	int*   parents;
}

struct Tnfa @local
{
	TnfaTransition* transitions;
	uint            num_transitions;
	TnfaTransition* initial;
	TnfaTransition* final;
	SubmatchData*   submatch_data;
	char*           firstpos_chars;
	int             first_char;
	sz              num_submatches;
	int*            tag_directions;
	int*            minimal_tags;
	int             num_tags;
	int             num_minimals;
	int             end_tag;
	int             num_states;
	CompileFlags    cflags;
	bool            have_backrefs;
	Allocator       allocator;     // The allocator used to persist the compiled TNFA structures
}

// ============================================================
//  Internal: AST structures
// ============================================================

struct PosAndTags @local
{
	StateId  position;
	int      code_min;
	int      code_max;
	TagId*   tags;
	int      assertions;
	int      backref;
	int      class;
	int*     neg_classes;
}

struct TreTnfaReach @local
{
	TnfaTransition* state;
	TagId*          tags;
}

struct TreReachPos @local
{
	sz      pos;
	TagId** tags;
}

struct TreBacktrackItem @local
{
	RegOff          pos;
	char*           str_byte;
	TnfaTransition* state;
	StateId         state_id;
	int             next_c;
	TagId*          tags;
}

struct TreBacktrack @local
{
	TreBacktrackItem item;
	TreBacktrack*    prev;
	TreBacktrack*    next;
}

fn int tre_tag_order(int num_tags, int* tag_directions, TagId* t1, TagId* t2) @local
{
	for (int i = 0; i < num_tags; i++)
	{
		if (tag_directions[i] == TRE_TAG_MINIMIZE)
		{
			if (t1[i] < t2[i]) return 1;
			if (t1[i] > t2[i]) return 0;
		}
		else
		{
			if (t1[i] > t2[i]) return 1;
			if (t1[i] < t2[i]) return 0;
		}
	}
	return 0;
}

struct AstNode @local
{
	int         type;
	void*       obj;
	bool        nullable;
	int         submatch_id;
	int         num_submatches;
	int         num_tags;
	PosAndTags* firstpos;
	PosAndTags* lastpos;
}

struct Literal @local
{
	long    code_min;
	long    code_max;
	StateId position;
	int     class;
	int*    neg_classes;
}

struct Catenation @local
{
	AstNode* left;
	AstNode* right;
}

struct Iteration @local
{
	AstNode* arg;
	int      min;
	int      max;
	bool     minimal;
}

typedef Union @local = Catenation;


// ============================================================
//  Internal: Stack
// ============================================================

union StackItem @local
{
	void* ptr;
	int   int_value;
}

struct Stack @local
{
	Allocator  allocator;
	int        size;
	int        max_size;
	int        increment;
	int        count;
	StackItem* stack;
}

fn Stack* stack_new(Allocator allocator, int size, int max_size, int increment) @local
{
	Stack* s = alloc::malloc(allocator, Stack::size);
	if (!s) return null;
	s.allocator = allocator;
	s.stack = alloc::malloc(allocator, (size * StackItem::size));
	if (!s.stack) { alloc::free(allocator, s); return null; }
	s.size = size;
	s.max_size = max_size;
	s.increment = increment;
	s.count = 0;
	return s;
}

fn void stack_destroy(Stack* s) @local
{
	if (!s) return;
	alloc::free(s.allocator, s.stack);
	alloc::free(s.allocator, s);
}

<*
 @param [&inout] s
*>
fn void? stack_push(Stack* s, StackItem value) @local
{
	if (s.count < s.size)
	{
		s.stack[s.count++] = value;
	}
	else
	{
		if (s.size >= s.max_size) return OUT_OF_MEMORY~;
		int new_size = s.size + s.increment;
		if (new_size > s.max_size) new_size = s.max_size;
		StackItem* nb = alloc::realloc(s.allocator, s.stack, (usz)((sz)new_size * StackItem::size));
		if (!nb) return OUT_OF_MEMORY~;
		s.stack = nb;
		s.size = new_size;
		s.stack[s.count++] = value;
	}
}

<*
 @param [&inout] s
*>
fn void? stack_push_int(Stack* s, int v) @local
{
	StackItem item; item.int_value = v;
	stack_push(s, item)!;
}

<*
 @param [&inout] s
*>
fn void? stack_push_ptr(Stack* s, void* v) @local
{
	StackItem item; item.ptr = v;
	stack_push(s, item)!;
}

<*
 @param [&inout] s
 @require s.count > 0
*>
fn int stack_pop_int(Stack* s) @local => s.stack[--s.count].int_value;

<*
 @param [&inout] s
 @require s.count > 0
*>
fn void* stack_pop_ptr(Stack* s) @local => s.stack[--s.count].ptr;

// ============================================================
//  AST constructors
// ============================================================

fn AstNode*? ast_new_node(Allocator arena, int type, void* obj) @local
{
	AstNode* node = alloc::malloc(arena, AstNode::size);
	if (!node || !obj) return OUT_OF_MEMORY~;

	*node = { .type = type, .obj = obj, .nullable = false, .submatch_id = -1 };
	return node;
}

fn AstNode*? ast_new_literal(Allocator arena, int code_min, int code_max, StateId position) @local
{
	Literal* lit = alloc::malloc(arena, Literal::size);
	if (!lit) return OUT_OF_MEMORY~;

	*lit = { .code_min = code_min, .code_max = code_max, .position = position };
	return ast_new_node(arena, LITERAL, lit)!;
}

<*
 @param [&in] arg
 @require min >= 0
*>
fn AstNode*? ast_new_iter(Allocator arena, AstNode* arg, int min, int max, bool minimal) @local
{
	Iteration* iter = alloc::malloc(arena, Iteration::size);
	if (!iter) return OUT_OF_MEMORY~;

	*iter = { .arg = arg, .min = min, .max = max, .minimal = minimal };

	AstNode* node = ast_new_node(arena, ITERATION, iter)!;
	if (node) node.num_submatches = arg.num_submatches;
	return node;
}

fn AstNode*? ast_binary(Allocator arena, AstNode* left, AstNode* right, int type_id) <Type> @local
{
	if (!left) return right;
	if (!right) return null;

	Type* binary = alloc::malloc(arena, Type::size);
	if (!binary) return OUT_OF_MEMORY~;

	*binary = { .left = left, .right = right };

	AstNode* node = ast_new_node(arena, type_id, binary)!;
	if (node) node.num_submatches = left.num_submatches + right.num_submatches;
	return node;
}

fn AstNode*? ast_new_union(Allocator arena, AstNode* left, AstNode* right) @local
{
	return ast_binary{Union}(arena, left, right, UNION)!;
}

fn AstNode*? ast_new_catenation(Allocator arena, AstNode* left, AstNode* right) @local
{
	return ast_binary{Catenation}(arena, left, right, CATENATION)!;
}

// ============================================================
//  Parse context
// ============================================================

struct ParseCtx @local
{
	Allocator    mem;
	Stack*       stack;
	AstNode*     node;
	char*        cursor;
	char*        start;
	sz           submatch_id;
	StateId      position;
	int          max_backref;
	uint         backref_ok;
	CompileFlags cflags;
}

// ============================================================
//  Character class helpers (portable, ASCII-only for now)
// ============================================================

fn bool is_word_char(int c) @local => c == '_' || (c >= 0 && c <= 255 && ascii::@is_alnum(c));

fn int hexval(int c) @local
{
	if (c - '0' < 10) return c - '0';
	c |= 32;
	if (c - 'a' < 6) return c - 'a' + 10;
	return -1;
}

fn int tre_ctype(ZString name) @local
{
	if (name.eq("alnum"))  return 1;
	if (name.eq("alpha"))  return 2;
	if (name.eq("blank"))  return 3;
	if (name.eq("cntrl"))  return 4;
	if (name.eq("digit"))  return 5;
	if (name.eq("graph"))  return 6;
	if (name.eq("lower"))  return 7;
	if (name.eq("print"))  return 8;
	if (name.eq("punct"))  return 9;
	if (name.eq("space"))  return 10;
	if (name.eq("upper"))  return 11;
	if (name.eq("xdigit")) return 12;
	return 0;
}


fn bool tre_isctype(int c, int class) @local
{
	if (c < 0 || c > 127) return false;
	switch (class)
	{
		case 1:  return ascii::@is_alnum(c);
		case 2:  return ascii::@is_alpha(c);
		case 3:  return c == ' ' || c == '\t';
		case 4:  return ascii::@is_cntrl(c);
		case 5:  return ascii::@is_digit(c);
		case 6:  return ascii::@is_graph(c);
		case 7:  return ascii::@is_lower(c);
		case 8:  return ascii::@is_print(c);
		case 9:  return ascii::@is_punct(c);
		case 10: return ascii::@is_space(c);
		case 11: return ascii::@is_upper(c);
		case 12: return ascii::@is_xdigit(c);
	}
	return false;
}

fn bool tre_neg_char_classes_match(int* classes, int wc, bool icase) @local
{
	int* c = classes;
	while (*c != 0)
	{
		if (!icase)
		{
			if (tre_isctype(wc, *c)) return true;
		}
		else
		{
			if (tre_isctype(ascii::@to_lower((char)wc), *c) ||
				tre_isctype(ascii::@to_upper((char)wc), *c)) return true;
		}
		c++;
	}
	return false;
}

// ============================================================
//  Macro expansions: \w \s \d etc. (matches musl)
// ============================================================

fn ZString tre_expand_macro(char c) @local
{
	switch (c)
	{
		case 't': return "\t";
		case 'n': return "\n";
		case 'r': return "\r";
		case 'f': return "\f";
		case 'a': return "\a";
		case 'e': return "\x1b";
		case 'w': return "[[:alnum:]_]";
		case 'W': return "[^[:alnum:]_]";
		case 's': return "[[:space:]]";
		case 'S': return "[^[:space:]]";
		case 'd': return "[[:digit:]]";
		case 'D': return "[^[:digit:]]";
		default:  return null;
	}
}

// ============================================================
//  Parser implementation
// ============================================================

fn int add_icase_literals(Literal* ls, int* ls_len, int min, int max) @local
{
	// Overlap with lowercase alphabet ['a', 'z'] --> map to uppercase
	int start_lower = (min > 'a') ? min : 'a';
	int end_lower = (max < 'z') ? max : 'z';
	if (start_lower <= end_lower)
	{
		if (*ls_len >= 256) return -1;
		ls[*ls_len] = {
			.code_min = start_lower - 32l,
			.code_max = end_lower - 32l,
			.class = 0,
			.neg_classes = null,
			.position = -1
		};
		(*ls_len)++;
	}

	// Overlap with uppercase alphabet ['A', 'Z'] --> map to lowercase
	int start_upper = (min > 'A') ? min : 'A';
	int end_upper = (max < 'Z') ? max : 'Z';
	if (start_upper <= end_upper)
	{
		if (*ls_len >= 256) return -1;
		ls[*ls_len] = {
			.code_min = start_upper + 32l,
			.code_max = end_upper + 32l,
			.class = 0,
			.neg_classes = null,
			.position = -1
		};
		(*ls_len)++;
	}

	return 0;
}

// parse_bracket: s points to the char AFTER '['.
// Uses a fixed-size local buffer for class name lookup (matching musl).
fn void? parse_bracket(ParseCtx* ctx, char* s) @local
{
	char* start = s;
	bool negate = (*s == '^');
	if (negate) { s++; }
	char* contents_start = s;

	Literal[256] ls;
	int ls_len = 0;
	int[64] neg_classes;
	int neg_len = 0;

	for (;;)
	{
		if (!*s) return EBRACK~;
		if (*s == ']' && s != contents_start)
		{
			ctx.cursor = s + 1;
			break;
		}
		if (*s == '[' && (s[1] == '.' || s[1] == '=')) return ECOLLATE~;

		int min = *s;
		int max = min;
		int class = 0;

		if (*s == '[' && s[1] == ':')
		{
			// Character class [[:name:]], copy name to null-terminated buffer like musl.
			s += 2;
			char[32] tmp;
			int len = 0;
			while (*s && *s != ':' && len < 30) { tmp[len++] = *s++; }
			if (*s != ':' || s[1] != ']') return ECTYPE~;
			tmp[len] = 0;
			class = tre_ctype((ZString)&tmp[0]);
			if (!class) return ECTYPE~;
			s += 2; // skip :]
			min = 0;
			max = CHAR_MAX_CODEPOINT;
		}
		else
		{
			s++;
			if (*s == '-' && s[1] != ']')
			{
				s++;
				if (!*s) return EBRACK~;
				max = (int)*s;
				if (min > max) return ERANGE~;
				s++;
			}
		}

		if (class != 0 && negate)
		{
			if (neg_len >= 64) return OUT_OF_MEMORY~;
			neg_classes[neg_len++] = class;
		}
		else
		{
			if (ls_len >= 256) return OUT_OF_MEMORY~;
			ls[ls_len] = {
				.code_min = min,
				.code_max = max,
				.class = class,
				.neg_classes = null,
				.position = -1
			};
			ls_len++;

			if ((ctx.cflags & ICASE) && class == 0)
			{
				if (add_icase_literals(&ls[0], &ls_len, min, max) < 0) return OUT_OF_MEMORY~;
			}
		}
	}

	int* nc = null;
	if (negate)
	{
		// With REG_NEWLINE, newlines are excluded from negated brackets.
		if (ctx.cflags & NEWLINE)
		{
			if (ls_len >= 256) return OUT_OF_MEMORY~;
			ls[ls_len] = {
				.code_min = '\n',
				.code_max = '\n',
				.class = 0,
				.neg_classes = null,
				.position = -1
			};
			ls_len++;
		}
		// Sort by code_min.
		for (int i = 0; i < ls_len - 1; i++)
		{
			for (int j = i + 1; j < ls_len; j++)
			{
				if (ls[i].code_min > ls[j].code_min)
				{
					Literal tmp = ls[i];
					ls[i] = ls[j];
					ls[j] = tmp;
				}
			}
		}
		// Sentinel.
		if (ls_len >= 256) return OUT_OF_MEMORY~;
		ls[ls_len] = {
			.code_min = CHAR_MAX_CODEPOINT + 1,
			.code_max = CHAR_MAX_CODEPOINT + 1,
			.class = 0,
			.neg_classes = null,
			.position = -1
		};
		ls_len++;
		// Negated classes array.
		if (neg_len > 0)
		{
			nc = alloc::malloc(ctx.mem, (sz)(neg_len + 1) * int::size);
			if (!nc) return OUT_OF_MEMORY~;
			mem::copy(nc, &neg_classes[0], neg_len * int::size);
			nc[neg_len] = 0;
		}
	}

	AstNode* node = null;
	int negmin, negmax;

	for (int i = 0; i < ls_len; i++)
	{
		int min = (int)ls[i].code_min;
		int max = (int)ls[i].code_max;
		int class = ls[i].class;

		if (negate)
		{
			if (min <= negmin)
			{
				if (max + 1 > negmin) negmin = max + 1;
				continue;
			}
			negmax = min - 1;
			min = negmin;
			max = negmax;
			negmin = (int)ls[i].code_max + 1;
			class = 0;
		}

		Literal* lit = alloc::malloc(ctx.mem, Literal::size);
		if (!lit) return OUT_OF_MEMORY~;
		*lit = {
			.code_min = min,
			.code_max = max,
			.position = ctx.position,
			.class = class,
			.neg_classes = nc
		};

		AstNode* n = ast_new_node(ctx.mem, LITERAL, lit)!;
		node = ast_new_union(ctx.mem, node, n)!;
	}

	ctx.position++;
	ctx.node = node;
}

// Helper: create a literal node with REG_ICASE handling (musl parse_literal: label).
fn AstNode*? make_literal(ParseCtx* ctx, int wc) @local
{
	char c = (char)wc;

	if ((ctx.cflags & ICASE) && (ascii::@is_lower(c) || ascii::@is_upper(c)))
	{
		int u = ascii::@to_upper(c);
		int l = ascii::@to_lower(c);

		AstNode* t1 = ast_new_literal(ctx.mem, u, u, ctx.position)!;
		AstNode* t2 = ast_new_literal(ctx.mem, l, l, ctx.position)!;
		return ast_new_union(ctx.mem, t1, t2);
	}
	return ast_new_literal(ctx.mem, wc, wc, ctx.position);
}

// parse_atom: s is the current position in the pattern.
// Updates ctx.cursor to point after the atom on success.
// musl's parse_atom.
fn void? parse_atom(ParseCtx* ctx, char* s) @local
{
	bool ere = (bool)(ctx.cflags & EXTENDED);
	AstNode* node = null;

	switch (*s)
	{
		case '[':
			return parse_bracket(ctx, s + 1)!;

		case '\\':
		{
			ZString p = tre_expand_macro(s[1]);
			if (p != null)
			{
				parse_atom(ctx, p)!;
				ctx.cursor = s + 2;
				return;
			}
			char* nx = s + 1;
			if (!*nx) return EESCAPE~;

			switch (*nx)
			{
				case 'b':
					node = ast_new_literal(ctx.mem, ASSERTION, ASSERT_AT_WB, -1)!;
					ctx.cursor = nx + 1;
				case 'B':
					node = ast_new_literal(ctx.mem, ASSERTION, ASSERT_AT_WB_NEG, -1)!;
					ctx.cursor = nx + 1;
				case '<':
					node = ast_new_literal(ctx.mem, ASSERTION, ASSERT_AT_BOW, -1)!;
					ctx.cursor = nx + 1;
				case '>':
					node = ast_new_literal(ctx.mem, ASSERTION, ASSERT_AT_EOW, -1)!;
					ctx.cursor = nx + 1;
				case 'x':
				{
					// \xHH or \x{HHHH}
					char* p2 = nx + 1;
					int v = 0;
					int len = 2;
					if (*p2 == '{') { len = 8; p2++; }
					for (int i = 0; i < len && v < 0x110000; i++)
					{
						int c = hexval(*p2);
						if (c < 0) break;
						v = 16 * v + c;
						p2++;
					}
					if (len == 8)
					{
						if (*p2 != '}') return EBRACE~;
						p2++;
					}
					node = ast_new_literal(ctx.mem, v, v, ctx.position++)!;
					ctx.cursor = p2;
				}
				case '{':
				case '+':
				case '?':
					if (!ere)
					{
						// \{, \+, \? in BRE after an empty expression is an error.
						return BADRPT~;
					}
					node = make_literal(ctx, *nx)!;
					ctx.position++;
					ctx.cursor = nx + 1;
				case '|':
					if (!ere)
					{
						// \| in BRE: emit EMPTY and leave ctx.cursor pointing at '\' so
						// tre_parse's alternation check sees \| and handles it.
						node = ast_new_literal(ctx.mem, EMPTY, -1, -1)!;
						ctx.cursor = s;
					}
					else
					{
						node = make_literal(ctx, '|')!;
						ctx.position++;
						ctx.cursor = nx + 1;
					}
				default:
					if (!ere && *nx >= '1' && *nx <= '9')
					{
						// BRE back reference \1-\9.
						int val = *nx - '0';
						if (!(ctx.backref_ok & (1 << val))) return ESUBREG~;
						node = ast_new_literal(ctx.mem, BACKREF, val, ctx.position++)!;
						if (val > ctx.max_backref) ctx.max_backref = val;
						ctx.cursor = nx + 1;
					}
					else
					{
						// Unknown escape, treat as literal (\| in ERE, \( in BRE opening a group
						// is handled by tre_parse, so we'd never see it here).
						int wc = *nx;
						node = make_literal(ctx, wc)!;
						ctx.position++;
						ctx.cursor = nx + 1;
					}
			}
		}

		case '.':
			if (ctx.cflags & NEWLINE)
			{
				// REG_NEWLINE: '.' matches anything except '\n'.
				AstNode* t1 = ast_new_literal(ctx.mem, 0, '\n' - 1, ctx.position++)!;
				AstNode* t2 = ast_new_literal(ctx.mem, '\n' + 1, CHAR_MAX_CODEPOINT, ctx.position++)!;
				node = ast_new_union(ctx.mem, t1, t2)!;
			}
			else
			{
				node = ast_new_literal(ctx.mem, 0, CHAR_MAX_CODEPOINT, ctx.position++)!;
			}
			ctx.cursor = s + 1;

		case '^':
			// In BRE, '^' is only special at the very start of the expression/branch.
			if (!ere && s != ctx.start)
			{
				node = make_literal(ctx, '^')!;
				ctx.position++;
			}
			else
			{
				node = ast_new_literal(ctx.mem, ASSERTION, ASSERT_AT_BOL, -1)!;
			}
			ctx.cursor = s + 1;

		case '$':
			// In BRE, '$' is only special at end of subexpr (before \) or \| or end).
			if (!ere && s[1] && (s[1] != '\\' || (s[2] != ')' && s[2] != '|')))
			{
				node = make_literal(ctx, '$')!;
				ctx.position++;
			}
			else
			{
				node = ast_new_literal(ctx.mem, ASSERTION, ASSERT_AT_EOL, -1)!;
			}
			ctx.cursor = s + 1;

		// *, {, +, ? in ERE at start of expression: error.
		// In BRE: not repetition operators at atom level, treat as literal.
		case '*':
		case '{':
		case '+':
		case '?':
			if (ere) return BADRPT~;
			node = make_literal(ctx, *s)!;
			ctx.position++;
			ctx.cursor = s + 1;

		// '|' in BRE: literal. '|' in ERE or NUL: EMPTY (tre_parse handles alternation).
		case '|':
			if (!ere)
			{
				node = make_literal(ctx, '|')!;
				ctx.position++;
				ctx.cursor = s + 1;
			}
			else
			{
				node = ast_new_literal(ctx.mem, EMPTY, -1, -1)!;
				ctx.cursor = s;
			}

		case '\0':
			node = ast_new_literal(ctx.mem, EMPTY, -1, -1)!;
			ctx.cursor = s;

		default:
			// Default: normal literal character.
			node = make_literal(ctx, *s)!;
			ctx.position++;
			ctx.cursor = s + 1;
	}

	ctx.node = node;
}

fn void? marksub(ParseCtx* ctx, AstNode* node, int subid) @local
{
	if (node.submatch_id >= 0)
	{
		AstNode* n = ast_new_literal(ctx.mem, EMPTY, -1, -1)!;
		n = ast_new_catenation(ctx.mem, n, node)!;
		n.num_submatches = node.num_submatches;
		node = n;
	}
	node.submatch_id = subid;
	node.num_submatches++;
	ctx.node = node;
	// POSIX BRE backreferences are \1–\9 only, so only groups 1–9 can ever be
	// referenced; tracking groups >= 10 in backref_ok would never be checked.
	if (subid < 10) ctx.backref_ok |= 1 << subid;
}

fn char* parse_dup_count(char* s, int* n) @local
{
	*n = -1;
	if (!ascii::@is_digit(*s)) return s;
	*n = 0;
	for (;;)
	{
		*n = 10 * *n + (*s - '0');
		s++;
		if (!ascii::@is_digit(*s) || *n > RE_DUP_MAX) break;
	}
	return s;
}

fn char*? parse_dup(char* s, bool ere, int* pmin, int* pmax) @local
{
	int min, max;
	s = parse_dup_count(s, &min);
	if (*s == ',')
	{
		s = parse_dup_count(s + 1, &max);
	}
	else
	{
		max = min;
	}

	if ((max < min && max >= 0) || max > RE_DUP_MAX || min > RE_DUP_MAX || min < 0) return BADBR~;
	if (!ere) { if (*s++ != '\\') return BADBR~; }
	if (*s++ != '}') return BADBR~;
	*pmin = min;
	*pmax = max;
	return s;
}

// tre_parse: flat iterative main parse loop, matching musl's tre_parse exactly.
// Uses ctx.stack for group state. No depth parameter, depth is tracked locally.
fn void? tre_parse(ParseCtx* ctx) @local
{
	bool ere = (bool)(ctx.cflags & EXTENDED);
	AstNode* nbranch = null;
	AstNode* nunion = null;
	char* s = ctx.start;
	int subid = 0;
	int depth = 0;
	Stack* stack = ctx.stack;

	// Push the initial subid=0 for the implicit outermost submatch.
	stack_push_int(stack, subid++)!;

	// have_atom: skip parse_atom and go straight to quantifiers.
	// Used after closing a group (equivalent to musl's goto parse_iter).
	bool have_atom = false;

	for (;;)
	{
		if (!have_atom)
		{
			// Group open: ( in ERE or \( in BRE.
			if ((!ere && *s == '\\' && s[1] == '(') || (ere && *s == '('))
			{
				stack_push_ptr(stack, nunion)!;
				stack_push_ptr(stack, nbranch)!;
				stack_push_int(stack, subid++)!;
				s++;
				if (!ere) s++; // skip second char of \(
				depth++;
				nbranch = nunion = null;
				ctx.start = s;
				continue;
			}

			// Group close: ) in ERE (depth > 0) or \) in BRE.
			// Emit EMPTY so the closing delimiter is handled below.
			if ((!ere && *s == '\\' && s[1] == ')') || (ere && *s == ')' && depth > 0))
			{
				ctx.node = ast_new_literal(ctx.mem, EMPTY, -1, -1)!;
				// s is NOT advanced; the closing delimiter is consumed in the
				// alternation section below.
			}
			else
			{
				parse_atom(ctx, s)!;
				s = ctx.cursor;
			}
		}
		have_atom = false;

		// Quantifier loop: apply *, +, ?, {m,n} to ctx.node.
		for (;;)
		{
			int min, max;
			if (*s != '\\' && *s != '*')
			{
				if (!ere) break;
				if (*s != '+' && *s != '?' && *s != '{') break;
			}
			if (*s == '\\' && ere) break;
			if (*s == '\\' && s[1] != '+' && s[1] != '?' && s[1] != '{') break;
			if (*s == '\\') s++;
			// Handle ^* at start of BRE.
			if (!ere && s == ctx.start + 1 && s[-1] == '^') break;

			if (*s == '{')
			{
				s = parse_dup(s + 1, ere, &min, &max)!;
			}
			else
			{
				min = 0; max = -1;
				if (*s == '+') min = 1;
				if (*s == '?') max = 1;
				s++;
			}
			if (max == 0)
			{
				ctx.node = ast_new_literal(ctx.mem, EMPTY, -1, -1)!;
			}
			else
			{
				ctx.node = ast_new_iter(ctx.mem, ctx.node, min, max, false)!;
			}
		}

		// Catenate the atom/iteration onto the current branch.
		nbranch = ast_new_catenation(ctx.mem, nbranch, ctx.node)!;

		// Check for alternation, group close, or end of pattern.
		if ((ere && *s == '|') ||
			(ere && *s == ')' && depth > 0) ||
			(!ere && *s == '\\' && s[1] == ')') ||
			(!ere && *s == '\\' && s[1] == '|') ||
			!*s)
		{
			int c = *s;
			nunion = ast_new_union(ctx.mem, nunion, nbranch)!;
			nbranch = null;

			if (c == '\\' && s[1] == '|')
			{
				// BRE \| extension: alternation.
				s += 2;
				ctx.start = s;
			}
			else if (c == '|')
			{
				// ERE alternation.
				s++;
				ctx.start = s;
			}
			else
			{
				// Group close or end of pattern.
				if (c == '\\')
				{
					if (depth == 0) return EPAREN~;
					s += 2;
				}
				else if (c == ')') s++;
				depth--;

				marksub(ctx, nunion, stack_pop_int(stack))!;

				if (!c && depth < 0)
				{
					// End of pattern at outermost level: success.
					ctx.submatch_id = subid;
					ctx.cursor = s;
					return;
				}
				if (!c || depth < 0) return EPAREN~;

				// Restore outer group state.
				nbranch = stack_pop_ptr(stack);
				nunion = stack_pop_ptr(stack);
				// Apply quantifiers to the closed group (musl: goto parse_iter).
				have_atom = true;
			}
		}
	}
}

fn void? tre_add_tag_left(Allocator arena, AstNode* node, TagId tag_id) @local => tre_add_tag{true}(arena, node, tag_id);
fn void? tre_add_tag_right(Allocator arena, AstNode* node, TagId tag_id) @local => tre_add_tag{false}(arena, node, tag_id);

fn void? tre_add_tag(Allocator arena, AstNode* node, TagId tag_id) <TAG_LEFT> @local
{
	Catenation* c = alloc::malloc(arena, Catenation::size);
	if (!c) return OUT_OF_MEMORY~;

	AstNode* tag_node = ast_new_literal(arena, TAG, tag_id, -1)!;

	AstNode* other_node = alloc::malloc(arena, AstNode::size);
	if (!other_node) return OUT_OF_MEMORY~;
	*other_node = *node;

	if (TAG_LEFT)
	{
		*c = { .left = tag_node, .right = other_node };
	}
	else
	{
		*c = { .left = other_node, .right = tag_node };
	}

	node.obj = c;
	node.type = CATENATION;
}

fn void tre_purge_regset(int* regset, Tnfa* tnfa, TagId tag) @local
{
	for (int i = 0; regset[i] >= 0; i++)
	{
		int id = regset[i] / 2;
		bool start = (regset[i] % 2 == 0);
		if (tnfa)
		{
			if (start) { tnfa.submatch_data[id].so_tag = tag; }
			else       { tnfa.submatch_data[id].eo_tag = tag; }
		}
	}
	regset[0] = -1;
}

constdef AddTagsSymbol : inline int @local
{
	RECURSE,
	AFTER_ITERATION,
	AFTER_UNION_LEFT,
	AFTER_UNION_RIGHT,
	AFTER_CAT_LEFT,
	AFTER_CAT_RIGHT,
	SET_SUBMATCH_END
}

struct TreTagStates @local
{
	TagId tag;
	TagId next_tag;
}

fn void tre_add_minimal_tag(Tnfa* tnfa, TagId tag, int* minimal_tag, int* num_minimals) @local
{
	if (*minimal_tag >= 0)
	{
		int i;
		for (i = 0; tnfa.minimal_tags[i] >= 0; i++);
		tnfa.minimal_tags[i] = tag;
		tnfa.minimal_tags[i + 1] = *minimal_tag;
		tnfa.minimal_tags[i + 2] = -1;
		*minimal_tag = -1;
		(*num_minimals)++;
	}
}

fn void? tre_add_tags(Allocator arena, Stack* stack, AstNode* tree, Tnfa* tnfa) @local
{
	int bottom = stack.count;
	bool first_pass = (arena == null || tnfa == null);

	int[512] regset_buf;
	int* regset = &regset_buf[0];
	int[256] parents_buf;
	int* parents = &parents_buf[0];

	regset[0] = -1;
	parents[0] = -1;

	int num_tags = 0;
	int num_minimals = 0;
	TagId tag = 0;
	TagId next_tag = 1;
	int minimal_tag = -1;
	int direction = TRE_TAG_MINIMIZE;
	AstNode* node = null;

	if (!first_pass)
	{
		tnfa.end_tag = 0;
		tnfa.minimal_tags[0] = -1;
	}

	stack_push_ptr(stack, tree)!;
	stack_push_int(stack, AddTagsSymbol.RECURSE)!;

	while (stack.count > bottom)
	{
		AddTagsSymbol symbol = (AddTagsSymbol)stack_pop_int(stack);

		switch (symbol)
		{
			case SET_SUBMATCH_END:
			{
				int id = stack_pop_int(stack);
				int i;
				for (i = 0; regset[i] >= 0; i++);
				regset[i] = id * 2 + 1;
				regset[i + 1] = -1;
				for (i = 0; parents[i] >= 0; i++);
				parents[i - 1] = -1;
			}

			case RECURSE:
			{
				node = stack_pop_ptr(stack);
				if (node.submatch_id >= 0)
				{
					int id = node.submatch_id;
					int i;
					for (i = 0; regset[i] >= 0; i++);
					regset[i] = id * 2;
					regset[i + 1] = -1;

					if (!first_pass)
					{
						for (i = 0; parents[i] >= 0; i++);
						tnfa.submatch_data[id].parents = null;
						if (i > 0)
						{
							int* p = alloc::malloc(tnfa.allocator, (sz)(i + 1) * int::size);
							if (!p) return OUT_OF_MEMORY~;
							tnfa.submatch_data[id].parents = p;
							mem::copy(p, parents, (usz)i * int::size);
							p[i] = -1;
						}
					}

					stack_push_int(stack, node.submatch_id)!;
					stack_push_int(stack, AddTagsSymbol.SET_SUBMATCH_END)!;
				}

				switch (node.type)
				{
					case LITERAL:
					{
						Literal* lit = node.obj;
						if (lit.code_min >= 0 || lit.code_min == EMPTY || lit.code_min == ASSERTION || lit.code_min == TAG || lit.code_min == BACKREF)
						{
							if (!(lit.code_min < 0) || lit.code_min == (long)BACKREF)
							{
								if (regset[0] >= 0)
								{
									if (!first_pass)
									{
										tre_add_tag_left(arena, node, tag)!;
										tnfa.tag_directions[tag] = direction;
										tre_add_minimal_tag(tnfa, tag, &minimal_tag, &num_minimals);
										tre_purge_regset(regset, tnfa, tag);
									}
									else
									{
										node.num_tags = 1;
									}
									regset[0] = -1;
									tag = next_tag;
									num_tags++;
									next_tag++;
								}
							}
						}
					}

					case CATENATION:
					{
						Catenation* cat = node.obj;
						AstNode* left = cat.left;
						AstNode* right = cat.right;
						int reserved_tag = -1;

						stack_push_ptr(stack, node)!;
						stack_push_int(stack, AddTagsSymbol.AFTER_CAT_RIGHT)!;

						stack_push_ptr(stack, right)!;
						stack_push_int(stack, AddTagsSymbol.RECURSE)!;

						stack_push_int(stack, next_tag + left.num_tags)!;
						if (left.num_tags > 0 && right.num_tags > 0)
						{
							reserved_tag = next_tag;
							next_tag++;
						}
						stack_push_int(stack, reserved_tag)!;
						stack_push_int(stack, AddTagsSymbol.AFTER_CAT_LEFT)!;

						stack_push_ptr(stack, left)!;
						stack_push_int(stack, AddTagsSymbol.RECURSE)!;
					}

					case ITERATION:
					{
						Iteration* iter = node.obj;
						if (first_pass)
						{
							stack_push_int(stack, (regset[0] >= 0 || iter.minimal) ? 1 : 0)!;
						}
						else
						{
							stack_push_int(stack, tag)!;
							stack_push_int(stack, iter.minimal ? 1 : 0)!;
						}
						stack_push_ptr(stack, node)!;
						stack_push_int(stack, AddTagsSymbol.AFTER_ITERATION)!;

						stack_push_ptr(stack, iter.arg)!;
						stack_push_int(stack, AddTagsSymbol.RECURSE)!;

						if (regset[0] >= 0 || iter.minimal)
						{
							if (!first_pass)
							{
								tre_add_tag_left(arena, node, tag)!;
								if (iter.minimal) { tnfa.tag_directions[tag] = TRE_TAG_MAXIMIZE; }
								else { tnfa.tag_directions[tag] = direction; }
								tre_add_minimal_tag(tnfa, tag, &minimal_tag, &num_minimals);
								tre_purge_regset(regset, tnfa, tag);
							}
							else
							{
								node.num_tags = 1;
							}
							regset[0] = -1;
							tag = next_tag;
							num_tags++;
							next_tag++;
						}
						direction = TRE_TAG_MINIMIZE;
					}

					case UNION:
					{
						Union* uni = node.obj;
						AstNode* left = uni.left;
						AstNode* right = uni.right;
						TagId left_tag;
						TagId right_tag;

						if (regset[0] >= 0)
						{
							left_tag = next_tag;
							right_tag = next_tag + 1;
						}
						else
						{
							left_tag = tag;
							right_tag = next_tag;
						}

						stack_push_int(stack, right_tag)!;
						stack_push_int(stack, left_tag)!;
						stack_push_ptr(stack, regset)!;
						stack_push_int(stack, (regset[0] >= 0) ? 1 : 0)!;
						stack_push_ptr(stack, node)!;
						stack_push_ptr(stack, right)!;
						stack_push_ptr(stack, left)!;
						stack_push_int(stack, AddTagsSymbol.AFTER_UNION_RIGHT)!;

						stack_push_ptr(stack, right)!;
						stack_push_int(stack, AddTagsSymbol.RECURSE)!;

						stack_push_int(stack, AddTagsSymbol.AFTER_UNION_LEFT)!;

						stack_push_ptr(stack, left)!;
						stack_push_int(stack, AddTagsSymbol.RECURSE)!;

						if (regset[0] >= 0)
						{
							if (!first_pass)
							{
								tre_add_tag_left(arena, node, tag)!;
								tnfa.tag_directions[tag] = direction;
								tre_add_minimal_tag(tnfa, tag, &minimal_tag, &num_minimals);
								tre_purge_regset(regset, tnfa, tag);
							}
							else
							{
								node.num_tags = 1;
							}
							regset[0] = -1;
							tag = next_tag;
							num_tags++;
							next_tag++;
						}

						if (node.num_submatches > 0)
						{
							next_tag++;
							tag = next_tag;
							next_tag++;
						}
					}
				}

				if (node.submatch_id >= 0)
				{
					int i;
					for (i = 0; parents[i] >= 0; i++);
					parents[i] = node.submatch_id;
					parents[i + 1] = -1;
				}
			}

			case AFTER_ITERATION:
			{
				int minimal = 0;
				TagId enter_tag = 0;
				node = stack_pop_ptr(stack);
				if (first_pass)
				{
					Iteration* iter = node.obj;
					node.num_tags = iter.arg.num_tags + stack_pop_int(stack);
					minimal_tag = -1;
				}
				else
				{
					minimal = stack_pop_int(stack);
					enter_tag = (TagId)stack_pop_int(stack);
					if (minimal != 0) minimal_tag = enter_tag;
				}

				if (!first_pass)
				{
					if (minimal != 0) { direction = TRE_TAG_MINIMIZE; }
					else { direction = TRE_TAG_MAXIMIZE; }
				}
			}

			case AFTER_CAT_LEFT:
			{
				TagId new_tag = (TagId)stack_pop_int(stack);
				next_tag = (TagId)stack_pop_int(stack);
				if (new_tag >= 0) tag = new_tag;
			}

			case AFTER_CAT_RIGHT:
			{
				node = stack_pop_ptr(stack);
				if (first_pass)
				{
					Catenation* cat = node.obj;
					node.num_tags = cat.left.num_tags + cat.right.num_tags;
				}
			}

			case AFTER_UNION_LEFT:
			{
				while (regset[0] >= 0) regset++;
			}

			case AFTER_UNION_RIGHT:
			{
				int added_tags = 0;
				AstNode* left = stack_pop_ptr(stack);
				AstNode* right = stack_pop_ptr(stack);
				node = stack_pop_ptr(stack);
				added_tags = stack_pop_int(stack);
				if (first_pass)
				{
					Union* uni = node.obj;
					node.num_tags = uni.left.num_tags + uni.right.num_tags + added_tags + ((node.num_submatches > 0) ? 2 : 0);
				}
				regset = stack_pop_ptr(stack);
				TagId tag_left = (TagId)stack_pop_int(stack);
				TagId tag_right = (TagId)stack_pop_int(stack);
				if (node.num_submatches > 0)
				{
					if (!first_pass)
					{
						tre_add_tag_right(arena, left, tag_left)!;
						tre_add_tag_right(arena, right, tag_right)!;
						tnfa.tag_directions[tag_left] = TRE_TAG_MAXIMIZE;
						tnfa.tag_directions[tag_right] = TRE_TAG_MAXIMIZE;
					}
					num_tags += 2;
				}
				direction = TRE_TAG_MAXIMIZE;
			}
		}
	}

	if (!first_pass)
	{
		tre_purge_regset(regset, tnfa, tag);
	}

	tre_add_minimal_tag(tnfa, tag, &minimal_tag, &num_minimals);

	if (first_pass)
	{
		tree.num_tags = num_tags;
	}
	else
	{
		tnfa.end_tag = num_tags;
		tnfa.num_tags = num_tags;
		tnfa.num_minimals = num_minimals;
	}
}

fn PosAndTags* tre_set_empty(Allocator arena) @local
{
	PosAndTags* new_set = alloc::malloc(arena, 2 * PosAndTags::size);
	if (!new_set) return null;
	new_set[0] = { .position = -1 };
	return new_set;
}

fn PosAndTags* tre_set_one(Allocator arena, StateId position, int code_min, int code_max, int class, int* neg_classes, int backref) @local
{
	PosAndTags* new_set = alloc::malloc(arena, 2 * PosAndTags::size);
	if (!new_set) return null;

	new_set[0] = {
		.position = position,
		.code_min = code_min,
		.code_max = code_max,
		.class = class,
		.neg_classes = neg_classes,
		.backref = backref
	};
	new_set[1] = { .position = -1 };
	return new_set;
}

fn PosAndTags* tre_set_union(Allocator arena, PosAndTags* set1, PosAndTags* set2, TagId* tags, int assertions) @local
{
	int s1 = 0;
	int s2 = 0;
	if (set1) for (s1 = 0; set1[s1].position >= 0; s1++);
	if (set2) for (s2 = 0; set2[s2].position >= 0; s2++);

	PosAndTags* new_set = alloc::malloc(arena, (sz)(s1 + s2 + 1) * PosAndTags::size);
	if (!new_set) return null;

	int i;
	for (i = 0; i < s1; i++)
	{
		new_set[i] = set1[i];
		new_set[i].assertions |= assertions;
		if (tags && tags[0] >= 0)
		{
			int num_tags = 0;
			if (set1[i].tags) for (num_tags = 0; set1[i].tags[num_tags] >= 0; num_tags++);
			int add_tags = 0;
			for (add_tags = 0; tags[add_tags] >= 0; add_tags++);

			TagId* new_tags = alloc::malloc(arena, (sz)(num_tags + add_tags + 1) * TagId::size);
			if (!new_tags) return null;

			mem::copy(new_tags, tags, add_tags * TagId::size);
			mem::copy(&new_tags[add_tags], set1[i].tags, num_tags * TagId::size);
			new_tags[add_tags + num_tags] = -1;

			new_set[i].tags = new_tags;
		}
	}

	for (i = 0; i < s2; i++)
	{
		new_set[s1 + i] = set2[i];
	}
	new_set[s1 + s2] = { .position = -1 };
	return new_set;
}


constdef NflSymbol : inline int @local
{
	RECURSE,
	POST_UNION,
	POST_CATENATION,
	POST_ITERATION
}

fn void? tre_match_empty(Stack* stack, AstNode* node, TagId* tags, int* assertions, int* num_tags_seen) @local
{
	if (num_tags_seen) *num_tags_seen = 0;

	int bottom = stack.count;
	stack_push_ptr(stack, node)!;

	while (stack.count > bottom)
	{
		node = stack_pop_ptr(stack);

		switch (node.type)
		{
			case LITERAL:
			{
				Literal* lit = node.obj;
				if (lit.code_min == (int)TAG)
				{
					if (lit.code_max >= 0)
					{
						if (tags)
						{
							int i = 0;
							for (i = 0; tags[i] >= 0; i++)
							{
								if (tags[i] == (int)lit.code_max) break;
							}
							if (tags[i] < 0)
							{
								tags[i] = (TagId)lit.code_max;
								tags[i + 1] = -1;
							}
						}
						if (num_tags_seen) (*num_tags_seen)++;
					}
				}
				else if (lit.code_min == (int)ASSERTION)
				{
					if (assertions) *assertions |= (int)lit.code_max;
				}
			}

			case UNION:
			{
				Union* uni = node.obj;
				if (uni.left.nullable) { stack_push_ptr(stack, uni.left)!; }
				else if (uni.right.nullable) { stack_push_ptr(stack, uni.right)!; }
			}

			case CATENATION:
			{
				Catenation* cat = node.obj;
				stack_push_ptr(stack, cat.right)!;
				stack_push_ptr(stack, cat.left)!;
			}

			case ITERATION:
			{
				Iteration* iter = node.obj;
				if (iter.arg.nullable) stack_push_ptr(stack, iter.arg)!;
			}
		}
	}
}

fn void? tre_compute_nfl(Allocator arena, Stack* stack, AstNode* tree) @local
{
	int bottom = stack.count;
	stack_push_ptr(stack, tree)!;
	stack_push_int(stack, NflSymbol.RECURSE)!;

	while (stack.count > bottom)
	{
		NflSymbol symbol = (NflSymbol)stack_pop_int(stack);
		AstNode* node = stack_pop_ptr(stack);

		switch (symbol)
		{
			case RECURSE:
				switch (node.type)
				{
					case LITERAL:
					{
						Literal* lit = node.obj;
						if (lit.code_min == BACKREF)
						{
							node.nullable = false;
							node.firstpos = tre_set_one(arena, lit.position, 0, CHAR_MAX_CODEPOINT, 0, null, -1);
							node.lastpos = tre_set_one(arena, lit.position, 0, CHAR_MAX_CODEPOINT, 0, null, (int)lit.code_max);
						}
						else if (lit.code_min < 0)
						{
							node.nullable = true;
							node.firstpos = tre_set_empty(arena);
							node.lastpos = tre_set_empty(arena);
						}
						else
						{
							node.nullable = false;
							node.firstpos = tre_set_one(arena, lit.position, (int)lit.code_min, (int)lit.code_max, lit.class, lit.neg_classes, -1);
							node.lastpos = tre_set_one(arena, lit.position, (int)lit.code_min, (int)lit.code_max, lit.class, lit.neg_classes, -1);
						}
					}

					case UNION:
					{
						Union* uni = node.obj;
						stack_push_ptr(stack, node)!;
						stack_push_int(stack, NflSymbol.POST_UNION)!;
						stack_push_ptr(stack, uni.right)!;
						stack_push_int(stack, NflSymbol.RECURSE)!;
						stack_push_ptr(stack, uni.left)!;
						stack_push_int(stack, NflSymbol.RECURSE)!;
					}

					case CATENATION:
					{
						Catenation* cat = node.obj;
						stack_push_ptr(stack, node)!;
						stack_push_int(stack, NflSymbol.POST_CATENATION)!;
						stack_push_ptr(stack, cat.right)!;
						stack_push_int(stack, NflSymbol.RECURSE)!;
						stack_push_ptr(stack, cat.left)!;
						stack_push_int(stack, NflSymbol.RECURSE)!;
					}

					case ITERATION:
					{
						Iteration* iter = node.obj;
						stack_push_ptr(stack, node)!;
						stack_push_int(stack, NflSymbol.POST_ITERATION)!;
						stack_push_ptr(stack, iter.arg)!;
						stack_push_int(stack, NflSymbol.RECURSE)!;
					}
				}

			case POST_UNION:
			{
				Union* uni = node.obj;
				node.nullable = uni.left.nullable || uni.right.nullable;
				node.firstpos = tre_set_union(arena, uni.left.firstpos, uni.right.firstpos, null, 0);
				node.lastpos = tre_set_union(arena, uni.left.lastpos, uni.right.lastpos, null, 0);
			}

			case POST_ITERATION:
			{
				Iteration* iter = node.obj;
				node.nullable = iter.min == 0 || iter.arg.nullable;
				node.firstpos = iter.arg.firstpos;
				node.lastpos = iter.arg.lastpos;
			}

			case POST_CATENATION:
			{
				Catenation* cat = node.obj;
				node.nullable = cat.left.nullable && cat.right.nullable;

				if (cat.left.nullable)
				{
					int assertions = 0;
					TagId[32] tags;
					tags[0] = -1;
					tre_match_empty(stack, cat.left, &tags[0], &assertions, null)!;
					node.firstpos = tre_set_union(arena, cat.right.firstpos, cat.left.firstpos, &tags[0], assertions);
				}
				else
				{
					node.firstpos = cat.left.firstpos;
				}

				if (cat.right.nullable)
				{
					int assertions = 0;
					TagId[32] tags;
					tags[0] = -1;
					tre_match_empty(stack, cat.right, &tags[0], &assertions, null)!;
					node.lastpos = tre_set_union(arena, cat.left.lastpos, cat.right.lastpos, &tags[0], assertions);
				}
				else
				{
					node.lastpos = cat.right.lastpos;
				}
			}
		}
	}
}

fn void? tre_make_trans(Allocator mem, PosAndTags* p1, PosAndTags* p2, TnfaTransition* transitions, int* counts, int* offs) @local
{
	PosAndTags* orig_p2 = p2;

	if (transitions)
	{
		while (p1.position >= 0)
		{
			p2 = orig_p2;
			int prev_p2_pos = -1;
			while (p2.position >= 0)
			{
				if (p2.position == prev_p2_pos)
				{
					p2++;
					continue;
				}
				prev_p2_pos = p2.position;

				TnfaTransition* trans = &transitions[offs[p1.position]];
				while (trans.state) trans++;

				if (trans.state == null) (trans + 1).state = null;

				trans.code_min = p1.code_min;
				trans.code_max = p1.code_max;
				trans.state = &transitions[offs[p2.position]];
				trans.state_id = p2.position;
				trans.assertions = p1.assertions | p2.assertions;

				if (p1.backref >= 0)
				{
					trans.u_class_or_backref = p1.backref;
					trans.assertions |= ASSERT_BACKREF;
				}
				if (p1.class != 0)
				{
					trans.u_class_or_backref = p1.class;
					trans.assertions |= ASSERT_CHAR_CLASS;
				}
				if (p1.neg_classes != null)
				{
					int size = 0;
					while (p1.neg_classes[size] != 0) size++;
					trans.neg_classes = alloc::malloc(mem, (usz)(size + 1) * int::size);
					if (!trans.neg_classes) return OUT_OF_MEMORY~;
					mem::copy(trans.neg_classes, p1.neg_classes, (usz)(size + 1) * int::size);
					trans.assertions |= ASSERT_CHAR_CLASS_NEG;
				}

				// Tag merging
				int num_p1_tags = 0;
				if (p1.tags) while (p1.tags[num_p1_tags] >= 0) num_p1_tags++;
				int num_p2_tags = 0;
				if (p2.tags) while (p2.tags[num_p2_tags] >= 0) num_p2_tags++;

				if (num_p1_tags + num_p2_tags > 0)
				{
					trans.tags = alloc::malloc(mem, (usz)(num_p1_tags + num_p2_tags + 1) * int::size);
					if (!trans.tags) return OUT_OF_MEMORY~;

					if (p1.tags)
					{
						mem::copy(trans.tags, p1.tags, (usz)num_p1_tags * int::size);
					}

					int total_tags = num_p1_tags;
					if (p2.tags)
					{
						for (int t2 = 0; t2 < num_p2_tags; t2++)
						{
							bool dup = false;
							for (int t1 = 0; t1 < num_p1_tags; t1++)
							{
								if (trans.tags[t1] == p2.tags[t2])
								{
									dup = true;
									break;
								}
							}
							if (!dup)
							{
								trans.tags[total_tags++] = p2.tags[t2];
							}
						}
					}
					trans.tags[total_tags] = -1;
				}
				p2++;
			}
			p1++;
		}
	}
	else
	{
		while (p1.position >= 0)
		{
			p2 = orig_p2;
			int prev_p2_pos = -1;
			while (p2.position >= 0)
			{
				if (p2.position == prev_p2_pos) { p2++; continue; }
				prev_p2_pos = p2.position;
				counts[p1.position]++;
				p2++;
			}
			p1++;
		}
	}

}

fn void? tre_ast_to_tnfa(Allocator mem, AstNode* node, TnfaTransition* transitions, int* counts, int* offs) @local
{
	switch (node.type)
	{
		case UNION:
		{
			Union* uni = node.obj;
			tre_ast_to_tnfa(mem, uni.left, transitions, counts, offs)!;
			tre_ast_to_tnfa(mem, uni.right, transitions, counts, offs)!;
		}

		case CATENATION:
		{
			Catenation* cat = node.obj;
			tre_make_trans(mem, cat.left.lastpos, cat.right.firstpos, transitions, counts, offs)!;
			tre_ast_to_tnfa(mem, cat.left, transitions, counts, offs)!;
			tre_ast_to_tnfa(mem, cat.right, transitions, counts, offs)!;
		}

		case ITERATION:
		{
			Iteration* iter = node.obj;
			if (iter.max == -1 || iter.max > 1)
			{
				tre_make_trans(mem, iter.arg.lastpos, iter.arg.firstpos, transitions, counts, offs)!;
			}
			tre_ast_to_tnfa(mem, iter.arg, transitions, counts, offs)!;
		}

		case LITERAL:
			// do nothing, matching musl regcomp.c
	}
}

const int COPY_REMOVE_TAGS	      @private = 1;
const int COPY_MAXIMIZE_FIRST_TAG @private = 2;

enum CopyastSymbol @local
{
	COPY_SET_RESULT_PTR,
	COPY_RECURSE
}

enum ExpandAstSymbol @local
{
	EXPAND_RECURSE,
	EXPAND_AFTER_ITER
}
fn bool is_special(Literal* lit) @local => lit.code_min < 0;
fn bool is_backref(Literal* lit) @local => lit.code_min == BACKREF;
fn bool is_tag(Literal* lit) @local => lit.code_min == TAG;

fn void? tre_copy_ast(Allocator mem, Stack* stack, AstNode* ast, int flags, int* pos_add, int* tag_directions, AstNode** copy, int* max_pos) @local
{
	int bottom = stack.count;
	int num_copied = 0;
	int first_tag = 1;
	AstNode** result = copy;

	stack_push_ptr(stack, ast)!;
	stack_push_int(stack, (int)CopyastSymbol.COPY_RECURSE)!;

	while (stack.count > bottom)
	{
		CopyastSymbol symbol = (CopyastSymbol)stack_pop_int(stack);
		switch (symbol)
		{
			case COPY_SET_RESULT_PTR:
				result = stack_pop_ptr(stack);

			case COPY_RECURSE:
				AstNode* node = stack_pop_ptr(stack);
				switch (node.type)
				{
					case LITERAL:
					{
						Literal* lit = node.obj;
						StateId pos = lit.position;
						int min = (int)lit.code_min;
						int max = (int)lit.code_max;
						if (!is_special(lit) || is_backref(lit))
						{
							pos += (StateId)*pos_add;
							num_copied++;
						}
						else if (is_tag(lit) && (flags & COPY_REMOVE_TAGS) != 0)
						{
							min = (int)EMPTY;
							max = -1;
							pos = -1;
						}
						else if (is_tag(lit) && (flags & COPY_MAXIMIZE_FIRST_TAG) != 0 && first_tag != 0)
						{
							if (tag_directions) tag_directions[max] = TRE_TAG_MAXIMIZE;
							first_tag = 0;
						}
						*result = ast_new_literal(mem, min, max, pos)!;
						Literal* p = (*result).obj;
						p.class = lit.class;
						p.neg_classes = lit.neg_classes;
						if (pos > *max_pos) *max_pos = pos;
					}

					case UNION:
					{
						Union* uni = node.obj;
						*result = ast_new_union(mem, uni.left, uni.right)!;
						if (*result == null)
						{
							return OUT_OF_MEMORY~;
						}
						Union* tmp = (*result).obj;
						result = &tmp.left;
						stack_push_ptr(stack, uni.right)!;
						stack_push_int(stack, (int)CopyastSymbol.COPY_RECURSE)!;
						stack_push_ptr(stack, &tmp.right)!;
						stack_push_int(stack, (int)CopyastSymbol.COPY_SET_RESULT_PTR)!;
						stack_push_ptr(stack, uni.left)!;
						stack_push_int(stack, (int)CopyastSymbol.COPY_RECURSE)!;
					}

					case CATENATION:
					{
						Catenation* cat = node.obj;
						*result = ast_new_catenation(mem, cat.left, cat.right)!;
						if (*result == null)
						{
							return OUT_OF_MEMORY~;
						}
						Catenation* tmp = (*result).obj;
						tmp.left = null;
						tmp.right = null;
						result = &tmp.left;

						stack_push_ptr(stack, cat.right)!;
						stack_push_int(stack, (int)CopyastSymbol.COPY_RECURSE)!;
						stack_push_ptr(stack, &tmp.right)!;
						stack_push_int(stack, (int)CopyastSymbol.COPY_SET_RESULT_PTR)!;
						stack_push_ptr(stack, cat.left)!;
						stack_push_int(stack, (int)CopyastSymbol.COPY_RECURSE)!;
					}

					case ITERATION:
					{
						Iteration* iter = node.obj;
						stack_push_ptr(stack, iter.arg)!;
						stack_push_int(stack, (int)CopyastSymbol.COPY_RECURSE)!;
						*result = ast_new_iter(mem, iter.arg, iter.min, iter.max, iter.minimal)!;
						iter = (*result).obj;
						result = &iter.arg;
					}
				}
		}
	}
	*pos_add += num_copied;
}

fn void? tre_expand_ast(Allocator mem, Stack* stack, AstNode* ast, int* position, int* tag_directions) @local
{
	int bottom = stack.count;
	int pos_add = 0;
	int pos_add_total = 0;
	int max_pos = 0;
	int iter_depth = 0;

	stack_push_ptr(stack, ast)!;
	stack_push_int(stack, (int)ExpandAstSymbol.EXPAND_RECURSE)!;

	while (stack.count > bottom)
	{
		ExpandAstSymbol symbol = (ExpandAstSymbol)stack_pop_int(stack);
		AstNode* node = stack_pop_ptr(stack);

		switch (symbol)
		{
			case EXPAND_RECURSE:
				switch (node.type)
				{
					case LITERAL:
					{
						Literal* lit = node.obj;
						if (!is_special(lit) || is_backref(lit))
						{
							lit.position += (StateId)pos_add;
							if (lit.position > max_pos) max_pos = lit.position;
						}
					}
					case UNION:
					{
						Union* uni = node.obj;
						stack_push_ptr(stack, uni.right)!;
						stack_push_int(stack, (int)ExpandAstSymbol.EXPAND_RECURSE)!;
						stack_push_ptr(stack, uni.left)!;
						stack_push_int(stack, (int)ExpandAstSymbol.EXPAND_RECURSE)!;
					}
					case CATENATION:
					{
						Catenation* cat = node.obj;
						stack_push_ptr(stack, cat.right)!;
						stack_push_int(stack, (int)ExpandAstSymbol.EXPAND_RECURSE)!;
						stack_push_ptr(stack, cat.left)!;
						stack_push_int(stack, (int)ExpandAstSymbol.EXPAND_RECURSE)!;
					}
					case ITERATION:
					{
						Iteration* iter = node.obj;
						stack_push_int(stack, pos_add)!;
						stack_push_ptr(stack, node)!;
						stack_push_int(stack, (int)ExpandAstSymbol.EXPAND_AFTER_ITER)!;
						stack_push_ptr(stack, iter.arg)!;
						stack_push_int(stack, (int)ExpandAstSymbol.EXPAND_RECURSE)!;
						if (iter.min > 1 || iter.max > 1) pos_add = 0;
						iter_depth++;
					}
				}

			case EXPAND_AFTER_ITER:
			{
				Iteration* iter = node.obj;
				pos_add = stack_pop_int(stack);
				int pos_add_last = pos_add;

				if (iter.min > 1 || iter.max > 1)
				{
					AstNode* seq1 = null;
					AstNode* seq2 = null;
					int pos_add_save = pos_add;

					// Build the mandatory sequence (copies of the iteration argument)
					for (int j = 0; j < iter.min; j++)
					{
						AstNode* copy = null;
						int flags = (j + 1 < iter.min) ? COPY_REMOVE_TAGS : COPY_MAXIMIZE_FIRST_TAG;
						pos_add_save = pos_add;
						tre_copy_ast(mem, stack, iter.arg, flags, &pos_add, tag_directions, &copy, &max_pos)!;
						seq1 = seq1 ? ast_new_catenation(mem, seq1, copy)! : copy;
					}

					// Build the optional / trailing sequence
					if (iter.max == -1)
					{
						pos_add_save = pos_add;
						tre_copy_ast(mem, stack, iter.arg, 0, &pos_add, null, &seq2, &max_pos)!;
						seq2 = ast_new_iter(mem, seq2, 0, -1, false)!;
					}
					else
					{
						for (int j = iter.min; j < iter.max; j++)
						{
							AstNode* copy = null;
							pos_add_save = pos_add;
							tre_copy_ast(mem, stack, iter.arg, 0, &pos_add, null, &copy, &max_pos)!;
							seq2 = seq2 ? ast_new_catenation(mem, copy, seq2)! : copy;
							AstNode* tmp = ast_new_literal(mem, EMPTY, -1, -1)!;
							seq2 = ast_new_union(mem, tmp, seq2)!;
						}
					}

					pos_add = pos_add_save;
					if (seq1 == null)
					{
						seq1 = seq2;
					}
					else if (seq2 != null)
					{
						seq1 = ast_new_catenation(mem, seq1, seq2)!;
					}
					if (seq1 == null) return OUT_OF_MEMORY~;

					node.obj = seq1.obj;
					node.type = seq1.type;
				}

				iter_depth--;
				pos_add_total += pos_add - pos_add_last;
				if (iter_depth == 0) pos_add = pos_add_total;
			}
		}
	}
	*position += pos_add_total;
	if (max_pos > *position) *position = max_pos;
}


fn void? re_compile(Regex* preg, String regex, CompileFlags cflags, Allocator allocator = mem) @private
{
	@pool()
	{
		String regex_nt = regex;
		// Ensure the pattern is null-terminated: the parser uses *s == '\0' as its stop condition.
		if (regex.len == 0 || regex.ptr[regex.len] != '\0')
		{
			char* buf = alloc::malloc(tmem, regex.len + 1);
			if (!buf) return OUT_OF_MEMORY~;
			mem::copy(buf, regex.ptr, regex.len);
			buf[regex.len] = '\0';
			regex_nt = (String)buf[0 : regex.len];
		}

		Stack* stack = stack_new(tmem, 512, 1024000, 128);
		if (!stack) return OUT_OF_MEMORY~;
		defer catch stack_destroy(stack);

		ParseCtx ctx = {
			.mem      = tmem,
			.stack    = stack,
			.start    = regex_nt.ptr,
			.cflags   = cflags,
			.max_backref = -1, // sentinel: -1 = no backrefs found; used by have_backrefs check
		};

		tre_parse(&ctx)!;

		// Tagging Pass 1: Count
		tre_add_tags(tmem, stack, ctx.node, null)!;

		// Allocate TNFA and submatch data
		Tnfa* tnfa = alloc::calloc(allocator, Tnfa::size);
		if (!tnfa) return OUT_OF_MEMORY~;
		tnfa.allocator = allocator;
		tnfa.num_submatches = ctx.submatch_id; // subid is total groups including group 0

		// ON FAILURE CLEANUP If compilation fails
		// If successful, we set success = true at the end to skip cleanup.
		bool success = false;
		defer {
			if (!success) {
				Regex temp_preg = { .__tnfa = tnfa };
				re_free(&temp_preg);
			}
		}

		tnfa.submatch_data = alloc::calloc(allocator, tnfa.num_submatches * SubmatchData::size);
		if (!tnfa.submatch_data) return OUT_OF_MEMORY~;

		tnfa.num_tags = ctx.node.num_tags;
		tnfa.minimal_tags = alloc::calloc(allocator, (sz)(tnfa.num_tags * 2 + 1) * int::size);
		if (!tnfa.minimal_tags) return OUT_OF_MEMORY~;

		if (tnfa.num_tags > 0)
		{
			tnfa.tag_directions = alloc::calloc(allocator, tnfa.num_tags * int::size);
			if (!tnfa.tag_directions) return OUT_OF_MEMORY~;
			tnfa.tag_directions[0 : tnfa.num_tags] = TRE_TAG_MAXIMIZE;
		}
		tnfa.end_tag = tnfa.num_tags;

		// Tagging Pass 2: Set submatch tags and directions
		tre_add_tags(tmem, stack, ctx.node, tnfa)!;

		// Expand iteration/quantifier nodes.
		tre_expand_ast(tmem, stack, ctx.node, &ctx.position, tnfa.tag_directions)!;

		// Add dummy NUL literal for the final state (as in Musl regcomp.c:2804)
		Literal* empty_lit = alloc::malloc(tmem, Literal::size);
		if (!empty_lit) return OUT_OF_MEMORY~;
		*empty_lit = { .code_min = 0, .code_max = 0, .position = ctx.position++ };

		AstNode* final_node = ast_new_node(tmem, LITERAL, empty_lit)!;
		ctx.node = ast_new_catenation(tmem, ctx.node, final_node)!;

		// Compute NFL (nullable, firstpos, lastpos)
		tre_compute_nfl(tmem, stack, ctx.node)!;

		// Build transition table
		int num_states = ctx.position;
		int* counts = alloc::calloc(tmem, num_states * int::size);
		int* offs = alloc::calloc(tmem, num_states * int::size);

		tre_ast_to_tnfa(tmem, ctx.node, null, counts, offs)!;

		int total_trans = 0;
		for (int i = 0; i < num_states; i++)
		{
			offs[i] = total_trans;
			total_trans += counts[i] + 1;
			counts[i] = 0;  // reset for second pass
		}

		tnfa.transitions = alloc::calloc(allocator, total_trans * TnfaTransition::size);
		if (!tnfa.transitions) return OUT_OF_MEMORY~;
		tre_ast_to_tnfa(allocator, ctx.node, tnfa.transitions, counts, offs)!;

		tnfa.num_transitions = (uint)total_trans;
		tnfa.num_states = num_states;
		tnfa.cflags = cflags;
		tnfa.have_backrefs = ctx.max_backref >= 0;

		if (ctx.node.firstpos)
		{
			int i = 0;
			for (PosAndTags* p = ctx.node.firstpos; p.position >= 0; p++) i++;

			tnfa.initial = alloc::calloc(allocator, (sz)(i + 1) * TnfaTransition::size);
			if (!tnfa.initial) return OUT_OF_MEMORY~;
			i = 0;
			for (PosAndTags* p = ctx.node.firstpos; p.position >= 0; p++)
			{
				tnfa.initial[i].state = &tnfa.transitions[offs[p.position]];
				tnfa.initial[i].state_id = p.position;
				tnfa.initial[i].assertions = p.assertions;
				if (p.tags)
				{
					int j = 0;
					for (j = 0; p.tags[j] >= 0; j++);
					tnfa.initial[i].tags = alloc::malloc(allocator, (sz)(j + 1) * int::size);
					if (!tnfa.initial[i].tags) return OUT_OF_MEMORY~;
					mem::copy(tnfa.initial[i].tags, p.tags, (sz)(j + 1) * int::size);
				}
				i++;
			}
			tnfa.initial[i].state = null;
		}

		if (ctx.node.lastpos && ctx.node.lastpos[0].position >= 0)
		{
			tnfa.final = &tnfa.transitions[offs[ctx.node.lastpos[0].position]];
		}

		preg.re_nsub = ctx.submatch_id - 1;
		preg.__tnfa = tnfa;

		success = true; // Safely keeps the compiled TNFA alive on exit
		stack_destroy(stack);
	};
}


fn bool check_assertions(int assertions, CompileFlags cflags, int prev_c, int next_c, sz pos, bool notbol, bool noteol) @local
{
	switch
	{
		case (assertions & ASSERT_AT_BOL)    != 0 && (pos > 0 || notbol) && (prev_c != '\n' || !(cflags & NEWLINE)):
		case (assertions & ASSERT_AT_EOL)    != 0 && (next_c != 0 || noteol) && (next_c != '\n' || !(cflags & NEWLINE)):
		case (assertions & ASSERT_AT_BOW)    != 0 && (is_word_char(prev_c) || !is_word_char(next_c)):
		case (assertions & ASSERT_AT_EOW)    != 0 && (!is_word_char(prev_c) || is_word_char(next_c)):
		case (assertions & ASSERT_AT_WB)     != 0 && (is_word_char(prev_c) == is_word_char(next_c)):
		case (assertions & ASSERT_AT_WB_NEG) != 0 && (is_word_char(prev_c) != is_word_char(next_c)):
			return true;
	}
	return false;
}

fn bool check_char_classes(TnfaTransition* trans, Tnfa* tnfa, int prev_c) @local
{
	bool icase = (bool)(tnfa.cflags & ICASE);
	if ((trans.assertions & ASSERT_CHAR_CLASS) != 0)
	{
		int class = trans.u_class_or_backref;
		if (!icase)
		{
			if (!tre_isctype(prev_c, class)) return true;
		}
		else
		{
			if (!tre_isctype(ascii::@to_lower((char)prev_c), class) &&
				!tre_isctype(ascii::@to_upper((char)prev_c), class)) return true;
		}
	}
	if ((trans.assertions & ASSERT_CHAR_CLASS_NEG) != 0)
	{
		if (trans.neg_classes != null)
		{
			if (tre_neg_char_classes_match(trans.neg_classes, prev_c, icase)) return true;
		}
	}
	return false;
}

fn int decode_utf8(char* s, int* len) @private
{
	if (*s == 0)
	{
		*len = 0;
		return 0;
	}
	sz width = 4;

	Char32? cp_opt = conv::utf8_to_char32(s, &width);

	if (catch cp_opt)
	{
		// Fallback for invalid UTF-8 (e.g. raw Latin-1 bytes in POSIX tests or binary payloads)
		// to match as raw bytes.
		*len = 1;
		return *s & 0xFF;
	}

	*len = (int)width;
	return (int)cp_opt;
}

fn void tre_fill_pmatch(sz nmatch, RegMatch* pmatch, CompileFlags cflags, Tnfa* tnfa, TagId* tags, RegOff match_eo) @local
{
	sz limit = 0;
	if (match_eo >= 0 && !(cflags & NOSUB))
	{
		limit = (tnfa.num_submatches < nmatch) ? tnfa.num_submatches : nmatch;
		SubmatchData* submatch_data = tnfa.submatch_data;

		foreach (idx, data : submatch_data[0 : limit])
		{
			if (data.so_tag == tnfa.end_tag)
			{
				pmatch[idx].rm_so = match_eo;
			}
			else
			{
				pmatch[idx].rm_so = (data.so_tag >= 0 && tags) ? tags[data.so_tag] : -1;
			}

			if (data.eo_tag == tnfa.end_tag)
			{
				pmatch[idx].rm_eo = match_eo;
			}
			else
			{
				pmatch[idx].rm_eo = (data.eo_tag >= 0 && tags) ? tags[data.eo_tag] : -1;
			}

			if (pmatch[idx].rm_so == -1 || pmatch[idx].rm_eo == -1)
			{
				pmatch[idx].rm_so = -1;
				pmatch[idx].rm_eo = -1;
			}
		}

		foreach (idx, data : submatch_data[0 : limit])
		{
			int* parents = data.parents;
			if (parents != null)
			{
				for (int j = 0; parents[j] >= 0; j++)
				{
					if (pmatch[idx].rm_so < pmatch[parents[j]].rm_so ||
						pmatch[idx].rm_eo > pmatch[parents[j]].rm_eo)
					{
						pmatch[idx].rm_so = -1;
						pmatch[idx].rm_eo = -1;
					}
				}
			}
		}
	}

	if (limit < nmatch)
	{
		foreach (&match : pmatch[limit : nmatch - limit])
		{
			*match = { .rm_so = -1, .rm_eo = -1 };
		}
	}
}

fn void? bt_stack_push(TreBacktrack** pstack, Tnfa* tnfa, RegOff pos, char* str_byte, TnfaTransition* trans, int next_c, TagId* tags) @local @inline
{
	TreBacktrack* stack = *pstack;
	if (!stack.next)
	{
		TreBacktrack* s_node = alloc::calloc(tmem, TreBacktrack::size);
		if (!s_node) return OUT_OF_MEMORY~;
		s_node.prev = stack;
		s_node.next = null;
		s_node.item.tags = alloc::malloc(tmem, tnfa.num_tags * TagId::size);
		if (!s_node.item.tags) return OUT_OF_MEMORY~;
		stack.next = s_node;
		*pstack = s_node;
	}
	else
	{
		*pstack = stack.next;
	}

	TreBacktrack* active = *pstack;
	active.item.pos = pos;
	active.item.str_byte = str_byte;
	active.item.state = trans.state;
	active.item.state_id = trans.state_id;
	active.item.next_c = next_c;

	for (int i = 0; i < tnfa.num_tags; i++)
	{
		active.item.tags[i] = tags[i];
	}

	TagId* tmp = trans.tags;
	if (tmp)
	{
		while (*tmp >= 0)
		{
			active.item.tags[*tmp] = (TagId)pos;
			tmp++;
		}
	}
}

fn void get_next_wchar(int* prev_c, int* next_c, RegOff* pos, RegOff* pos_add_next, char** str_byte) @local @inline
{
	*prev_c = *next_c;
	*pos += *pos_add_next;
	int len = 0;
	*next_c = decode_utf8(*str_byte, &len);
	if (len <= 0) { *pos_add_next = 1; }
	else { *pos_add_next = len; }
	*str_byte += *pos_add_next;
}

fn void? tre_tnfa_run_backtrack(Tnfa* tnfa, String string, TagId* match_tags, ExecFlags eflags, RegOff* match_end_ofs) @local
{
	int prev_c = 0;
	int next_c = 0;
	char* str_byte = string.ptr;
	RegOff pos = 0;
	RegOff pos_add_next = 1;


	int next_c_start = 0;
	char* str_byte_start = null;
	RegOff pos_start = -1;

	RegOff match_eo = -1;
	TagId* tags = null;
	TnfaTransition* state = null;
	int* states_seen = null;

	TreBacktrack* stack = alloc::calloc(tmem, TreBacktrack::size);
	if (!stack) return OUT_OF_MEMORY~;
	stack.prev = null;
	stack.next = null;

	if (tnfa.num_tags > 0)
	{
		tags = mem::malloc(tnfa.num_tags * TagId::size);
		if (!tags) return OUT_OF_MEMORY~;
	}
	defer if (tags) mem::free(tags);

	RegMatch* pmatch = null;
	if (tnfa.num_submatches > 0)
	{
		pmatch = mem::malloc(tnfa.num_submatches * RegMatch::size);
		if (!pmatch) return OUT_OF_MEMORY~;
	}
	defer if (pmatch) mem::free(pmatch);

	if (tnfa.num_states > 0)
	{
		states_seen = mem::malloc(tnfa.num_states * int::size);
		if (!states_seen) return OUT_OF_MEMORY~;
	}
	defer if (states_seen) mem::free(states_seen);

	const int M_PROCESS = 0;
	const int M_BACKTRACK = 1;
	const int M_RETRY = 2;
	const int M_EXIT = 3;

	int mstate = M_RETRY;

	while (mstate != M_EXIT)
	{
		switch (mstate)
		{
			case M_RETRY:
			{
				for (int i = 0; i < tnfa.num_tags; i++)
				{
					tags[i] = -1;
					if (match_tags) match_tags[i] = -1;
				}
				for (int i = 0; i < tnfa.num_states; i++)
				{
					states_seen[i] = 0;
				}
				state = null;
				pos = pos_start;

				// Read next char and advance
				get_next_wchar(&prev_c, &next_c, &pos, &pos_add_next, &str_byte);

				pos_start = pos;
				next_c_start = next_c;
				str_byte_start = str_byte;

				TagId* next_tags = null;
				for (TnfaTransition* trans_i = tnfa.initial; trans_i.state; trans_i++)
				{
					if (trans_i.assertions != 0 &&
						check_assertions(trans_i.assertions, tnfa.cflags, prev_c, next_c, pos, (bool)(eflags & NOTBOL), (bool)(eflags & NOTEOL)))
					{
						continue;
					}
					if (state == null)
					{
						state = trans_i.state;
						next_tags = trans_i.tags;
					}
					else
					{
						bt_stack_push(&stack, tnfa, pos, str_byte, trans_i, next_c, tags)!;
					}
				}

				if (next_tags)
				{
					for (; *next_tags >= 0; next_tags++)
					{
						tags[*next_tags] = (TagId)pos;
					}
				}

				if (state == null) { mstate = M_BACKTRACK; }
				else { mstate = M_PROCESS; }
			}

			case M_PROCESS:
			{
				if (state == tnfa.final)
				{
					if (match_eo < pos || (match_eo == pos && match_tags && tre_tag_order(tnfa.num_tags, tnfa.tag_directions, tags, match_tags) != 0))
					{
						match_eo = pos;
						if (match_tags)
						{
							for (int i = 0; i < tnfa.num_tags; i++)
							{
								match_tags[i] = tags[i];
							}
						}
					}
					mstate = M_BACKTRACK;
					break;
				}

				int empty_br_match = 0;
				TnfaTransition* trans_i = state;
				if (trans_i.state && (trans_i.assertions & ASSERT_BACKREF) != 0)
				{
					RegOff so, eo;
					int bt = trans_i.u_class_or_backref;
					RegOff bt_len;
					int result;

					tre_fill_pmatch((sz)(bt + 1), pmatch, tnfa.cflags & ~NOSUB, tnfa, tags, pos);
					so = pmatch[bt].rm_so;
					eo = pmatch[bt].rm_eo;
					bt_len = eo - so;

					if (so >= 0 && eo >= 0)
					{
						result = mem::equals(string.ptr + so, str_byte - 1, bt_len) ? 0 : 1;
					}
					else
					{
						result = -1;
					}

					if (result == 0)
					{
						if (bt_len == 0) empty_br_match = 1;
						if (empty_br_match != 0 && states_seen[trans_i.state_id] != 0)
						{
							mstate = M_BACKTRACK;
							break;
						}
						states_seen[trans_i.state_id] = empty_br_match;
						str_byte += (sz)(bt_len - 1);
						pos += bt_len - 1;

						// Read next char and advance
						get_next_wchar(&prev_c, &next_c, &pos, &pos_add_next, &str_byte);
					}
					else
					{
						mstate = M_BACKTRACK;
						break;
					}
				}
				else
				{
					if (next_c == 0)
					{
						mstate = M_BACKTRACK;
						break;
					}
					// Read next char and advance
					get_next_wchar(&prev_c, &next_c, &pos, &pos_add_next, &str_byte);
				}

				TnfaTransition* next_state = null;
				int* next_tags = null;
				for (trans_i = state; trans_i.state; trans_i++)
				{
					if (prev_c >= trans_i.code_min && prev_c <= trans_i.code_max)
					{
						if (trans_i.assertions != 0 &&
							(check_assertions(trans_i.assertions, tnfa.cflags, prev_c, next_c, (sz)pos, (bool)(eflags & NOTBOL), (bool)(eflags & NOTEOL)) || check_char_classes(trans_i, tnfa, prev_c)))
						{
							continue;
						}

						if (next_state == null)
						{
							next_state = trans_i.state;
							next_tags = trans_i.tags;
						}
						else
						{
							bt_stack_push(&stack, tnfa, pos, str_byte, trans_i, next_c, tags)!;
						}
					}
				}

				if (next_state != null)
				{
					state = next_state;
					if (next_tags)
					{
						while (*next_tags >= 0)
						{
							tags[*next_tags] = (TagId)pos;
							next_tags++;
						}
					}
				}
				else
				{
					mstate = M_BACKTRACK;
				}
			}

			case M_BACKTRACK:
			{
				if (stack.prev)
				{
					if ((stack.item.state.assertions & ASSERT_BACKREF) != 0)
					{
						states_seen[stack.item.state_id] = 0;
					}
					// BT_STACK_POP inline
					pos = stack.item.pos;
					str_byte = stack.item.str_byte;
					state = stack.item.state;
					next_c = stack.item.next_c;
					for (int i = 0; i < tnfa.num_tags; i++)
					{
						tags[i] = stack.item.tags[i];
					}
					stack = stack.prev;
					mstate = M_PROCESS;
				}
				else if (match_eo < 0)
				{
					if (next_c == 0) { mstate = M_EXIT; }
					else
					{
						str_byte = str_byte_start;
						next_c = next_c_start;
						mstate = M_RETRY;
					}
				}
				else
				{
					mstate = M_EXIT;
				}
			}

			case M_EXIT:
				break;
		}
	}

	*match_end_ofs = match_eo;
	if (match_eo < 0) return NOMATCH~;
}

fn void? tre_tnfa_run_parallel(Tnfa* tnfa, String string, TagId* match_tags, ExecFlags eflags, RegOff* match_end_ofs) @local
{
	int prev_c = 0;
	int next_c = 0;
	sz pos = 0;
	bool notbol = (bool)(eflags & NOTBOL);
	bool noteol = (bool)(eflags & NOTEOL);
	int num_tags = (match_tags != null) ? tnfa.num_tags : 0;
	int num_states = tnfa.num_states;

	// Allocate all temporary tables cleanly on tmem
	TagId* tmp_tags = null;
	if (num_tags > 0)
	{
		tmp_tags = alloc::calloc(tmem, (sz)num_tags * TagId::size);
		if (!tmp_tags) return OUT_OF_MEMORY~;
	}

	TreTnfaReach* reach_next = alloc::calloc(tmem, (sz)(num_states + 1) * TreTnfaReach::size);
	TreTnfaReach* reach = alloc::calloc(tmem, (sz)(num_states + 1) * TreTnfaReach::size);
	TreReachPos* reach_pos = alloc::malloc(tmem, (sz)num_states * TreReachPos::size);

	if (!reach_next || !reach || !reach_pos) return OUT_OF_MEMORY~;

	// Allocate a single flat block for all tags to maximize spatial locality
	if (num_tags > 0)
	{
		TagId* tags_block = alloc::calloc(tmem, (sz)(num_states * 2 * num_tags) * TagId::size);
		if (!tags_block) return OUT_OF_MEMORY~;
		for (int i = 0; i < num_states; i++)
		{
			reach[i].tags = &tags_block[i * num_tags];
			reach_next[i].tags = &tags_block[(num_states + i) * num_tags];
			reach_pos[i].pos = (sz)-1;
		}
	}
	else
	{
		for (int i = 0; i < num_states; i++)
		{
			reach_pos[i].pos = (sz)-1;
		}
	}

	RegOff match_eo = -1;

	// UTF-8 iteration
	StringIterator it;
	it.utf8 = string;
	it.reset();
	next_c = (int)(it.next() ?? 0);


	TreTnfaReach* reach_next_i = reach_next;
	while (true)
	{
		// Add initial states
		if (match_eo == -1)
		{
			TnfaTransition* trans_i = tnfa.initial;
			while (trans_i && trans_i.state)
			{
				if (reach_pos[trans_i.state_id].pos < pos || reach_pos[trans_i.state_id].pos == (sz)-1)
				{
					if (trans_i.assertions != 0 && check_assertions(trans_i.assertions, tnfa.cflags, prev_c, next_c, pos, notbol, noteol))
					{
						trans_i++;
						continue;
					}

					reach_next_i.state = trans_i.state;
					reach_next_i.tags[0 : num_tags] = -1;

					int* tag_i = trans_i.tags;
					if (tag_i)
					{
						for (int j = 0; tag_i[j] >= 0; j++)
						{
							if (tag_i[j] < num_tags) reach_next_i.tags[tag_i[j]] = (TagId)pos;
						}
					}

					if (reach_next_i.state == tnfa.final && (match_eo == -1 || (num_tags > 0 && reach_next_i.tags[0] <= match_tags[0])))
					{
						match_eo = (RegOff)pos;
						if (num_tags > 0) mem::copy(match_tags, reach_next_i.tags, (usz)num_tags * int::size);
					}

					reach_pos[trans_i.state_id].pos = pos;
					reach_pos[trans_i.state_id].tags = &reach_next_i.tags;
					reach_next_i++;
				}
				trans_i++;
			}
			reach_next_i.state = null;
		}
		else
		{
			if (num_tags == 0 || reach_next_i == reach_next) break;
		}

		if (next_c == 0) break;

		// Move window forward by decoding next character once
		prev_c = next_c;
		next_c = (int)(it.next() ?? 0);

		pos++;

		// Swap reach
		TreTnfaReach* tmp_reach = reach;
		reach = reach_next;
		reach_next = tmp_reach;
		reach_next_i = reach_next;

		// Reset reach_next for the next iteration
		for (int i = 0; i < num_states; i++) reach_next[i].state = null;

		// Transitions
		TreTnfaReach* reach_i = reach;
		reach_next_i = reach_next;

		while (reach_i.state)
		{
			TnfaTransition* trans_i = reach_i.state;
			while (trans_i.state)
			{
				if (prev_c >= trans_i.code_min && prev_c <= trans_i.code_max)
				{
					if (trans_i.assertions != 0 && (check_assertions(trans_i.assertions, tnfa.cflags, prev_c, next_c, pos, notbol, noteol) || check_char_classes(trans_i, tnfa, prev_c)))
					{
						trans_i++;
						continue;
					}

					if (num_tags > 0) mem::copy(tmp_tags, reach_i.tags, (usz)num_tags * int::size);
					TagId* tags = trans_i.tags;
					if (tags)
					{
						for (int j = 0; tags[j] >= 0; j++)
						{
							if (tags[j] < num_tags) tmp_tags[tags[j]] = (TagId)pos;
						}
					}

					if (reach_pos[trans_i.state_id].pos < pos || reach_pos[trans_i.state_id].pos == (sz)-1)
					{
						reach_next_i.state = trans_i.state;
						TagId* swap_tags = reach_next_i.tags;
						reach_next_i.tags = tmp_tags;
						tmp_tags = swap_tags;

						reach_pos[trans_i.state_id].pos = pos;
						reach_pos[trans_i.state_id].tags = &reach_next_i.tags;

						if (reach_next_i.state == tnfa.final && (match_eo == -1 || (num_tags > 0 && reach_next_i.tags[0] <= match_tags[0])))
						{
							match_eo = (RegOff)pos;
							if (num_tags > 0) mem::copy(match_tags, reach_next_i.tags, (usz)num_tags * int::size);
						}
						reach_next_i++;
					}
					else
					{
						if (tre_tag_order(num_tags, tnfa.tag_directions, tmp_tags, *reach_pos[trans_i.state_id].tags))
						{
							TagId* tmp_iptr = *reach_pos[trans_i.state_id].tags;
							*reach_pos[trans_i.state_id].tags = tmp_tags;
							if (trans_i.state == tnfa.final)
							{
								match_eo = (RegOff)pos;
								if (num_tags > 0) mem::copy(match_tags, *reach_pos[trans_i.state_id].tags, (usz)num_tags * int::size);
							}
							tmp_tags = tmp_iptr;
						}
					}
				}
				trans_i++;
			}
			reach_i++;
		}
	}

	*match_end_ofs = match_eo;
	if (match_eo < 0) return NOMATCH~;
}

<*
 @param [&in] preg
 @require nmatch >= 0
*>
fn void? re_exec(Regex* preg, String string, sz nmatch, RegMatch* pmatch, ExecFlags eflags) @private
{
	@pool()
	{
		Tnfa* tnfa = preg.__tnfa;
		if (!tnfa) return BADPAT~;

		// Ensure target string is null-terminated for backtrack and parallel execution
		String string_nt = string;
		if (string.len == 0 || string.ptr[string.len] != '\0')
		{
			char* buf = alloc::malloc(tmem, string.len + 1);
			if (!buf) return OUT_OF_MEMORY~;
			mem::copy(buf, string.ptr, string.len);
			buf[string.len] = '\0';
			string_nt = (String)buf[0 : string.len];
		}

		TagId* match_tags = null;
		if (tnfa.num_tags > 0)
		{
			match_tags = mem::talloc_array(TagId, tnfa.num_tags);
			match_tags[0 : tnfa.num_tags] = -1;
		}

		RegOff match_end_ofs = -1;

		if (tnfa.have_backrefs)
		{
			tre_tnfa_run_backtrack(tnfa, string_nt, match_tags, eflags, &match_end_ofs)!;
		}
		else
		{
			tre_tnfa_run_parallel(tnfa, string_nt, match_tags, eflags, &match_end_ofs)!;
		}

		if (pmatch && nmatch > 0)
		{
			tre_fill_pmatch(nmatch, pmatch, tnfa.cflags, tnfa, match_tags, match_end_ofs);
		}
	};
}

fn void re_free(Regex* preg) @private
{
	Tnfa* tnfa = preg.__tnfa;
	if (!tnfa) return;
	Allocator allocator = tnfa.allocator;

	if (tnfa.transitions)
	{
		for (uint i = 0; i < tnfa.num_transitions; i++)
		{
			if (tnfa.transitions[i].state)
			{
				if (tnfa.transitions[i].tags)       alloc::free(allocator, tnfa.transitions[i].tags);
				if (tnfa.transitions[i].neg_classes) alloc::free(allocator, tnfa.transitions[i].neg_classes);
			}
		}
		alloc::free(allocator, tnfa.transitions);
	}

	if (tnfa.initial)
	{
		for (sz i = 0; tnfa.initial[i].state; i++)
		{
			if (tnfa.initial[i].tags) alloc::free(allocator, tnfa.initial[i].tags);
		}
		alloc::free(allocator, tnfa.initial);
	}

	if (tnfa.submatch_data)
	{
		for (sz i = 0; i < tnfa.num_submatches; i++)
		{
			if (tnfa.submatch_data[i].parents) alloc::free(allocator, tnfa.submatch_data[i].parents);
		}
		alloc::free(allocator, tnfa.submatch_data);
	}

	if (tnfa.tag_directions) alloc::free(allocator, tnfa.tag_directions);
	if (tnfa.minimal_tags)   alloc::free(allocator, tnfa.minimal_tags);
	if (tnfa.firstpos_chars) alloc::free(allocator, tnfa.firstpos_chars);
	alloc::free(allocator, tnfa);
	preg.__tnfa = null;
}