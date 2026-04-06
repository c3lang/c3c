module std::regex;
import std::core::string::conv;
import std::core::string::iterator;
import std::core::ascii;

// ============================================================
// Parser helpers
// ============================================================

macro bool @at_end(ParseCtx* ctx) => ctx.pos >= ctx.pattern.len;

macro char @peek(ParseCtx* ctx)
{
	if (ctx.pos >= ctx.pattern.len) return 0;
	return ctx.pattern[ctx.pos];
}

macro char @peek2(ParseCtx* ctx)
{
	if (ctx.pos + 1 >= ctx.pattern.len) return 0;
	return ctx.pattern[ctx.pos + 1];
}

macro char @consume(ParseCtx* ctx)
{
	char c = ctx.pattern[ctx.pos];
	ctx.pos++;
	return c;
}

macro bool @try_consume(ParseCtx* ctx, char c)
{
	if (ctx.pos < ctx.pattern.len && ctx.pattern[ctx.pos] == c)
	{
		ctx.pos++;
		return true;
	}
	return false;
}

fn void skip_freespacing(ParseCtx* ctx) @local @inline
{
	if ((ctx.flags & FLAG_EXTENDED) == FLAG_NONE) return;
	while (!@at_end(ctx))
	{
		char c = ctx.pattern[ctx.pos];
		if (c == '#')
		{
			// Skip to end of line
			while (!@at_end(ctx) && ctx.pattern[ctx.pos] != '\n') ctx.pos++;
		}
		else if (c == ' ' || c == '\t' || c == '\r' || c == '\n')
		{
			ctx.pos++;
		}
		else
		{
			break;
		}
	}
}

fn AstNode* ast_new(AstKind kind)
{
	AstNode* n = alloc::new(tmem, AstNode);
	n.kind = kind;
	n.group_id = -1;
	return n;
}

fn void ctx_push_named_group(ParseCtx* ctx, NamedGroupEntry entry)
{
	if (ctx.named_groups_count >= ctx.named_groups_cap)
	{
		usz new_cap = ctx.named_groups_cap ? ctx.named_groups_cap * 2 : 8;
		NamedGroupEntry* buf = alloc::new_array(tmem, NamedGroupEntry, new_cap);
		if (ctx.named_groups_buf)
		{
			mem::copy(buf, ctx.named_groups_buf, ctx.named_groups_count * NamedGroupEntry.sizeof);
		}
		ctx.named_groups_buf = buf;
		ctx.named_groups_cap = new_cap;
	}
	ctx.named_groups_buf[ctx.named_groups_count++] = entry;
}

// ============================================================
// Parse hex digits for \uXXXX / \UXXXXXXXX
// ============================================================

// Read the next full Unicode codepoint from ctx.pattern at ctx.pos (UTF-8 aware).
fn Char32? parse_codepoint(ParseCtx* ctx) @local @inline
{
	if (ctx.pos >= ctx.pattern.len) return INVALID_PATTERN~;
	StringIterator it = { .utf8 = ctx.pattern, .current = ctx.pos };
	Char32 cp = it.next()!;
	ctx.pos = it.current;
	return cp;
}

// Parse \x{HH...} braced hex escape (1+ hex digits). Caller must have already consumed '{'.
fn Char32? parse_hex_escape_braced(ParseCtx* ctx) @local
{
	if (@at_end(ctx) || @peek(ctx) == '}') return INVALID_PATTERN~;
	Char32 val = 0;
	bool has_digits = false;
	while (!@at_end(ctx) && @peek(ctx) != '}')
	{
		char hc = @consume(ctx);
		if (!ascii::is_xdigit(hc)) return INVALID_PATTERN~;
		val = val * 16 + (Char32)hc.from_hex();
		has_digits = true;
	}
	if (!has_digits || !@try_consume(ctx, '}')) return INVALID_PATTERN~;
	if (val > 0x10FFFF) return INVALID_PATTERN~;
	return val;
}

fn Char32? parse_hex_escape(ParseCtx* ctx, int digits)
{
	uint val = 0;
	for (int i = 0; i < digits; i++)
	{
		if (@at_end(ctx)) return INVALID_PATTERN~;
		char c = @consume(ctx);
		if (!ascii::is_xdigit(c)) return INVALID_PATTERN~;
		val = val * 16 + (uint)c.from_hex();
	}
	return (Char32)val;
}

fn Char32? parse_classchar(ParseCtx* ctx)
{
	if (@at_end(ctx)) return INVALID_PATTERN~;
	char c = ctx.pattern[ctx.pos];
	if (c != '\\')
	{
		return parse_codepoint(ctx)!;
	}
	ctx.pos++;
	if (@at_end(ctx)) return INVALID_PATTERN~;
	char esc = @consume(ctx);
	switch (esc)
	{
		case 'n': return '\n';
		case 'r': return '\r';
		case 't': return '\t';
		case 'f': return '\f';
		case 'v': return '\v';
		case 'a': return '\a';
		case 'e': return 0x1B;
		case '\\': return '\\';
		case ']': return ']';
		case '-': return '-';
		case '^': return '^';
		case 'u': return parse_hex_escape(ctx, 4);
		case 'U': return parse_hex_escape(ctx, 8);
		case 'x':
			if (@peek(ctx) == '{') { ctx.pos++; return parse_hex_escape_braced(ctx); }
			return parse_hex_escape(ctx, 2);
		case 'c':
			if (@at_end(ctx)) return INVALID_PATTERN~;
			char ctrl_cc = @consume(ctx);
			if (ctrl_cc >= '@' && ctrl_cc <= '_') return (Char32)(ctrl_cc ^ 64);
			if (ctrl_cc >= 'a' && ctrl_cc <= 'z') return (Char32)((ctrl_cc - 0x20) ^ 64);
			return INVALID_PATTERN~;
		case '0': return 0;
		default:  return (Char32)esc;
	}
}

fn void charclass_add_range(CharClass* cls, TempRangeList* unicode, Char32 lo, Char32 hi)
{
	if (lo < 128)
	{
		Char32 ascii_hi = hi < 127 ? hi : 127;
		for (Char32 cp = lo; cp <= ascii_hi; cp++) cls.ascii_bits |= (AsciiCharset)1 << (char)cp;
		if (hi < 128) return;
		lo = 128;
	}
	unicode.push({ .lo = lo, .hi = hi });
}

fn void charclass_merge_set(CharClass* cls, AsciiCharset set)
{
	cls.ascii_bits |= set;
}

fn void charclass_merge_property(CharClass* cls, UnicodePropMask prop)
{
	cls.props |= prop;
}

fn void charclass_merge_complement_property(CharClass* cls, UnicodePropMask prop)
{
	cls.complement_props |= prop;
}

fn UnicodePropMask? parse_unicode_property_name(ParseCtx* ctx)
{
	if (!@try_consume(ctx, '{')) return INVALID_PATTERN~;
	usz name_start = ctx.pos;
	while (!@at_end(ctx) && @peek(ctx) != '}') ctx.pos++;
	if (!@try_consume(ctx, '}')) return INVALID_PATTERN~;
	String name = ctx.pattern[name_start : ctx.pos - name_start - 1];
	switch (name)
	{
		case "ASCII": return PROP_ASCII;
		case "White_Space":
		case "space":
		case "Space":
		case "Xps": return PROP_WHITE_SPACE;
		case "Horiz_Space":
		case "HSpace":
		case "hspace": return PROP_HSPACE;
		case "Vert_Space":
		case "VSpace":
		case "vspace": return PROP_VSPACE;
		case "Word":
		case "word":
		case "Xwd": return PROP_WORD;
		default: return INVALID_PATTERN~;
	}
}

fn CharClass? parse_property_charclass(ParseCtx* ctx, bool negated)
{
	UnicodePropMask prop = parse_unicode_property_name(ctx)!;
	CharClass cls = { .props = prop, .negated = negated };
	if (prop == PROP_ASCII) cls.ascii_bits = ~(AsciiCharset)0;
	return cls;
}

fn bool? parse_posix_bracket(ParseCtx* ctx, CharClass* cls)
{
	if (@peek(ctx) != ':') return false;
	usz saved = ctx.pos;
	ctx.pos++;
	usz name_start = ctx.pos;
	while (!@at_end(ctx) && @peek(ctx) != ':' && @peek(ctx) != ']') ctx.pos++;
	if (@at_end(ctx) || @peek(ctx) != ':') { ctx.pos = saved; return false; }
	String name = ctx.pattern[name_start : ctx.pos - name_start];
	ctx.pos++;
	if (@at_end(ctx) || @peek(ctx) != ']') { ctx.pos = saved; return false; }
	ctx.pos++;

	switch (name)
	{
		case "alpha":  charclass_merge_set(cls, ascii::ALPHA_SET);
		case "digit":  charclass_merge_set(cls, ascii::NUMBER_SET);
		case "lower":  charclass_merge_set(cls, ascii::ALPHA_LOWER_SET);
		case "upper":  charclass_merge_set(cls, ascii::ALPHA_UPPER_SET);
		case "space":  charclass_merge_set(cls, ascii::WHITESPACE_SET);
		case "alnum":  charclass_merge_set(cls, ascii::ALPHANUMERIC_SET);
		case "blank":  charclass_merge_set(cls, BLANK_SET);
		case "punct":  charclass_merge_set(cls, PUNCT_SET);
		case "cntrl":  charclass_merge_set(cls, CNTRL_SET);
		case "graph":  charclass_merge_set(cls, GRAPH_SET);
		case "print":  charclass_merge_set(cls, PRINT_SET);
		case "xdigit": charclass_merge_set(cls, XDIGIT_SET);
		case "word":   charclass_merge_set(cls, WORD_SET);
		default:       ctx.pos = saved; return false;
	}
	return true;
}

fn AstNode*? parse_charclass_body(ParseCtx* ctx)
{
	CharClass cls = {};
	TempRangeList unicode;
	unicode.tinit(4);
	defer unicode.free();

	bool negated = false;
	if (@try_consume(ctx, '^')) negated = true;

	bool first = true;
	while (true)
	{
		if (@at_end(ctx)) return INVALID_PATTERN~;
		char c = @peek(ctx);
		if (c == ']' && !first) break;
		first = false;

		if (c == '[')
		{
			ctx.pos++;
			bool? matched = parse_posix_bracket(ctx, &cls);
			if (catch matched) return INVALID_PATTERN~;
			if (matched) continue;
			charclass_add_range(&cls, &unicode, '[', '[');
			continue;
		}

		if (c == '\\' && ctx.pos + 1 < ctx.pattern.len)
		{
			char esc = ctx.pattern[ctx.pos + 1];
			switch (esc)
			{
				case 'd': charclass_merge_set(&cls, ascii::NUMBER_SET);         charclass_merge_property(&cls, PROP_DIGIT);       ctx.pos += 2; continue;
				case 'D': charclass_merge_complement_property(&cls, PROP_DIGIT);                                                       ctx.pos += 2; continue;
				case 'w': charclass_merge_set(&cls, WORD_SET);                  charclass_merge_property(&cls, PROP_WORD);        ctx.pos += 2; continue;
				case 'W': charclass_merge_complement_property(&cls, PROP_WORD);                                                        ctx.pos += 2; continue;
				case 's': charclass_merge_set(&cls, ascii::WHITESPACE_SET);     charclass_merge_property(&cls, PROP_WHITE_SPACE); ctx.pos += 2; continue;
				case 'S': charclass_merge_complement_property(&cls, PROP_WHITE_SPACE);                                                 ctx.pos += 2; continue;
				case 'h': charclass_merge_set(&cls, HSPACE_SET);                charclass_merge_property(&cls, PROP_HSPACE);      ctx.pos += 2; continue;
				case 'H': charclass_merge_complement_property(&cls, PROP_HSPACE);                                                     ctx.pos += 2; continue;
				case 'v': charclass_merge_set(&cls, VSPACE_SET);                charclass_merge_property(&cls, PROP_VSPACE);      ctx.pos += 2; continue;
				case 'V': charclass_merge_complement_property(&cls, PROP_VSPACE);                                                     ctx.pos += 2; continue;
				case 'p':
					ctx.pos += 2;
					CharClass pcls = parse_property_charclass(ctx, false)!;
					cls.ascii_bits |= pcls.ascii_bits;
					cls.props |= pcls.props;
					continue;
				case 'P':
					ctx.pos += 2;
					CharClass p_inv = parse_property_charclass(ctx, true)!;
					cls.complement_props |= p_inv.props;
					continue;
				default: break;
			}
		}

		Char32 lo = parse_classchar(ctx)!;
		if (@peek(ctx) == '-' && ctx.pos + 1 < ctx.pattern.len && ctx.pattern[ctx.pos + 1] != ']')
		{
			ctx.pos++;
			Char32 hi = parse_classchar(ctx)!;
			if (hi < lo) return INVALID_PATTERN~;
			charclass_add_range(&cls, &unicode, lo, hi);
		}
		else
		{
			charclass_add_range(&cls, &unicode, lo, lo);
		}
	}
	ctx.pos++;
	cls.negated = negated;

	// Copy unicode ranges into tmem (moved to range_pool during NFA build)
	UnicodeRange* tmp_ranges = null;
	usz tmp_count = unicode.size;
	if (tmp_count > 0)
	{
		tmp_ranges = alloc::new_array(tmem, UnicodeRange, tmp_count);
		mem::copy(tmp_ranges, unicode.entries, tmp_count * UnicodeRange.sizeof);
	}
	cls.ranges = tmp_ranges;
	cls.ranges_count = tmp_count;

	AstNode* n = ast_new(AST_CHARCLASS);
	n.char_class = cls;
	return n;
}

// ============================================================
// Recursive descent parser
// ============================================================

fn AstNode*? parse_expr(ParseCtx* ctx) => parse_alt(ctx);

fn AstNode*? parse_alt(ParseCtx* ctx)
{
	AstNode* left = parse_concat(ctx)!;
	skip_freespacing(ctx);
	while (@peek(ctx) == '|')
	{
		ctx.pos++;
		AstNode* right = parse_concat(ctx)!;
		AstNode* n = ast_new(AST_ALT);
		n.left  = left;
		n.right = right;
		left = n;
		skip_freespacing(ctx);
	}
	return left;
}

fn AstNode*? parse_concat(ParseCtx* ctx)
{
	AstNode* result = null;
	while (true)
	{
		skip_freespacing(ctx);
		char c = @peek(ctx);
		if (c == '|' || c == ')' || c == 0) break;
		AstNode* atom = parse_quantifier(ctx)!;
		if (result == null)
		{
			result = atom;
		}
		else
		{
			AstNode* n = ast_new(AST_CONCAT);
			n.left  = result;
			n.right = atom;
			result = n;
		}
	}
	if (result == null)
	{
		// Empty alternative — epsilon node
		result = ast_new(AST_CONCAT);
		result.left = null;
		result.right = null;
	}
	return result;
}

fn bool ast_can_consume(AstNode* node) @local
{
	if (node == null) return false;
	switch (node.kind)
	{
		case AST_LITERAL:
		case AST_CHARCLASS:
		case AST_LINEBREAK:
		case AST_NON_NEWLINE:
		case AST_BACKREF:
			return true;
		case AST_CONCAT:
		case AST_ALT:
			return ast_can_consume(node.left) || ast_can_consume(node.right);
		case AST_GROUP:
		case AST_NCGROUP:
		case AST_ATOMIC:
		case AST_BRANCH_RESET:
			return ast_can_consume(node.left);
		case AST_REPEAT:
			return node.repeat_max != 0 && ast_can_consume(node.left);
		case AST_STAR:
		case AST_PLUS:
		case AST_QUEST:
			return ast_can_consume(node.left);
		case AST_COND_GROUP:
			return ast_can_consume(node.left) || (node.right != null && ast_can_consume(node.right));
		case AST_ANCHOR_START:
		case AST_ANCHOR_END:
		case AST_ANCHOR_ABSOLUTE_START:
		case AST_ANCHOR_ABSOLUTE_END:
		case AST_ANCHOR_ABSOLUTE_END_NL:
		case AST_WORD_BOUND:
		case AST_WORD_NBOUND:
		case AST_LOOKAHEAD_POS:
		case AST_LOOKAHEAD_NEG:
		case AST_LOOKBEHIND_POS:
		case AST_LOOKBEHIND_NEG:
		case AST_KEEP:
		case AST_ANCHOR_GPOS:
			return false;
		default:
			return false;
	}
}

fn AstNode*? parse_quantifier(ParseCtx* ctx)
{
	AstNode* atom = parse_atom(ctx)!;
	skip_freespacing(ctx);
	char c = @peek(ctx);
	AstNode* q = null;
	switch (c)
	{
		case '*':
			if (!ast_can_consume(atom)) return INVALID_PATTERN~;
			ctx.pos++;
			q = ast_new(AST_STAR);
			q.left = atom;
		case '+':
			if (!ast_can_consume(atom)) return INVALID_PATTERN~;
			ctx.pos++;
			q = ast_new(AST_PLUS);
			q.left = atom;
		case '?':
			if (!ast_can_consume(atom)) return INVALID_PATTERN~;
			ctx.pos++;
			q = ast_new(AST_QUEST);
			q.left = atom;
		case '{':
			if (!ast_can_consume(atom)) return INVALID_PATTERN~;
			q = parse_repeat_quantifier(ctx, atom)!;
		default:
			return atom;
	}
	// Check for possessive + suffix (before lazy ?) — wraps the quantifier in an atomic group
	if (@peek(ctx) == '+')
	{
		ctx.pos++;
		AstNode* poss = ast_new(AST_ATOMIC);
		poss.left = q;
		return poss;
	}
	bool has_lazy_suffix = @try_consume(ctx, '?');
	bool ungreedy = (ctx.flags & FLAG_UNGREEDY) != FLAG_NONE;
	q.lazy = has_lazy_suffix ^ ungreedy;
	return q;
}

fn AstNode*? parse_repeat_quantifier(ParseCtx* ctx, AstNode* atom)
{
	usz saved = ctx.pos;
	ctx.pos++;

	int min_val = 0;
	bool has_digits = false;
	while (@peek(ctx) >= '0' && @peek(ctx) <= '9')
	{
		min_val = min_val * 10 + (@consume(ctx) - '0');
		has_digits = true;
		if (min_val > 1000) return COMPILE_ERROR~;
	}
	if (!has_digits) { ctx.pos = saved; return INVALID_PATTERN~; }

	int max_val;
	if (@try_consume(ctx, ','))
	{
		if (@peek(ctx) == '}')
		{
			max_val = -1;
		}
		else
		{
			max_val = 0;
			bool has_max = false;
			while (@peek(ctx) >= '0' && @peek(ctx) <= '9')
			{
				max_val = max_val * 10 + (@consume(ctx) - '0');
				has_max = true;
				if (max_val > 1000) return COMPILE_ERROR~;
			}
			if (!has_max) { ctx.pos = saved; return INVALID_PATTERN~; }
		}
	}
	else
	{
		max_val = min_val;
	}

	if (@peek(ctx) != '}') { ctx.pos = saved; return INVALID_PATTERN~; }
	ctx.pos++;
	if (max_val != -1 && max_val < min_val) return INVALID_PATTERN~;

	AstNode* n = ast_new(AST_REPEAT);
	n.left       = atom;
	n.repeat_min = min_val;
	n.repeat_max = max_val;
	return n;
}

fn AstNode*? parse_atom(ParseCtx* ctx)
{
	if (@at_end(ctx)) return INVALID_PATTERN~;
	char c = @consume(ctx);
	switch (c)
	{
		case '(':
			return parse_group(ctx);
		case '[':
			return parse_charclass_body(ctx);
		case '.':
			AstNode* dot = ast_new(AST_CHARCLASS);
			dot.char_class = CLASS_DOT;
			return dot;
		case '^':
			return ast_new(AST_ANCHOR_START);
		case '$':
			return ast_new(AST_ANCHOR_END);
		case '\\':
			return parse_escape_atom(ctx);
		default:
			// Back up and re-read as full UTF-8 codepoint (handles multi-byte sequences)
			ctx.pos--;
			AstNode* lit = ast_new(AST_LITERAL);
			lit.codepoint = parse_codepoint(ctx)!;
			return lit;
	}
}

fn AstNode*? parse_group(ParseCtx* ctx)
{
	char c = @peek(ctx);
	if (c == '?')
	{
		ctx.pos++;
		char kind = @peek(ctx);
		switch (kind)
		{
			case '#':
				// (?#comment) — skip to closing )
				ctx.pos++;
				while (!@at_end(ctx) && @peek(ctx) != ')') ctx.pos++;
				if (!@try_consume(ctx, ')')) return INVALID_PATTERN~;
				AstNode* comment_eps = ast_new(AST_NCGROUP);
				comment_eps.left = null;
				return comment_eps;
			case ':':
				ctx.pos++;
				AstNode* inner = parse_expr(ctx)!;
				if (!@try_consume(ctx, ')')) return INVALID_PATTERN~;
				AstNode* ncg = ast_new(AST_NCGROUP);
				ncg.left = inner;
				return ncg;
			case '=':
				ctx.pos++;
				AstNode* la_inner = parse_expr(ctx)!;
				if (!@try_consume(ctx, ')')) return INVALID_PATTERN~;
				AstNode* lap = ast_new(AST_LOOKAHEAD_POS);
				lap.left = la_inner;
				return lap;
			case '!':
				ctx.pos++;
				AstNode* lna_inner = parse_expr(ctx)!;
				if (!@try_consume(ctx, ')')) return INVALID_PATTERN~;
				AstNode* lan = ast_new(AST_LOOKAHEAD_NEG);
				lan.left = lna_inner;
				return lan;
			case '|':
				// (?|...) — branch reset group: each alternative reuses the same group IDs
				ctx.pos++;
				int breset_base = ctx.next_group_id;
				int breset_max = breset_base;
				AstNode* br_result = null;
				while (true)
				{
					ctx.next_group_id = breset_base;
					AstNode* br_branch = parse_concat(ctx)!;
					if (ctx.next_group_id > breset_max) breset_max = ctx.next_group_id;
					if (br_result == null) { br_result = br_branch; }
					else { AstNode* bra = ast_new(AST_ALT); bra.left = br_result; bra.right = br_branch; br_result = bra; }
					if (!@try_consume(ctx, '|')) break;
				}
				ctx.next_group_id = breset_max;
				if (!@try_consume(ctx, ')')) return INVALID_PATTERN~;
				AstNode* br_ncg = ast_new(AST_BRANCH_RESET); br_ncg.left = br_result; return br_ncg;
			case '(':
				// (?(N)yes|no) or (?(name)yes|no) — conditional pattern
				ctx.pos++;
				int cond_gid = -1;
				if (@peek(ctx) >= '1' && @peek(ctx) <= '9')
				{
					cond_gid = 0;
					while (@peek(ctx) >= '0' && @peek(ctx) <= '9') { cond_gid = cond_gid * 10 + (@consume(ctx) - '0'); }
				}
				else
				{
					usz cond_name_start = ctx.pos;
					while (!@at_end(ctx) && @peek(ctx) != ')') ctx.pos++;
					String cond_name = ctx.pattern[cond_name_start : ctx.pos - cond_name_start];
					if (cond_name.len == 0) return INVALID_PATTERN~;
					for (usz ci = 0; ci < ctx.named_groups_count; ci++)
					{
						if (ctx.named_groups_buf[ci].name == cond_name) { cond_gid = ctx.named_groups_buf[ci].group_id; break; }
					}
					if (cond_gid < 0) return INVALID_PATTERN~;
				}
				if (!@try_consume(ctx, ')')) return INVALID_PATTERN~;
				AstNode* cond_yes = parse_concat(ctx)!;
				AstNode* cond_no = null;
				if (@try_consume(ctx, '|')) cond_no = parse_concat(ctx)!;
				if (!@try_consume(ctx, ')')) return INVALID_PATTERN~;
				AstNode* cond_node = ast_new(AST_COND_GROUP);
				cond_node.group_id = cond_gid;
				cond_node.left = cond_yes;
				cond_node.right = cond_no;
				return cond_node;
			case '>':
				// (?>...) — atomic group
				ctx.pos++;
				AstNode* atm_inner = parse_expr(ctx)!;
				if (!@try_consume(ctx, ')')) return INVALID_PATTERN~;
				AstNode* atm = ast_new(AST_ATOMIC);
				atm.left = atm_inner;
				return atm;
			case '<':
				ctx.pos++;
				char next = @peek(ctx);
				if (next == '=')
				{
					ctx.pos++;
					AstNode* lb_inner = parse_expr(ctx)!;
					if (!@try_consume(ctx, ')')) return INVALID_PATTERN~;
					AstNode* lbp = ast_new(AST_LOOKBEHIND_POS);
					lbp.left = lb_inner;
					return lbp;
				}
				else if (next == '!')
				{
					ctx.pos++;
					AstNode* lbn_inner = parse_expr(ctx)!;
					if (!@try_consume(ctx, ')')) return INVALID_PATTERN~;
					AstNode* lbn = ast_new(AST_LOOKBEHIND_NEG);
					lbn.left = lbn_inner;
					return lbn;
				}
				else
				{
					return parse_named_group(ctx);
				}
			case 'i':
			case 'm':
			case 's':
			case 'U':
			case 'x':
			case '-':
				return parse_inline_flags(ctx);
			default:
				return INVALID_PATTERN~;
		}
	}

	int id = ++ctx.next_group_id;
	AstNode* inner = parse_expr(ctx)!;
	if (!@try_consume(ctx, ')')) return INVALID_PATTERN~;
	AstNode* grp = ast_new(AST_GROUP);
	grp.left = inner;
	grp.group_id = id;
	return grp;
}

fn AstNode*? parse_named_group(ParseCtx* ctx)
{
	usz name_start = ctx.pos;
	while (!@at_end(ctx) && @peek(ctx) != '>') ctx.pos++;
	if (@at_end(ctx)) return INVALID_PATTERN~;
	String name = ctx.pattern[name_start : ctx.pos - name_start];
	if (name.len == 0) return INVALID_PATTERN~;
	ctx.pos++;

	int id = ++ctx.next_group_id;
	AstNode* inner = parse_expr(ctx)!;
	if (!@try_consume(ctx, ')')) return INVALID_PATTERN~;
	AstNode* grp = ast_new(AST_GROUP);
	grp.left = inner;
	grp.group_id = id;
	grp.group_name = name;
	ctx_push_named_group(ctx, { .name = name, .group_id = id });
	return grp;
}

fn AstNode*? parse_inline_flags(ParseCtx* ctx)
{
	bool adding = true;
	RegexFlags new_flags = ctx.flags;
	while (!@at_end(ctx))
	{
		char c = @consume(ctx);
		switch (c)
		{
			case 'i': if (adding) { new_flags |= FLAG_IGNORECASE; } else { new_flags &= ~FLAG_IGNORECASE; }
			case 'm': if (adding) { new_flags |= FLAG_MULTILINE;  } else { new_flags &= ~FLAG_MULTILINE; }
			case 's': if (adding) { new_flags |= FLAG_DOTALL;     } else { new_flags &= ~FLAG_DOTALL; }
			case 'U': if (adding) { new_flags |= FLAG_UNGREEDY;   } else { new_flags &= ~FLAG_UNGREEDY; }
			case 'x': if (adding) { new_flags |= FLAG_EXTENDED;   } else { new_flags &= ~FLAG_EXTENDED; }
			case '-': adding = false;
			case ')':
				ctx.flags = new_flags;
				AstNode* n = ast_new(AST_NCGROUP);
				n.left = null;
				return n;
			default:
				return INVALID_PATTERN~;
		}
	}
	return INVALID_PATTERN~;
}

fn AstNode*? parse_escape_atom(ParseCtx* ctx)
{
	if (@at_end(ctx)) return INVALID_PATTERN~;
	char esc = @consume(ctx);
	switch (esc)
	{
		case 'd':
			AstNode* nd = ast_new(AST_CHARCLASS); nd.char_class = CLASS_DIGIT; return nd;
		case 'D':
			AstNode* nD = ast_new(AST_CHARCLASS); nD.char_class = CLASS_DIGIT_INV; return nD;
		case 'w':
			AstNode* nw = ast_new(AST_CHARCLASS); nw.char_class = CLASS_WORD; return nw;
		case 'W':
			AstNode* nW = ast_new(AST_CHARCLASS); nW.char_class = CLASS_WORD_INV; return nW;
		case 's':
			AstNode* ns = ast_new(AST_CHARCLASS); ns.char_class = CLASS_SPACE; return ns;
		case 'S':
			AstNode* nS = ast_new(AST_CHARCLASS); nS.char_class = CLASS_SPACE_INV; return nS;
		case 'p':
			AstNode* np = ast_new(AST_CHARCLASS); np.char_class = parse_property_charclass(ctx, false)!; return np;
		case 'P':
			AstNode* nP = ast_new(AST_CHARCLASS); nP.char_class = parse_property_charclass(ctx, true)!; return nP;
		case 'A': return ast_new(AST_ANCHOR_ABSOLUTE_START);
		case 'z': return ast_new(AST_ANCHOR_ABSOLUTE_END);
		case 'Z': return ast_new(AST_ANCHOR_ABSOLUTE_END_NL);
		case 'R':
			// \R: matches \r\n | [\r\n\v\f\x85\u2028\u2029]
			AstNode* r_lit  = ast_new(AST_LITERAL); r_lit.codepoint = '\r';
			AstNode* n_lit  = ast_new(AST_LITERAL); n_lit.codepoint = '\n';
			AstNode* rn_seq = ast_new(AST_CONCAT); rn_seq.left = r_lit; rn_seq.right = n_lit;
			AstNode* lb_cls_node = ast_new(AST_CHARCLASS);
			AsciiCharset lb_ascii = (AsciiCharset)0;
			lb_ascii |= (AsciiCharset)1 << '\n';
			lb_ascii |= (AsciiCharset)1 << '\r';
			lb_ascii |= (AsciiCharset)1 << '\v';
			lb_ascii |= (AsciiCharset)1 << '\f';
			UnicodeRange* lb_ranges = alloc::new_array(tmem, UnicodeRange, 2);
			lb_ranges[0] = { .lo = 0x85, .hi = 0x85 };
			lb_ranges[1] = { .lo = 0x2028, .hi = 0x2029 };
			lb_cls_node.char_class = { .ascii_bits = lb_ascii, .ranges = lb_ranges, .ranges_count = 2 };
			AstNode* lb_alt = ast_new(AST_ALT); lb_alt.left = rn_seq; lb_alt.right = lb_cls_node;
			AstNode* lb_ncg = ast_new(AST_NCGROUP); lb_ncg.left = lb_alt;
			return lb_ncg;
		case 'b': return ast_new(AST_WORD_BOUND);
		case 'B': return ast_new(AST_WORD_NBOUND);
		case 'K': return ast_new(AST_KEEP);
		case 'N': return ast_new(AST_NON_NEWLINE);
		case 'h': AstNode* nh = ast_new(AST_CHARCLASS); nh.char_class = CLASS_HSPACE; return nh;
		case 'H': AstNode* nH = ast_new(AST_CHARCLASS); nH.char_class = CLASS_HSPACE_INV; return nH;
		case 'v': AstNode* nv = ast_new(AST_CHARCLASS); nv.char_class = CLASS_VSPACE; return nv;
		case 'V': AstNode* nV = ast_new(AST_CHARCLASS); nV.char_class = CLASS_VSPACE_INV; return nV;
		case 'n': AstNode* nl = ast_new(AST_LITERAL); nl.codepoint = '\n'; return nl;
		case 'r': AstNode* cr = ast_new(AST_LITERAL); cr.codepoint = '\r'; return cr;
		case 't': AstNode* ht = ast_new(AST_LITERAL); ht.codepoint = '\t'; return ht;
		case 'f': AstNode* ff = ast_new(AST_LITERAL); ff.codepoint = '\f'; return ff;
		case 'a': AstNode* bell = ast_new(AST_LITERAL); bell.codepoint = '\a'; return bell;
		case 'u':
			Char32 u4 = parse_hex_escape(ctx, 4)!;
			AstNode* u4n = ast_new(AST_LITERAL); u4n.codepoint = u4; return u4n;
		case 'U':
			Char32 u8 = parse_hex_escape(ctx, 8)!;
			AstNode* u8n = ast_new(AST_LITERAL); u8n.codepoint = u8; return u8n;
		case 'e':
			AstNode* esc_lit = ast_new(AST_LITERAL); esc_lit.codepoint = 0x1B; return esc_lit;
		case 'G':
			return ast_new(AST_ANCHOR_GPOS);
		case 'c':
			if (@at_end(ctx)) return INVALID_PATTERN~;
			char ctrl_c = @consume(ctx);
			Char32 ctrl_cp;
			if (ctrl_c >= '@' && ctrl_c <= '_') { ctrl_cp = (Char32)(ctrl_c ^ 64); }
			else if (ctrl_c >= 'a' && ctrl_c <= 'z') { ctrl_cp = (Char32)((ctrl_c - 0x20) ^ 64); }
			else { return INVALID_PATTERN~; }
			AstNode* ctrl_node = ast_new(AST_LITERAL); ctrl_node.codepoint = ctrl_cp; return ctrl_node;
		case 'x':
			if (@peek(ctx) == '{')
			{
				ctx.pos++;
				Char32 xhv = parse_hex_escape_braced(ctx)!;
				AstNode* xhn = ast_new(AST_LITERAL); xhn.codepoint = xhv; return xhn;
			}
			Char32 x2h = parse_hex_escape(ctx, 2)!;
			AstNode* x2n = ast_new(AST_LITERAL); x2n.codepoint = x2h; return x2n;
		case 'Q':
			// \Q...\E — literal quoting: treat everything until \E as literal characters
			AstNode* qlast = null;
			while (!@at_end(ctx))
			{
				if (@peek(ctx) == '\\' && ctx.pos + 1 < ctx.pattern.len && ctx.pattern[ctx.pos + 1] == 'E')
				{
					ctx.pos += 2;
					break;
				}
				Char32 qcp = parse_codepoint(ctx)!;
				AstNode* qlit = ast_new(AST_LITERAL); qlit.codepoint = qcp;
				if (qlast == null) { qlast = qlit; }
				else { AstNode* qcon = ast_new(AST_CONCAT); qcon.left = qlast; qcon.right = qlit; qlast = qcon; }
			}
			if (qlast == null) { AstNode* qeps = ast_new(AST_NCGROUP); qeps.left = null; return qeps; }
			return qlast;
		case 'k':
			// \k<name> named backreference
			if (@peek(ctx) != '<') return INVALID_PATTERN~;
			ctx.pos++;
			usz kname_start = ctx.pos;
			while (!@at_end(ctx) && @peek(ctx) != '>') ctx.pos++;
			if (@at_end(ctx)) return INVALID_PATTERN~;
			String kname = ctx.pattern[kname_start : ctx.pos - kname_start];
			ctx.pos++;
			int kgid = -1;
			for (usz ki = 0; ki < ctx.named_groups_count; ki++)
			{
				if (ctx.named_groups_buf[ki].name == kname) { kgid = ctx.named_groups_buf[ki].group_id; break; }
			}
			if (kgid < 0) return INVALID_PATTERN~;
			AstNode* br_k = ast_new(AST_BACKREF);
			br_k.group_id = kgid;
			return br_k;
		default:
			if (esc >= '1' && esc <= '9')
			{
				AstNode* br = ast_new(AST_BACKREF);
				br.group_id = (int)(esc - '0');
				return br;
			}
			AstNode* lit = ast_new(AST_LITERAL);
			lit.codepoint = (Char32)esc;
			return lit;
	}
}
