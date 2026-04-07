module std::regex;

// ============================================================
// NFA construction — Thompson fragment model
// ============================================================

fn RegexNfaState* nfa_new(RegexBuildCtx* bctx, RegexStateKind kind)
{
	RegexNfaState* s = alloc::new(bctx.alloc, RegexNfaState);
	s.kind = kind;
	s.index = bctx.all_states.size;
	bctx.all_states.push(s);
	return s;
}

fn RegexNfaFrag frag_make(RegexNfaState* start)
{
	RegexNfaFrag f;
	f.start = start;
	f.out.tinit(4);
	return f;
}

fn RegexNfaFrag frag_literal(RegexBuildCtx* bctx, Char32 c)
{
	RegexNfaState* s = nfa_new(bctx, STATE_LITERAL);
	s.codepoint = c;
	RegexNfaFrag f = frag_make(s);
	f.out.push(&s.out1);
	return f;
}

fn RegexNfaFrag frag_charset(RegexBuildCtx* bctx, RegexCharClass cls)
{
	RegexNfaState* s = nfa_new(bctx, STATE_CHARSET);
	if (cls.ranges_count > 0)
	{
		usz pool_offset = bctx.range_pool.size;
		for (usz i = 0; i < cls.ranges_count; i++) bctx.range_pool.push(cls.ranges[i]);
		s.char_class = cls;
		s.char_class.ranges = (RegexUnicodeRange*)(uptr)pool_offset;
	}
	else
	{
		s.char_class = cls;
		s.char_class.ranges = null;
	}
	RegexNfaFrag f = frag_make(s);
	f.out.push(&s.out1);
	return f;
}

fn void frag_patch(RegexNfaFrag* f, RegexNfaState* target)
{
	foreach (pp : f.out.array_view()) *pp = target;
}

fn RegexNfaFrag frag_concat(RegexNfaFrag e1, RegexNfaFrag e2)
{
	frag_patch(&e1, e2.start);
	e1.out.free();
	RegexNfaFrag result;
	result.start = e1.start;
	result.out = e2.out;
	return result;
}

fn RegexNfaFrag frag_alt(RegexBuildCtx* bctx, RegexNfaFrag e1, RegexNfaFrag e2)
{
	RegexNfaState* split = nfa_new(bctx, STATE_SPLIT);
	split.out1 = e1.start;
	split.out2 = e2.start;
	RegexNfaFrag result;
	result.start = split;
	result.out.tinit(e1.out.size + e2.out.size);
	foreach (pp : e1.out.array_view()) result.out.push(pp);
	foreach (pp : e2.out.array_view()) result.out.push(pp);
	e1.out.free();
	e2.out.free();
	return result;
}

fn RegexNfaFrag frag_star(RegexBuildCtx* bctx, RegexNfaFrag e, bool lazy)
{
	RegexNfaState* split = nfa_new(bctx, STATE_SPLIT);
	split.lazy = lazy;
	frag_patch(&e, split);
	e.out.free();
	RegexNfaFrag result;
	result.start = split;
	result.out.tinit(2);
	if (lazy)
	{
		split.out2 = e.start;
		result.out.push(&split.out1);
	}
	else
	{
		split.out1 = e.start;
		result.out.push(&split.out2);
	}
	return result;
}

fn RegexNfaFrag frag_plus(RegexBuildCtx* bctx, RegexNfaFrag e, bool lazy)
{
	RegexNfaState* split = nfa_new(bctx, STATE_SPLIT);
	split.lazy = lazy;
	frag_patch(&e, split);
	RegexNfaFrag result;
	result.start = e.start;
	e.out.free();
	result.out.tinit(2);
	if (lazy)
	{
		split.out2 = e.start;
		result.out.push(&split.out1);
	}
	else
	{
		split.out1 = e.start;
		result.out.push(&split.out2);
	}
	return result;
}

fn RegexNfaFrag frag_quest(RegexBuildCtx* bctx, RegexNfaFrag e, bool lazy)
{
	RegexNfaState* split = nfa_new(bctx, STATE_SPLIT);
	split.lazy = lazy;
	RegexNfaFrag result;
	result.start = split;
	result.out.tinit(e.out.size + 1);
	if (lazy)
	{
		split.out2 = e.start;
		result.out.push(&split.out1);
	}
	else
	{
		split.out1 = e.start;
		result.out.push(&split.out2);
	}
	foreach (pp : e.out.array_view()) result.out.push(pp);
	e.out.free();
	return result;
}

// Epsilon fragment: a SPLIT state with one dangling out (out1) and null out2.
// Use instead of STATE_MATCH for transparent pass-through nodes (empty groups, comments).
fn RegexNfaFrag frag_epsilon(RegexBuildCtx* bctx)
{
	RegexNfaState* s = nfa_new(bctx, STATE_SPLIT);
	RegexNfaFrag f = frag_make(s);
	f.out.tinit(1);
	f.out.push(&s.out1);
	return f;
}

fn usz? ast_fixed_width(RegexAstNode* node)
{
	if (node == null) return 0;
	switch (node.kind)
	{
		case AST_LITERAL:
			return utf8_codepoint_width(node.codepoint);
		case AST_CHARCLASS:
			return charclass_fixed_width(node.char_class);
		case AST_CONCAT:
			usz a = ast_fixed_width(node.left)!;
			usz b = ast_fixed_width(node.right)!;
			return a + b;
		case AST_GROUP:
		case AST_NCGROUP:
			return ast_fixed_width(node.left);
		case AST_REPEAT:
			if (node.repeat_min != node.repeat_max || node.repeat_max == -1) return INVALID_PATTERN~;
			usz w = ast_fixed_width(node.left)!;
			return w * (usz)node.repeat_min;
		case AST_ALT:
			usz aw = ast_fixed_width(node.left)!;
			usz bw = ast_fixed_width(node.right)!;
			if (aw != bw) return INVALID_PATTERN~;
			return aw;
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
			return 0;
		case AST_LINEBREAK:
		case AST_STAR:
		case AST_PLUS:
		case AST_QUEST:
			return INVALID_PATTERN~;
		case AST_KEEP:
			return 0;
		case AST_NON_NEWLINE:
			return INVALID_PATTERN~;
		case AST_ANCHOR_GPOS:
			return 0;
		case AST_BACKREF:
			return INVALID_PATTERN~;
		case AST_ATOMIC:
		case AST_BRANCH_RESET:
			return ast_fixed_width(node.left);
		case AST_COND_GROUP:
			usz cond_w_yes = ast_fixed_width(node.left)!;
			usz cond_w_no = node.right != null ? ast_fixed_width(node.right)! : 0;
			if (cond_w_yes != cond_w_no) return INVALID_PATTERN~;
			return cond_w_yes;
		default:
			return INVALID_PATTERN~;
	}
}

fn RegexNfaFrag? build_nfa(RegexBuildCtx* bctx, RegexAstNode* node, RegexFlags flags)
{
	if (node == null) return frag_epsilon(bctx);

	switch (node.kind)
	{
		case AST_LITERAL:
			return frag_literal(bctx, node.codepoint);
		case AST_CHARCLASS:
			if ((flags & FLAG_DOTALL) != FLAG_NONE
				&& node.char_class.ascii_bits == CLASS_DOT.ascii_bits
				&& node.char_class.ranges_count == 0
				&& !node.char_class.negated)
			{
				RegexCharClass all = { .ascii_bits = ~(AsciiCharset)0 };
				return frag_charset(bctx, all);
			}
			return frag_charset(bctx, node.char_class);
		case AST_CONCAT:
			if (node.left == null && node.right == null)
			{
				return frag_epsilon(bctx);
			}
			RegexNfaFrag cl = build_nfa(bctx, node.left, flags)!;
			RegexNfaFrag cr = build_nfa(bctx, node.right, flags)!;
			return frag_concat(cl, cr);
		case AST_ALT:
			RegexNfaFrag al = build_nfa(bctx, node.left, flags)!;
			RegexNfaFrag ar = build_nfa(bctx, node.right, flags)!;
			return frag_alt(bctx, al, ar);
		case AST_STAR:
			RegexNfaFrag se = build_nfa(bctx, node.left, flags)!;
			return frag_star(bctx, se, node.lazy);
		case AST_PLUS:
			RegexNfaFrag pe = build_nfa(bctx, node.left, flags)!;
			return frag_plus(bctx, pe, node.lazy);
		case AST_QUEST:
			RegexNfaFrag qe = build_nfa(bctx, node.left, flags)!;
			return frag_quest(bctx, qe, node.lazy);
		case AST_REPEAT:
			int min_v = node.repeat_min;
			int max_v = node.repeat_max;
			const int MAX_REPEAT = 1000;
			if (min_v > MAX_REPEAT || (max_v > MAX_REPEAT && max_v != -1)) return COMPILE_ERROR~;
			if (min_v == 0 && max_v == 0)
			{
				return frag_epsilon(bctx);
			}
			RegexNfaFrag rep_result;
			bool rep_first = true;
			for (int i = 0; i < min_v; i++)
			{
				RegexNfaFrag copy = build_nfa(bctx, node.left, flags)!;
				if (rep_first) { rep_result = copy; rep_first = false; }
				else { rep_result = frag_concat(rep_result, copy); }
			}
			if (max_v == -1)
			{
				RegexNfaFrag tail = build_nfa(bctx, node.left, flags)!;
				tail = frag_star(bctx, tail, node.lazy);
				if (rep_first) { rep_result = tail; }
				else { rep_result = frag_concat(rep_result, tail); }
			}
			else
			{
				for (int i = min_v; i < max_v; i++)
				{
					RegexNfaFrag opt = build_nfa(bctx, node.left, flags)!;
					opt = frag_quest(bctx, opt, node.lazy);
					if (rep_first) { rep_result = opt; rep_first = false; }
					else { rep_result = frag_concat(rep_result, opt); }
				}
			}
			if (rep_first)
			{
				RegexNfaFrag base0 = build_nfa(bctx, node.left, flags)!;
				rep_result = frag_quest(bctx, base0, node.lazy);
			}
			return rep_result;
		case AST_GROUP:
			int gid = node.group_id;
			RegexNfaState* cs = nfa_new(bctx, STATE_CAPTURE_START);
			cs.group_id = gid;
			RegexNfaFrag gi = build_nfa(bctx, node.left, flags)!;
			RegexNfaState* ce = nfa_new(bctx, STATE_CAPTURE_END);
			ce.group_id = gid;
			RegexNfaFrag csf = frag_make(cs);
			csf.out.tinit(1);
			csf.out.push(&cs.out1);
			RegexNfaFrag cef = frag_make(ce);
			cef.out.tinit(1);
			cef.out.push(&ce.out1);
			return frag_concat(frag_concat(csf, gi), cef);
		case AST_NCGROUP:
			if (node.left == null) return frag_epsilon(bctx);
			return build_nfa(bctx, node.left, flags);
		case AST_LOOKAHEAD_POS:
		case AST_LOOKAHEAD_NEG:
		case AST_LOOKBEHIND_POS:
		case AST_LOOKBEHIND_NEG:
			RegexNfaFrag la_f = build_nfa(bctx, node.left, flags)!;
			RegexNfaState* la_match = nfa_new(bctx, STATE_MATCH);
			frag_patch(&la_f, la_match);
			la_f.out.free();
			RegexStateKind la_kind;
			switch (node.kind)
			{
				case AST_LOOKAHEAD_POS: la_kind = STATE_LOOKAHEAD_POS;
				case AST_LOOKAHEAD_NEG: la_kind = STATE_LOOKAHEAD_NEG;
				case AST_LOOKBEHIND_POS: la_kind = STATE_LOOKBEHIND_POS;
				case AST_LOOKBEHIND_NEG: la_kind = STATE_LOOKBEHIND_NEG;
				default: unreachable();
			}
			RegexNfaState* la_s = nfa_new(bctx, la_kind);
			la_s.lookaround_start = la_f.start;
			if (node.kind == AST_LOOKBEHIND_POS || node.kind == AST_LOOKBEHIND_NEG)
			{
				usz? w = ast_fixed_width(node.left);
				if (try w)
				{
					la_s.lookbehind_len = w;
					la_s.lookbehind_variable = false;
				}
				else
				{
					la_s.lookbehind_len = 0;
					la_s.lookbehind_variable = true;
				}
			}
			RegexNfaFrag la_res = frag_make(la_s);
			la_res.out.tinit(1);
			la_res.out.push(&la_s.out1);
			return la_res;
		case AST_ANCHOR_START:
			RegexNfaState* anc_s = nfa_new(bctx, STATE_ANCHOR_START);
			RegexNfaFrag asf = frag_make(anc_s);
			asf.out.tinit(1); asf.out.push(&anc_s.out1);
			return asf;
		case AST_ANCHOR_END:
			RegexNfaState* anc_e = nfa_new(bctx, STATE_ANCHOR_END);
			RegexNfaFrag aef = frag_make(anc_e);
			aef.out.tinit(1); aef.out.push(&anc_e.out1);
			return aef;
		case AST_ANCHOR_ABSOLUTE_START:
			RegexNfaState* abs_s = nfa_new(bctx, STATE_ANCHOR_ABSOLUTE_START);
			RegexNfaFrag absf = frag_make(abs_s);
			absf.out.tinit(1); absf.out.push(&abs_s.out1);
			return absf;
		case AST_ANCHOR_ABSOLUTE_END:
			RegexNfaState* abs_e = nfa_new(bctx, STATE_ANCHOR_ABSOLUTE_END);
			RegexNfaFrag abef = frag_make(abs_e);
			abef.out.tinit(1); abef.out.push(&abs_e.out1);
			return abef;
		case AST_ANCHOR_ABSOLUTE_END_NL:
			RegexNfaState* abs_enl = nfa_new(bctx, STATE_ANCHOR_ABSOLUTE_END_NL);
			RegexNfaFrag abenlf = frag_make(abs_enl);
			abenlf.out.tinit(1); abenlf.out.push(&abs_enl.out1);
			return abenlf;
		case AST_WORD_BOUND:
			RegexNfaState* wb = nfa_new(bctx, STATE_WORD_BOUND);
			RegexNfaFrag wbf = frag_make(wb);
			wbf.out.tinit(1); wbf.out.push(&wb.out1);
			return wbf;
		case AST_WORD_NBOUND:
			RegexNfaState* wn = nfa_new(bctx, STATE_WORD_NBOUND);
			RegexNfaFrag wnf = frag_make(wn);
			wnf.out.tinit(1); wnf.out.push(&wn.out1);
			return wnf;
		case AST_LINEBREAK:
			return COMPILE_ERROR~;
		case AST_KEEP:
			RegexNfaState* ks = nfa_new(bctx, STATE_KEEP);
			RegexNfaFrag kf = frag_make(ks);
			kf.out.tinit(1); kf.out.push(&ks.out1);
			return kf;
		case AST_NON_NEWLINE:
			return frag_charset(bctx, CLASS_DOT);
		case AST_BACKREF:
			RegexNfaState* brs = nfa_new(bctx, STATE_BACKREF);
			brs.backref_id = node.group_id;
			RegexNfaFrag brf = frag_make(brs);
			brf.out.tinit(1); brf.out.push(&brs.out1);
			return brf;
		case AST_ATOMIC:
			RegexNfaFrag atm_f = build_nfa(bctx, node.left, flags)!;
			RegexNfaState* atm_match = nfa_new(bctx, STATE_MATCH);
			frag_patch(&atm_f, atm_match);
			atm_f.out.free();
			RegexNfaState* atm_s = nfa_new(bctx, STATE_ATOMIC);
			atm_s.lookaround_start = atm_f.start;
			RegexNfaFrag atm_res = frag_make(atm_s);
			atm_res.out.tinit(1); atm_res.out.push(&atm_s.out1);
			return atm_res;
		case AST_ANCHOR_GPOS:
			RegexNfaState* gpos_s = nfa_new(bctx, STATE_ANCHOR_GPOS);
			RegexNfaFrag gposf = frag_make(gpos_s);
			gposf.out.tinit(1); gposf.out.push(&gpos_s.out1);
			return gposf;
		case AST_BRANCH_RESET:
			if (node.left == null) return frag_epsilon(bctx);
			return build_nfa(bctx, node.left, flags);
		case AST_COND_GROUP:
			RegexNfaFrag cond_yes_f = build_nfa(bctx, node.left, flags)!;
			RegexNfaState* cond_yes_m = nfa_new(bctx, STATE_MATCH);
			frag_patch(&cond_yes_f, cond_yes_m);
			cond_yes_f.out.free();

			RegexNfaState* cond_no_entry;
			if (node.right != null)
			{
				RegexNfaFrag cond_no_f = build_nfa(bctx, node.right, flags)!;
				RegexNfaState* cond_no_m = nfa_new(bctx, STATE_MATCH);
				frag_patch(&cond_no_f, cond_no_m);
				cond_no_f.out.free();
				cond_no_entry = cond_no_f.start;
			}
			else
			{
				RegexNfaFrag cond_eps = frag_epsilon(bctx);
				RegexNfaState* cond_eps_m = nfa_new(bctx, STATE_MATCH);
				frag_patch(&cond_eps, cond_eps_m);
				cond_eps.out.free();
				cond_no_entry = cond_eps.start;
			}
			RegexNfaState* cond_s = nfa_new(bctx, STATE_COND_GROUP);
			cond_s.backref_id = node.group_id;
			cond_s.lookaround_start = cond_yes_f.start;
			cond_s.out2 = cond_no_entry;
			RegexNfaFrag cond_res = frag_make(cond_s);
			cond_res.out.tinit(1); cond_res.out.push(&cond_s.out1);
			return cond_res;
	}
}
