module std::regex;
import std::core::string::iterator;
import std::collections::list;

// ============================================================
// Thompson NFA simulation
// ============================================================

struct RegexThread @local
{
	RegexNfaState* state;
	MatchGroup* captures;
	bool greedy;
}

alias RegexThreadList = List{RegexThread};

fn bool is_word_char(Char32 c)
{
	if (c < 128) return WORD_SET.contains((char)c);
	if (c == 0xAA || c == 0xB5 || c == 0xBA) return true;
	return (c >= 0xC0 && c <= 0xD6)
		|| (c >= 0xD8 && c <= 0xF6)
		|| (c >= 0xF8 && c <= 0xFF);
}

fn void add_thread(
	RegexThreadList* list,
	RegexNfaState* state,
	MatchGroup* caps,
	usz* gen,
	usz cur_gen,
	String input,
	usz pos,
	bool is_start,
	Char32 prev_char,
	Regex* re,
	int n_groups,
	bool parent_greedy
)
{
	if (state == null) return;
	if (gen[state.index] == cur_gen) return;
	gen[state.index] = cur_gen;

	switch (state.kind)
	{
		case STATE_SPLIT:
			MatchGroup* caps2 = alloc::new_array(tmem, MatchGroup, (usz)n_groups);
			mem::copy(caps2, caps, (usz)n_groups * MatchGroup.sizeof);
			if (state.lazy)
			{
				add_thread(list, state.out1, caps, gen, cur_gen, input, pos, is_start, prev_char, re, n_groups, false);
				add_thread(list, state.out2, caps2, gen, cur_gen, input, pos, is_start, prev_char, re, n_groups, parent_greedy);
			}
			else
			{
				add_thread(list, state.out1, caps, gen, cur_gen, input, pos, is_start, prev_char, re, n_groups, parent_greedy);
				add_thread(list, state.out2, caps2, gen, cur_gen, input, pos, is_start, prev_char, re, n_groups, true);
			}
		case STATE_CAPTURE_START:
			MatchGroup* nc_s = alloc::new_array(tmem, MatchGroup, (usz)n_groups);
			mem::copy(nc_s, caps, (usz)n_groups * MatchGroup.sizeof);
			if (state.group_id < n_groups)
			{
				nc_s[state.group_id].start = pos;
				nc_s[state.group_id].matched = false;
			}
			add_thread(list, state.out1, nc_s, gen, cur_gen, input, pos, is_start, prev_char, re, n_groups, parent_greedy);
		case STATE_CAPTURE_END:
			MatchGroup* nc_e = alloc::new_array(tmem, MatchGroup, (usz)n_groups);
			mem::copy(nc_e, caps, (usz)n_groups * MatchGroup.sizeof);
			if (state.group_id < n_groups)
			{
				nc_e[state.group_id].end = pos;
				nc_e[state.group_id].matched = true;
			}
			add_thread(list, state.out1, nc_e, gen, cur_gen, input, pos, is_start, prev_char, re, n_groups, parent_greedy);
		case STATE_ANCHOR_START:
			bool ok_s = (re.flags & FLAG_MULTILINE) != FLAG_NONE
				? is_start || (pos > 0 && input[pos - 1] == '\n')
				: is_start;
			if (ok_s) add_thread(list, state.out1, caps, gen, cur_gen, input, pos, is_start, prev_char, re, n_groups, parent_greedy);
		case STATE_ANCHOR_END:
			bool ok_e = (re.flags & FLAG_MULTILINE) != FLAG_NONE
				? pos >= input.len || input[pos] == '\n'
				: pos >= input.len;
			if (ok_e) add_thread(list, state.out1, caps, gen, cur_gen, input, pos, is_start, prev_char, re, n_groups, parent_greedy);
		case STATE_WORD_BOUND:
			bool cur_w = is_word_position(input, pos);
			bool prev_w = is_word_char(prev_char);
			if (cur_w != prev_w) add_thread(list, state.out1, caps, gen, cur_gen, input, pos, is_start, prev_char, re, n_groups, parent_greedy);
		case STATE_WORD_NBOUND:
			bool cur_wn = is_word_position(input, pos);
			bool prev_wn = is_word_char(prev_char);
			if (cur_wn == prev_wn) add_thread(list, state.out1, caps, gen, cur_gen, input, pos, is_start, prev_char, re, n_groups, parent_greedy);
		case STATE_LOOKAHEAD_POS:
			if (backtrack_match(state.lookaround_start, input, pos, re))
			{
				add_thread(list, state.out1, caps, gen, cur_gen, input, pos, is_start, prev_char, re, n_groups, parent_greedy);
			}
		case STATE_LOOKAHEAD_NEG:
			if (!backtrack_match(state.lookaround_start, input, pos, re))
			{
				add_thread(list, state.out1, caps, gen, cur_gen, input, pos, is_start, prev_char, re, n_groups, parent_greedy);
			}
		case STATE_LOOKBEHIND_POS:
			bool lb_ok;
			if (state.lookbehind_variable)
			{
				lb_ok = variable_lookbehind_has_match(state, input, pos, re);
			}
			else
			{
				lb_ok = pos >= state.lookbehind_len
					&& backtrack_match(state.lookaround_start, input, pos - state.lookbehind_len, re);
			}
			if (lb_ok) add_thread(list, state.out1, caps, gen, cur_gen, input, pos, is_start, prev_char, re, n_groups, parent_greedy);
		case STATE_LOOKBEHIND_NEG:
			bool lbn_ok;
			if (state.lookbehind_variable)
			{
				lbn_ok = !variable_lookbehind_has_match(state, input, pos, re);
			}
			else
			{
				lbn_ok = pos < state.lookbehind_len
					|| !backtrack_match(state.lookaround_start, input, pos - state.lookbehind_len, re);
			}
			if (lbn_ok) add_thread(list, state.out1, caps, gen, cur_gen, input, pos, is_start, prev_char, re, n_groups, parent_greedy);
		case STATE_KEEP:
			MatchGroup* kc = alloc::new_array(tmem, MatchGroup, (usz)n_groups);
			mem::copy(kc, caps, (usz)n_groups * MatchGroup.sizeof);
			kc[0].start = pos;
			add_thread(list, state.out1, kc, gen, cur_gen, input, pos, is_start, prev_char, re, n_groups, parent_greedy);
		case STATE_ANCHOR_ABSOLUTE_START:
			if (pos == 0) add_thread(list, state.out1, caps, gen, cur_gen, input, pos, is_start, prev_char, re, n_groups, parent_greedy);
		case STATE_ANCHOR_ABSOLUTE_END:
			if (pos >= input.len) add_thread(list, state.out1, caps, gen, cur_gen, input, pos, is_start, prev_char, re, n_groups, parent_greedy);
		case STATE_ANCHOR_ABSOLUTE_END_NL:
			bool ok_enl = pos >= input.len || (pos + 1 == input.len && input[pos] == '\n');
			if (ok_enl) add_thread(list, state.out1, caps, gen, cur_gen, input, pos, is_start, prev_char, re, n_groups, parent_greedy);
		case STATE_ANCHOR_GPOS:
			if (pos == re.gpos) add_thread(list, state.out1, caps, gen, cur_gen, input, pos, is_start, prev_char, re, n_groups, parent_greedy);
		case STATE_COND_GROUP:
			bool cond_ok = (state.backref_id > 0 && state.backref_id < n_groups)
				&& caps[state.backref_id].matched;
			add_thread(list, cond_ok ? state.lookaround_start : state.out2,
				caps, gen, cur_gen, input, pos, is_start, prev_char, re, n_groups, parent_greedy);
		default:
			list.push({ .state = state, .captures = caps, .greedy = parent_greedy });
	}
}

struct RegexBacktrackChoice @local
{
	RegexNfaState* state;
	usz pos;
	MatchGroup* captures;
}

alias RegexBacktrackChoiceList = List{RegexBacktrackChoice};

struct RegexBacktrackFrame @local
{
	RegexNfaState* state;
	usz pos;
	MatchGroup* captures;
	RegexBacktrackChoiceList choices;
	bool exact_end;
	usz end_pos;
	RegexBacktrackFrame* parent;
	MatchGroup* parent_captures;
	RegexNfaState* resume_state;
}

alias RegexBacktrackFrameList = List{RegexBacktrackFrame*};

fn MatchGroup* clone_match_groups(MatchGroup* caps, int n_groups) @local
{
	MatchGroup* clone = alloc::new_array(tmem, MatchGroup, (usz)n_groups);
	mem::copy(clone, caps, (usz)n_groups * MatchGroup.sizeof);
	return clone;
}

fn void push_backtrack_choice(RegexBacktrackChoiceList* choices, RegexNfaState* state, usz pos, MatchGroup* caps, int n_groups) @local
{
	if (state == null) return;
	choices.push({
		.state = state,
		.pos = pos,
		.captures = clone_match_groups(caps, n_groups)
	});
}

fn RegexBacktrackFrame* create_backtrack_frame(
	RegexNfaState* state,
	usz pos,
	MatchGroup* caps,
	bool exact_end,
	usz end_pos,
	RegexBacktrackFrame* parent = null,
	MatchGroup* parent_captures = null,
	RegexNfaState* resume_state = null
) @local
{
	RegexBacktrackFrame* frame = mem::tnew(RegexBacktrackFrame);
	*frame = {
		.state = state,
		.pos = pos,
		.captures = caps,
		.exact_end = exact_end,
		.end_pos = end_pos,
		.parent = parent,
		.parent_captures = parent_captures,
		.resume_state = resume_state,
	};
	frame.choices.tinit(32);
	return frame;
}

fn usz? backtrack_execute(RegexNfaState* state, String input, usz pos, Regex* re, MatchGroup* caps, int n_groups, bool exact_end, usz end_pos, MatchGroup** final_caps) @local
{
	if (state == null) return NO_MATCH~;

	RegexBacktrackFrameList frames;
	frames.tinit(8);
	frames.push(create_backtrack_frame(state, pos, caps, exact_end, end_pos));

	while FRAME_LOOP: (!frames.is_empty())
	{
		RegexBacktrackFrame* frame = frames[frames.size - 1];
		while EXECUTE_PATH: (frame.state != null)
		{
			switch (frame.state.kind)
			{
				case STATE_MATCH:
					if (!frame.exact_end || frame.pos == frame.end_pos)
					{
						if (frame.parent == null)
						{
							if (final_caps != null) *final_caps = frame.captures;
							return frame.pos;
						}
						mem::copy(frame.parent_captures, frame.captures, (usz)n_groups * MatchGroup.sizeof);
						RegexBacktrackFrame* parent = frame.parent;
						parent.pos = frame.pos;
						parent.state = frame.resume_state;
						frames.pop()!!;
						continue FRAME_LOOP;
					}
					break EXECUTE_PATH;
				case STATE_LITERAL:
					if (frame.pos >= input.len || (frame.exact_end && frame.pos >= frame.end_pos)) break EXECUTE_PATH;
					StringIterator lit_it = { .utf8 = input, .current = frame.pos };
					Char32? lit_cp = lit_it.next();
					if (catch lit_cp) break EXECUTE_PATH;
					Char32 lit_input = lit_cp;
					Char32 lit_state = frame.state.codepoint;
					if ((re.flags & FLAG_IGNORECASE) != FLAG_NONE)
					{
						lit_input = unicode_case_fold(lit_input);
						lit_state = unicode_case_fold(lit_state);
					}
					if (lit_input != lit_state) break EXECUTE_PATH;
					frame.pos = lit_it.current;
					frame.state = frame.state.out1;
					continue;
				case STATE_CHARSET:
					if (frame.pos >= input.len || (frame.exact_end && frame.pos >= frame.end_pos)) break EXECUTE_PATH;
					StringIterator set_it = { .utf8 = input, .current = frame.pos };
					Char32? set_cp = set_it.next();
					if (catch set_cp) break EXECUTE_PATH;
					bool set_icase = (re.flags & FLAG_IGNORECASE) != FLAG_NONE;
					bool set_dotall = (re.flags & FLAG_DOTALL) != FLAG_NONE;
					bool set_is_dot = frame.state.char_class.ascii_bits == CLASS_DOT.ascii_bits
						&& frame.state.char_class.ranges_count == 0
						&& !frame.state.char_class.negated;
					bool set_ok = (set_is_dot && !set_dotall)
						? set_cp != '\n'
						: frame.state.char_class.matches(set_cp, set_icase);
					if (!set_ok) break EXECUTE_PATH;
					frame.pos = set_it.current;
					frame.state = frame.state.out1;
					continue;
				case STATE_SPLIT:
					RegexNfaState* first = frame.state.lazy ? frame.state.out2 : frame.state.out1;
					RegexNfaState* second = frame.state.lazy ? frame.state.out1 : frame.state.out2;
					push_backtrack_choice(&frame.choices, second, frame.pos, frame.captures, n_groups);
					frame.state = first;
					continue;
				case STATE_CAPTURE_START:
					if (frame.state.group_id < n_groups)
					{
						frame.captures[frame.state.group_id].start = frame.pos;
						frame.captures[frame.state.group_id].matched = false;
					}
					frame.state = frame.state.out1;
					continue;
				case STATE_CAPTURE_END:
					if (frame.state.group_id < n_groups)
					{
						frame.captures[frame.state.group_id].end = frame.pos;
						frame.captures[frame.state.group_id].matched = true;
					}
					frame.state = frame.state.out1;
					continue;
				case STATE_KEEP:
					frame.captures[0].start = frame.pos;
					frame.state = frame.state.out1;
					continue;
				case STATE_ANCHOR_START:
					bool ok_as = frame.pos == 0 || ((re.flags & FLAG_MULTILINE) != FLAG_NONE && frame.pos > 0 && input[frame.pos - 1] == '\n');
					if (!ok_as) break EXECUTE_PATH;
					frame.state = frame.state.out1;
					continue;
				case STATE_ANCHOR_END:
					bool ok_ae = frame.pos >= input.len || ((re.flags & FLAG_MULTILINE) != FLAG_NONE && input[frame.pos] == '\n');
					if (!ok_ae) break EXECUTE_PATH;
					frame.state = frame.state.out1;
					continue;
				case STATE_ANCHOR_ABSOLUTE_START:
					if (frame.pos != 0) break EXECUTE_PATH;
					frame.state = frame.state.out1;
					continue;
				case STATE_ANCHOR_ABSOLUTE_END:
					if (frame.pos < input.len) break EXECUTE_PATH;
					frame.state = frame.state.out1;
					continue;
				case STATE_ANCHOR_ABSOLUTE_END_NL:
					bool ok_enl = frame.pos >= input.len || (frame.pos + 1 == input.len && input[frame.pos] == '\n');
					if (!ok_enl) break EXECUTE_PATH;
					frame.state = frame.state.out1;
					continue;
				case STATE_WORD_BOUND:
					bool wb_cur = is_word_position(input, frame.pos);
					bool wb_prev = is_word_char(previous_codepoint_before(input, frame.pos));
					if (wb_cur == wb_prev) break EXECUTE_PATH;
					frame.state = frame.state.out1;
					continue;
				case STATE_WORD_NBOUND:
					bool wnb_cur = is_word_position(input, frame.pos);
					bool wnb_prev = is_word_char(previous_codepoint_before(input, frame.pos));
					if (wnb_cur != wnb_prev) break EXECUTE_PATH;
					frame.state = frame.state.out1;
					continue;
				case STATE_LOOKAHEAD_POS:
					if (!backtrack_match(frame.state.lookaround_start, input, frame.pos, re)) break EXECUTE_PATH;
					frame.state = frame.state.out1;
					continue;
				case STATE_LOOKAHEAD_NEG:
					if (backtrack_match(frame.state.lookaround_start, input, frame.pos, re)) break EXECUTE_PATH;
					frame.state = frame.state.out1;
					continue;
				case STATE_LOOKBEHIND_POS:
					bool lbp_ok;
					if (frame.state.lookbehind_variable)
					{
						lbp_ok = variable_lookbehind_has_match(frame.state, input, frame.pos, re);
					}
					else
					{
						lbp_ok = frame.pos >= frame.state.lookbehind_len
							&& backtrack_match(frame.state.lookaround_start, input, frame.pos - frame.state.lookbehind_len, re);
					}
					if (!lbp_ok) break EXECUTE_PATH;
					frame.state = frame.state.out1;
					continue;
				case STATE_LOOKBEHIND_NEG:
					bool lbn_ok;
					if (frame.state.lookbehind_variable)
					{
						lbn_ok = !variable_lookbehind_has_match(frame.state, input, frame.pos, re);
					}
					else
					{
						lbn_ok = frame.pos < frame.state.lookbehind_len
							|| !backtrack_match(frame.state.lookaround_start, input, frame.pos - frame.state.lookbehind_len, re);
					}
					if (!lbn_ok) break EXECUTE_PATH;
					frame.state = frame.state.out1;
					continue;
				case STATE_BACKREF:
					int brid = frame.state.backref_id;
					if (brid <= 0 || brid >= n_groups) break EXECUTE_PATH;
					if (!frame.captures[brid].matched) break EXECUTE_PATH;
					usz br_len = frame.captures[brid].end - frame.captures[brid].start;
					if (frame.pos + br_len > input.len || (frame.exact_end && frame.pos + br_len > frame.end_pos)) break EXECUTE_PATH;
					if ((re.flags & FLAG_IGNORECASE) != FLAG_NONE)
					{
						StringIterator br_a = { .utf8 = input, .current = frame.captures[brid].start };
						StringIterator br_b = { .utf8 = input, .current = frame.pos };
						usz br_end = frame.captures[brid].end;
						while (br_a.current < br_end)
						{
							Char32? br_ca = br_a.next();
							Char32? br_cb = br_b.next();
							if (catch br_ca) break EXECUTE_PATH;
							if (catch br_cb) break EXECUTE_PATH;
							if (unicode_case_fold(br_ca) != unicode_case_fold(br_cb)) break EXECUTE_PATH;
						}
						frame.pos = br_b.current;
					}
					else
					{
						if (!mem::equals(input[frame.pos : br_len].ptr, input[frame.captures[brid].start : br_len].ptr, br_len)) break EXECUTE_PATH;
						frame.pos += br_len;
					}
					frame.state = frame.state.out1;
					continue;
				case STATE_ATOMIC:
					MatchGroup* atm_caps = clone_match_groups(frame.captures, n_groups);
					frames.push(create_backtrack_frame(
						frame.state.lookaround_start,
						frame.pos,
						atm_caps,
						frame.exact_end,
						frame.end_pos,
						frame,
						frame.captures,
						frame.state.out1
					));
					continue FRAME_LOOP;
				case STATE_ANCHOR_GPOS:
					if (frame.pos != re.gpos) break EXECUTE_PATH;
					frame.state = frame.state.out1;
					continue;
				case STATE_COND_GROUP:
					bool cond_ok = (frame.state.backref_id > 0 && frame.state.backref_id < n_groups)
						&& frame.captures[frame.state.backref_id].matched;
					RegexNfaState* cond_branch = cond_ok ? frame.state.lookaround_start : frame.state.out2;
					MatchGroup* cond_caps = clone_match_groups(frame.captures, n_groups);
					frames.push(create_backtrack_frame(
						cond_branch,
						frame.pos,
						cond_caps,
						frame.exact_end,
						frame.end_pos,
						frame,
						frame.captures,
						frame.state.out1
					));
					continue FRAME_LOOP;
				default:
					break EXECUTE_PATH;
			}
		}
		RegexBacktrackChoice? choice = frame.choices.pop();
		if (try choice)
		{
			frame.state = choice.state;
			frame.pos = choice.pos;
			frame.captures = choice.captures;
			continue;
		}
		if (frame.parent == null) return NO_MATCH~;
		RegexBacktrackFrame* parent = frame.parent;
		frames.pop()!!;
		parent.state = null;
	}
	return NO_MATCH~;
}

fn bool backtrack_match(RegexNfaState* state, String input, usz pos, Regex* re)
{
	if (state == null) return false;
	int n_groups = re.group_count + 1;
	@pool()
	{
		MatchGroup* caps = alloc::new_array(tmem, MatchGroup, (usz)n_groups);
		mem::set(caps, 0, (usz)n_groups * MatchGroup.sizeof);
		caps[0].start = pos;
		return @ok(backtrack_full(state, input, pos, re, caps, n_groups));
	};
}

fn bool backtrack_match_at_end(RegexNfaState* state, String input, usz pos, usz end_pos, Regex* re)
{
	if (state == null) return false;
	int n_groups = re.group_count + 1;
	@pool()
	{
		MatchGroup* caps = alloc::new_array(tmem, MatchGroup, (usz)n_groups);
		mem::set(caps, 0, (usz)n_groups * MatchGroup.sizeof);
		caps[0].start = pos;
		return @ok(backtrack_full_exact(state, input, pos, end_pos, re, caps, n_groups));
	};
}

fn bool variable_lookbehind_has_match(RegexNfaState* state, String input, usz pos, Regex* re) @local
{
	for (usz ls = 0; ls < pos; ls = next_utf8_codepoint_start(input, ls, pos))
	{
		if (backtrack_match_at_end(state.lookaround_start, input, ls, pos, re)) return true;
	}
	return false;
}

fn usz? backtrack_full_exact(RegexNfaState* state, String input, usz pos, usz end_pos, Regex* re, MatchGroup* caps, int n_groups)
{
	MatchGroup* final_caps = caps;
	usz? result = backtrack_execute(state, input, pos, re, caps, n_groups, true, end_pos, &final_caps);
	if (try result && final_caps != caps)
	{
		mem::copy(caps, final_caps, (usz)n_groups * MatchGroup.sizeof);
	}
	return result;
}

fn usz? backtrack_full(RegexNfaState* state, String input, usz pos, Regex* re, MatchGroup* caps, int n_groups)
{
	MatchGroup* final_caps = caps;
	usz? result = backtrack_execute(state, input, pos, re, caps, n_groups, false, 0, &final_caps);
	if (try result && final_caps != caps)
	{
		mem::copy(caps, final_caps, (usz)n_groups * MatchGroup.sizeof);
	}
	return result;
}

fn Match? try_match_at_bt(Regex* re, String input, usz start_pos, Allocator result_alloc)
{
	int n_groups = re.group_count + 1;
	re.gpos = start_pos;
	@pool()
	{
		MatchGroup* caps = alloc::new_array(tmem, MatchGroup, (usz)n_groups);
		mem::set(caps, 0, (usz)n_groups * MatchGroup.sizeof);
		caps[0].start = start_pos;

		usz? end_pos = backtrack_full(&re.states[re.start_idx], input, start_pos, re, caps, n_groups);
		if (catch end_pos) return NO_MATCH~;

		caps[0].end = end_pos;
		caps[0].matched = true;

		MatchGroup* result_groups = alloc::new_array(result_alloc, MatchGroup, (usz)n_groups);
		mem::copy(result_groups, caps, (usz)n_groups * MatchGroup.sizeof);
		Match result = {
			.regex = re,
			.input = input,
			.groups = result_groups,
			.group_count = (usz)n_groups,
			.allocator = result_alloc
		};
		return result;
	};
}

fn void charclass_merge_ranges(RegexCharClass* dst, RegexTempRangeList* unicode, RegexCharClass src) @local
{
	dst.ascii_bits |= src.ascii_bits;
	dst.props |= src.props;
	for (usz i = 0; i < src.ranges_count; i++)
	{
		RegexUnicodeRange r = src.ranges[i];
		charclass_add_range(dst, unicode, r.lo, r.hi);
	}
}

fn void collect_start_filter(
	RegexNfaState* state,
	bool* visited,
	RegexCharClass* filter,
	RegexTempRangeList* unicode,
	bool* supported,
	bool* reaches_match
) @local
{
	if (state == null || !*supported) return;
	if (visited[state.index]) return;
	visited[state.index] = true;

	switch (state.kind)
	{
		case STATE_MATCH:
			*reaches_match = true;
			return;
		case STATE_LITERAL:
			charclass_add_range(filter, unicode, state.codepoint, state.codepoint);
			return;
		case STATE_CHARSET:
			if (state.char_class.negated || state.char_class.complement_props != PROP_NONE)
			{
				*supported = false;
				return;
			}
			charclass_merge_ranges(filter, unicode, state.char_class);
			return;
		case STATE_SPLIT:
		case STATE_CAPTURE_START:
		case STATE_CAPTURE_END:
		case STATE_ANCHOR_START:
		case STATE_ANCHOR_END:
		case STATE_WORD_BOUND:
		case STATE_WORD_NBOUND:
		case STATE_LOOKAHEAD_POS:
		case STATE_LOOKAHEAD_NEG:
		case STATE_LOOKBEHIND_POS:
		case STATE_LOOKBEHIND_NEG:
		case STATE_KEEP:
		case STATE_ANCHOR_ABSOLUTE_START:
		case STATE_ANCHOR_ABSOLUTE_END:
		case STATE_ANCHOR_ABSOLUTE_END_NL:
		case STATE_ANCHOR_GPOS:
			collect_start_filter(state.out1, visited, filter, unicode, supported, reaches_match);
			if (state.kind == STATE_SPLIT) collect_start_filter(state.out2, visited, filter, unicode, supported, reaches_match);
			return;
		default:
			*supported = false;
			return;
	}
}

fn void Regex.compute_start_filter(&self)
{
	if (self.states == null || self.state_count == 0) return;
	@pool()
	{
		bool* visited = alloc::new_array(tmem, bool, self.state_count);
		mem::set(visited, 0, self.state_count * bool.sizeof);
		RegexCharClass filter = {};
		RegexTempRangeList unicode;
		unicode.tinit(4);
		defer unicode.free();
		bool supported = true;
		bool reaches_match = false;
		collect_start_filter(&self.states[self.start_idx], visited, &filter, &unicode, &supported, &reaches_match);
		if (!supported || reaches_match) return;
		if (filter.ascii_bits == (AsciiCharset)0 && filter.props == PROP_NONE && unicode.size == 0) return;
		self.start_filter = filter;
		if (unicode.size > 0)
		{
			self.start_filter.ranges = alloc::new_array(self.allocator, RegexUnicodeRange, unicode.size);
			mem::copy(self.start_filter.ranges, unicode.entries, unicode.size * RegexUnicodeRange.sizeof);
			self.start_filter.ranges_count = unicode.size;
		}
		self.has_start_filter = true;
	};
}

fn Match? try_match_at(Regex* re, String input, usz start_pos, Allocator result_alloc)
{
	if (re.has_backref || re.has_atomic || re.has_gpos || re.has_cond) return try_match_at_bt(re, input, start_pos, result_alloc);

	int n_groups = re.group_count + 1;
	usz n_states = re.state_count;

	@pool()
	{
		usz* gen = alloc::new_array(tmem, usz, n_states);
		mem::set(gen, 0, n_states * usz.sizeof);

		RegexThreadList current; current.tinit(32);
		RegexThreadList next_t; next_t.tinit(32);

		usz cur_gen = 1;
		MatchGroup* best = null;

		MatchGroup* init_caps = alloc::new_array(tmem, MatchGroup, (usz)n_groups);
		mem::set(init_caps, 0, (usz)n_groups * MatchGroup.sizeof);
		init_caps[0].start = start_pos;

		bool is_start = start_pos == 0;
		Char32 prev_char = is_start ? 0 : previous_codepoint_before(input, start_pos);

		add_thread(&current, &re.states[re.start_idx], init_caps, gen, cur_gen, input, start_pos, is_start, prev_char, re, n_groups, true);

		foreach (t : current.array_view())
		{
			if (t.state.kind == STATE_MATCH)
			{
				MatchGroup* mc = t.captures;
				mc[0].end = start_pos;
				mc[0].matched = true;
				if (t.greedy || best == null) best = mc;
				break;
			}
		}

		StringIterator it = { .utf8 = input, .current = start_pos };
		usz pos = start_pos;

		while (it.has_next())
		{
			Char32? maybe_cp = it.next();
			if (catch maybe_cp) break;
			Char32 cp = maybe_cp;
			usz next_pos = it.current;
			cur_gen++;

			bool icase = (re.flags & FLAG_IGNORECASE) != FLAG_NONE;
			bool dotall = (re.flags & FLAG_DOTALL) != FLAG_NONE;

			foreach (t : current.array_view())
			{
				RegexNfaState* s = t.state;
				bool matched_char = false;
				switch (s.kind)
				{
					case STATE_MATCH:
						MatchGroup* mc = t.captures;
						mc[0].end = pos;
						mc[0].matched = true;
						if (t.greedy || best == null) best = mc;
						continue;
					case STATE_LITERAL:
						Char32 lc = s.codepoint;
						Char32 tc = cp;
						if (icase) lc = unicode_case_fold(lc);
						if (icase) tc = unicode_case_fold(tc);
						matched_char = tc == lc;
					case STATE_CHARSET:
						bool is_dot = s.char_class.ascii_bits == CLASS_DOT.ascii_bits
							&& s.char_class.ranges_count == 0
							&& !s.char_class.negated;
						matched_char = (is_dot && !dotall)
							? cp != '\n'
							: s.char_class.matches(cp, icase);
					default:
						continue;
				}
				if (matched_char)
				{
					add_thread(&next_t, s.out1, t.captures, gen, cur_gen, input, next_pos, false, cp, re, n_groups, t.greedy);
				}
			}

			pos = next_pos;
			prev_char = cp;
			is_start = false;

			RegexThreadList tmp = current;
			current = next_t;
			next_t = tmp;
			next_t.clear();
		}

		foreach (t : current.array_view())
		{
			if (t.state.kind == STATE_MATCH)
			{
				MatchGroup* mc = t.captures;
				mc[0].end = pos;
				mc[0].matched = true;
				if (t.greedy || best == null) best = mc;
				break;
			}
		}

		if (best == null || !best[0].matched) return NO_MATCH~;

		MatchGroup* result_groups = alloc::new_array(result_alloc, MatchGroup, (usz)n_groups);
		mem::copy(result_groups, best, (usz)n_groups * MatchGroup.sizeof);
		Match result = {
			.regex = re,
			.input = input,
			.groups = result_groups,
			.group_count = (usz)n_groups,
			.allocator = result_alloc
		};
		return result;
	};
}
