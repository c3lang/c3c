<* This module is scheduled for removal, use std::core::ascii *>
module std::ascii;

macro bool in_range_m(c, start, len) => (uint)(c - start) < len;
macro bool is_lower_m(c)  => in_range_m(c, 0x61, 26);
macro bool is_upper_m(c)  => in_range_m(c, 0x41, 26);
macro bool is_digit_m(c)  => in_range_m(c, 0x30, 10);
macro bool is_bdigit_m(c) => in_range_m(c, 0x30, 2);
macro bool is_odigit_m(c) => in_range_m(c, 0x30, 8);
macro bool is_xdigit_m(c) => in_range_m(c | 32, 0x61, 6) || is_digit_m(c);
macro bool is_alpha_m(c)  => in_range_m(c | 32, 0x61, 26);
macro bool is_print_m(c)  => in_range_m(c, 0x20, 95);
macro bool is_graph_m(c)  => in_range_m(c, 0x21, 94);
macro bool is_space_m(c)  => in_range_m(c, 0x9, 5) || c == 0x20;
macro bool is_alnum_m(c)  => is_alpha_m(c) || is_digit_m(c);
macro bool is_punct_m(c)  => !is_alnum_m(c) && is_graph_m(c);
macro bool is_blank_m(c)  => c == 0x20 || c == 0x9;
macro bool is_cntrl_m(c)  => c < 0x20 || c == 0x7f;
macro to_lower_m(c) => is_upper_m(c) ? c + 0x20 : c;
macro to_upper_m(c) => is_lower_m(c) ? c - 0x20 : c;

fn bool in_range(char c, char start, char len) => in_range_m(c, start, len);

fn bool char.in_range(char c, char start, char len) => in_range_m(c, start, len);

fn bool uint.in_range(uint c, uint start, uint len) => in_range_m(c, start, len);
fn bool uint.is_lower(uint c)  @deprecated => is_lower_m(c);
fn bool uint.is_upper(uint c)  @deprecated => is_upper_m(c);
fn bool uint.is_digit(uint c)  @deprecated => is_digit_m(c);
fn bool uint.is_bdigit(uint c) @deprecated => is_bdigit_m(c);
fn bool uint.is_odigit(uint c) @deprecated => is_odigit_m(c);
fn bool uint.is_xdigit(uint c) @deprecated => is_xdigit_m(c);
fn bool uint.is_alpha(uint c)  @deprecated => is_alpha_m(c);
fn bool uint.is_print(uint c)  @deprecated => is_print_m(c);
fn bool uint.is_graph(uint c)  @deprecated => is_graph_m(c);
fn bool uint.is_space(uint c)  @deprecated => is_space_m(c);
fn bool uint.is_alnum(uint c)  @deprecated => is_alnum_m(c);
fn bool uint.is_punct(uint c)  @deprecated => is_punct_m(c);
fn bool uint.is_blank(uint c)  @deprecated => is_blank_m(c);
fn bool uint.is_cntrl(uint c)  @deprecated => is_cntrl_m(c);
fn uint uint.to_lower(uint c)  @deprecated => (uint)to_lower_m(c);
fn uint uint.to_upper(uint c)  @deprecated => (uint)to_upper_m(c);
