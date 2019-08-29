// Copyright (c) 2019 Christoffer Lerno. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

#include "compiler_internal.h"



/*
BuiltinType builtin_common_type[16][16] = {
		//  err,  wptr,  nil,  bool,  char, short,   int,  long,  byte,ushort,  int,  ulong, float,double, ctint, ctreal
		{ TERR, TERR, TERR, TERR,  TERR,  TERR,  TERR,  TERR,  TERR,  TERR,  TERR,  TERR,  TERR,  TERR,  TERR,  TERR }, // Error
		{ TERR, TERR, TERR, TERR,  TERR,  TERR,  TERR,  TERR,  TERR,  TERR,  TERR,  TERR,  TERR,  TERR,  TERR,  TERR }, // void
		{ TERR, TERR, TERR, TERR,  TERR,  TERR,  TERR,  TERR,  TERR,  TERR,  TERR,  TERR,  TERR,  TERR,  TERR,  TERR }, // wptr
		{ TERR, TERR, TERR, T_U1,  T_I8,  T_I16, T_I32, T_I64, T_U8,  T_U16, T_U32, T_U64, T_F32, T_F64, T_U1,  T_FXX }, // bool
		{ TERR, TERR, TERR, T_I8,  T_I8,  T_I16, T_I32, T_I64, T_I8,  T_I16, T_I32, T_I64, T_F32, T_F64, T_I8,  T_FXX }, // char
		{ TERR, TERR, TERR, T_I16, T_I16, T_I16, T_I32, T_I64, T_I16, T_I16, T_I32, T_I64, T_F32, T_F64, T_I16, T_FXX }, // short
		{ TERR, TERR, TERR, T_I32, T_I32, T_I32, T_I32, T_I64, T_I32, T_I32, T_I32, T_I64, T_F32, T_F64, T_I32, T_FXX }, // int
		{ TERR, TERR, TERR, T_I64, T_I64, T_I64, T_I64, T_I64, T_I64, T_I64, T_I64, T_I64, T_F32, T_F64, T_I64, T_FXX }, // long
		{ TERR, TERR, TERR, T_U8,  T_I8,  T_I16, T_I32, T_I64, T_U8,  T_U16, T_U32, T_U64, T_F32, T_F64, T_U8,  T_FXX }, // byte
		{ TERR, TERR, TERR, T_U16, T_I16, T_I16, T_I32, T_I64, T_U16, T_U16, T_U32, T_U64, T_F32, T_F64, T_U16, T_FXX }, // ushort
		{ TERR, TERR, TERR, T_U32, T_I32, T_I32, T_I32, T_I64, T_U32, T_U32, T_U32, T_U64, T_F32, T_F64, T_U32, T_FXX }, // uint
		{ TERR, TERR, TERR, T_U64, T_I64, T_I64, T_I64, T_I64, T_U64, T_U64, T_U64, T_U64, T_F32, T_F64, T_U64, T_FXX }, // ulong
		{ TERR, TERR, TERR, T_F32, T_F32, T_F32, T_F32, T_F32, T_F32, T_F32, T_F32, T_F32, T_F32, T_F64, T_F32, T_F32 }, // float
		{ TERR, TERR, TERR, T_F64, T_F64, T_F64, T_F64, T_F64, T_F64, T_F64, T_F64, T_F64, T_F64, T_F64, T_F64, T_F64 }, // double
		{ TERR, TERR, TERR, T_U1,  T_I8,  T_I16, T_I32, T_I64, T_U8,  T_U16, T_U32, T_U64, T_F32, T_F64, T_IXX, T_FXX }, // ctint
		{ TERR, TERR, TERR, T_FXX, T_FXX, T_FXX, T_FXX, T_FXX, T_FXX, T_FXX, T_FXX, T_FXX, T_F32, T_F64, T_FXX, T_FXX }, // ctreal
};
void
BuiltinConv BUILTIN_ASSIGN_CONVERSION[16][16] = {
		//e  v  w  b  c  s  i  l  b  u  u  u  f  d  c  c
		//r  o  p  o  h  h  n  o  y  s  i  l  l  o  t  t
		//r  i  t  o  a  o  t  n  t  h  n  o  o  u  i  r
		{ 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 }, // error
		{ 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 }, // void =
		{ 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 }, // wptr =
		{ 0, 0, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1 }, // bool =
		{ 0, 0, 0, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0 }, // char =
		{ 0, 0, 0, 1, 1, 1, 0, 0, 1, 0, 0, 0, 0, 0, 1, 0 }, // short =
		{ 0, 0, 0, 1, 1, 1, 1, 0, 1, 1, 0, 0, 0, 0, 1, 0 }, // int =
		{ 0, 0, 0, 1, 1, 1, 1, 1, 1, 1, 1, 0, 0, 0, 1, 0 }, // long =
		{ 0, 0, 0, 1, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 1, 0 }, // byte =
		{ 0, 0, 0, 1, 0, 0, 0, 0, 1, 1, 0, 0, 0, 0, 1, 0 }, // ushort =
		{ 0, 0, 0, 1, 0, 0, 0, 0, 1, 1, 1, 0, 0, 0, 1, 0 }, // uint =
		{ 0, 0, 0, 1, 0, 0, 0, 0, 1, 1, 1, 1, 0, 0, 1, 0 }, // ulong =
		{ 0, 0, 0, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0, 1, 1 }, // float =
		{ 0, 0, 0, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1 }, // double =
		{ 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 }, // ctint
		{ 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 }, // ctfloat
};

// x += x -= x /= x *=
bool BUILTIN_ASSIGN_UPDATE_CONVERSION[16][16] = {
		//e  v  w  b  c  s  i  l  b  u  u  u  f  d  c  c
		//r  o  p  o  h  h  n  o  y  s  i  l  l  o  t  t
		//r  i  t  o  a  o  t  n  t  h  n  o  o  u  i  r
		{ 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 }, // error
		{ 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 }, // void =
		{ 0, 0, 1, 0, 1, 1, 1, 1, 1, 1, 1, 1, 0, 0, 1, 0 }, // wptr =
		{ 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1 }, // bool =
		{ 0, 0, 0, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0 }, // char =
		{ 0, 0, 0, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0, 0, 1, 0 }, // short =
		{ 0, 0, 0, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0, 0, 1, 0 }, // int =
		{ 0, 0, 0, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0, 0, 1, 0 }, // long =
		{ 0, 0, 0, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0, 0, 1, 0 }, // byte =
		{ 0, 0, 0, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0, 0, 1, 0 }, // ushort =
		{ 0, 0, 0, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0, 0, 1, 0 }, // uint =
		{ 0, 0, 0, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0, 0, 1, 0 }, // ulong =
		{ 0, 0, 0, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1 }, // float =
		{ 0, 0, 0, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1 }, // double =
		{ 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 }, // ctint
		{ 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 }, // ctfloat
};
*/


/*
void builtin_convert(ExprConst *value, BuiltinConv conversion, Type *canonical_target)
{
	assert(canonical_target == canonical_target->canonical);
	switch (conversion)
	{
		case C_XXXX:
			FATAL_ERROR("Should never be called");
			break;
		case C_FPFP:
			if (canonical_target->builtin.bitsize == 16)
			{
				value->f = (float)value->f;
			}
			if (canonical_target->builtin.bitsize == 32)
			{
				value->f = (double)value->f;
			}
			break;
		case C_FPSI:
			if (value->f < 0)
			{
				value->integer.i = (uint64_t)(-value->f);
				value->type = CONST_NEGATIVE_INT;
			}
			else
			{
				value->integer.i = (uint64_t)(-value->f);
				value->type = CONST_POSITIVE_INT;
			}
			break;
		case C_FPUI:
			assert(value->f >= 0);
			value->integer.i = (uint64_t)(value->f);
			value->type = CONST_POSITIVE_INT;
			break;
		case C_SIFP:
		case C_UIFP:
			value->f = value->integer.i;
			if (value->type == CONST_NEGATIVE_INT) value->f = -value->f;
			value->type = CONST_FLOAT;
			break;
		case C_SISI:
//			value_convert(value, VALUE_TYPE_INT, canonical_target->decl->builtin.bitsize, false, true);
			break;
		case C_SIUI:
//			value_convert(value, VALUE_TYPE_INT, canonical_target->decl->builtin.bitsize, true, true);
			break;
		case C_UISI:
//			value_convert(value, VALUE_TYPE_INT, canonical_target->decl->builtin.bitsize, false, true);
			break;
		case C_UIUI:
//			value_convert(value, VALUE_TYPE_INT, canonical_target->decl->builtin.bitsize, true, true);
			break;
		case C_PTXI:
//			value_convert(value, VALUE_TYPE_INT, canonical_target->decl->builtin.bitsize, true, true);
			break;
		case C_XIPT:
//			value_convert(value, VALUE_TYPE_INT, canonical_target->decl->builtin.bitsize, true, true);
			break;
		case C_BOFP:
//			value_convert(value, VALUE_TYPE_FLOAT, canonical_target->decl->builtin.bitsize, false, true);
			break;
		case C_BOSI:
//			value_convert(value, VALUE_TYPE_INT, canonical_target->decl->builtin.bitsize, false, true);
			break;
		case C_BOUI:
//			value_convert(value, VALUE_TYPE_INT, canonical_target->decl->builtin.bitsize, false, true);
			break;
		case C_FPBO:
//			value_convert(value, VALUE_TYPE_BOOL, canonical_target->decl->builtin.bitsize, true, true);
			break;
		case C_XIBO:
//			value_convert(value, VALUE_TYPE_BOOL, canonical_target->decl->builtin.bitsize, true, true);
			break;
		case C_PTBO:
//			value_convert(value, VALUE_TYPE_BOOL, canonical_target->decl->builtin.bitsize, true, true);
			break;
		default:
			UNREACHABLE
	}
}
*/

