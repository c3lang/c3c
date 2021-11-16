module acorn::parser::ast;
import acorn::arr;

/** Parser for Acorn compiler. See Acorn documentation for syntax diagrams.
 *
 * @file
 *
 * This source file is part of avm - Acorn Virtual Machine.
 * See Copyright Notice in avm.h
 */


/* **********************
 * Abstract Syntax Tree construction helpers for parser
 * (isolates that we are working with arrays to encode s-expressions)
 * **********************/

/** Append a value to AST segment - growing as needed */
fn void addValue(Value th, Value astseg, Value val) @inline
{
    arr::add(th, astseg, val);
}

/** Get a value within the AST segment */
fn Value get(Value th, Value astseg, AuintIdx idx) @inline
{
    return arr::get(th, astseg, idx);
}

/** Set a value within the AST segment */
fn void set(Value th, Value astseg, AuintIdx idx, Value val)
{
    arr::set(th, astseg, idx, val);
}

/** Create and append a new AST segment (of designated size) to current segment.
 * Append the AST op to the new segment, then return it */
Value addSeg(Value th, Value oldseg, Value astop, AuintIdx size)
{
	Value newseg = pushArray(th, aNull, size);
	arr::add(th, oldseg, newseg);
	th.popValue();
	arr::add(th, newseg, astop);
	return newseg;
}

/** Create and append a new AST segment (with two slots) to current segment.
 * Append the AST op and val to the new segment, then return it */
Value addSeg2(Value th, Value oldseg, Value astop, Value val)
{
	Value newseg = pushArray(th, aNull, 2);
	arr::add(th, oldseg, newseg);
	popValue(th);
	arr::add(th, newseg, astop);
	arr::add(th, newseg, val);
	return newseg;
}

/** Get last node from ast segment */
Value getLast(Value th, Value astseg) @inline
{
    return get(th, astseg, arr_size(astseg) - 1);
}

/** Create a new segment of designated size to replace last value of oldseg.
 * Append the AST op and the value from the oldseg to the new segment,
 * then return it. */
Value insSeg(Value th, Value oldseg, Value astop, AuintIdx size)
{
	AuintIdx oldpos = arr_size(oldseg)-1;
	Value saveval = arr::get(th, oldseg, oldpos);
	Value newseg = pushArray(th, aNull, size);
	arr::set(th, oldseg, oldpos, newseg);
	popValue(th);
	arr::add(th, newseg, astop);
	arr::add(th, newseg, saveval);
	return newseg;
}

/** Create a new segment of designated size to replace last value of oldseg.
 * Append the AST op, propval, and the value from the oldseg to the new segment,
 * then return it. */
Value astInsSeg2(Value th, Value oldseg, Value astop, Value propval, AuintIdx size)
{
	AuintIdx oldpos = arr_size(oldseg) - 1;
	Value saveval = arr::get(th, oldseg, oldpos);
	Value newseg = pushArray(th, aNull, size);
	arrSet(th, oldseg, oldpos, newseg);
	th.popValue();
	arr::add(th, newseg, astop);
	if (isSym(propval))
	{
		if (propval == vmlit(SymThis))
		{
			arr::add(th, newseg, propval);
		}
		else
		{
			Value propseg = addSeg(th, newseg, vmlit(SymLit), 2); // Assume propval is a literal symbol
			arr::add(th, propseg, propval);
		}
	}
	arr::add(th, newseg, saveval);
	return newseg;
}

/** Create a new untethered, sized AST segment that has astop as first element */
Value pushNew(Value th, Value astop, AuintIdx size)
{
	Value newseg = pushArray(th, aNull, size);
	arr::add(th, newseg, astop);
	return newseg;
}

/** Attach newseg into last slot of oldseg, whose old value is appended to newseg */
void popNew(Value th, Value oldseg, Value newseg)
{
	AuintIdx oldpos = arr_size(oldseg) - 1;
	Value saveval = arr::get(th, oldseg, oldpos);
	arr::set(th, oldseg, oldpos, newseg);
	arr::add(th, newseg, saveval);
	th.popValue();
}

/** Return true if ast segment can be assigned a value: variable or settable property/method */
fn bool isLval(Value th, Value astseg)
{
	if (!astseg.isArr()) return false;
	Value op = get(th, astseg, 0);
	return op == vmlit(SYM_LOCAL) || op == vmlit(SYM_GLOGAL) || op==vmlit(SYM_ACT_PROP) || op==vmlit(SYM_RAW_PROP) || op==vmlit(SYM_CALL_PROP);
}
