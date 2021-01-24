/** All type methods and properties
 *
 * @file
 *
 * This source file is part of avm - Acorn Virtual Machine.
 * See Copyright Notice in avm.h
 */

#include "avmlib.h"

/** <=> */
int Value.all_compare(Value* th)
{
	if (th.getTop() > 1 && th.getLocal(0) == th.getLocal(1))
	{
		th.pushValue(anInt(0));
		return 1;
	}
	return 0;
}

/** === Exact match of values */
int Value.all_same(Value* th)
{
	th.pushValue(th.getTop()> 1 && th.getLocal(0) == th.getLocal(1)? aTrue : aFalse);
	return 1;
}

macro auto @all_rocket!($th)
{
	if (th.getTop(th) < 2) return 0;
	th.pushValue(vmlit(SymRocket));
	th.pushValue(th.getLocal(0));
	th.pushValue(th.getLocal(1));
	th.getCall(2, 1);
	return th.popValue();
}

/** ~~, == equal using <=> */
int Value.all_equal(Value *th)
{
    th.pushValue(@all_rocket!(th) == anInt(0) ? aTrue : aFalse);
	return 1;
}

/** < */
int Value.all_lesser(Value *th)
{
	th.pushValue(@all_rocket!(th) == anInt(-1)? aTrue : aFalse);
	return 1;
}

/** > */
int Value.all_greater(Value *th) {
	th.pushValue(@all_rocket!(th) == anInt(1)? aTrue : aFalse);
	return 1;
}

/** <= */
int Value.all_lesseq(Value* th)
{
	th.pushValue(@all_rocket!(th) == anInt(-1) || ret == anInt(0)? aTrue : aFalse);
	return 1;
}

/** >= */
int Value.all_greateq(Value* th)
{
    Value ret = all_rocket!(th);
	th.pushValue(ret == anInt(1) || ret == anInt(0)? aTrue : aFalse);
	return 1;
}

/** executable? */
int Value.all_isexec(Value* th)
{
	th.pushValue(canCall(th.getLocal(0)) ? aTrue : aFalse);
	return 1;
}

/** type */
int Value.all_type(Value* th)
{
	th.pushValue(th.getType(th.getLocal(0)));
	return 1;
}

/** property */
int Value.all_property(Value* th)
{
	if (th.getTop() > 1)
	{
		th.pushValue(th.getProperty(th.getLocal(0), th.getLocal(1)));
		return 1;
	}
	return 0;
}

/** .Mixin(mixin) */
int Value.all_mixin(Value* th)
{
	if (th.getTop() > 1) th.addMixin(th.getLocal(0), th.getLocal(1));
	th.setTop(1);
	return 1;
}

/** Initialize the All type */
void core_all_init(Value th)
{
	vmlit(TypeAll) = th.pushMixin(vmlit(TypeObject), aNull, 32);
		th.pushSym("All");
		popProperty(th, 0, "_name");
		pushCMethod(th, all_compare);
		popProperty(th, 0, "<=>");
		pushCMethod(th, all_equal);
		popProperty(th, 0, "~~");
		pushCMethod(th, all_equal);
		popProperty(th, 0, "==");
		pushCMethod(th, all_same);
		popProperty(th, 0, "===");
		pushCMethod(th, all_lesser);
		popProperty(th, 0, "<");
		pushCMethod(th, all_lesseq);
		popProperty(th, 0, "<=");
		pushCMethod(th, all_greater);
		popProperty(th, 0, ">");
		pushCMethod(th, all_greateq);
		popProperty(th, 0, ">=");
		pushCMethod(th, all_isexec);
		popProperty(th, 0, "callable?");
		pushCMethod(th, all_property);
		popProperty(th, 0, "property");
		pushCMethod(th, all_type);
		popProperty(th, 0, "type");
		pushCMethod(th, all_mixin);
		popProperty(th, 0, "Mixin");
	th.popGloVar("All");
	return;
}

