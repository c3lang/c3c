module acorn::stack;
import acorn::sym;

/** Implements the data stack that belongs to a thread.
 * A thread has one data stack which is an allocated array of Values, initialized to 'null'.
 *
 * The stack implementation is optimized for lean performance first, as its functions
 * are called several times for every method call. Therefore, stack indices are not checked for
 * validity (except when running in debug mode, where invalid indices generate exceptions).
 *
 * A current method's area of the data stack is bounded by pointers:
 * - th(th)->curmethod->begin points to the bottom (at 0 index)
 * - th(th)->stk_top points just above the last (top) value
 * - th(th)->curmethod->end points just above last allocated value on stack for method
 *
 * @file
 *
 * This source file is part of avm - Acorn Virtual Machine.
 * See Copyright Notice in avm.h
 */


/* ****************************************
   HELPER MACROS
   ***************************************/

/** Size of the method's stack area: base to top */
func AintIdx stkSz(Value th) @inline
{
    return th(th).stk_top - th(th).curmethod.begin;
}

/** Is there room to increment stack top up by 1 and null it to ensure we do not mark it when making it available for a new value */
#define stkCanIncTop(th) {assert((th(th)->stk_top+1 <= th(th)->curmethod->end) && "stack top overflow");*th(th)->stk_top=aNull;}

/** Point to current method's stack value at position i.
 * For a method: i=0 is self, i=1 is first parameter, etc. */
func void Value.at(Value* th, AintIdx i) @inline
{
    @assert_exp(i >= 0 && i < stkSz(th), "invalid stack index");
    return &th(*th).curmethod.begin[i];
}

/* ****************************************
   INDEX-ONLY STACK MANIPULATION
   ***************************************/

/* Retrieve the stack value at the index. Be sure 0<= idx < top.
 * Good for getting method's parameters: 0=self, 1=parm 1, etc. */
func Value Value.getLocal(Value *th, AintIdx idx)
{
	return *th.at(idx);
}

/* Put the value on the stack at the designated position. Be sure 0<= idx < top. */
func void Value.setLocal(Value th, AintIdx idx, Value val)
{
	*th.at(idx) = val;
	mem::markChk(th, th, val);
}

/* Copy the stack value at fromidx into toidx */
func void Value.copyLocal(Value* th, AintIdx toidx, AintIdx fromidx)
{
	*th.at(toidx) = *th.at(fromidx);
}

/**
 * Remove the value at index (shifting down all values above it to top)
 * @require stkSz(th) > 0
 */
func void Value.deleteLocal(Value* th, AintIdx idx)
{
	Value* p = th.at(idx);
	memmove(p, p + 1, sizeof(Value)*(stkSz(th) - idx - 1));
	th(*th).stk_top--;
}

/**
 * Insert the popped value into index (shifting up all values above it)
 * @require stkSz(th) > 0
 */
func void Value.insertLocal(Value *th, AintIdx idx)
{
	Value *p = th.at(idx);
	Value val = *(th(*th).stk_top - 1);
	memmove(p+1, p, sizeof(Value) * (stkSz(th) - idx - 1));
	*p = val;
}


/* ****************************************
   TOP-BASED STACK MANIPULATION
   ***************************************/

/* Push a value on the stack's top */
func Value Value.pushValue(Value* th, Value val)
{
	stkCanIncTop(th); /* Check if there is room */
	*th(*th).stk_top++ = val;
	mem::markChk(th, th, val); // Keep, if marked for deletion?
	return val;
}

/* Push and return the corresponding Symbol value for a 0-terminated c-string */
func Value Value.pushSym(Value* th, string str)
{
	stkCanIncTop(th); /* Check if there is room */
	return sym::newSym(*th, th(*th).stk_top++, str);
}

/* Push and return the corresponding Symbol value for a byte sequence of specified length */
func Value Value.pushSyml(Value th, string str)
{
	stkCanIncTop(th); /* Check if there is room */
	return sym::newSym(*th, th(*th).stk_top++, str);
}

/* Push and return a new String value */
Value pushString(Value th, Value type, const char *str)
{
	stkCanIncTop(th); /* Check if there is room */
	return newStr(th, th(th)->stk_top++, (type==aNull)? vmlit(TypeTextm) : type, str, strlen(str));
}

/* Push and return a new String value of size with a copy of str bytes */
Value pushStringl(Value th, Value type, const char *str, AuintIdx size) {
	stkCanIncTop(th); /* Check if there is room */
	return newStr(th, th(th)->stk_top++, (type==aNull)? vmlit(TypeTextm) : type, str, size);
}

/* Push and return a new typed CData value of size */
Value pushCData(Value th, Value type, unsigned char cdatatyp, AuintIdx size, unsigned int extrahdr) {
	stkCanIncTop(th); /* Check if there is room */
	return newCData(th, th(th)->stk_top++, type, cdatatyp, size, extrahdr);
}

/* Push and return a new Array value */
Value pushArray(Value th, Value type, AuintIdx size) {
	stkCanIncTop(th); /* Check if there is room */
	return newArr(th, th(th)->stk_top++, (type==aNull)? vmlit(TypeListm) : type,  size);
}

/* Push and return a new Closure value.
   Size is get and set methods plus closure variables, all pushed on stack */
Value pushClosure(Value th, AintIdx size) {
	Value closure;
	assert(size>=2 && stkSz(th)>=size); // All closure variables should be on stack
	stkCanIncTop(th); /* Check if there is room */
	closure = newClosure(th, th(th)->stk_top++, vmlit(TypeClom),  size);
	// Copy closure variables into closure
	for (int i=0; i<size; i++)
		arrSet(th, closure, i, *(th(th)->stk_top-size-1+i));
	*(th(th)->stk_top-size-1) = closure; // move created closure down
	th(th)->stk_top -= size; // pop off closure variables
	return closure;
}

/* Push a closure variable. */
Value pushCloVar(Value th, AuintIdx idx) {
	stkCanIncTop(th); /* Check if there is room */
	Value closure = *th(th)->curmethod->methodbase;
	return *th(th)->stk_top++ = (isArr(closure) && idx<getSize(closure))? arrGet(th, closure, idx) : aNull;
}

/* Pop a value into a closure variable. */
void popCloVar(Value th, AuintIdx idx) {
	assert(stkSz(th)>0); // Must be at least one value to remove!
	Value closure = *th(th)->curmethod->methodbase;
	if (isArr(closure) && idx<getSize(closure))
		arrSet(th, closure, idx, *--th(th)->stk_top);
	else
		--th(th)->stk_top;
}

/* Push and return a new hashed table value */
Value pushTbl(Value th, Value type, AuintIdx size) {
	stkCanIncTop(th); /* Check if there is room */
	return newTbl(th, th(th)->stk_top++, (type==aNull)? vmlit(TypeIndexm) : type, size);
}

/* Push and return a new Type value */
Value pushType(Value th, Value type, AuintIdx size) {
	stkCanIncTop(th); /* Check if there is room */
	return newType(th, th(th)->stk_top++, (type==aNull)? vmlit(TypeObject) : type, size);
}

/* Push and return a new Mixin value */
Value pushMixin(Value th, Value type, Value inheritype, AuintIdx size) {
	stkCanIncTop(th); /* Check if there is room */
	return newMixin(th, th(th)->stk_top++, (type==aNull)? vmlit(TypeObject) : type, inheritype, size);
}

/* Push and return the value for a method written in C */
Value pushCMethod(Value th, AcMethodp meth)
{
	stkCanIncTop(th); /* Check if there is room */
	return newCMethod(th, th(th)->stk_top++, meth);
}

/* Push and return the VM's value */
Value pushVM(Value th) {
	stkCanIncTop(th); /* Check if there is room */
	return *th(th)->stk_top++ = vm(th);
}

/* Push and return a new CompInfo value, compiler state for an Acorn method */
Value pushCompiler(Value th, Value src, Value url) {
	stkCanIncTop(th); /* Check if there is room */
	return newCompiler(th, th(th)->stk_top++, src, url);
}

/* Push a value's serialized Text */
Value pushSerialized(Value th, Value val) {
	Value serstr = pushStringl(th, aNull, NULL, 16);
	serialize(th, serstr, 0, val);
	return serstr;
}

/* Push and return the value of the named member of the table found at the stack's specified index */
Value pushTblGet(Value th, AintIdx tblidx, const char *mbrnm) {
	stkCanIncTop(th); /* Check if there is room */
	Value tbl = *stkAt(th, tblidx);
	assert(isTbl(tbl));
	newSym(th, th(th)->stk_top++, mbrnm, strlen(mbrnm));
	return *(th(th)->stk_top-1) = tblGet(th, tbl, *(th(th)->stk_top-1));
}

/* Put the local stack's top value into the named member of the table found at the stack's specified index */
void popTblSet(Value th, AintIdx tblidx, const char *mbrnm) {
	assert(stkSz(th)>0); // Must be at least one value to remove!
	Value tbl = *stkAt(th, tblidx);
	assert(isTbl(tbl));
	stkCanIncTop(th); /* Check if there is room */
	newSym(th, th(th)->stk_top++, mbrnm, strlen(mbrnm));
	tblSet(th, tbl, *(th(th)->stk_top-1), *(th(th)->stk_top-2));
	th(th)->stk_top -= 2; // Pop key & value after value is safely in table
}

/* Push and return the value held by the uncalled property of the value found at the stack's specified index. */
Value pushProperty(Value th, AintIdx validx, const char *propnm) {
	stkCanIncTop(th); /* Check if there is room */
	Value val = *stkAt(th, validx);
	newSym(th, th(th)->stk_top++, propnm, strlen(propnm));
	return *(th(th)->stk_top-1) = getProperty(th, val, *(th(th)->stk_top-1));
}

/* Store the local stack's top value into the uncalled property of the type found at the stack's specified index
 * Note: Unlike pushProperty, popProperty is restricted to the type being changed. */
void popProperty(Value th, AintIdx typeidx, const char *mbrnm) {
	assert(stkSz(th)>0); // Must be at least one value to remove!
	Value tbl = *stkAt(th, typeidx);
	stkCanIncTop(th); /* Check if there is room */
	newSym(th, th(th)->stk_top++, mbrnm, strlen(mbrnm));
	if (isType(tbl))
		tblSet(th, tbl, *(th(th)->stk_top-1), *(th(th)->stk_top-2));
	th(th)->stk_top -= 2; // Pop key & value after value is stored
}

/* Push and return the value held by the perhaps-called property of the value found at the stack's specified index.
 * Note: This lives in between pushProperty (which never calls) and getCall (which always calls).
 * This calls the property's value only if it is callable, otherwise it just pushes the property's value. */
Value pushGetActProp(Value th, AintIdx selfidx, const char *propnm) {
	stkCanIncTop(th); /* Check if there is room */
	Value self = *stkAt(th, selfidx);
	newSym(th, th(th)->stk_top++, propnm, strlen(propnm));
	Value ret = *(th(th)->stk_top-1) = getProperty(th, self, *(th(th)->stk_top-1));

	// If it is callable (e.g., a method), call it to get property value
	if (canCall(ret)) {
		// Finish setting up stack for call
		stkCanIncTop(th); /* Check if there is room for self */
		*(th(th)->stk_top++) = self;
		// Do the call, expecting (and returning) just one return value
		switch (canCallMorC(th(th)->stk_top-2)? callMorCPrep(th, th(th)->stk_top-2, 1, 0)
			: callYielderPrep(th, th(th)->stk_top-2, 1, 0)) {
			case MethodBC:
				methodRunBC(th);
				break;
		}
		ret = *(th(th)->stk_top-1);
	}
	return ret;
}

/* Store the local stack's top value into the perhaps-called property of the value found at the stack's specified index
 * Note: This lives in between popProperty (which never calls) and setCall (which always calls).
 * This calls the property's value only if it is a closure with a set method.
 * Otherwise, it sets the property's value directly if (and only if) self is a type. */
void popSetActProp(Value th, AintIdx selfidx, const char *mbrnm) {
	assert(stkSz(th)>0); // Must be at least one value to remove!
	Value self = *stkAt(th, selfidx);
	stkCanIncTop(th); /* Check if there is room for symbol */
	newSym(th, th(th)->stk_top++, mbrnm, strlen(mbrnm));
	Value propval = getProperty(th, self, *(th(th)->stk_top-1));

	// If it is callable (e.g., a method), call it to set property value
	if (canCall(propval)) {
		// Set up stack for call
		stkCanIncTop(th); /* Check if there is room for self */
		Value set = getFromTop(th, 1); // the value to set
		*(th(th)->stk_top-2) = propval;
		*(th(th)->stk_top-1) = self;
		*(th(th)->stk_top++) = set;
		// Do the set call, expecting (and returning) just one return value
		switch (canCallMorC(propval)? callMorCPrep(th, th(th)->stk_top-3, 1, 0)
			: callYielderPrep(th, th(th)->stk_top-3, 1, 0)) {
			case MethodBC:
				methodRunBC(th);
				break;
		}
	}
	else {
		// Only if self is a type, store value in property
		if (isType(self))
			tblSet(th, self, *(th(th)->stk_top-1), *(th(th)->stk_top-2));
		th(th)->stk_top -= 2; // Pop key & value
	}
}

/* Push a copy of a stack's value at index onto the stack's top */
func Value Value.pushLocal(Value* th, AintIdx idx)
{
	stkCanIncTop(th); /* Check if there is room */
	return *th(*th).stk_top++ = th.getLocal(idx);
}

/**
 * Pop a value off the top of the stack
 * @require stkSz(th) > 0
 */
func Value Value.popValue()
{
	return *--th(*th).stk_top;
}

/**
 * Pops the top value and writes it at idx. Often used to set return value
 * @require stkSz(th) > 0, idx >= 0, idx < stkSz(th) - 1
 */
func void Value.popLocal(Value* th, AintIdx idx)
{
	th.setLocal(idx, *(th(*th).stk_top - 1));
	// Pop after value is safely in Global
	--th(*th).stk_top;
}

/**
 * Retrieve the stack value at the index from top. Be sure 0<= idx < top.
 * @require idx >= 0, idx < stkSz(th)
 */
func Value Value.getFromTop(Value* th, AintIdx idx)
{
	return *th.at(stkSz(th) - idx - 1);
}

/**
 * Return number of values on the current method's stack
 */
func AuintIdx Value.getTop(Value* th)
{
	return cast(stkSz(th), AuintIdx);
}

/**
 * When index is positive, this indicates how many Values are on the method's stack.
 * This can shrink the stack or grow it (padding with 'null's).
 * A negative index removes that number of values off the top.
 */
func void Value.setTop(Value* th, AintIdx idx)
{
    // TODO
	Value *base = th(*th).curmethod.begin;

	// If positive, idx is the index of top value on stack
	if (idx >= 0)
	{
		assert((base + idx <= th(th)->stk_last) && "stack top overflow"); // Cannot grow past established limit
		while (th(th)->stk_top < base + idx)
			*th(th)->stk_top++ = aNull; // If growing, fill with nulls
		th(th)->stk_top = base + idx;
	}
	// If negative, idx is which Value from old top is new top (-1 means no change, -2 pops one)
	else {
		assert((-(idx) <= th(th)->stk_top - base) && "invalid new top");
		th(th)->stk_top += idx;  // Adjust top using negative index
	}
}

/* ****************************************
   GLOBAL VARIABLE ACCESS
   ***************************************/

/**
 * Push and return the symbolically-named global variable's value
 * @require vm(*th).global.isTbl()
 **/
func Value Value.pushGloVar(Value* th, string var)
{
    // Check if there is room
	stkCanIncTop(th);
	Value val = sym::newSym(th, th(th).stk_top++, var);
	mem::markChk(th, th, val); /* Mark it if needed */
	return *(th(*th).stk_top - 1) = tbl::get(th, vm(th).global, val);
}

/**
 * Alter the symbolically-named global variable to have the value popped off the local stack
 * @require stkSz(th) > 0, vm(th).global.isTbl()
 **/
func void Value.popGloVar(Value* th, string var)
{
    // Check if there is room
	stkCanIncTop(th);
	Value val = sym::newSym(th, th(th).stk_top++, var);
	tbl::set(th, vm(th).global, *(th(th)->stk_top-1), *(th(th)->stk_top-2));
	th(*th).stk_top -= 2; // Pop key & value after value is safely in Global
}

/* Push the value of the current process thread's global variable table. */
Value pushGlobal(Value th)
{
	stkCanIncTop(th); /* Check if there is room */
	return *th(th).stk_top++ = vm(th).global;
}

/**
 * Internal function to re-allocate stack's size
 * @require newsize <= STACK_MAXSIZE || newsize == STACK_ERRORSIZE
 **/
func void realloc(Value th, int newsize)
{
    // Incremental GC before memory allocation events
	mem::gccheck(th);
	Value *oldstack = th(th).stack;
	int osize = th(th).size; // size of old stack

	// Ensure we not asking for more than allowed, and that old stack's values are consistent
	assert(osize == 0 || ((th(th).stk_last - th(th).stack) == th(th)->size - STACK_EXTRA));

	// Allocate new stack (assume success) and fill any growth with nulls
	mem::reallocvector(th, th(th)->stack, th(th)->size, newsize, Value);
	for (; osize < newsize; osize++)
	{
	    th(th).stack[osize] = aNull;
	}

	// Correct stack values for new size
	th(th)->size = newsize;
	th(th)->stk_last = th(th)->stack + newsize - STACK_EXTRA;

	// Correct all data stack pointers, given that data stack may have moved in memory
	if (oldstack) {
		CallInfo *ci;
		AintIdx shift = th(th)->stack - oldstack;
		th(th)->stk_top = th(th)->stk_top + shift;
		for (ci = th(th)->curmethod; ci != NULL; ci = ci->previous) {
			ci->end += shift;
			ci->methodbase += shift;
			ci->retTo += shift;
			ci->begin += shift;
		}
	}
}

/** Internal function to grow current method's stack area by at least n past stk_top.
   May double stack instead. May abort if beyond stack max. */
void stkGrow(Value th, AuintIdx extra) {

	// Already past max?  Abort!
	if (th(th)->size > STACK_MAXSIZE) {
		logSevere("Acorn VM wants to overflow max stack size. Runaway recursive method?");
		return;
	}

	// Calculate the max between how much we need (based on requested growth)
	// and doubling the stack size (capped at maximum)
	AuintIdx needed = (AuintIdx)(th(th)->stk_top - th(th)->stack) + extra + STACK_EXTRA;
	AuintIdx newsize = 2 * th(th)->size;
	if (newsize > STACK_MAXSIZE)
		newsize = STACK_MAXSIZE;
	if (newsize < needed) newsize = needed;

	// re-allocate stack (preserves contents)
    if (newsize > STACK_MAXSIZE) {
		stkRealloc(th, STACK_ERRORSIZE); // How much we give if asking for too much
    }
    else
		stkRealloc(th, newsize);
}

/* Ensure method's stack has room for 'needed' values above top. Return 0 on failure.
 * This may grow the stack, but never shrinks it.
 */
int needMoreLocal(Value th, AuintIdx needed) {
	int success;
	CallInfo *ci = th(th)->curmethod;
	vm_lock(th);

	// Check if we already have enough allocated room on stack for more values
	if ((AuintIdx)(th(th)->stk_last - th(th)->stk_top) > needed + STACK_EXTRA)
		success = 1;  // Success! Stack is already big enough
	else {
		// Will this overflow max stack size?
		if ((AuintIdx)(th(th)->stk_top - th(th)->stack) > STACK_MAXSIZE - needed - STACK_EXTRA)
			success = 0;  // Fail! - don't grow
		else {
			stkGrow(th, needed);
			success = 1;
		}
	}

	// adjust method's last allowed value upwards, as needed
	if (success && ci->end < th(th)->stk_top + needed)
		ci->end = th(th)->stk_top + needed;

	vm_unlock(th);
	return success;
}

