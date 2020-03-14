module acornvm::value;


/** A convenience macro for assert(), establishing the conditions expected to be true,
 * before returning expression e */
macro assert_exp($c, $e)
{
    assert($c);
    return $e;
}

/**
    Define Value and C-types.
    We want all our Value-based types sized the same,
    according to the architecture (e.g., all 32-bit or all 64-bit).
*/

/** A signed integer, whose size matches Value */
typedef isize Aint;
/** An unsigned integer, whose size matches Value */
typedef usize Auint;


/** A float, whose size matches Value (see avm_env.h) */
$assert(usize.size == 8 || usize.size == 4)
$if (usize.size == 8)
{
    typedef double as Afloat;
}
$else
{
    typedef float as Afloat;
}

/** A unicode character */
typedef ulong Auchar;

/** A fixed-sized, self-typed encoded value which holds any kind of data.
 * It can be passed to or returned from Acorn or C-methods.
 * Never manipulate a Value directly; always use an AcornVM api function.
 *
 * Its size is that of a full address-space pointer (32- or 64-bits).
 * It holds either an immediate value (null, true, false, integer, float, symbol)
 * or a pointer to a compound/complex value's header.
 */
typedef void* as distinct Value

/** Prototype for a C method callable by the VM.
	It is passed the thread, through which it obtains parameters via the data stack.
	When done, it returns how many return values it has placed on the stack. */
typedef func int(Value) as AcMethodp;

/** Quick, exact equivalence check between two values ('===')
 * Great for null, false, true, integers and symbols.
 * Less suitable for floats (no epsilon) and comparing contents of containers (e.g., strings).
 * Is fast because it avoids using type-specific methods. */
macro isSame(a, b) { return (a == b); }

/** What type of data is encoded within the value, as established by the last 2 bits.
 * Because of 32/64-bit allocation alignment, pointers always have 0 in last 2 bits.
 * Thus, non-zero bits can be used to indicate a non-pointer Value. */
enum ValBits
{
	POINTER = 0,		/*! Value points to a compound value's header */
	INT = 1,  	        /*! Value is a signed integer */
	FLOAT = 2,	        /*! Value is a floating-point number */
	CONS = 3		    /*! Value is a constant (null, false, true) */
}

/** The mask used to isolate the value's ValBits info */
const int VAL_MASK = 0x3;
/** How many bits to shift a Value to remove or make space for ValBits info */
const int VAL_SHIFT = 2;


func bool Value.isEnc(Value *value, EncType type) @inline
{
    return value.isPtr() && @cast(value, MemInfo*).enctyp == type;
}

/* Return true if the value is a c-String, otherwise 0 */
bool Value.isStr(Value *str)
{
	return str.isEnc(StrEnc) && !(str_info(str)->flags1 & StrCData);
}

macro isType(v, ValBits e)
{
    return cast(v, Auint) & VAL_MASK == e;
}

// Integer value functions


/** Is v an Integer? */
func bool Value.isInt(Value *v)
{
    return @isType(*v, INT);
}

/** Cast c-integer n into an Integer value
 * This loses top two-bits of integer precision.
 * If integer is too large, this could result in an unexpected value and change of sign. */
macro anInt(n)
{
    return cast(cast(n, Aint) << VAL_SHIFT + ValInt, Value);
}

/** Cast an Integer value into a c-integer
 * Note: It assumes (and won't verify) that v is an Integer */
macro toAint(v)
{
    return cast(v, Aint) >> VAL_SHIFT;
}

// Float value functions

/** Is v a Float? */
func Value.isFloat(Value *v)
{
    return @isType(*v, FLOAT);
}

/** Cast c-float n into a Float value
 * This loses bottom two-bits of Float mantissa precision. */
AVM_API Value aFloat(Afloat n);

/** Cast an Float value into a c-float
 * Note: It assumes (and won't verify) that v is an Float */
AVM_API Afloat toAfloat(Value v);

/* *******************************************************
   null, false and true values and functions.
   (they are encoded in the impossible space for a symbol pointer
   **************************************************** */

/** The null value */
macro aNull()
{
    return @cast(0 << VAL_SHIFT, ValCons, Value);
}

/** The false value */
macro aFalse()
{
    return @cast(1 << VAL_SHIFT + ValCons, Value);
}

/** The true value */
macro aTrue()
{
    return @cast(2 << VAL_SHIFT + ValCons, Value);
}


/**
 * Is value null?
 * @require value != nil
 */
func bool Value.isNull(Value *value) @inline
{
    return *v == aNull;
}

/**
 * Is value false or null?
 * @require value != nil
 */
func bool Value.isFalse(Value *value) @inline
{
    return *v == aFalse || *v == aNull;
}

/**
 * Is value true or false?
 */
func bool Value.isBool(Value *value) @inline
{
    return *v >= aFalse;
}


// Pointer functions.

/** Is value a pointer? */
func bool Value.isPtr(Value *value) @inline
{
    return @isType(*v, POINTER);
}


/** Append serialized val to end of str. */
void serialize(Value th, Value str, int indent, Value val);

