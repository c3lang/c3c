/** Bytecode generator for Acorn compiler
 *
 * @file
 *
 * This source file is part of avm - Acorn Virtual Machine.
 * See Copyright Notice in avm.h
 */

#include "acorn.h"

#ifdef __cplusplus
namespace avm {
extern "C" {
#endif

/* Create a new bytecode method value. */
void newBMethod(Value th, Value *dest) {
	BMethodInfo *meth = (BMethodInfo*) mem_new(th, MethEnc, sizeof(BMethodInfo));
	*dest = (Value) meth;

	methodFlags(meth) = 0;
	methodNParms(meth) = 1; // 'self'

	meth->code = NULL;
	meth->maxstacksize = 20;
	meth->avail = 0;
	meth->size = 0;
	meth->lits = NULL;
	meth->litsz = 0;
	meth->nbrlits = 0;
	meth->nbrexterns = 0;
	meth->nbrlocals = 0;
}

/* Put new instruction in code array */
void genPutInstr(CompInfo *comp, AuintIdx loc, Instruction i) {
	mem_growvector(comp->th, comp->method->code, loc, comp->method->avail, Instruction, INT_MAX);
	comp->method->code[loc] = i;
}

/* Append new instruction to code array */
void genAddInstr(CompInfo *comp, Instruction i) {
	mem_growvector(comp->th, comp->method->code, comp->method->size, comp->method->avail, Instruction, INT_MAX);
	comp->method->code[comp->method->size++] = i;
}

/* Add a literal and return its index */
int genAddLit(CompInfo *comp, Value val) {
	BMethodInfo* f = comp->method;

	// See if we already have it
	int i = f->nbrlits;
	while (i-- > 0)
		if (f->lits[i] == val)
			return i;

	// If not found, add it
	mem_growvector(comp->th, f->lits, f->nbrlits, f->litsz, Value, INT_MAX);
	if (isStr(val))
		str_info(val)->flags1 |= StrLiteral; // Make strings read only
	f->lits[f->nbrlits] = val;
	mem_markChk(comp->th, comp, val);
	return f->nbrlits++;
}

/* Indicate the method has a variable number of parameters */
void genVarParms(CompInfo *comp) {
	methodFlags(comp->method) = METHOD_FLG_VARPARM;
}

/** Allocate block's local variables */
Value genLocalVars(CompInfo *comp, Value blockvarseg,int nexpected) {
	Value th = comp->th;
	Value svLocalVars = comp->locvarseg;
	if (blockvarseg!=aNull) {
		int nbrvars = arr_size(blockvarseg)-2;
		if (nbrvars>0) {
			comp->locvarseg = blockvarseg;
			arrSet(th, comp->locvarseg, 1, anInt(comp->nextreg));
			if (nbrvars-nexpected>0)
				genAddInstr(comp, BCINS_ABC(OpLoadNulls, comp->nextreg+nexpected, nbrvars-nexpected, 0));
			comp->nextreg += nbrvars;
			if (comp->method->maxstacksize < comp->nextreg+nbrvars)
				comp->method->maxstacksize = comp->nextreg+nbrvars;
		}
	}
	return svLocalVars;
}

/* Raise method's max stack size if register is above it */
void genMaxStack(CompInfo *comp, AuintIdx reg) {
	if (comp->method->maxstacksize < reg)
		comp->method->maxstacksize = reg+1;
}

/** Get a node from an AST segment */
#define astGet(th, astseg, idx) (arrGet(th, astseg, idx))

void genExp(CompInfo *comp, Value astseg);
void genStmts(CompInfo *comp, Value astseg);
void genDoProp(CompInfo *comp, Value astseg, char byteop, Value rval, int nexpected);

/** Return next available register to load values into */
unsigned int genNextReg(CompInfo *comp) {
	// Keep track of high-water mark for later stack allocation purposes
	if (comp->method->maxstacksize < comp->nextreg+1)
		comp->method->maxstacksize = comp->nextreg+1;
	return comp->nextreg++;
}

/** Return register number for expression (if it already is one), otherwise return -1 */
int genExpReg(CompInfo *comp, Value astseg) {
	Value th = comp->th;
	if (isSym(astseg)) {
		if (vmlit(SymThis) == astseg)
			return comp->thisreg;
		else if (vmlit(SymSelf) == astseg)
			return 0;
	} else {
		Value op = astGet(th, astseg, 0);
		if (vmlit(SymLocal) == op)
			return findLocalVar(comp, astGet(th, astseg, 1));
		else
			return -1;
	}
	return -1;
}

/** Get the destination where Jump is going */
int genGetJump(CompInfo *comp, int ip) {
	int offset = bc_j(comp->method->code[ip]);
	if (offset == BCNO_JMP)  /* point to itself represents end of list */
		return BCNO_JMP;  /* end of list */
	else
		return (ip+1)+offset;  /* turn offset into absolute position */
}

/** Set the Jump instruction at ip to jump to dest instruction */
void genSetJump(CompInfo *comp, int ip, int dest) {
	if (ip==BCNO_JMP)
		return;
	Instruction *jmp = &comp->method->code[ip];
	int offset = dest-(ip+1);
	assert(dest != BCNO_JMP);
	if (((offset+BCBIAS_J) >> 16)!=0)
		assert(0 && "control structure too long");
	*jmp = setbc_j(*jmp, offset);
}

/* Set the jump instruction link chain starting at listip to jump to dest */
void genSetJumpList(CompInfo *comp, int listip, int dest) {
	while (listip != BCNO_JMP) {
		int next = genGetJump(comp, listip);
		genSetJump(comp, listip, dest);
		listip = next;
	}
}

/** Generate a jump that goes forward, possibly as part of an jump chain */
void genFwdJump(CompInfo *comp, int op, int reg, int *ipchain) {
	// If part of a jmp chain, add this jump to the chain
	if (*ipchain != BCNO_JMP) {
		// Find last jump in chain
		int jumpip;
		int nextip = *ipchain;
		do {
			jumpip = nextip;
			nextip = genGetJump(comp, jumpip);
		} while (nextip != BCNO_JMP);
		// Fix it to point to jump we are about to generate
		genSetJump(comp, jumpip, comp->method->size);
	}
	else
		*ipchain = comp->method->size; // New chain starts with this jump
	genAddInstr(comp, BCINS_AJ(op, reg, BCNO_JMP));
}

/** Generate conditional tests & appropriate jump(s), handled recursively for boolean operators.
	failjump is ip for first jump past the code to run on condition's success.
	passjump is ip for first jump directly to condition's success.
	notflag is true if under influence of 'not' operator: reversing jumps and and/or.
	lastjump specifies how last jump should behave: true for fail jump, false for passjump. true reverses jump condition. */
void genJumpExp(CompInfo *comp, Value astseg, int *failjump, int *passjump, bool notflag, bool lastjump) {
	Value th = comp->th;
	unsigned int svnextreg = comp->nextreg;
	Value condop = isArr(astseg)? astGet(th, astseg, 0) : astseg;
	bool revjump = notflag ^ lastjump;		// Reverse jump based on not flag and lastjump

	// Comparison ops (e.g., == or <) based on rocket operator - generation code comes later.
	int jumpop;
	if (condop == vmlit(SymLt)) jumpop = revjump? OpJGeN : OpJLt;
	else if (condop == vmlit(SymLe)) jumpop = revjump? OpJGtN : OpJLe;
	else if (condop == vmlit(SymGt)) jumpop = revjump? OpJLeN : OpJGt;
	else if (condop == vmlit(SymGe)) jumpop = revjump? OpJLtN : OpJGe;
	else if (condop == vmlit(SymEq)) jumpop = revjump? OpJNeN : OpJEq;
	else if (condop == vmlit(SymNe)) jumpop = revjump? OpJEqN : OpJNe;

	// '===' exact equivalence
	else if (condop == vmlit(SymEquiv)) {
		genExp(comp, astGet(th, astseg, 1));
		Value arg2 = astGet(th, astseg, 2);
		if (isArr(arg2) && astGet(th, arg2, 0)==vmlit(SymLit) && astGet(th, arg2, 1)==aNull) {
			genFwdJump(comp, revjump? OpJNNull : OpJNull, svnextreg, lastjump? failjump : passjump);
		}
		else {
			genExp(comp, arg2);
			genFwdJump(comp, revjump? OpJDiff : OpJSame, svnextreg, lastjump? failjump : passjump);
		}
		comp->nextreg = svnextreg;
		return;
	}

	// '~~' pattern match
	else if (condop == vmlit(SymMatchOp)) {
		genAddInstr(comp, BCINS_ABx(OpLoadLit, genNextReg(comp), genAddLit(comp, vmlit(SymMatchOp))));
		genExp(comp, astGet(th, astseg, 2)); // '~~' uses right hand value for object call
		genExp(comp, astGet(th, astseg, 1));
		genAddInstr(comp, BCINS_ABC(OpGetCall, svnextreg, comp->nextreg - svnextreg-1, 1));
		genFwdJump(comp, revjump? OpJFalse : OpJTrue, svnextreg,  lastjump? failjump : passjump);
		comp->nextreg = svnextreg;
		return;
	}

	else if (condop == vmlit(SymNot)) {
		genJumpExp(comp, astGet(th, astseg, 1), failjump, passjump, !notflag, lastjump);
		return;
	}

	else if (condop == vmlit(SymOr) || condop == vmlit(SymAnd)) {
		bool isAnd = (condop == vmlit(SymAnd)) ^ notflag; // Treat it as 'And' (or 'Or')?
		AuintIdx segi = 1;
		if (isAnd) {
			while (segi < getSize(astseg)-1) {
				genJumpExp(comp, astGet(th, astseg, segi++), failjump, passjump, notflag, true);
			}
			genJumpExp(comp, astGet(th, astseg, segi), failjump, passjump, notflag, lastjump);
			return;
		}
		else {
			int newpassjump = BCNO_JMP;
			while (segi < getSize(astseg)-1) {
				int newfailjump = BCNO_JMP;
				genJumpExp(comp, astGet(th, astseg, segi++), &newfailjump, &newpassjump, notflag, false);
				genSetJump(comp, newfailjump, comp->method->size);
			}
			genJumpExp(comp, astGet(th, astseg, segi), failjump, &newpassjump, notflag, lastjump);
			genSetJumpList(comp, newpassjump, comp->method->size); // Fix 'or' jumps to here
			return;
		}
	}

	// Otherwise, an expression to be interpreted as false/null or true (anything else)
	// (which includes explicit use of <==>)
	else {
		genExp(comp, astseg);
		genFwdJump(comp, revjump? OpJFalse : OpJTrue, svnextreg,  lastjump? failjump : passjump);
		comp->nextreg = svnextreg;
		return;
	}

	// Generate code for rocket-based comparisons
	genAddInstr(comp, BCINS_ABx(OpLoadLit, genNextReg(comp), genAddLit(comp, vmlit(SymRocket))));
	genExp(comp, astGet(th, astseg, 1));
	genExp(comp, astGet(th, astseg, 2));
	genAddInstr(comp, BCINS_ABC(OpGetCall, svnextreg, comp->nextreg - svnextreg-1, 1));
	genFwdJump(comp, jumpop, svnextreg,  lastjump? failjump : passjump);
	comp->nextreg = svnextreg;
}

/** Generate return or yield */
void genReturn(CompInfo *comp, Value aststmt, int op, int expected) {
	Value th = comp->th;
	AuintIdx svnextreg = comp->nextreg;
	Value retexp = astGet(th, aststmt, 1);
	if (retexp==aNull)
		genAddInstr(comp, BCINS_ABC(op, 0, 0, expected)); // return with no values
	else {
		int reg = genExpReg(comp, retexp);
		// Return from a local variable registers
		if (reg>=0)
			genAddInstr(comp, BCINS_ABC(op, reg, 1, expected));
		// Do tail call if we are calling another method as the return value
		else if (op==OpReturn && isArr(retexp) && astGet(th, retexp, 0)==vmlit(SymCallProp))
			genDoProp(comp, retexp, OpTailCall, aNull, 1);
		// For solo splat, load parameter varargs and return them
		else if (retexp == vmlit(SymSplat)) {
			genAddInstr(comp, BCINS_ABC(OpLoadVararg, svnextreg, 0xFF, 0));
			genAddInstr(comp, BCINS_ABC(op, svnextreg, 0xFF, expected));
		}
		// For comma-separated rvals, special handling in case ... splat appears (at end)
		else if (isArr(retexp) && arrGet(th, retexp, 0)==vmlit(SymComma)) {
			int nvals = arr_size(retexp)-1;
			bool varrvals = false;
			for (int i=1; i<=nvals; i++) {
				Value rvali = astGet(th, retexp, i);
				if (i==nvals && rvali==vmlit(SymSplat)) {
					genAddInstr(comp, BCINS_ABC(OpLoadVararg, genNextReg(comp), 0xFF, 0));
					varrvals = true;
				}
				else if (i==nvals && isArr(rvali) && astGet(th, rvali, 0)==vmlit(SymYield)) {
					genReturn(comp, rvali, OpYield, 0xFF);
					varrvals = true;
				}
				else
					genExp(comp, rvali);
			}
			genAddInstr(comp, BCINS_ABC(op, svnextreg, varrvals? 0xFF : comp->nextreg - svnextreg, expected));
		}
		// Return calculated values on stack
		else {
			genExp(comp, retexp);
			genAddInstr(comp, BCINS_ABC(op, svnextreg, comp->nextreg - svnextreg, expected));
		}
	}
	comp->nextreg = svnextreg;
}

/** Return nonzero opcode if ast operator is a property/method call */
char genIsProp(Value th, Value op, int setflag) {
	if (vmlit(SymActProp) == op)
		return setflag? OpSetActProp : OpGetActProp;
	else if (vmlit(SymRawProp) == op)
		return setflag? OpSetProp : OpGetProp;
	else if (vmlit(SymCallProp) == op)
		return setflag? OpSetCall : OpGetCall;
	return 0;
}

/** Generate code for some kind of property/method call.
    rval is aNull for 'get' mode and either a register integer or ast segment for 'set' mode.
	nexpected specifies how many return values expected from called method */
void genDoProp(CompInfo *comp, Value astseg, char byteop, Value rval, int nexpected) {
	Value th = comp->th;
	unsigned int svreg = comp->nextreg; // Save

	// <<<< optimize here by seeing if property is a std symbol and self is in register

	genExp(comp, astGet(th, astseg, 2)); // property
	genExp(comp, astGet(th, astseg, 1)); // self

	// Handle value to be set (if provided) as first parameter
	if (isInt(rval)) // already loaded into a register
		genAddInstr(comp, BCINS_ABC(OpLoadReg, genNextReg(comp), toAint(rval), 0));
	else if (rval!=aNull) {
		AuintIdx rvalreg = comp->nextreg;
		genExp(comp, rval); // Load into next available register
		comp->nextreg = rvalreg+1;
	}

	// Load as many parameters as we have, then do property get
	bool varparms = false;
	for (AuintIdx i = 3; i<getSize(astseg); i++) {
		AuintIdx rvalreg = comp->nextreg;
		Value parm = astGet(th, astseg, i);
		if (parm == vmlit(SymSplat)) {
			genAddInstr(comp, BCINS_ABC(OpLoadVararg, rvalreg, 0xFF, 0));
			varparms = true;
			break;
		}
		else if (i==getSize(astseg)-1 && isArr(parm) && arrGet(th, parm, 0)==vmlit(SymYield)) {
			genReturn(comp, parm, OpYield, 0xFF);
			varparms = true;
			break;
		}
		else {
			genExp(comp, parm);
			comp->nextreg = rvalreg+1;
		}
	}
	genAddInstr(comp, BCINS_ABC(byteop, svreg, varparms? 0xFF : comp->nextreg - svreg-1, nexpected));
	comp->nextreg = svreg+1;
}

/** Generate code for an assignment */
void genAssign(CompInfo *comp, Value lval, Value rval) {
	Value th = comp->th;
	Value lvalop = isArr(lval)? astGet(th, lval, 0) : aNull;

	// Handle assignment to property or method
	char opcode = genIsProp(th, lvalop, true);
	if (opcode)
		genDoProp(comp, lval, opcode, rval, 1);
	else {
		// Handle parallel, local, closure, variable assignments where rval is loaded first
		int nlvals = lvalop==vmlit(SymComma)? arr_size(lval)-1 : 1;
		bool varrvals = false;
		AuintIdx rvalreg;
		if (isInt(rval))
			rvalreg = toAint(rval); // rval is already in a register, so use that reg
		else {
			// Special handling for right-hand values for parallel assignment
			rvalreg = comp->nextreg; // Save where we put rvals
			int opcode;
			// For method call, specify expected number of return values
			if (isArr(rval) && (opcode = genIsProp(th, astGet(th, rval, 0), false))) {
				genDoProp(comp, rval, opcode, aNull, nlvals);
				varrvals = true;
			}
			else if (isArr(rval) && arrGet(th, rval, 0)==vmlit(SymYield)) {
				genReturn(comp, rval, OpYield, nlvals);
				varrvals = true;
			}
			// For solo splat, load needed number from parameter varargs
			else if (rval == vmlit(SymSplat)) {
				genAddInstr(comp, BCINS_ABC(OpLoadVararg, genNextReg(comp), nlvals, 0));
				varrvals = true;
			}
			// For comma-separated rvals, special handling in case ... splat appears (at end)
			else if (nlvals>1 && isArr(rval) && arrGet(th, rval, 0)==vmlit(SymComma)) {
				int nvals = arr_size(rval)-1;
				for (int i=1; i<=nvals; i++) {
					Value rvali = astGet(th, rval, i);
					if (i==nvals && i<=nlvals && rvali==vmlit(SymSplat)) {
						genAddInstr(comp, BCINS_ABC(OpLoadVararg, genNextReg(comp), nlvals-i+1, 0));
						varrvals = true;
					}
					else
						genExp(comp, rvali);
				}
			}
			else
				genExp(comp, rval);
		}
		// Handle parallel assignment for lvals
		if (vmlit(SymComma) == lvalop) {
			int nrneed = varrvals? 0 : nlvals - (comp->nextreg - rvalreg);
			// Ensure we fill up right values with nulls to as high as left values
			if (nrneed > 0) {
				genAddInstr(comp, BCINS_ABC(OpLoadNulls, comp->nextreg, nrneed, 0));
				comp->nextreg += nrneed;
				// Keep track of high-water mark for later stack allocation purposes
				if (comp->method->maxstacksize < comp->nextreg+nrneed)
					comp->method->maxstacksize = comp->nextreg+nrneed;
			}
			// Assign each lval, one at a time, from corresponding loaded rval in a register
			for (int i = 0; i<nlvals; i++) {
				genAssign(comp, astGet(th, lval, i+1), anInt(rvalreg+i));
			}
		}
		// Load into local or closure variable
		else if (vmlit(SymLocal) == lvalop) {
			Value symnm = astGet(th, lval, 1);
			int localreg = findLocalVar(comp, symnm);
			if (localreg != -1)
				genAddInstr(comp, BCINS_ABC(OpLoadReg, localreg, rvalreg, 0));
			else if ((localreg = findClosureVar(comp, symnm))!=-1)
				genAddInstr(comp, BCINS_ABC(OpSetClosure, localreg, rvalreg, 0));
		// Load into a variable
		} else if (vmlit(SymGlobal) == lvalop)
			genAddInstr(comp, BCINS_ABx(OpSetGlobal, rvalreg, genAddLit(comp, astGet(th, lval, 1))));
	}
	// We do not consume values, so we don't restore comp->nextreg
}

/** Generate optimized code for assignment when it is just a statement and
    its right-hand values do not have to be put on stack */
void genOptAssign(CompInfo *comp, Value lval, Value rval) {
	Value th = comp->th;
	Value lvalop = astGet(th, lval, 0);

	// Handle assignments that require we load rval (and other stuff) first
	unsigned int fromreg = genExpReg(comp, rval);
	if (vmlit(SymLocal) == lvalop) {
		Value symnm = astGet(th, lval, 1);
		int localreg = findLocalVar(comp, symnm);
		if (localreg != -1) {
			// Optimize load straight into register, if possible (this, self, local var)
			if (fromreg!=-1)
				genAddInstr(comp, BCINS_ABC(OpLoadReg, localreg, fromreg, 0));
			else if (vmlit(SymBaseurl) == rval)
				genAddInstr(comp, BCINS_ABx(OpLoadLit, localreg, genAddLit(comp, comp->lex->url)));
			else {
				Value rvalop = astGet(th, rval, 0);
				if (vmlit(SymLit) == rvalop) {
					Value litval = astGet(th, rval, 1);
					if (litval==aNull)
						genAddInstr(comp, BCINS_ABC(OpLoadPrim, localreg, 0, 0));
					else if (litval==aFalse)
						genAddInstr(comp, BCINS_ABC(OpLoadPrim, localreg, 1, 0));
					else if (litval==aTrue)
						genAddInstr(comp, BCINS_ABC(OpLoadPrim, localreg, 2, 0));
					else
						genAddInstr(comp, BCINS_ABx(OpLoadLit, localreg, genAddLit(comp, litval)));
				} else if (vmlit(SymLocal) == rvalop) {
					// We did local already - this must be a load from a closure variable
					genAddInstr(comp, BCINS_ABC(OpGetClosure, localreg, findClosureVar(comp, astGet(th, rval, 1)), 0));
				} else if (vmlit(SymGlobal) == rvalop) {
					genAddInstr(comp, BCINS_ABx(OpGetGlobal, localreg, genAddLit(comp, astGet(th, rval, 1))));
				} else {
					fromreg = comp->nextreg; // Save where we put rvals
					genExp(comp, rval);
					genAddInstr(comp, BCINS_ABC(OpLoadReg, localreg, fromreg, 0));
				}
			}
		}
		else if ((localreg = findClosureVar(comp, symnm))!=-1) {
			fromreg = comp->nextreg; // Save where we put rvals
			genExp(comp, rval);
			genAddInstr(comp, BCINS_ABC(OpSetClosure, localreg, fromreg, 0));
		}
	} else if (vmlit(SymGlobal) == lvalop) {
		if (fromreg != -1)
			genAddInstr(comp, BCINS_ABx(OpSetGlobal, fromreg, genAddLit(comp, astGet(th, lval, 1))));
		else {
			fromreg = comp->nextreg; // Save where we put rvals
			genExp(comp, rval);
			genAddInstr(comp, BCINS_ABx(OpSetGlobal, fromreg, genAddLit(comp, astGet(th, lval, 1))));
		}
	} else
		genAssign(comp, lval, rval);
}

/** Return true if the expression makes no use of any logical or comparative operators */
bool hasNoBool(Value th, Value astseg) {
	for (AuintIdx segi = 1; segi < getSize(astseg)-1; segi++) {
		Value op = astGet(th, astseg, segi);
		op = isArr(op)? astGet(th, op, 0) : op;
		if (vmlit(SymAnd)==op || vmlit(SymOr)==op || vmlit(SymNot)==op
			|| vmlit(SymEquiv) == op || vmlit(SymMatchOp) == op
			|| vmlit(SymEq)==op || vmlit(SymNe)==op
			|| vmlit(SymGt)==op || vmlit(SymGe)==op || vmlit(SymLt)==op || vmlit(SymLe)==op)
			return false;
	}
	return true;
}

/** Generate the appropriate code for something that places one or more values on the stack
	beginning at comp->nextreg (which should be saved before calling this). The last value is at comp->nextreg-1 */
void genExp(CompInfo *comp, Value astseg) {
	Value th = comp->th;
	if (isSym(astseg)) {
		if (vmlit(SymThis) == astseg)
			genAddInstr(comp, BCINS_ABC(OpLoadReg, genNextReg(comp), comp->thisreg, 0));
		else if (vmlit(SymSelf) == astseg)
			genAddInstr(comp, BCINS_ABC(OpLoadReg, genNextReg(comp), 0, 0));
		else if (vmlit(SymContext) == astseg)
			genAddInstr(comp, BCINS_ABC(OpLoadContext, genNextReg(comp), 0, 0));
		else if (vmlit(SymSelfMeth) == astseg)
			genAddInstr(comp, BCINS_ABC(OpLoadContext, genNextReg(comp), 1, 0));
		else if (vmlit(SymBaseurl) == astseg)
			genAddInstr(comp, BCINS_ABx(OpLoadLit, genNextReg(comp), genAddLit(comp, comp->lex->url)));
		else if (vmlit(SymSplat) == astseg)
			genAddInstr(comp, BCINS_ABC(OpLoadVararg, genNextReg(comp), 1, 0)); // By default, only get one value
	} else if (isArr(astseg)) {
		Value op = astGet(th, astseg, 0);
		char opcode = genIsProp(th, op, false);
		if (opcode) // Property or method use
			genDoProp(comp, astseg, opcode, aNull, 1);
		else if (vmlit(SymComma) == op) {
			int nvals = arr_size(astseg)-1;
			for (int i=1; i<=nvals; i++)
				genExp(comp, astGet(th, astseg, i));
		} else if (vmlit(SymLit) == op) {
			Value litval = astGet(th, astseg, 1);
			if (litval==aNull)
				genAddInstr(comp, BCINS_ABC(OpLoadPrim, genNextReg(comp), 0, 0));
			else if (litval==aFalse)
				genAddInstr(comp, BCINS_ABC(OpLoadPrim, genNextReg(comp), 1, 0));
			else if (litval==aTrue)
				genAddInstr(comp, BCINS_ABC(OpLoadPrim, genNextReg(comp), 2, 0));
			else
				genAddInstr(comp, BCINS_ABx(OpLoadLit, genNextReg(comp), genAddLit(comp, litval)));
		} else if (vmlit(SymExt) == op) {
			genAddInstr(comp, BCINS_ABx(OpLoadLit, genNextReg(comp), toAint(astGet(th, astseg, 1))));
		} else if (vmlit(SymLocal) == op) {
			Value symnm = astGet(th, astseg, 1);
			Aint idx;
			if ((idx = findLocalVar(comp, symnm))!=-1)
				genAddInstr(comp, BCINS_ABC(OpLoadReg, genNextReg(comp), idx, 0));
			else if ((idx = findClosureVar(comp, symnm))!=-1)
				genAddInstr(comp, BCINS_ABC(OpGetClosure, genNextReg(comp), idx, 0));
		} else if (vmlit(SymGlobal) == op) {
			genAddInstr(comp, BCINS_ABx(OpGetGlobal, genNextReg(comp), genAddLit(comp, astGet(th, astseg, 1))));
		} else if (vmlit(SymAssgn) == op) {
			genAssign(comp, astGet(th, astseg, 1), astGet(th, astseg, 2));
		} else if (vmlit(SymYield) == op) {
			genReturn(comp, astseg, OpYield, 1);
		} else if (vmlit(SymClosure) == op) {
			Value newcloseg = astGet(th, astseg, 2);
			// If no closure variables nor set method, don't generate closure, just the 'get' method
			Value setmethseg = astGet(th, newcloseg, 4);
			if (arr_size(newcloseg)==5 && isArr(setmethseg) && astGet(th, setmethseg, 1)==vmlit(SymNull))
				genExp(comp, astGet(th, newcloseg, 3));
			else
				genExp(comp, newcloseg);
		} else if (vmlit(SymOrAssgn) == op) {
			// Assumes that lvar is a local variable
			assert(astGet(th, astGet(th, astseg, 1), 0)==vmlit(SymLocal));
			int varreg = findLocalVar(comp, astGet(th, astGet(th, astseg, 1), 1));
			int jumpip = BCNO_JMP;
			genFwdJump(comp, OpJNNull, varreg, &jumpip);
			Value valseg = astGet(th, astseg, 2);
			int reg = genExpReg(comp, astseg);
			if (reg>=0)
				genAddInstr(comp, BCINS_ABC(OpLoadReg, varreg, reg, 0));
			else if (isArr(valseg) && astGet(th, valseg, 0) == vmlit(SymLit))
				genAddInstr(comp, BCINS_ABx(OpLoadLit, varreg, genAddLit(comp, astGet(th, valseg, 1))));
			else {
				AuintIdx rreg = comp->nextreg; // Save where we put rvals
				genExp(comp, valseg);
				genAddInstr(comp, BCINS_ABC(OpLoadReg, varreg, rreg, 0));
			}
			genSetJumpList(comp, jumpip, comp->method->size);
		} else if (vmlit(SymThisBlock) == op) {
			unsigned int svthis = comp->thisreg;
			unsigned int svthisopreg = comp->thisopreg;
			comp->thisopreg = 0;
			// Generate "using" operator, if specified
			Value thisop = astGet(th, astseg, 3);
			if (thisop != aNull) {
				comp->thisopreg = comp->nextreg;
				genExp(comp, thisop);
			}
			// Generate 'this' value
			int thisreg = comp->nextreg;
			genExp(comp, astGet(th, astseg, 1));
			comp->nextreg = thisreg+1; // Only use first value
			comp->thisreg = thisreg;
			// Optimize "using" operator to a method
			if (thisop != aNull)
				genAddInstr(comp, BCINS_ABC(OpGetMeth, comp->thisopreg, 0, 0));
			Value svLocalVars = genLocalVars(comp, astGet(th, astseg, 2), 0);
			genStmts(comp, astGet(th, astseg, 4));
			// Value of a this block is 'this'. Needed for returns or this blocks within this blocks.
			if (thisop != aNull) {
				// Move 'this' down, so its value is in the right place
				genAddInstr(comp, BCINS_ABC(OpLoadReg, comp->thisopreg, comp->thisreg, 0));
				comp->nextreg = comp->thisreg;
			}
			else
				comp->nextreg = comp->thisreg+1;
			comp->locvarseg = svLocalVars;
			comp->thisopreg = svthisopreg;
			comp->thisreg = svthis;
		} else if (vmlit(SymQuestion) == op) { // Ternary
			int svnextreg = comp->nextreg;
			int failjump = BCNO_JMP;
			int passjump = BCNO_JMP;
			genJumpExp(comp, astGet(th, astseg, 1), &failjump, NULL, false, true);
			int nextreg = genNextReg(comp);
			comp->nextreg = svnextreg;
			genExp(comp, astGet(th, astseg, 2));
			genFwdJump(comp, OpJump, 0, &passjump);
			genSetJumpList(comp, failjump, comp->method->size);
			comp->nextreg = svnextreg;
			genExp(comp, astGet(th, astseg, 3));
			genSetJumpList(comp, passjump, comp->method->size);
		} else if ((vmlit(SymOr)==op || vmlit(SymAnd)==op) && hasNoBool(th, astseg)) {
			// 'Pure' and/or conditional processing
			int svnextreg = comp->nextreg;
			int jumpip = BCNO_JMP;
			AuintIdx segi;
			for (segi = 1; segi < getSize(astseg)-1; segi++) {
				genExp(comp, astGet(th, astseg, segi));
				comp->nextreg = svnextreg;
				genFwdJump(comp, op==vmlit(SymOr)? OpJTrue : OpJFalse, svnextreg, &jumpip);
			}
			genExp(comp, astGet(th, astseg, segi));
			genSetJumpList(comp, jumpip, comp->method->size);
		} else if (vmlit(SymAnd)==op || vmlit(SymOr)==op || vmlit(SymNot)==op
			|| vmlit(SymEquiv) == op || vmlit(SymMatchOp) == op
			|| vmlit(SymEq)==op || vmlit(SymNe)==op
			|| vmlit(SymGt)==op || vmlit(SymGe)==op || vmlit(SymLt)==op || vmlit(SymLe)==op)
		{
			// Conditional/boolean expression, resolved to 'true' or 'false'
			int failjump = BCNO_JMP;
			genJumpExp(comp, astseg, &failjump, NULL, false, true);
			int nextreg = genNextReg(comp);
			genAddInstr(comp, BCINS_ABC(OpLoadPrim, nextreg, 2, 0));
			genAddInstr(comp, BCINS_AJ(OpJump, 0, 1));
			genSetJumpList(comp, failjump, comp->method->size);
			genAddInstr(comp, BCINS_ABC(OpLoadPrim, nextreg, 1, 0));
		}
	}
	return;
}

/** Generate all if/elif/else blocks */
void genIf(CompInfo *comp, Value astseg) {
	Value th = comp->th;

	int jumpEndIp = BCNO_JMP;	// Instruction pointer to first jump to end of if

	// Process all condition/blocks in astseg
	AuintIdx ifindx = 1;		// Index into astseg for each cond/block group
	do {
		unsigned int savereg = comp->nextreg;
		// Generate conditional jump for bypassing block on condition failure
		Value condast = astGet(th, astseg, ifindx);
		Value svLocalVars = genLocalVars(comp, astGet(th, astseg, ifindx+1), 0);
		int jumpNextIp = BCNO_JMP;	// Instruction pointer to jump to next elif/else block
		if (condast != vmlit(SymElse)) {
			unsigned int condreg = comp->nextreg;
			genJumpExp(comp, condast, &jumpNextIp, NULL, false, true);
			comp->nextreg = condreg;
		}
		genStmts(comp, astGet(th, astseg, ifindx+2)); // Generate block
		// Generate/fix jumps after clause's block
		if (condast != vmlit(SymElse)) {
			if (ifindx+2 < getSize(astseg))
				genFwdJump(comp, OpJump, 0, &jumpEndIp);
			genSetJumpList(comp, jumpNextIp, comp->method->size); // Fix jumps to next elif/else block
		}
		comp->locvarseg = svLocalVars;
		comp->nextreg = savereg;
		ifindx += 3;
	} while (ifindx < getSize(astseg));
	genSetJumpList(comp, jumpEndIp, comp->method->size); // Fix jumps to end of 'if'
}

/* Generate specific match call */
void genMatchWith(CompInfo *comp, Value pattern, unsigned int matchreg, int nexpected) {
	// pattern '~~' matchval
	comp->nextreg = matchreg+2;
	genAddInstr(comp, BCINS_ABC(OpLoadReg, genNextReg(comp), matchreg+1, 0));
	genExp(comp, pattern);
	comp->nextreg = matchreg+4; // only want one value from genExp
	genAddInstr(comp, BCINS_ABC(OpLoadReg, genNextReg(comp), matchreg, 0));
	genAddInstr(comp, BCINS_ABC(OpGetCall, matchreg+2, 2, nexpected==0? 1 : nexpected));
}

/** Generate match block */
void genMatch(CompInfo *comp, Value astseg) {
	Value th = comp->th;

	int jumpEndIp = BCNO_JMP;	// Instruction pointer of first jump to end of match
	unsigned int matchreg = comp->nextreg;
	genExp(comp, astGet(th, astseg, 1));
	Value mtchmethexp = astGet(th, astseg, 2);
	if (mtchmethexp==vmlit(SymMatchOp))
		genAddInstr(comp, BCINS_ABx(OpLoadLit, genNextReg(comp), genAddLit(comp, mtchmethexp)));
	else
		genExp(comp, mtchmethexp);

	// Process all 'with' blocks in astseg
	AuintIdx mtchindx = 3;		// Index into astseg for each 'with' block
	while (mtchindx < getSize(astseg)) {
		comp->nextreg = matchreg+2;
		Value condast = astGet(th, astseg, mtchindx);
		int nexpected = toAint(astGet(th, astseg, mtchindx+2));
		// Perform match and then bypass block on failure
		int jumpNextIp = BCNO_JMP;	// Instruction pointer to jump past this block
		if (isArr(condast) && arrGet(th, condast, 0)==vmlit(SymComma)) {
			int jumpDoIp = BCNO_JMP;
			for (AuintIdx i=1; i<arr_size(condast); i++) {
				genMatchWith(comp, arrGet(th,condast,i), matchreg, nexpected);
				if (i==arr_size(condast)-1)
					genFwdJump(comp, OpJFalse, matchreg+2, &jumpNextIp);
				else
					genFwdJump(comp, OpJTrue, matchreg+2, &jumpDoIp);
			}
			genSetJumpList(comp, jumpDoIp, comp->method->size); // Fix jumps to block
		}
		else if (condast != vmlit(SymElse)) {
			genMatchWith(comp, condast, matchreg, nexpected);
			genFwdJump(comp, OpJFalse, matchreg+2, &jumpNextIp);
		}
		comp->nextreg = matchreg+2;
		Value svLocalVars = genLocalVars(comp, astGet(th, astseg, mtchindx+1), nexpected);
		genStmts(comp, astGet(th, astseg, mtchindx+3)); // Generate block
		// Generate/fix jumps after clause's block
		if (condast != vmlit(SymElse)) {
			if (mtchindx+2 < getSize(astseg))
				genFwdJump(comp, OpJump, 0, &jumpEndIp);
			genSetJumpList(comp, jumpNextIp, comp->method->size); // Fix jumps to next with/else block
		}
		comp->locvarseg = svLocalVars;
		mtchindx += 4;
	}
	genSetJumpList(comp, jumpEndIp, comp->method->size); // Fix jumps to end of 'match'
	comp->nextreg = matchreg;
}

/** Generate while block */
void genWhile(CompInfo *comp, Value astseg) {
	Value th = comp->th;
	unsigned int savereg = comp->nextreg;

	// Allocate block's local variables
	Value svLocalVars = genLocalVars(comp, astGet(th, astseg, 1), 0);

	// Perform conditional expression and jump
	int svJumpBegIp = comp->whileBegIp;
	int svJumpEndIp = comp->whileEndIp;
	comp->whileBegIp = comp->method->size;
	comp->whileEndIp = BCNO_JMP;
	genJumpExp(comp, astGet(th, astseg, 2), &comp->whileEndIp, NULL, false, true);

	// Generate block and jump to beginning. Fix conditional jump to after 'while' block
	genStmts(comp, astGet(th, astseg, 3)); // Generate block
	genAddInstr(comp, BCINS_AJ(OpJump, 0, comp->whileBegIp - comp->method->size-1));
	genSetJumpList(comp, comp->whileEndIp, comp->method->size); // Fix jump to end of 'while' block

	// Restore block's saved values
	comp->nextreg = savereg;
	comp->whileBegIp = svJumpBegIp;
	comp->whileEndIp = svJumpEndIp;
	comp->locvarseg = svLocalVars;
}

/** Generate each block */
void genEach(CompInfo *comp, Value astseg) {
	Value th = comp->th;
	unsigned int savereg = comp->nextreg;

	// Prepare iterator for 'each' block outside of main loop (loaded in savereg)
	Value iter = astGet(th, astseg, 3);
	if (iter == vmlit(SymSplat))
		genAddInstr(comp, BCINS_ABx(OpLoadLit, genNextReg(comp), genAddLit(comp, anInt(0))));
	else {
		int fromreg = genExpReg(comp, iter);
		if (fromreg==-1) {
			genExp(comp, iter);
			genAddInstr(comp, BCINS_ABC(OpEachPrep, savereg, savereg, 0));
		}
		else
			genAddInstr(comp, BCINS_ABC(OpEachPrep, genNextReg(comp), fromreg, 0));
	}

	// Allocate block's local variables
	Value svLocalVars = genLocalVars(comp, astGet(th, astseg, 1), 0);

	// Perform conditional expression and jump
	int svJumpBegIp = comp->whileBegIp;
	int svJumpEndIp = comp->whileEndIp;
	comp->whileBegIp = comp->method->size;
	comp->whileEndIp = BCNO_JMP;
	genAddInstr(comp, BCINS_ABC(iter == vmlit(SymSplat)? OpEachSplat : OpEachCall, savereg, 0, toAint(astGet(th, astseg,2))));
	genFwdJump(comp, OpJFalse, savereg+1, &comp->whileEndIp);

	// Generate block and jump to beginning. Fix conditional jump to after 'while' block
	genStmts(comp, astGet(th, astseg, 4)); // Generate block
	genAddInstr(comp, BCINS_AJ(OpJump, 0, comp->whileBegIp - comp->method->size-1));
	genSetJumpList(comp, comp->whileEndIp, comp->method->size); // Fix jump to end of 'while' block

	// Restore block's saved values
	comp->nextreg = savereg;
	comp->whileBegIp = svJumpBegIp;
	comp->whileEndIp = svJumpEndIp;
	comp->locvarseg = svLocalVars;
}

/** Generate do block */
void genDo(CompInfo *comp, Value astseg) {
	Value th = comp->th;
	unsigned int savereg = comp->nextreg;
	unsigned int lowreg, highreg;

	Value svLocalVars = genLocalVars(comp, astGet(th, astseg, 1), 0);
	Value exp = astGet(th, astseg, 2);
	if (exp!=aNull) {
		lowreg = comp->nextreg;
		genExp(comp, exp);
		highreg = comp->nextreg;
		for (unsigned int reg=lowreg; reg<highreg; reg++) {
			genAddInstr(comp, BCINS_ABx(OpLoadLit, genNextReg(comp), genAddLit(comp, vmlit(SymBegin))));
			genAddInstr(comp, BCINS_ABC(OpLoadReg, genNextReg(comp), reg, 0));
			genAddInstr(comp, BCINS_ABC(OpGetCall, highreg, 1, 0));
		}
	}
	genStmts(comp, astGet(th, astseg, 3));
	if (exp!=aNull) {
		comp->nextreg = highreg;
		for (unsigned int reg=highreg-1; reg>=lowreg; reg--) {
			genAddInstr(comp, BCINS_ABx(OpLoadLit, genNextReg(comp), genAddLit(comp, vmlit(SymEnd))));
			genAddInstr(comp, BCINS_ABC(OpLoadReg, genNextReg(comp), reg, 0));
			genAddInstr(comp, BCINS_ABC(OpGetCall, highreg, 1, 0));
		}
	}

	// Restore block's saved values
	comp->nextreg = savereg;
	comp->locvarseg = svLocalVars;
}

/** Generate a statement */
void genStmt(CompInfo *comp, Value aststmt) {
	Value th = comp->th;
	AuintIdx svnextreg = comp->nextreg;

	// Set up a call for every statement
	AuintIdx svthisopreg;
	if (comp->thisopreg != 0) {
		svthisopreg = comp->nextreg;
		// We have to copy this+method, because the method's tail call may destroy them
		genAddInstr(comp, BCINS_ABC(OpLoadRegs, genNextReg(comp), comp->thisopreg, 2));
		comp->nextreg++;
	}

	// Handle various kinds of statements
	Value op = isArr(aststmt)? astGet(th, aststmt, 0) : aststmt;
	if (op==vmlit(SymIf)) genIf(comp, aststmt);
	else if (op==vmlit(SymMatch)) genMatch(comp, aststmt);
	else if (op==vmlit(SymWhile)) genWhile(comp, aststmt);
	else if (op==vmlit(SymEach)) genEach(comp, aststmt);
	else if (op==vmlit(SymDo)) genDo(comp, aststmt);
	else if (op==vmlit(SymBreak) && comp->whileBegIp!=-1)
		genFwdJump(comp, OpJump, 0, &comp->whileEndIp);
	else if (op==vmlit(SymContinue) && comp->whileBegIp!=-1)
		genAddInstr(comp, BCINS_AJ(OpJump, 0, comp->whileBegIp - comp->method->size-1));
	else if (op==vmlit(SymReturn))
		genReturn(comp, aststmt, OpReturn, 0);
	else if (op==vmlit(SymYield))
		genReturn(comp, aststmt, OpYield, 0);
	else if (op==vmlit(SymAssgn))
		genOptAssign(comp, astGet(th, aststmt,1), astGet(th, aststmt,2));
	else
		genExp(comp, aststmt);

	// Finish append (or other this op)
	if (comp->thisopreg != 0)
		genAddInstr(comp, BCINS_ABC(OpGetCall, svthisopreg, comp->nextreg - svthisopreg-1, 0));

	comp->nextreg = svnextreg;
}

/** Generate one or a sequence of statements */
void genStmts(CompInfo *comp, Value astseg) {
	Value th = comp->th;
	if (isArr(astseg) && astGet(comp->th, astseg, 0)==vmlit(SymSemicolon)) {
		for (AuintIdx i=1; i<getSize(astseg); i++) {
			genStmt(comp, astGet(comp->th, astseg, i));
		}
	}
	else
		genStmt(comp, astseg);
}

#define astAddValue(th, astseg, val) (arrAdd(th, astseg, val))
Value astAddSeg(Value th, Value oldseg, Value astop, AuintIdx size);
Value astAddSeg2(Value th, Value oldseg, Value astop, Value val);
Value astInsSeg(Value th, Value oldseg, Value astop, AuintIdx size);

/** Recursively turn a method's implicit returns in the AST into explicit returns */
void genFixReturns(CompInfo *comp, Value aststmts) {
	Value th = comp->th;
	if (!isArr(aststmts) || astGet(th, aststmts, 0)!=vmlit(SymSemicolon)) {
		vmLog("A method's block is not properly formed (should use ';' AST)");
		return;
	}
	Value laststmt = astGet(th, aststmts, arr_size(aststmts)-1);
	Value lastop = isArr(laststmt)? astGet(th, laststmt, 0) : laststmt;
	// Implicit return for loops is to return 'null' afterwards
	if (lastop==vmlit(SymWhile) || lastop==vmlit(SymEach) || lastop==vmlit(SymDo)
			|| lastop==vmlit(SymYield) || lastop==vmlit(SymBreak) || lastop==vmlit(SymContinue))
		astAddSeg2(th, aststmts, vmlit(SymReturn), aNull);
	// Implicit return for 'if'
	else if (lastop==vmlit(SymIf) || lastop==vmlit(SymMatch)) {
		// Recursively handle implicit return for each clause's statement block
		int step = lastop==vmlit(SymMatch)? 4 : 3;
		Auint i = lastop==vmlit(SymMatch)? 6 : 3;
		for (; i<arr_size(laststmt); i+=step)
			genFixReturns(comp, astGet(th, laststmt, i));
		// If 'if' or 'match' has no 'else', add one that returns null
		if (astGet(th, laststmt, i-step-step+1)!=vmlit(SymElse)) {
			astAddValue(th, laststmt, vmlit(SymElse));
			astAddValue(th, laststmt, aNull);
			if (lastop==vmlit(SymMatch))
				astAddValue(th, laststmt, anInt(0));
			astAddSeg2(th, laststmt, vmlit(SymReturn), aNull);
		}
	}
	// Replace non-return expression statement with 'return' in front
	else if (lastop!=vmlit(SymReturn))
		astInsSeg(th, aststmts, vmlit(SymReturn), 2);
}

/* Generate a complete byte-code method by walking the
 * Abstract Syntax Tree generated by the parser */
void genBMethod(CompInfo *comp) {
	Value th = comp->th;
	// AST: ('method', localvars, closurevars, parminitstmts, statements)
	// Initialize generation state for method
	comp->method->nbrexterns = comp->method->nbrlits;
	comp->nextreg = comp->method->maxstacksize = comp->method->nbrlocals;
	comp->thisreg = 0; // Starts with 'self'
	comp->thisopreg = 0;
	comp->locvarseg = astGet(comp->th, comp->ast, 1);
	arrSet(th, comp->locvarseg, 1, anInt(1));

	// If 'self' is bound to this closure, override passed self with it
	int idx;
	if ((idx = findClosureVar(comp, vmlit(SymSelf)))!=-1)
		genAddInstr(comp, BCINS_ABC(OpGetClosure, 0, idx, 0));

	// Generate the method's code based on AST
	int nbrnull = comp->method->nbrlocals - methodNParms(comp->method);
	if (nbrnull>0) // Initialize non-parm locals to null
		genAddInstr(comp, BCINS_ABC(OpLoadNulls, methodNParms(comp->method), nbrnull, 0));
	genStmts(comp, astGet(th, comp->ast, 2)); // Generate code for parameter defaults
	Value aststmts = astGet(th, comp->ast, 3);
	genFixReturns(comp, aststmts); // Turn implicit returns into explicit returns
	genStmts(comp, aststmts); // Generate method's code block
}

#ifdef __cplusplus
} // extern "C"
} // namespace avm
#endif