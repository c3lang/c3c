module acorn::parser;
import acorn::parser::ast;

/** Parser for Acorn compiler. See Acorn documentation for syntax diagrams.
 *
 * @file
 *
 * This source file is part of avm - Acorn Virtual Machine.
 * See Copyright Notice in avm.h
 */

/* Add a url literal and return its index */
bool resource_equal(Value res1, Value res2);

int genAddUrlLit(CompInfo *comp, Value val) {
	BMethodInfo* f = comp->method;

	// See if we already have resource with same url
	int i = f->nbrlits;
	while (i-- > 0)
		if (resource_equal(f->lits[i],val))
			return i;

	// If not found, add it
	mem_growvector(comp->th, f->lits, f->nbrlits, f->litsz, Value, INT_MAX);
	f->lits[f->nbrlits] = val;
	mem_markChk(comp->th, comp, val);
	return f->nbrlits++;
}

/* Add a method literal and return its index */
fn int CompInfo.genAddMethodLit(CompInfo *comp, Value val)
{
	BMethodInfo* f = comp.method;
	mem_growvector(comp->th, f->lits, f->nbrlits, f->litsz, Value, INT_MAX);
	f.lits[f.nbrlits] = val;
	mem_markChk(comp->th, comp, val);
	return f.nbrlits++;
}

/* Look for variable in locvars: return index if found, otherwise -1 */
int findBlockVar(Value th, Value locvars, Value varnm)
{
	int nbrlocals = arr_size(locvars);
	for (int idx = nbrlocals - 1; idx > 1; idx--) {
		if (arrGet(th, locvars, idx) == varnm)
			return idx-2+toAint(arrGet(th, locvars, 1)); // relative to base index
	}
	return -1;
}

/* Look for local variable. Returns idx if found, -1 otherwise. */
fn int CompInfo.findLocalVar(CompInfo *comp, Value varnm) throws SearchError
{
	assert(varnm.isSym());

	Value th = comp.th;
	Value locvars = comp.locvarseg;
	do
	{
		// Look to see if variable already defined as local
		// Ignore first two values (link pointer and base index number)
		int nbrlocals = arr_size(locvars);
		for (int idx = nbrlocals - 1; idx > 1; idx--)
		{
			if (arrGet(th, locvars, idx) == varnm)
				return idx-2+toAint(arrGet(th, locvars, 1)); // relative to base index
		}
		locvars = arrGet(th, locvars, 0); // link to prior local variables
	} while (locvars != aNull);
	throw SearchError.NOT_FOUND;
}

/* Look for closure variable. Returns idx if found, -1 otherwise. */
fn int CompInfo.findClosureVar(CompInfo *comp, Value varnm)
{
	assert(varnm.isSym());

	if (comp.clovarseg.isArr())
	{
		// Look to see if variable already defined as closure
		int nbrclosures = arr_size(comp->clovarseg);
		for (int idx = nbrclosures - 1; idx >= 0; idx--)
		{
		    // Adjust for position in closure array
			if (arrGet(comp->th, comp->clovarseg, idx) == varnm) return idx+2;
		}
	}
	return -1;
}

/** If variable not declared already, declare it */
void CompInfo.declareLocal(CompInfo *comp, Value varnm)
{
	Value th = comp->th;
	// If explicit 'local' declaration, declare if not found in block list
	if (comp->forcelocal) {
		if (findBlockVar(th, comp->locvarseg, varnm))
			arrAdd(th, comp->locvarseg, varnm);
	}
	// If implicit variable, declare as local or closure, if not found in this or any outer block
	else if (findLocalVar(comp, varnm)==-1 && findClosureVar(comp, varnm)==-1)
		// Declare as closure var if found as local in outer method. Otherwise, declare as local
		if (comp->prevcomp!=aNull && findLocalVar((CompInfo*)comp->prevcomp, varnm)!=-1) {
			arrAdd(th, comp->clovarseg, varnm);
			// Add initialization logic
			astAddSeg2(th, comp->newcloseg, vmlit(SYM_LOCAL), varnm); // Add its initialization to new closure segment
		}
		else
			arrAdd(th, comp->locvarseg, varnm);
}

/** Create and return new Closure AST segment
	Modifies comp->clovarseg and -> newcloseg */
fn Value parseNewClo(CompInfo* comp, Value astseg)
{
	Value th = comp->th;
	// ('Closure', clovars, ('callprop', Closure, New, getmethod, setmethod))
	Value closeg = ast::addSeg(th, astseg, vmlit(SYM_CLOSURE), 3);
	comp->clovarseg = pushArray(th, aNull, 4);
	arr::add(th, closeg, comp->clovarseg);
	th.popValue();
	Value newcloseg = ast::addSeg(th, closeg, vmlit(SYM_CALL_PROP), 8);
	ast::addSeg2(th, newcloseg, vmlit(SYM_GLOBAL), vmlit(SYM_CLOSURE));
	ast::addSeg2(th, newcloseg, vmlit(SYM_LIT), vmlit(SYM_NEW));
	ast::addSeg2(th, newcloseg, vmlit(SYM_LIT), vmlit(SYM_NULL));
	ast::addSeg2(th, newcloseg, vmlit(SYM_LIT), vmlit(SYM_NULL));
	return newcloseg;
}

/** Parse an atomic value: literal, variable or pseudo-variable */
void parseValue(CompInfo* comp, Value astseg)
{
	Value th = comp->th;
	// Literal token (number, symbol, string, true, false, null)
	if (comp->lex->toktype == Lit_Token)
	{
		ast::addSeg2(th, astseg, vmlit(SYM_LIT), comp.lex.token);
		lexGetNextToken(comp->lex);
	}
	// Static unquoted @url
	else if (comp->lex->toktype == Url_Token) {
		astAddSeg2(th, astseg, vmlit(SYM_EXT), anInt(genAddUrlLit(comp, comp->lex->token)));
		lexGetNextToken(comp->lex);
	}
	// Local or global variable / name token
	else if (comp->lex->toktype == Name_Token) {
		Value symnm = pushValue(th, comp->lex->token);
		lexGetNextToken(comp->lex);
		const char first = (toStr(symnm))[0];
		// If followed by ":" or ":=", it is a literal symbol
		if (lexMatch(comp->lex, ":") || lexMatch(comp->lex, ":="))
			astAddSeg2(th, astseg, vmlit(SymLit), symnm);
		else if (first=='$' || (first>='A' && first<='Z'))
		{
			astAddSeg2(th, astseg, vmlit(SYM_GLOBAL), symnm);
		}
		else
		{
			comp.declareLocal(symnm); // declare local if not already declared
			// We do not resolve locals to index until gen because of control clauses (declaration after use)
			astAddSeg2(th, astseg, vmlit(SYM_LOCAL), symnm);
		}
		popValue(th);
	}
	// 'baseurl' pseudo-variable
	else if (lexMatchNext(comp->lex, "baseurl")) {
		astAddValue(th, astseg, vmlit(SYM_BASEURL));
	}
	// 'this' pseudo-variable
	else if (lexMatchNext(comp->lex, "this")) {
		astAddValue(th, astseg, vmlit(SYM_THIS));
	}
	// 'self' pseudo-variable
	else if (lexMatchNext(comp->lex, "self")) {
		astAddValue(th, astseg, vmlit(SYM_SELF));
	}
	// 'selfmethod' pseudo-variable
	else if (lexMatchNext(comp->lex, "selfmethod")) {
		astAddValue(th, astseg, vmlit(SYM_SELF_METH));
	}
	// 'context' pseudo-variable
	else if (lexMatchNext(comp->lex, "context")) {
		astAddValue(th, astseg, vmlit(SYM_CONTEXT));
	}
	// '...' splat
	else if (lexMatchNext(comp->lex, "...")) {
		astAddValue(th, astseg, vmlit(SymSplat));
	}
	// 'yield' expression
	else if (lexMatchNext(comp->lex, "yield")) {
		Value newseg = astAddSeg(th, astseg, vmlit(SymYield), 2);
		parseThisExp(comp, newseg);
	}
	// parenthetically-enclosed expression
	else if (lexMatchNext(comp->lex, "(")) {
		parseExp(comp, astseg);
		if (!lexMatchNext(comp->lex, ")"))
			lexLog(comp->lex, "Expected ')'.");
	}
	// Method definition
	else if (lexMatch(comp->lex, "[") || lexMatch(comp->lex, "*[")) {
		Value svclovars = comp->clovarseg;
		Value svnewcloseg = comp->newcloseg;
		Value newcloseg = astseg;
		// Create closure segment just in case, if we are not already inside one...
		// ('Closure', clovars, ('callprop', Closure, New, getmethod, setmethod))
		if (!comp->explicitclo)
			newcloseg = comp->newcloseg = parseNewClo(comp,astseg);
		// Go compile method parms and code block using new compiler context but same lexer
		pushValue(th, vmlit(SymNew));
		pushGloVar(th, "Method");
		pushValue(th, comp);
		getCall(th, 2, 1);
		// Stick returned compiled method reference in extern section of this method's literals
		astAddSeg2(th, newcloseg, vmlit(SymExt), anInt(genAddMethodLit(comp, getFromTop(th, 0))));
		popValue(th);
		// Move method to its rightful place in closure segment
		if (!comp->explicitclo) {
			AuintIdx last = arr_size(newcloseg)-1;
			arrSet(th, newcloseg, 3, arrGet(th, newcloseg, last));
			arrSetSize(th, newcloseg, last);
		}
		comp->newcloseg = svnewcloseg;
		comp->clovarseg = svclovars;
	}
	// Explicit closure definition
	else if (lexMatchNext(comp->lex, "+[")) {
		Value svclovars = comp->clovarseg;
		Value svnewcloseg = comp->newcloseg;
		bool svexplicitclo = comp->explicitclo;

		Value newcloseg = parseNewClo(comp,astseg);

		// Process explicit closure variable declarations
		if (!lexMatch(comp->lex, "]")) {
			do {
				if (comp->lex->toktype == Name_Token || lexMatch(comp->lex, "self")) {
					// Closure variable name
					Value symnm = comp->lex->token;
					const char first = (toStr(symnm))[0];
					if (first=='$' || (first>='A' && first<='Z'))
						lexLog(comp->lex, "A global name may not be a closure variable");
					arrAdd(th, comp->clovarseg, symnm);
					lexGetNextToken(comp->lex);

					// Handle specified initializer expression
					if (lexMatchNext(comp->lex, "=")) {
						parseAppendExp(comp, newcloseg);
					}
					// No initializer expression? Initialize it using same named 'local' variable
					else if (symnm == vmlit(SymSelf))
						astAddValue(th, newcloseg, symnm);
					else
						astAddSeg2(th, newcloseg, vmlit(SymLocal), symnm);
				}
			} while (lexMatchNext(comp->lex, ","));
		}
		if (!lexMatchNext(comp->lex, "]"))
			lexLog(comp->lex, "Expected ']'.");

		comp->explicitclo = true;
		comp->newcloseg = newcloseg;
		// For get/set explicit closure, look for both
		if (lexMatchNext(comp->lex, "{")) {
			for (int i=0; i<2; i++) {
				parseExp(comp, newcloseg);
				AuintIdx last = arr_size(newcloseg)-1;
				arrSet(th, newcloseg, 3+i, arrGet(th, newcloseg, last));
				arrSetSize(th, newcloseg, last);
				lexMatchNext(comp->lex, ";");
			}
			lexMatchNext(comp->lex, "}");
		}
		// Not get/set? Get method, then move to its rightful place in closure segment
		else {
			parseValue(comp, newcloseg);
			AuintIdx last = arr_size(newcloseg)-1;
			arrSet(th, newcloseg, 3, arrGet(th, newcloseg, last));
			arrSetSize(th, newcloseg, last);
		}

		// Restore saved values
		comp->explicitclo = svexplicitclo;
		comp->newcloseg = svnewcloseg;
		comp->clovarseg = svclovars;
	}
	return;
}

/** Add a list of parameters to a AST propseg */
fn void CompInfo.parseParams(CompInfo* comp, Value propseg, const char *closeparen)
{
	bool saveforcelocal = comp->forcelocal;
	comp->forcelocal = false;

	parseAppendExp(comp, propseg);
	while (lexMatchNext(comp->lex, ","))
		parseAppendExp(comp, propseg);

	if (!lexMatchNext(comp->lex, closeparen))
		lexLog(comp->lex, "Expected ')' or ']' at end of parameter/index list.");

	comp->forcelocal = saveforcelocal;
}

/** Determine if token is '.' '.:' or '::' */
#define isdots(token) ((token)==vmlit(SymDot) || (token)==vmlit(SymColons) || (token)==vmlit(SymDotColon))

/** Parse a compound term, handling new and suffixes */
void parseTerm(CompInfo* comp, Value astseg) {
	Value th = comp->th;
	// Capture whether term began with a "+" prefix
	bool newflag = lexMatchNext(comp->lex, "+");
	// Obtain base value (dots as prefix implies 'this' as base value)
	if (!newflag && isdots(comp->lex->token))
		astAddValue(th, astseg, vmlit(SymThis));
	else
		parseValue(comp, astseg);
	// Handle suffix chains
	while (newflag || isdots(comp->lex->token) || lexMatch(comp->lex, "(") || lexMatch(comp->lex, "[")) {
		bool getparms = true;
		Value propseg = astInsSeg(th, astseg, vmlit(SymActProp), 4); // may adjust op later
		// Treat '+' as calling .New
		if (newflag) {
			astAddSeg2(th, propseg, vmlit(SymLit), vmlit(SymNew));
			newflag = false; // only works once
		}
		// For pure method call, adjust to be: self.method
		else if (lexMatch(comp->lex, "(")) {
			arrSet(th, propseg, 2, arrGet(th, propseg, 1));
			arrSet(th, propseg, 1, vmlit(SymSelf));
		}
		// For indexing, adjust to: base.'[]'
		else if (lexMatchNext(comp->lex, "[")) {
			astSetValue(th, propseg, 0, vmlit(SymCallProp)); // adjust because of parms
			astAddSeg2(th, propseg, vmlit(SymLit), vmlit(SymBrackets));
			comp.parseParams(propseg, "]");
			getparms = false;
		}
		// Handle '.', '.:', and '::'
		else {
			if (lexMatch(comp->lex, ".:")) {
				astSetValue(th, propseg, 0, vmlit(SymRawProp));
				getparms = false;
			}
			else if (lexMatch(comp->lex, "::")) {
				astSetValue(th, propseg, 0, vmlit(SymCallProp));
				astAddSeg2(th, propseg, vmlit(SymLit), vmlit(SymBrackets));
				getparms = false;
			}
			lexGetNextToken(comp->lex); // scan past dot(s) operator

			// Retrieve the property specified after the dot(s) operator
			if (comp->lex->toktype == Name_Token || comp->lex->toktype == Lit_Token) {
				astAddSeg2(th, propseg, vmlit(SymLit), comp->lex->token);
				lexGetNextToken(comp->lex);
			}
			// Calculated property symbol/method value
			else if (lexMatchNext(comp->lex, "(")) {
				parseExp(comp, propseg);
				if (!lexMatchNext(comp->lex, ")"))
					lexLog(comp->lex, "Expected ')' at end of property expression.");
			}
			else {
				astAddSeg2(th, propseg, vmlit(SymLit), aNull);
				lexLog(comp->lex, "Expected property expression after '.', '.:', or '::'");
			}
		}

		// Process parameter list, if appropriate for this term suffix
		if (getparms) {
			if (lexMatchNext(comp->lex, "(")) {
				astSetValue(th, propseg, 0, vmlit(SymCallProp)); // adjust because of parms
				comp.parseParams(propseg, ")");
			}
			// Treat Text or Symbol literal as a single parameter to pass
			else if (comp->lex->toktype == Lit_Token && (isStr(comp->lex->token) || isSym(comp->lex->token))) {
				astSetValue(th, propseg, 0, vmlit(SymCallProp)); // adjust because of parm
				astAddSeg2(th, propseg, vmlit(SymLit), comp->lex->token);
				lexGetNextToken(comp->lex);
			}
		}
	}
}

/** Parse a prefix operator */
void parsePrefixExp(CompInfo* comp, Value astseg) {
	Value th = comp->th;
	if (lexMatchNext(comp->lex, "-")) {
		parsePrefixExp(comp, astseg);

		// Optimize taking the negative of a literal number
		Value selfseg = astGetLast(th, astseg);
		if (astGet(th, selfseg, 0)==vmlit(SymLit) && isFloat(astGet(th, selfseg, 1)))
			astSetValue(th, selfseg, 1, aFloat(-toAfloat(astGet(th, selfseg, 1))));
		else if (astGet(th, selfseg, 0)==vmlit(SymLit) && isInt(astGet(th, selfseg, 1)))
			astSetValue(th, selfseg, 1, anInt(-toAint(astGet(th, selfseg, 1))));
		else { // Not a literal number? Do the property call
			astseg = astInsSeg(th, astseg, vmlit(SymCallProp), 3);
			Value litseg = astAddSeg(th, astseg, vmlit(SymLit), 2);
			astAddValue(th, litseg, vmlit(SYM_NEG));
		}
	}
	// '@' + symbol, text or '('exp')'
	else if (lexMatchNext(comp->lex, "@")) {
		// Symbol or text: treat as static, unquoted url
		if (comp->lex->toktype == Lit_Token) {
			assert(isStr(comp->lex->token) || isSym(comp->lex->token));
			// +Resource(token,baseurl)
			pushValue(th, vmlit(SymNew));
			pushValue(th, vmlit(TypeResc));
			pushValue(th, comp->lex->token);
			pushValue(th, comp->lex->url);
			getCall(th, 3, 1);
			// ('lit', resource)
			astAddSeg2(th, astseg, vmlit(SymExt), anInt(genAddUrlLit(comp, getFromTop(th, 0))));
			popValue(th);
			lexGetNextToken(comp->lex);
		}
		else {
			// ('callprop', ('callprop', glo'Resource', lit'New', parsed-value, 'baseurl'), 'Load')
			Value loadseg = astAddSeg(th, astseg, vmlit(SymCallProp), 3);
			Value newseg = astAddSeg(th, loadseg, vmlit(SymCallProp), 5);
			astAddSeg2(th, newseg, vmlit(SymGlobal), vmlit(SymResource));
			astAddSeg2(th, newseg, vmlit(SymLit), vmlit(SymNew));
			parsePrefixExp(comp, newseg);
			astAddValue(th, newseg, vmlit(SymBaseurl));
			astAddSeg2(th, loadseg, vmlit(SymLit), vmlit(SymLoad));
		}
	}
	else
		parseTerm(comp, astseg);
}

/** Parse the '**' operator */
fn void CompInfo.parsePowerExp(CompInfo* comp, Value astseg)
{
	Value th = comp.th;
	comp.parsePrefixExp(astseg);
	Value op = comp.lex.token;
	while (comp.matchNext("**"))
	{
		Value newseg = ast::insSeg(th, astseg, vmlit(SymCallProp), 4);
		ast::addSeg2(th, newseg, vmlit(SymLit), op);
		comp.parsePrefixExp(newseg);
	}
}

/** Parse the '*', '/' or '%' binary operator */
fn void CompInfo.parseMultDivExp(CompInfo* comp inline, Value astseg)
{
	Value th = comp.th;
	parsePowerExp(astseg);
	while (Value op = lex.token, matchNext("*") || matchNext("/") || matchNext("%"))
	{
		Value newseg = ast::insSeg(th, astseg, vmlit(SYM_CALL_PROP), 4);
		ast::addSeg2(th, newseg, vmlit(SYM_LIT), op);
		parsePowerExp(newseg);
	}
}

/** Parse the '+' or '-' binary operator */
fn void CompInfo.parseAddSubExp(CompInfo* comp, Value astseg)
{
	Value th = comp.th;
	comp.parseMultDivExp(astseg);
	while (int isAdd; (isAdd = comp.matchNext"+") || comp.matchNextcomp->lex, "-"))
	{
		Value newseg = ast::insSeg(th, astseg, vmlit(SYM_CALL_PROP), 4);
		ast::addSeg2(th, newseg, vmlit(SymLit), isAdd ? vmlit(SYM_PLUS) : vmlit(SYM_MINUS));
		comp.parseMultDivExp(newseg);
	}
}

/** Parse the range .. constructor operator */
fn void CompInfo.parseRangeExp(CompInfo* comp, Value astseg)
{
	Value th = comp.th;
	comp.parseAddSubExp(astseg);
	if (comp.matchNext(".."))
	{
		// ('CallProp', 'Range', 'New', from, to, step)
		Value newseg = ast::insSeg(th, astseg, vmlit(SYM_CALL_PROP), 4);
		Value from = th.pushValue(arr::get(th, newseg, 1));
		arr::del(th, newseg, 1, 1);
		ast::addSeg2(th, newseg, vmlit(SYM_GLOBAL), vmlit(SymRange));
		ast::addSeg2(th, newseg, vmlit(SymLit), vmlit(SymNew));
		ast::addValue(th, newseg, from);
		th.popValue();
		comp.parseAddSubExp(newseg);
		if (lexMatchNext(comp->lex, "..")) comp.parseAddSubExp(newseg);
	}
}

/** Parse the comparison operators */
fn void CompInfo.parseCompExp(CompInfo* comp, Value astseg) {
	Value th = comp.th;
	comp.parseRangeExp(astseg);
	Value op = comp.lex->token;
	if (lexMatchNext(comp.lex, "<=>"))
	{
		Value newseg = ast::insSeg(th, astseg, vmlit(SymCallProp), 4);
		ast::addSeg2(th, newseg, vmlit(SymLit), op);
		comp.parseRangeExp(newseg);
	}
	else if (lexMatchNext(comp->lex, "===") || lexMatchNext(comp->lex, "~~")
		|| lexMatchNext(comp->lex, "==") || lexMatchNext(comp->lex, "!=")
		|| lexMatchNext(comp->lex, "<=") || lexMatchNext(comp->lex, ">=")
		|| lexMatchNext(comp->lex, "<") || lexMatchNext(comp->lex, ">")) {
		Value newseg = ast::insSeg(th, astseg, op, 3);
		comp.parseRangeExp(newseg);
	}
}

/* Parse 'not' conditional logic operator */
fn void CompInfo.parseNotExp(CompInfo* comp, Value astseg)
{
	Value th = comp.th;
	bool takenot = false;
	while ((lexMatchNext(comp->lex, "!")) || lexMatchNext(comp->lex, "not")) takenot = !takenot;
	if (takenot)
	{
		Value newseg = astAddSeg(th, astseg, vmlit(SymNot), 2);
		parseCompExp(comp, newseg);
	}
	else
	{
		comp.parseCompExp(astseg);
	}
}

fn bool CompInfo.matchNext(CompInfo *comp, string s) @inline
{
    return comp.lex.matchNext(s);
}

/* Parse 'and' conditional logic operator */
fn void CompInfo.parseAndExp(CompInfo* comp, Value astseg)
{
	Value th = comp.th;
	comp.parseNotExp(astseg);
	if (comp.matchNext("&&") || comp.matchNext("and")
	{
		Value newseg = ast::insSeg(th, astseg, vmlit(SymAnd), 3);
		do
		{
			comp.parseNotExp(newseg);
		}
		while (comp.matchNext("&&") || comp.matchNext("and"));
	}
}

/** Parse 'or' conditional logic operator */
void parseLogicExp(CompInfo* comp, Value astseg)
{
	Value th = comp->th;
	parseAndExp(comp, astseg);
	if ((lexMatchNext(comp->lex, "||")) || lexMatchNext(comp->lex, "or")) {
		Value newseg = astInsSeg(th, astseg, vmlit(SymOr), 3);
		do {
			parseAndExp(comp, newseg);
		} while ((lexMatchNext(comp->lex, "||")) || lexMatchNext(comp->lex, "or"));
	}
}

/** Parse '?' 'else' ternary operator */
void parseTernaryExp(CompInfo* comp, Value astseg) {
	Value th = comp->th;
	parseLogicExp(comp, astseg);
	if ((lexMatchNext(comp->lex, "?"))) {
		Value newseg = astInsSeg(th, astseg, vmlit(SymQuestion), 4);
		parseLogicExp(comp, newseg);
		if (lexMatchNext(comp->lex, "else"))
			parseLogicExp(comp, newseg);
		else {
			astAddSeg2(th, newseg, vmlit(SymLit), aNull);
			lexLog(comp->lex, "Expected 'else' in ternary expression");
		}
	}
}

/** Parse append and prepend operators */
void parseAppendExp(CompInfo* comp, Value astseg) {
	Value th = comp->th;
	// If prefix, assume 'this'. Otherwise get left hand value
	if (lexMatch(comp->lex, "<<") || lexMatch(comp->lex, ">>"))
		astAddValue(th, astseg, vmlit(SymThis));
	else
		parseTernaryExp(comp, astseg);

	Value op;
	while ((op=comp->lex->token) && lexMatchNext(comp->lex, "<<") || lexMatchNext(comp->lex, ">>")) {
		Value newseg = astInsSeg(th, astseg, vmlit(SymCallProp), 4);
		astAddSeg2(th, newseg, vmlit(SymLit), op);
		parseTernaryExp(comp, newseg);
	}
}

/** Parse comma separated expressions */
void parseCommaExp(CompInfo* comp, Value astseg) {
	Value th = comp->th;
	parseAppendExp(comp, astseg);
	if (lexMatch(comp->lex, ",")) {
		Value commaseg = astInsSeg(th, astseg, vmlit(SymComma), 4);
		while (lexMatchNext(comp->lex, ",")) {
			parseAppendExp(comp, commaseg);
		}
	}
}

/** Parse an assignment or property setting expression */
void parseAssgnExp(CompInfo* comp, Value astseg) {
	Value th = comp->th;
	bool isColonEq;

	// Get lvals (could be rvals if no assignment operator is found)
	// Presence of 'local' ensures unknown locals are declared as locals vs. closure vars
	bool saveforcelocal = comp->forcelocal;
	comp->forcelocal = lexMatchNext(comp->lex, "local");
	parseCommaExp(comp, astseg);
	comp->forcelocal = saveforcelocal;

	// Process rvals depending on type of assignment
	if (lexMatch(comp->lex, "=")) {
		Value assgnseg = astInsSeg(th, astseg, vmlit(SymAssgn), 3);
		// Warn about unalterable literals or pseudo-variables to the left of "="
		Value lvalseg = arrGet(th, assgnseg, 1);
		if (arrGet(th, lvalseg, 0) == vmlit(SymComma)) {
			Value lval;
			for (Auint i = 1; i<arr_size(lvalseg); i++) {
				if (!astIsLval(th, lval = arrGet(th, lvalseg, i))) {
					lexLog(comp->lex, "Literals/pseudo-variables/expressions cannot be altered.");
					break;
				}
			}
		}
		else if (!astIsLval(th, lvalseg)) {
			lexLog(comp->lex, "Literals/pseudo-variables/expressions cannot be altered.");
		}
		lexGetNextToken(comp->lex); // Go past assignment operator
		parseAssgnExp(comp, assgnseg); // Get the values to the right
	}
	else if ((isColonEq = lexMatchNext(comp->lex, ":=")) || lexMatchNext(comp->lex, ":")) {
		// ('=', ('activeprop'/'callprop', 'this', ('[]',) property), value)
		Value assgnseg = astInsSeg(th, astseg, vmlit(SymAssgn), 3);
		Value indexseg = astPushNew(th, vmlit(isColonEq? SymCallProp : SymActProp), 4);
		astAddValue(th, indexseg, vmlit(SymThis));
		if (isColonEq)
			astAddSeg2(th, indexseg, vmlit(SymLit), vmlit(SymBrackets));
		astPopNew(th, assgnseg, indexseg);
		parseAssgnExp(comp, assgnseg);
	}
}

/** Parse an expression */
void parseExp(CompInfo* comp, Value astseg) {
	parseAssgnExp(comp, astseg);
}

/** Set up block variable list and add it to astseg */
Value parseNewBlockVars(CompInfo *comp, Value astseg) {
	Value th = comp->th;
	// Set up block variable list
	Value blkvars = pushArray(th, vmlit(TypeListm), 8);
	arrSet(th, blkvars, 0, comp->locvarseg);
	arrSet(th, blkvars, 1, anInt(0));
	comp->locvarseg = blkvars;
	astAddValue(th, astseg, blkvars);
	popValue(th); // blkvars
	return blkvars;
}

/** Parse an expression statement / 'this' block */
void parseThisExp(CompInfo* comp, Value astseg) {
	Value th = comp->th;
	Value svlocalvars = comp->locvarseg;
	parseAssgnExp(comp, astseg);
	if (lexMatchNext(comp->lex, "using")) {
		Value newseg = astInsSeg(th, astseg, vmlit(SymThisBlock), 5);
		parseNewBlockVars(comp, newseg);
		parseAssgnExp(comp, newseg);
		parseBlock(comp, newseg);
	}
	else if (lexMatch(comp->lex, "{")) {
		Value newseg = astInsSeg(th, astseg, vmlit(SymThisBlock), 5);
		parseNewBlockVars(comp, newseg);
		astAddValue(th, newseg, aNull);
		parseBlock(comp, newseg);
	}
	comp->locvarseg = svlocalvars;
}

/** Expect ';' at this point. Error if not found, then scan to find it. */
void parseSemi(CompInfo* comp, Value astseg) {
	// Allow right curly brace and end-of-file to stand in for a semi-colon
	if (!lexMatchNext(comp->lex, ";")&&!lexMatch(comp->lex, "}")&&comp->lex->toktype!=Eof_Token) {
		lexLog(comp->lex, "Unexpected token in statement. Ignoring all until block or ';'.");
		while (comp->lex->toktype != Eof_Token && !lexMatch(comp->lex, "}") && !lexMatchNext(comp->lex, ";"))
			if (lexMatch(comp->lex, "{"))
				parseBlock(comp, astseg);
			else
				lexGetNextToken(comp->lex);
	}
}

/** Parse the each clause: vars and iterator */
void parseEachClause(CompInfo *comp, Value newseg) {
	Value th = comp->th;

	// Set up block variable list
	Value blkvars = parseNewBlockVars(comp, newseg);

	// Parse list of 'each' variables (into new variable block)
	AuintIdx bvarsz=2;
	do {
		if (comp->lex->toktype==Name_Token) {
			arrSet(th, blkvars, bvarsz++, comp->lex->token);
			lexGetNextToken(comp->lex);
		}
		else
			lexLog(comp->lex, "Expected variable name");
		// Assign null variable for "key", if none specified using ':'
		if (bvarsz==3 && !lexMatch(comp->lex, ":")) {
			arrSet(th, blkvars, bvarsz++, arrGet(th, blkvars, 2));
			arrSet(th, blkvars, 2, aNull);
		}
	} while (lexMatchNext(comp->lex, ",") || lexMatchNext(comp->lex, ":"));
	astAddValue(th, newseg, anInt(bvarsz-2)); // expected number of 'each' values

	// 'in' clause
	if (!lexMatchNext(comp->lex, "in"))
		lexLog(comp->lex, "Expected 'in'");
	parseLogicExp(comp, newseg);
}

/** Parse 'if', 'while' or 'each' statement clauses */
void parseClause(CompInfo* comp, Value astseg, AuintIdx stmtvarsz) {
	Value th = comp->th;
	Value svlocalvars = comp->locvarseg; // Statement's local block
	Value deeplocalvars = aNull; // First/deepest clause's local block
	Value inslocalvars = aNull;  // prior/deeper clause's local block
	// Handle multiple clauses so they execute in reverse order
	while (lexMatch(comp->lex, "if") || lexMatch(comp->lex, "while") || lexMatch(comp->lex, "each")) {
		Value ctlseg;
		Value ctltype = comp->lex->token;
		if (lexMatchNext(comp->lex, "if")) {
			// Wrap 'if' single statement in a block (so that fixing implicit returns works)
			astInsSeg(th, astseg, vmlit(SymSemicolon), 2);
			ctlseg = astPushNew(th, ctltype, 4);
			parseLogicExp(comp, ctlseg);
			parseNewBlockVars(comp, ctlseg);
		}
		else if (lexMatchNext(comp->lex, "while")) {
			ctlseg = astPushNew(th, ctltype, 4);
			parseNewBlockVars(comp, ctlseg);
			parseLogicExp(comp, ctlseg);
		}
		else if (lexMatchNext(comp->lex, "each")) {
			ctlseg = astPushNew(th, ctltype, 5);
			parseEachClause(comp, ctlseg); // var and 'in' iterator
		}
		astPopNew(th, astseg, ctlseg); // swap in place of block

		// Linkage of variable scoping for clauses is intricate
		if (inslocalvars != aNull) // link prior clause to current
			arrSet(th, inslocalvars, 0, comp->locvarseg);
		if (deeplocalvars == aNull)
			deeplocalvars = comp->locvarseg; // Remember first/deepest
		inslocalvars = comp->locvarseg; // Remember most recent
		comp->locvarseg = svlocalvars; // Restore to statement block
	}
	parseSemi(comp, astseg);

	// Move any new locals declared in statement to deepest clause's block scope
	if (deeplocalvars!=aNull && stmtvarsz<arr_size(svlocalvars)) {
		comp->locvarseg = deeplocalvars;
		for (AuintIdx vari = arr_size(svlocalvars)-1; vari >= stmtvarsz; vari--) {
			// Pop off statement's declared local, and if not found, add to deepest block
			// Find check is needed to see each's declared variables, for example
			Value varnm = pushValue(th, arrGet(th, svlocalvars, vari));
			arrSetSize(th, svlocalvars, vari);
			if (findLocalVar(comp, varnm)==-1)
				arrAdd(th, deeplocalvars, varnm);
			popValue(th);
		}
		arrSetSize(th, svlocalvars, stmtvarsz); // Remove from outer block vars
		comp->locvarseg = svlocalvars;
	}
}

/** Parse a sequence of statements, each ending with ';' */
void parseStmts(CompInfo* comp, Value astseg) {
	Value th = comp->th;
	astseg = astAddSeg(th, astseg, vmlit(SymSemicolon), 16);
	Value newseg;
	while (comp->lex->toktype != Eof_Token && !lexMatch(comp->lex, "}")) {
		Value stmt = comp->lex->token;
		AuintIdx stmtvarsz = arr_size(comp->locvarseg); // Remember for clauses
		// 'if' block
		if (lexMatchNext(comp->lex, "if")) {
			newseg = astAddSeg(th, astseg, vmlit(SymIf), 4);
			Value svlocalvars = comp->locvarseg;
			parseLogicExp(comp, newseg);
			parseNewBlockVars(comp, newseg);
			parseBlock(comp, newseg);
			comp->locvarseg = svlocalvars;
			parseSemi(comp, astseg);
			while (lexMatchNext(comp->lex, "elif")) {
				parseLogicExp(comp, newseg);
				parseNewBlockVars(comp, newseg);
				parseBlock(comp, newseg);
				comp->locvarseg = svlocalvars;
				parseSemi(comp, astseg);
			}
			if (lexMatchNext(comp->lex, "else")) {
				astAddValue(th, newseg, vmlit(SymElse));
				parseNewBlockVars(comp, newseg);
				parseBlock(comp, newseg);
				comp->locvarseg = svlocalvars;
				parseSemi(comp, astseg);
			}
		}

		// 'match' block
		if (lexMatchNext(comp->lex, "match")) {
			newseg = astAddSeg(th, astseg, vmlit(SymMatch), 4);
			Value svlocalvars = comp->locvarseg;
			parseExp(comp, newseg);
			if (lexMatchNext(comp->lex, "using"))
				parseAssgnExp(comp, newseg);
			else
				astAddValue(comp, newseg, vmlit(SymMatchOp));
			Value matchInto = aNull;
			if (lexMatchNext(comp->lex, "into")) {
				matchInto = pushArray(th, aNull, 4);
				do {
					if (comp->lex->toktype==Name_Token) {
						arrAdd(th, matchInto, comp->lex->token);
						lexGetNextToken(comp->lex);
					}
					else
						lexLog(comp->lex, "Expected variable name");
				} while (lexMatchNext(comp->lex, ","));
			}
			parseSemi(comp, astseg);
			while (lexMatchNext(comp->lex, "with")) {
				parseExp(comp, newseg);
				parseNewBlockVars(comp, newseg);
				AuintIdx nInto = 2;
				if (lexMatchNext(comp->lex, "into")) {
					do {
						if (comp->lex->toktype==Name_Token) {
							arrSet(th, comp->locvarseg, nInto++, comp->lex->token);
							lexGetNextToken(comp->lex);
						}
						else
							lexLog(comp->lex, "Expected variable name");
					} while (lexMatchNext(comp->lex, ","));
				}
				else if (matchInto!=aNull) {
					for (AuintIdx i=0; i<arr_size(matchInto); i++) {
						arrSet(th, comp->locvarseg, nInto, arrGet(th, matchInto, nInto-2));
						nInto++;
					}
				}
				astAddValue(th, newseg, anInt(nInto-2));
				parseBlock(comp, newseg);
				comp->locvarseg = svlocalvars;
				parseSemi(comp, astseg);
			}
			if (lexMatchNext(comp->lex, "else")) {
				astAddValue(th, newseg, vmlit(SymElse));
				parseNewBlockVars(comp, newseg);
				astAddValue(th, newseg, anInt(0));
				parseBlock(comp, newseg);
				comp->locvarseg = svlocalvars;
				parseSemi(comp, astseg);
			}
			if (matchInto!=aNull)
				popValue(th);
		}

		// 'while' block
		else if (lexMatchNext(comp->lex, "while")) {
			newseg = astAddSeg(th, astseg, vmlit(SymWhile), 4);
			Value svlocalvars = comp->locvarseg;
			parseNewBlockVars(comp, newseg);
			parseLogicExp(comp, newseg);
			parseBlock(comp, newseg);
			comp->locvarseg = svlocalvars;
			parseSemi(comp, astseg);
		}

		// 'each': ('each', localvars, nretvals, iterator, block)
		else if (lexMatchNext(comp->lex, "each")) {
			newseg = astAddSeg(th, astseg, vmlit(SymEach), 5);
			Value svlocalvars = comp->locvarseg;
			parseEachClause(comp, newseg); // vars and 'in' iterator
			parseBlock(comp, newseg);
			comp->locvarseg = svlocalvars;
			parseSemi(comp, astseg);
		}

		// 'do': ('do', local, exp, block)
		else if (lexMatchNext(comp->lex, "do")) {
			newseg = astAddSeg(th, astseg, vmlit(SymDo), 4);
			Value svlocalvars = comp->locvarseg;
			parseNewBlockVars(comp, newseg);
			if (!lexMatch(comp->lex, "{"))
				parseExp(comp, newseg);
			else
				astAddValue(th, newseg, aNull);
			parseBlock(comp, newseg);
			comp->locvarseg = svlocalvars;
			parseSemi(comp, astseg);
		}

		// 'break' or 'continue' statement
		else if (lexMatchNext(comp->lex, "break") || lexMatchNext(comp->lex, "continue")) {
			astAddSeg(th, astseg, stmt, 1);
			parseClause(comp, astseg, stmtvarsz);
		}

		// 'return' statement
		else if (lexMatchNext(comp->lex, "return") || lexMatchNext(comp->lex, "yield")) {
			newseg = astAddSeg(th, astseg, stmt, 2);
			if (!lexMatch(comp->lex, ";") && !lexMatch(comp->lex, "if")
				&& !lexMatch(comp->lex, "each") && !lexMatch(comp->lex, "while"))
				parseThisExp(comp, newseg);
			else
				astAddValue(th, newseg, aNull);
			parseClause(comp, astseg, stmtvarsz);
		}

		// expression or 'this' block
		else {
			if (stmt != vmlit(SymSemicolon)) {
				parseThisExp(comp, astseg);
				parseClause(comp, astseg, stmtvarsz);
			}
		}
	}
	return;
}

/** Parse a block of statements enclosed by '{' and '}' */
void parseBlock(CompInfo* comp, Value astseg) {
	if (!lexMatchNext(comp->lex, "{"))
		return;
	parseStmts(comp, astseg);
	if (!lexMatchNext(comp->lex, "}"))
		lexLog(comp->lex, "Expected '}'");
	return;
}

/* Parse an Acorn program */
void parseProgram(CompInfo* comp) {
	Value th = comp->th;
	Value methast = comp->ast;
	astAddValue(th, methast, vmlit(SymMethod));

	// local variable list - starts with pointer to outer method's local variable list
	comp->locvarseg = astAddSeg(th, methast, aNull, 16);
	astAddValue(th, comp->locvarseg, anInt(1));

	// closure variable list already retrieved from outer method
	comp->explicitclo = false;

	Value parminitast = astAddSeg(th, methast, vmlit(SymSemicolon), 4);

	// process parameters as local variables
	bool isYielder = false;
	if (lexMatchNext(comp->lex, "[") || (isYielder = lexMatchNext(comp->lex, "*["))) {
		if (isYielder)
			methodFlags(comp->method) |= METHOD_FLG_YIELDER;

		// Process parameter declaration
		do {
			if (lexMatchNext(comp->lex, "...")) {
				methodFlags(comp->method) |= METHOD_FLG_VARPARM;
				break;
			}
			else if (comp->lex->toktype == Name_Token) {
				Value symnm = comp->lex->token;
				const char first = (toStr(symnm))[0];
				if (first=='$' || (first>='A' && first<='Z'))
					lexLog(comp->lex, "A global name may not be a method parameter");
				else {
					if (findLocalVar(comp, symnm)==-1) {
						arrAdd(th, comp->locvarseg, symnm);
						methodNParms(comp->method)++;
					}
					else
						lexLog(comp->lex, "Duplicate method parameter name");
				}
				lexGetNextToken(comp->lex);

				// Handle any specified parameter default value
				if (lexMatchNext(comp->lex, "=")) {
					// Produce this ast: parm||=default-expression
					Value oreqseg = astAddSeg(th, parminitast, vmlit(SymOrAssgn), 3);
					astAddSeg2(th, oreqseg, vmlit(SymLocal), symnm);
					parseAppendExp(comp, oreqseg);
				}
			}
		} while (lexMatchNext(comp->lex, ","));
		lexMatchNext(comp->lex, "]");
		parseBlock(comp, methast);
	}
	else
		parseStmts(comp, methast);

	comp->method->nbrlocals = arr_size(comp->locvarseg)-1;
}

#ifdef __cplusplus
} // extern "C"
} // namespace avm
#endif