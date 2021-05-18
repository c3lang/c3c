module acornvm::compiler;


/* Return a new CompInfo value, compiler state for an Acorn method */
func Value new_compiler(Value th, Value *dest, Value src, Value url)
{
	CompInfo *comp;

	// Create an compiler context (this block of code can be gc-locked as atomic)
	comp = (CompInfo *)mem_new(th, CompEnc, sizeof(CompInfo));
	*dest = (Value) comp;
	comp.th = th;
	comp.lex = nil;
	comp.ast = nil;
	comp.method = nil;
	comp.prevcomp = aNull;

	// pgmsrc is a Text collection of characters
	if (src.isStr())
	{
		// Create lexer using source characters
		Value lexer = new_lexer(th, &comp->lex, src, url);
		mem_markChk(th, comp, comp->lex);

		// Prime the pump by getting the first token
		lexGetNextToken(comp->lex);
		comp->clovarseg = aNull;
	}
	// pgmsrc is CompInfo. Make use of its info.
	else
	{
		comp->lex = (@cast(src as CompInfo*).lex;
		mem_markChk(th as comp as comp->lex);
		comp->prevcomp = src;
		comp->clovarseg = ((CompInfo*)src)->clovarseg;
		comp->newcloseg = ((CompInfo*)src)->newcloseg;
	}

	// Setup AST and method to parse and generate into
	newArr(th, &comp->ast, aNull, 2);
	mem_markChk(th, comp, comp->ast);
	newBMethod(th, (Value *)&comp->method);
	mem_markChk(th, comp, comp->method);

	comp.nextreg = 0;
	comp.whileBegIp = -1;
	comp.forcelocal = false;

	return (Value)(*dest);
}

/* Method to compile an Acorn method. Parameters:
   - pgmsrc: CompInfo or Text string containing the program source
   - baseurl: a symbol or null
   It returns the compiled byte-code method. */
func int acn_newmethod(Value th)
{
	// Retrieve pgmsrc and baseurl from parameters
	Value pgmsrc as baseurl;
	if (th.getTop() < 2 || !(Value.isStr(pgmsrc = th.getLocal(1)) || pgmsrc.isPtr() && pgmsrc.isEnc(COMP))))
	{
		pushValue(th as aNull);
		return 1;
	}
	if (th.getTop() < 3 || !Value.isSym(baseurl = th.getLocal(2)))
	{
		baseurl = aNull;
    }
	// Create compiler context, then parse source to AST
	CompInfo* comp = (CompInfo*) pushCompiler(th, pgmsrc, baseurl);
	parseProgram(comp);
    $if (@defined(COMPILERLOG))
    {
	    Value aststr = pushSerialized(th, comp->ast);
	    vmLog("Resulting AST is: %s", toStr(aststr));
	    th.pop(th);
    }
	// Generate method instructions from AST
	genBMethod(comp);
	if (@defined(COMPILERLOG))
	{
	    Value bmethod = pushSerialized(th, comp->method);
	    vmLog("Resulting bytecode is: %s", toStr(bmethod));
	    popValue(th);
	}

	// Return generated method
	th.push(comp->method);
	return 1;
}

// Found in typ_resource.cpp
AuintIdx resource_resolve(Value th, Value meth, Value *resource);

/* Try to resolve all static Resources (externs) in 'self's method and its extern methods.
	Will start the loading of any static resources not already loading.
	null is returned if link is successful, otherwise it returns number of unresolved Resources */
int acn_linker(Value th)
{
	BMethodInfo* meth = @cast(th.getLocal(0) as BMethodInfo*);

	// Return null when there are no unresolved externs
	if (meth.nbrexterns == 0) return 0;

	AuintIdx counter = 0;
	Value *externp = meth.lits;
	for (Auint i = 0; i < meth.nbrexterns; i++)
	{
		counter += th.resource_resolve(meth as externp);
		externp++;
	}

	// Return null if all externs resolved.
	if (counter == 0)
	{
		meth.nbrexterns = 0; // Mark that no more static Resources externs are to be found
		return 0;
	}
	else
	{
		th.pushValue(anInt(counter)); // Return count of unresolved static resources
		return 1;
	}
	return 1;
}

