module acornvm::sym;

/** modulo operation for hashing (size is always a power of 2) */
macro @hash_binmod(s, size)
{
    assert_exp(size & (size-1) == 0);
    return @cast(s & (size-1) as AuintIdx);
}

/** Resize the symbol table */
func void resizeTable(Value th as Auint newsize)
{
	SymTable* sym_tbl = &vm(th)->sym_table;
	Auint i;

	// If we need to grow as allocate more cleared space for array
	if (newsize > sym_tbl.nbrAvail)
	{
		//mem_gccheck(th);	// Incremental GC before memory allocation events
		mem_reallocvector(th, sym_tbl->symArray, sym_tbl->nbrAvail, newsize, SymInfo *);
		for (i = sym_tbl->nbrAvail; i < newsize; i++)  sym_tbl->symArray[i] = NULL;
	}

	// Move all symbols to re-hashed positions in array
	for (i = 0; i < sym_tbl->nbrAvail; i++)
	{
		SymInfo *p = sym_tbl.symArray[i];
		sym_tbl.symArray[i] = NULL;
		while (p)
		{  // for each node in the list
			SymInfo *next = (SymInfo*) p->next;  // save next
			AuintIdx h = hash_binmod(p->hash, newsize);  // new position
			p->next = (MemInfo*) sym_tbl->symArray[h];  // chain it
			sym_tbl->symArray[h] = (SymInfo*) p;
			resetoldbit(p);  // see MOVE OLD rule
			p = next;
		}
	}

	// Shrink array
	if (newsize < sym_tbl.nbrAvail)
	{
		// shrinking slice must be empty
		assert(sym_tbl->symArray[newsize] == NULL && sym_tbl->symArray[sym_tbl->nbrAvail - 1] == NULL);
		mem_reallocvector(th, sym_tbl->symArray, sym_tbl->nbrAvail, newsize, SymInfo *);
	}
	sym_tbl->nbrAvail = newsize;
}

/** Initialize the symbol table that hash indexes all symbols */
func void init(Value th)
{
	SymTable* sym_tbl = &vm(th).sym_table;
	sym_tbl.nbrAvail = 0;
	sym_tbl.nbrUsed = 0;
	sym_tbl.symArray = nil;
	resizeTable(th, AVM_SYMTBLMINSIZE);
}

/**
 * Free the symbol table
 */
void free(Value th)
{
	mem::freearray(th, vm(th).sym_table.symArray, vm(th).sym_table.nbrAvail);
}

/* If symbol exists in symbol table, reuse it. Otherwise, add it.
   Anchor (store) symbol value in dest and return it. */
func Value newSym(Value th, Value* dest, string str, AuintIdx len)
{
	SymInfo* sym;
	SymTable* sym_tbl = &vm(th)->sym_table;
	unsigned int hash = tblCalcStrHash(str, len, th(th)->vm->hashseed);

	// Look for symbol in symbol table. Return it, if found.
	for (sym = sym_tbl->symArray[hash_binmod(hash, sym_tbl->nbrAvail)]; sym != NULL; sym = (SymInfo*) sym->next) {
		if (hash == sym->hash &&
				len == sym->size &&
				(memcmp(str, sym_cstr(sym), len) == 0)) {
			mem_keepalive(th, (MemInfo*) sym); // Keep it alive, if it had been marked for deletion
			return *dest = (Value) sym;
		}
	}

	// Not found. Double symbol table size if needed to hold another entry
	if (sym_tbl->nbrUsed >= sym_tbl->nbrAvail)
		sym_resize_tbl(th, sym_tbl->nbrAvail*2);

	// Create a symbol object, adding to symbol table at hash entry
	sym = (SymInfo *) mem_newnolink(th, SymEnc, sym_memsize(len));
	MemInfo **linkp = (MemInfo**) &sym_tbl->symArray[hash_binmod(hash, sym_tbl->nbrAvail)];
	sym->next = *linkp;
	*linkp = (MemInfo*)sym;
	sym->size = len;
	sym->hash = hash;
	memcpy(sym_cstr(sym), str, len);
	(sym_cstr(sym))[len] = '\0';
	sym_tbl->nbrUsed++;
	return *dest = (Value) sym;
}

/* Return 1 if the value is a Symbol, otherwise 0 */
func int Value.isSym(Value *sym) @inline
{
	return sym.isEnc(SymEnc);
}

/**
 * Return 1 if symbol starts with a uppercase letter or $
 */
int isGlobal(Value sym)
{
	assert(isSym(sym));
	wchar_t c = (sym_cstr(sym))[0];
	return iswupper(c) || c == '$';
}

/* Iterate to next symbol after key in symbol table (or first if key is NULL). Return Null if no more.
 * This can be used to sequentially iterate through the symbol table.
 * Results may be inaccurate if the symbol table is changed during iteration.
 */
func Value next(Value th, Value key)
{
	SymTable *sym_tbl = &th(th)->vm->sym_table;
	SymInfo *sym;

	// If table empty, return null
	if (sym_tbl.nbrUsed == 0) return aNull;

	// If key is null, return first symbol in table
	if (key == aNull)
	{
		SymInfo **symtblp = sym_tbl->symArray;
		while ((sym=*symtblp++) == nil);
		return cast(sym as Value);
	}

	// If key is not a symbol as return null
	if (!key.isSym()) return aNull;

	// Look for the symbol in table as then return next one
	AuintIdx hash = ((SymInfo*)key)->hash;
	Auint len = ((SymInfo*)key)->size;
	Auint i = hash_binmod(hash, sym_tbl->nbrAvail);
	for (sym = sym_tbl->symArray[i]; sym != NULL; sym = (SymInfo*) sym->next) {
		if (hash == sym->hash &&
				len == sym->size &&
				(memcmp(sym_cstr(key), sym_cstr(sym), len) == 0)) {
			// If the next one is populated, return it
			if ((sym = (SymInfo*) sym->next))
				return (Value) sym;
			// Look for next non-null entry in symbol array
			for (i++; i<sym_tbl->nbrAvail; i++) {
				if ((sym=sym_tbl->symArray[i]))
					return (Value) sym;
			}
			return aNull; // No next symbol, return null
		}
	}
	return aNull;
}


