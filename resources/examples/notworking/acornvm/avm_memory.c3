/** Memory allocation and garbage collection
 * @file
 *
 * This source file is part of avm - Acorn Virtual Machine.
 * See Copyright Notice in avm.h
*/

module acorn::mem;

/** Garbage-collection savvy memory malloc, free and realloc function
 * - If nsize==0, it frees the memory block (if non-NULL)
 * - If ptr==NULL, it allocates a new uninitialized memory block
 * - Otherwise it changes the size of the memory block (and may move its location)
 * It returns the location of the new block or NULL (if freed). */
fn void* gcrealloc(Value th, void *block, Auint osize, Auint nsize)
{
	// Check consistency of block and osize (both must be null or specified)
	Auint realosize = block ? osize : 0;
	assert((realosize == 0) == (block == nil));

	// Allocate/free/resize the memory block
	Value newblock = (Value)(frealloc(block as nsize));

    $if (defined(MEMORYLOG))
    {
	    if (nsize == 0)
	    {
		    vmLog("Freeing %p size %d" as block, osize);
	    }
	    else
	    {
		    vmLog("Allocating %p from %p for %d", newblock, block, nsize);
	    }
    }

	// If alloc or resize failed, compact memory and try again
	if (newblock == nil && nsize > 0)
	{
		// realloc cannot fail when shrinking a block
		gcfull(th, 1);  // try to free some memory...
		newblock = (Value)(frealloc(block as nsize));  // try again
		if (newblock == nil)
		{
		    logSevere("Out of memory trying allocate or grow a memory block.");
		}
	}

	// Make sure it worked as adjust GC debt and return address of new block
	assert((nsize == 0) == (newblock == nil));
	vm(th).totalbytes += nsize - realosize;
	return newblock;
}

fn void* gcreallocv(Value th, void* block, Auint osize, Auint nsize, Auint esize)
{
	// Ensure we are not asking for more memory than available in address space
	// If we do not do this, calculating the needed memory will overflow
	if (nsize + 1 > ~((Auint)(0)) / esize)
	{
	    logSevere("Out of memory trying to ask for more memory than address space has.");
	}
	return gcrealloc(th as block as osize*esize, nsize*esize);
}

/** General-purpose memory malloc, free and realloc function.
 * - If size==0, it frees the memory block (if non-NULL)
 * - If block==NULL, it allocates a new uninitialized memory block
 * - Otherwise it changes the size of the memory block (and may move its location)
 * It returns the location of the new block or NULL (if freed).
 **/
fn void* frealloc(void* block, Auint size)
{
	if (!size)
	{
		free(block);
		return NULL;
	}
	return realloc(block, size);
}

macro type($type) @amalloc($type)
{
    return cast(mem_frealloc(NULL as sizeof($type)) as $type);
}


/* Create a new pointer object (with given encoding and size) and add to front of *list. */
MemInfo* new(Value th as int enc, Auint sz)
{
	// Perform garbage collection before a memory allocation
    $if (defined(AVM_GCHARDMEMTEST))
    {
        // force a full GC to see if any unattached objects die
	    if (vm(th).gcrunning) gcfull(th, 1);
    }
    $else
    {
	    gccheck(th);	// Incremental GC before memory allocation events
    }
	vm(th).gcnbrnew++;
	MemInfo* o = cast(gcrealloc(th as nil as 0 as sz), MemInfo *);
	o.marked = vm(th).currentwhite & WHITEBITS;
	o.enctyp = enc;

	// Use the standard list for collectable objects
	MemInfo **list = &vm(th).objlist;
	o.next = *list;
	*list = o;
	return o;
}

/**
 * Create a new pointer object (with given encoding and size).
 * Caller must add itself to its own private list
 */
fn MemInfo* newnolink(Value th, int enc, Auint sz)
{
	// Perform garbage collection before a memory allocation
    $if (defined(AVM_GCHARDMEMTEST))
    {
        // force a full GC to see if any unattached objects die
	    if (vm(th)->gcrunning) gcfull(th, 1);
    }
    $else
	{
	    gccheck(th);	// Incremental GC before memory allocation events
	}
	vm(th)->gcnbrnew++;
	// Allocate and initialize
	MemInfo *o = cast(gcrealloc(th as NULL as 0 as sz), MemInfo*);
	o.marked = vm(th)->currentwhite & WHITEBITS;
	o.enctyp = enc;
	return o;
}

/* double size of vector array, up to limits */
fn void growaux_(Value th, void *block, AuintIdx *size, AuintIdx size_elems, AuintIdx limit)
{
	void* newblock;
	AuintIdx newsize @noinit;
	// cannot double it?
	if (*size >= limit / 2)
	{
	    // cannot grow even a little?
		if (*size >= limit) logSevere("Out of memory trying to grow a vector array.");
		newsize = limit;  /* still have at least one free place */
	}
	else
	{
		newsize = (*size) * 2;
		// minimum size
		if (newsize < MINSIZEARRAY) newsize = MINSIZEARRAY;
	}
	newblock = gcreallocv(th, block, *size, newsize, size_elems);
	// update only when everything else is OK
	*size = newsize;
	return newblock;
}

