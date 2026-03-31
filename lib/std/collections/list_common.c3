module std::collections::list_common;

<*
 IMPORTANT The returned array must be freed using free_aligned.
*>
macro list_to_aligned_array($Type, self, Allocator allocator)
{
	if (!self.size) return ($Type[]){};
	$Type[] result = alloc::alloc_array_aligned(allocator, $Type, self.size);
	result[..] = self.entries[:self.size];
	return result;
}

macro list_to_array($Type, self, Allocator allocator)
{
	if (!self.size) return ($Type[]){};
	$Type[] result = alloc::alloc_array(allocator, $Type, self.size);
	result[..] = self.entries[:self.size];
	return result;
}

macro void list_reverse(self)
{
	if (self.size < 2) return;
	isz half = self.size / 2U;
	isz end = self.size - 1;
	for (isz i = 0; i < half; i++)
	{
		@swap(self.entries[i], self.entries[end - i]);
	}
}

macro isz list_remove_using_test(self, filter, bool $invert, ctx)
{
	isz size = self.size;
	for (isz i = size, isz k = size; k > 0; k = i)
	{
		// Find last index of item to be deleted.
		$if $invert:
			while (i > 0 && !filter(&self.entries[i - 1], ctx)) i--;
		$else
			while (i > 0 && filter(&self.entries[i - 1], ctx)) i--;
		$endif
		// Remove the items from this index up to the one not to be deleted.
		isz n = self.size - k;
		self.entries[i:n] = self.entries[k:n];
		self.size -= k - i;
		// Find last index of item not to be deleted.
		$if $invert:
			while (i > 0 && filter(&self.entries[i - 1], ctx)) i--;
		$else
			while (i > 0 && !filter(&self.entries[i - 1], ctx)) i--;
		$endif
	}
	return size - self.size;
}

macro isz list_compact(self)
{
	isz size = self.size;
	for (isz i = size; i > 0; i--)
	{
		if (self.entries[i - 1]) continue;
		for (isz j = i; j < size; j++)
		{
			self.entries[j - 1] = self.entries[j];
		}
		self.size--;
	}
	return size - self.size;
}

macro isz list_remove_item(self, value)
{
	isz size = self.size;
	for (isz i = size; i > 0; i--)
	{
		if (!equals(self.entries[i - 1], value)) continue;
		for (isz j = i; j < self.size; j++)
		{
			self.entries[j - 1] = self.entries[j];
		}
		self.size--;
	}
	return size - self.size;
}


macro isz list_remove_if(self, filter, bool $invert)
{
	isz size = self.size;
	for (isz i = size, isz k = size; k > 0; k = i)
	{
		// Find last index of item to be deleted.
		$if $invert:
			while (i > 0 && !filter(&self.entries[i - 1])) i--;
		$else
			while (i > 0 && filter(&self.entries[i - 1])) i--;
		$endif
		// Remove the items from this index up to the one not to be deleted.
		isz n = self.size - k;
		self.entries[i:n] = self.entries[k:n];
		self.size -= k - i;
		// Find last index of item not to be deleted.
		$if $invert:
			while (i > 0 && filter(&self.entries[i - 1])) i--;
		$else
			while (i > 0 && !filter(&self.entries[i - 1])) i--;
		$endif
	}
	return size - self.size;
}
