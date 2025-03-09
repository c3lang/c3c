module std::core::mem::allocator;


const usz WASM_BLOCK_SIZE = 65536;

WasmMemory wasm_memory;

struct WasmMemory
{
	usz allocation;
	uptr use;
}

fn char[]? WasmMemory.allocate_block(&self, usz bytes)
{
	if (!self.allocation)
	{
		self.allocation = $$wasm_memory_size(0) * WASM_BLOCK_SIZE;
	}
	isz bytes_required = bytes + self.use - self.allocation;
	if (bytes_required <= 0)
	{
		defer self.use += bytes;
		return ((char*)self.use)[:bytes];
	}

	usz blocks_required = (bytes_required + WASM_BLOCK_SIZE + 1) / WASM_BLOCK_SIZE;
	if ($$wasm_memory_grow(0, blocks_required) == -1) return mem::OUT_OF_MEMORY?;
	self.allocation = $$wasm_memory_size(0) * WASM_BLOCK_SIZE;
	defer self.use += bytes;
	return ((char*)self.use)[:bytes];
}