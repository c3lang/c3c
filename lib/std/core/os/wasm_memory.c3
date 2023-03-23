module std::core::mem::allocator;


const usz WASM_BLOCK_SIZE = 65536;

WasmMemory wasm_memory;

struct WasmMemory
{
	usz allocation;
	uptr use;
}

fn char[]! WasmMemory.allocate_block(WasmMemory* this, usz bytes)
{
	if (!this.allocation)
	{
		this.allocation = $$wasm_memory_size(0) * WASM_BLOCK_SIZE;
	}
	isz bytes_required = bytes + this.use - this.allocation;
    if (bytes_required <= 0)
	{
		defer this.use += bytes;
		return ((char*)this.use)[:bytes];
	}

	usz blocks_required = (bytes_required + WASM_BLOCK_SIZE + 1) / WASM_BLOCK_SIZE;
	if ($$wasm_memory_grow(0, blocks_required) == -1) return AllocationFailure.OUT_OF_MEMORY?;
	this.allocation = $$wasm_memory_size(0) * WASM_BLOCK_SIZE;
	defer this.use += bytes;
	return ((char*)this.use)[:bytes];
}