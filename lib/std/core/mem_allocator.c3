module std::core::mem::allocator;
import std::core::mem::alloc;
import std::core::mem::allocators @public;

// This functionality has moved to std::core::mem::alloc and std::core::mem::allocators

const DEFAULT_SIZE_PREFIX = usz.sizeof;
const DEFAULT_SIZE_PREFIX_ALIGNMENT = usz.alignof;

alias malloc @deprecated("Use the version in mem::alloc instead") = alloc::malloc;
alias calloc @deprecated("Use the version in mem::alloc instead") = alloc::calloc;
alias calloc_try @deprecated("Use the version in mem::alloc instead") = alloc::calloc_try;
alias realloc @deprecated("Use the version in mem::alloc instead") = alloc::realloc;
alias realloc_try @deprecated("Use the version in mem::alloc instead") = alloc::realloc_try;
alias free @deprecated("Use the version in mem::alloc instead") = alloc::free;
alias malloc_aligned @deprecated("Use the version in mem::alloc instead") = alloc::malloc_aligned;
alias calloc_aligned @deprecated("Use the version in mem::alloc instead") = alloc::calloc_aligned;
alias realloc_aligned @deprecated("Use the version in mem::alloc instead") = alloc::realloc_aligned;
alias free_aligned @deprecated("Use the version in mem::alloc instead") = alloc::free_aligned;
alias new @deprecated("Use the version in mem::alloc instead") = alloc::new;
alias new_try @deprecated("Use the version in mem::alloc instead") = alloc::new_try;
alias new_aligned @deprecated("Use the version in mem::alloc instead") = alloc::new_aligned;
alias new_with_padding @deprecated("Use the version in mem::alloc instead") = alloc::new_with_padding;
alias alloc @deprecated("Use the version in mem::alloc instead") = alloc::alloc;
alias malloc_try @deprecated("Use the version in mem::alloc instead") = alloc::malloc_try;
alias alloc_try @deprecated("Use the version in mem::alloc instead") = alloc::alloc_try;
alias alloc_aligned @deprecated("Use the version in mem::alloc instead") = alloc::alloc_aligned;
alias alloc_with_padding @deprecated("Use the version in mem::alloc instead") = alloc::alloc_with_padding;
alias new_array @deprecated("Use the version in mem::alloc instead") = alloc::new_array;
alias new_array_try @deprecated("Use the version in mem::alloc instead") = alloc::new_array_try;
alias wrap @deprecated("Use the version in mem::alloc instead") = allocators::wrap;
alias new_array_aligned @deprecated("Use the version in mem::alloc instead") = alloc::new_array_aligned;
alias alloc_array @deprecated("Use the version in mem::alloc instead") = alloc::alloc_array;
alias alloc_array_aligned @deprecated("Use the version in mem::alloc instead") = alloc::alloc_array_aligned;
alias alloc_array_try @deprecated("Use the version in mem::alloc instead") = alloc::alloc_array_try;
alias realloc_array @deprecated("Use the version in mem::alloc instead") = alloc::realloc_array;
alias realloc_array_aligned @deprecated("Use the version in mem::alloc instead") = alloc::realloc_array_aligned;
alias realloc_array_try @deprecated("Use the version in mem::alloc instead") = alloc::realloc_array_try;
alias clone @deprecated("Use the version in mem::alloc instead") = alloc::clone;
alias clone_slice @deprecated("Use the version in mem::alloc instead") = alloc::clone_slice;
alias clone_aligned @deprecated("Use the version in mem::alloc instead") = alloc::clone_aligned;
alias clone_any @deprecated("Use the version in mem::alloc instead") = alloc::clone_any;
alias @aligned_alloc @deprecated("Use the version in mem::allocators instead") = allocators::@aligned_alloc;
alias @aligned_alloc_fn @deprecated("Use the version in mem::allocators instead") = allocators::@aligned_alloc_fn;
alias @aligned_free @deprecated("Use the version in mem::allocators instead") = allocators::@aligned_free;
alias @aligned_free_fn @deprecated("Use the version in mem::allocators instead") = allocators::@aligned_free_fn;
alias @aligned_realloc @deprecated("Use the version in mem::allocators instead") = allocators::@aligned_realloc;
alias @aligned_realloc_fn @deprecated("Use the version in mem::allocators instead") = allocators::@aligned_realloc_fn;
alias push_pool @deprecated("Use the version in mem::allocators instead") = allocators::push_pool;
alias pop_pool @deprecated("Use the version in mem::allocators instead") = allocators::pop_pool;
alias destroy_temp_allocators @deprecated("Use the version in mem::allocators instead") = allocators::destroy_temp_allocators;
alias thread_allocator @deprecated("Use the version in mem::allocators instead") = allocators::thread_allocator;
alias alignment_for_allocation @deprecated("Use the version in mem::allocators instead") = allocators::alignment_for_allocation;

fn Allocator heap() @deprecated("Use 'mem' instead.") => mem;
fn Allocator temp() @deprecated("Use 'tmem' instead") => tmem;