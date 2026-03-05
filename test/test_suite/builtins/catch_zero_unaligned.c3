fn void unaligned_load_store(void* dst, void* src) @nostrip
{
    $$unaligned_store(dst, $$unaligned_load((char*)src, 2, false), 0, false); // #error:  Alignment must not be zero
}