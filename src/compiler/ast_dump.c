#include "compiler_internal.h"

const uint64_t SEGMENT_HEADER = 0xFD55C25FD5B9F4FE;
typedef struct
{
    uint64_t header;
    char name[64];
    const void* start;
    size_t length;
    bool next_segment_exists;
    char contents[];
} Segment;

static void write_segment(const void* ptr, size_t length, bool next_exists, char* name)
{
    length = (length + 7) / 8 * 8;

    Segment segment = {
        .header = SEGMENT_HEADER,
        .start = ptr,
        .length = length,
        .next_segment_exists = next_exists,
    };

    strncpy(segment.name, name, sizeof(segment.name) - 1);

    ssize_t written = write(fileno(stdout), &segment, sizeof(Segment));
    written += write(fileno(stdout), ptr, length);
    ssize_t requested = sizeof(Segment) + length;
    if(written < requested)
    {
        eprintf("Failed to dump segment, %llu bytes written out of %llu bytes requested. Error: ", written, requested);
        perror("");
    }
}

void compiler_ast_dump(void) 
{
    write_segment(&compiler.context, sizeof(GlobalContext), true, "global_context");
    write_segment((void*)arena_zero, (uintptr_t)calloc_arena(16) - arena_zero, true, "malloc_arena");
    write_segment((void*)char_arena_zero, (uintptr_t)calloc_string(16) - char_arena_zero, true, "char_arena");

    write_segment(ast_arena.ptr, ast_arena.allocated, true, "ast_arena");
    write_segment(expr_arena.ptr, expr_arena.allocated, true, "expr_arena");
    write_segment(decl_arena.ptr, decl_arena.allocated, true, "decl_arena");
    write_segment(type_info_arena.ptr, type_info_arena.allocated, true, "type_info_arena");
    write_segment(sourceloc_arena.ptr, sourceloc_arena.allocated, true, "sourceloc_arena");
    
    const char *final_arg = args[arg_count - 1];
    write_segment(args[0], (size_t)(final_arg + strlen(final_arg) - args[0]), false, "args_arena");
}