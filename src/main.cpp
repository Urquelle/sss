#include <windows.h>
#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <string.h>
#include <vector>
#include <cctype>
#include <assert.h>

#include "os/os.cpp"
#include "util/util.cpp"
#include "sss/sss.cpp"

Arena *perm_arena;
Arena *temp_arena;

ALLOCATOR(custom_alloc) {
    void *result = arena_alloc(perm_arena, size);

    return result;
}

ALLOCATOR(custom_alloct) {
    void *result = arena_alloc(temp_arena, size);

    return result;
}

REALLOCATOR(custom_realloc) {
    void *result = arena_realloc(perm_arena, mem, size);

    return result;
}

DEALLOCATOR(custom_dealloc) {
}

CALLOCATOR(custom_calloc) {
    void *result = calloc(num, size);

    return result;
}

int main(int argc, char* argv[]) {
    using namespace Urq::Os::api;
    using namespace Urq::Sss::api;

    perm_arena = arena_new(1024);
    temp_arena = arena_new(1024);

    urq_alloc   = custom_alloc;
    urq_alloct  = custom_alloct;
    urq_calloc  = custom_calloc;
    urq_realloc = custom_realloc;
    urq_dealloc = custom_dealloc;

    os_init();
    resolver_init();

    char *file_name = NULL;
    Line_Args args = line_arg_push(NULL, line_arg(&file_name, "dateiname", "f", "Pfad zur Programmdatei", LINE_ARG_NOT_REQUIRED));
    parse_args(argc, argv, args);

    if ( argc < 2 ) {
        sss_repl();
    } else {
        char *content = "";
        os_file_read(file_name, &content);

        auto tokens   = tokenize(argv[1], content);
        auto parsed   = parse(&tokens);
        auto resolved = resolve(parsed);
        auto code     = build(parsed);
             code     = optimize(code);
                        eval(code);
    }

    return 0;
}

