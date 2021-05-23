#include <windows.h>
#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <string.h>
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

using namespace Urq::Os::api;
using namespace Urq::Sss::api;

#define TEST_EXPR_DBG(Label, Content, Expected_Result, Debug)        \
{                                                                    \
    char *content = NULL;                                            \
         content  = buf_printf(content, "master :: proc() { %s; }", Content); \
    auto tokens   = tokenize("test_expr", content);                  \
    auto ast      = parse(&tokens);                                  \
                    resolve(ast, false);                             \
    auto bc       = compile(ast, mem);                               \
                                                                     \
    if ( Debug ) {                                                   \
        debug(bc, "test_output.S");                                  \
    }                                                                \
                                                                     \
    auto result   = eval(bc, mem);                                   \
                                                                     \
    printf("test ausführen: %-20s", Label);                          \
                                                                     \
    bool test_success = (result == Expected_Result);                 \
    if ( test_success ) {                                            \
        printf(" \x1b[92mIN ORDNUNG");                               \
    } else {                                                         \
        printf(" \x1b[91mFEHLSCHLAG");                               \
    }                                                                \
                                                                     \
    printf("\x1b[0m\n");                                             \
                                                                     \
    success = success && test_success;                               \
                                                                     \
    mem_reset(mem);                                                  \
    resolver_reset();                                                \
    vm_reset(bc);                                                    \
}

#define TEST_EXPR(Label, Content, Expected_Result) TEST_EXPR_DBG(Label, Content, Expected_Result, false)

#define TEST_STMT_DBG(Label, Content, Expected_Result, Debug)        \
{                                                                    \
    char *content = NULL;                                            \
         content  = buf_printf(content, "master :: proc() -> u32 { %s }", Content); \
    auto tokens   = tokenize("test_stmt", content);                  \
    auto ast      = parse(&tokens);                                  \
                    resolve(ast, false);                             \
    auto bc       = compile(ast, mem);                               \
                                                                     \
    if ( Debug ) {                                                   \
        debug(bc, "test_output.S");                                  \
    }                                                                \
                                                                     \
    auto result   = eval(bc, mem);                                   \
                                                                     \
    printf("test ausführen: %-20s", Label);                          \
                                                                     \
    bool test_success = (result == Expected_Result);                 \
    if ( test_success ) {                                            \
        printf(" \x1b[92mIN ORDNUNG");                               \
    } else {                                                         \
        printf(" \x1b[91mFEHLSCHLAG");                               \
    }                                                                \
                                                                     \
    printf("\x1b[0m\n");                                             \
                                                                     \
    success = success && test_success;                               \
                                                                     \
    mem_reset(mem);                                                  \
    resolver_reset();                                                \
    vm_reset(bc);                                                    \
}

#define TEST_STMT(Label, Content, Expected_Result) TEST_STMT_DBG(Label, Content, Expected_Result, false)

bool
test_expr(Mem *mem) {
    bool success = true;

    printf("\n================ EXPRS ================\n\n");
    TEST_EXPR("3 + 5", "3 + 5", 8)
    TEST_EXPR("3 * 2 + 5", "3 * 2 + 5", 11)
    TEST_EXPR("3 * (2 + 5)", "3 * (2 + 5)", 21)
    TEST_EXPR("3 * (2 + 5) - 10", "3 * (2 + 5) - 10", 11)
    TEST_EXPR("wahr", "wahr", 1)
    TEST_EXPR("falsch", "falsch", 0)
    TEST_EXPR("wahr == wahr", "wahr == wahr", 1)
    TEST_EXPR("wahr == falsch", "wahr == falsch", 0)
    TEST_EXPR("wahr == !falsch", "wahr == !falsch", 1)
    TEST_EXPR("wahr != falsch", "wahr != falsch", 1)
    TEST_EXPR("wahr && falsch", "wahr && falsch", 0)
    TEST_EXPR("wahr && wahr", "wahr && wahr", 1)
    TEST_EXPR("1 < 2", "1 < 2", 1)
    TEST_EXPR("1 <= 2", "1 <= 2", 1)
    TEST_EXPR("1 == 2", "1 == 2", 0)
    TEST_EXPR("2 == 2", "2 == 2", 1)
    TEST_EXPR("3 >= 2", "3 >= 2", 1)
    TEST_EXPR("5 > 2", "5 > 2", 1)

    return success;
}

bool
test_stmt(Mem *mem) {
    bool success = true;

    printf("\n================ STMTS ================\n\n");
    TEST_STMT("a := 5", "a : u32 = 5; res a;", 5)
    TEST_STMT("b := a", "a : u32 = 5; b := a; res b;", 5)
    TEST_STMT("wenn 1 != 2", "wenn 1 != 2 { res 1; } sonst { res 2; }", 1)
    TEST_STMT("wenn 1 > 2", "wenn 1 > 2 { res 1; } sonst { res 2; }", 2)
    TEST_STMT("wenn 1 > 2", "wenn 1 > 2 { res 1; } sonst 2 == 1 { res 2; } sonst { res 5; }", 5)
    TEST_STMT("wenn !falsch", "wenn !falsch { res 1; } sonst !wahr { res 2; } sonst { res 3; }", 1)
    TEST_STMT("wenn a < 5", "a := 5; wenn a < 5 { res 1; } sonst a == 5 { res 2; } sonst { res 3; }", 2)
    TEST_STMT("iter", "a : u32 = 0; iter 0..5 { a += 1; } res a;", 5)

    return success;
}

int main(int argc, char* argv[]) {

    perm_arena = arena_new(1024);
    temp_arena = arena_new(1024);

    urq_alloc   = custom_alloc;
    urq_alloct  = custom_alloct;
    urq_calloc  = custom_calloc;
    urq_realloc = custom_realloc;
    urq_dealloc = custom_dealloc;

    os_init();
    resolver_init();

    Mem *mem = mem_new(1024*1024);

    bool success = true;

    success = test_expr(mem) && success;
    success = test_stmt(mem) && success;

    return !success;
}

