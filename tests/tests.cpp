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

#include "macros.h"

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

bool
test_expr(Mem *mem) {
    bool success = true;

    printf("\n================ EXPRS ================\n\n");
    TEST("3 + 5", "3 + 5;", 8)
    TEST("3 * 2 + 5", "3 * 2 + 5;", 11)
    TEST("3 * (2 + 5)", "3 * (2 + 5);", 21)
    TEST("3 * (2 + 5) - 10", "3 * (2 + 5) - 10;", 11)
    TEST("wahr", "wahr;", 1)
    TEST("falsch", "falsch;", 0)
    TEST("wahr == wahr", "wahr == wahr;", 1)
    TEST("wahr == falsch", "wahr == falsch;", 0)
    TEST("wahr == !falsch", "wahr == !falsch;", 1)
    TEST("wahr != falsch", "wahr != falsch;", 1)
    TEST("wahr && falsch", "wahr && falsch;", 0)
    TEST("wahr && wahr", "wahr && wahr;", 1)
    TEST("1 < 2", "1 < 2;", 1)
    TEST("1 <= 2", "1 <= 2;", 1)
    TEST("1 == 2", "1 == 2;", 0)
    TEST("2 == 2", "2 == 2;", 1)
    TEST("3 >= 2", "3 >= 2;", 1)
    TEST("5 > 2", "5 > 2;", 1)

    return success;
}

bool
test_stmt(Mem *mem) {
    bool success = true;

    printf("\n================ STMTS ================\n\n");
    TEST("a := 5", "a : u32 = 5;", 5)
    TEST("a += 1", "a : u32 = 5; a += 1;", 6)
    TEST("a -= 1", "a : u32 = 5; a -= 1;", 4)
    TEST("b := a", "a : u32 = 5; b := a;", 5)
    TEST("a : *u32", "b: u32; a := *b; @a = 5; b;", 5)
    TEST("a : [3] u32", "a : [3] u32; a[2] = 5; a[2];", 5)
    TEST("a : Vec3", "Vec3 :: struktur { x, y, z : u32; } a : Vec3; a.y = 5; a.y;", 5)
    TEST_CAST("a : string", "a := \"abcdef\"; a[0];", 'a', char)
    TEST("wenn 1 != 2", "wenn 1 != 2 { 1; } sonst { 2; }", 1)
    TEST("wenn 1 > 2", "wenn 1 > 2 { 1; } sonst { 2; }", 2)
    TEST("wenn 1 > 2", "wenn 1 > 2 { 1; } sonst 2 == 1 { 2; } sonst { 5; }", 5)
    TEST("wenn !falsch", "wenn !falsch { 1; } sonst !wahr { 2; } sonst { 3; }", 1)
    TEST("wenn a < 5", "a := 5; wenn a < 5 { 1; } sonst a == 5 { 2; } sonst { 3; }", 2)
    TEST("iter", "a : u32 = 0; iter 0..5 { a += 1; } a;", 5)
    TEST("iter it", "a : u32 = 0; iter it: 0..5 { a += it; } a;", 10)

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

