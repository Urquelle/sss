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

using namespace Urq::Os::api;
using namespace Urq::Sss::api;

#include "tests/string.cpp"

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

bool
test_lex() {
    bool success = true;

    /* @AUFGABE: implementieren */

    return success;
}

bool
test_expr() {
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

    TEST("log_and_F_T", "falsch && wahr;", 0);
    TEST("log_and_T_F", "wahr && falsch;", 0);
    TEST("log_and_T_T", "wahr && wahr;", 1);
    TEST("log_or_F_T", "falsch || wahr;", 1);
    TEST("log_or_T_F", "wahr || falsch;", 1);
    TEST("log_or_F_F", "falsch || falsch;", 0);
    TEST("log_and_cplx1", "(1 == 1 && 2 == 2) && (3 == 3);", 1);
    TEST("log_and_cplx2", "(1 == 1 && 1 == 2) && (3 == 3);", 0);
    TEST("log_or_cplx1", "(1 == 2 || 1 == 1) || (3 == 3);", 1);
    TEST("log_or_cplx2", "(1 == 1 || 1 == 2) || (3 == 3);", 1);

    TEST("sdiv_1", "a:s32 = -10; b:s32 = 2; a/b;", -5);
    TEST("sdiv_2", "a:s32 = 10; b:s32 = -2; a/b;", -5);
    TEST("sdiv_3", "a:s32 = -10; b:s32 = -2; a/b;", 5);
    TEST("sdiv_4", "a:s32 = 7; b:s32 = 2; a/b;", 3);
    TEST("sdiv_5", "a:s32 = -7; b:s32 = 2; a/b;", -3);

    TEST("smul_1", "a:s32 = -5; b:s32 = 3; a*b;", -15);
    TEST("smul_2", "a:s32 = -5; b:s32 = -3; a*b;", 15);
    TEST("smul_3", "a:s32 = 5; b:s32 = -3; a*b;", -15);
    TEST("smul_4", "a:s32 = 0; b:s32 = -3; a*b;", 0);
    TEST("smul_5", "a:s32 = -3; b:s32 = 0; a*b;", 0);

    return success;
}

bool
test_stmt() {
    bool success = true;

    printf("\n================ STMTS ================\n\n");
    TEST("a := 5", "a : n32 = 5;", 5)
    TEST("a += 1", "a : n32 = 5; a += 1;", 6)
    TEST("a -= 1", "a : n32 = 5; a -= 1;", 4)
    TEST("b := a", "a : n32 = 5; b := a;", 5)
    TEST("a : *n32", "b: n32; a := *b; @a = 5; b;", 5)
    TEST("a : [3] n32", "a : [3] n32; a[2] = 5; a[2];", 5)
    TEST("a : Vec3", "Vec3 :: obj { x, y, z : n32; } a : Vec3; a.y = 5; a.y;", 5)
    TEST_DBG("a : string", "a := \"abcdef\"; a[0];", 'a')
    TEST("wenn 1 != 2", "wenn 1 != 2 { 1; } sonst { 2; }", 1)
    TEST("wenn 1 > 2", "wenn 1 > 2 { 1; } sonst { 2; }", 2)
    TEST("wenn 1 > 2", "wenn 1 > 2 { 1; } sonst 2 == 1 { 2; } sonst { 5; }", 5)
    TEST("wenn !falsch", "wenn !falsch { 1; } sonst !wahr { 2; } sonst { 3; }", 1)
    TEST("wenn a < 5", "a := 5; wenn a < 5 { 1; } sonst a == 5 { 2; } sonst { 3; }", 2)
    TEST("iter", "a : n32 = 0; iter 0..5 { a += 1; } a;", 5)
    TEST("iter it", "a : n32 = 0; iter it: 0..5 { a += it; } a;", 10)

    TEST("cast_s32_s16", "a : s32 = 0x12345678; b : s16 = a as s16; b;", 0x5678);
    TEST("cast_s32_s8", "a : s32 = 0x12345678; b : s8 = a as s8; b;", 0x78);
    TEST("cast_s8_s32_pos", "a : s8 = 100; b : s32 = a as s32; b;", 100);
    TEST("cast_s8_s32_neg", "a : s8 = -1; b : s32 = a as s32; b;", -1);
    TEST("cast_s8_s32_neg_edge", "a : s8 = -128; b : s32 = a as s32; b;", -128);
    TEST("cast_u8_s32", "a : u8 = 255; b : s32 = a as s32; b;", 255);
    TEST("cast_s16_u8", "a : s16 = -2; b : u8 = a as u8; b;", 0xFE); // -2 (s16) is 0xFFFE, as u8 is 0xFE
    TEST("cast_u16_s8", "a : u16 = 0xFF80; b : s8 = a as s8; b;", -128); // 0xFF80 (u16) is 65408, as s8 (via u8) is 0x80, which is -128

    return success;
}

#include <cstdio> // For printf, fflush if not already included

static Vm::Operand* test_s_operand_rax_helper(uint32_t size) { return Vm::operand_reg(Vm::REG_RAX, size); }
static Vm::Operand* test_s_operand_rcx_helper(uint32_t size) { return Vm::operand_reg(Vm::REG_RCX, size); }

static uint8_t* compile_vm_instrs_to_obj_helper(Vm::Vm* vm, uint64_t entry_point) {
    return obj_create(
        vm->rdata->mem, vm->rdata->used,
        (uint8_t*)vm->text, buf_len(vm->text) * sizeof(Vm::Instr*),
        vm->data->mem, vm->data->used,
        entry_point
    );
}

bool test_syscalls() {
    printf("\n================ SYSCALLS ================\n");
    bool success = true;
    Vm::Vm *vm = nullptr;
    uint8_t *obj = nullptr;
    uint64_t eval_ret;

    printf("Test Syscall: Print Integer (789)... ");
    fflush(stdout);
    vm = Vm::vm_new();
    Vm::vm_emit(vm, Vm::vm_instr(&loc_none, Vm::OP_MOV, test_s_operand_rcx_helper(8), Vm::operand_imm(Vm::value((uint64_t)789, 8), 8)));
    Vm::vm_emit(vm, Vm::vm_instr(&loc_none, Vm::OP_MOV, test_s_operand_rax_helper(8), Vm::operand_imm(Vm::value((uint64_t)1, 8), 8)));
    Vm::vm_emit(vm, Vm::vm_instr(&loc_none, Vm::OP_SYSCALL));
    Vm::vm_emit(vm, Vm::vm_instr(&loc_none, Vm::OP_HLT));
    obj = compile_vm_instrs_to_obj_helper(vm, 0);
    eval_ret = Vm::eval(obj, Vm::VM_FLAGS_NONE);
    if (eval_ret == 0) { printf("OK (RAX=0)\n"); }
    else { printf("FAIL (RAX=%llu)\n", (unsigned long long)eval_ret); success = false; }

    printf("Test Syscall: Print String ('Syscall Test String')... ");
    fflush(stdout);
    vm = Vm::vm_new();
    char test_str[] = "Syscall Test String";
    uint32_t str_offset = Vm::mem_push(vm->rdata, test_str, sizeof(test_str));
    Vm::vm_emit(vm, Vm::vm_instr(&loc_none, Vm::OP_MOV, test_s_operand_rcx_helper(8), Vm::operand_imm(Vm::value((uint64_t)str_offset, 8), 8)));
    Vm::vm_emit(vm, Vm::vm_instr(&loc_none, Vm::OP_MOV, test_s_operand_rax_helper(8), Vm::operand_imm(Vm::value((uint64_t)2, 8), 8)));
    Vm::vm_emit(vm, Vm::vm_instr(&loc_none, Vm::OP_SYSCALL));
    Vm::vm_emit(vm, Vm::vm_instr(&loc_none, Vm::OP_HLT));
    obj = compile_vm_instrs_to_obj_helper(vm, 0);
    eval_ret = Vm::eval(obj, Vm::VM_FLAGS_NONE);
    if (eval_ret == 0) { printf("OK (RAX=0)\n"); }
    else { printf("FAIL (RAX=%llu)\n", (unsigned long long)eval_ret); success = false; }

    printf("Test Syscall: Print Newline... ");
    fflush(stdout);
    vm = Vm::vm_new();
    Vm::vm_emit(vm, Vm::vm_instr(&loc_none, Vm::OP_MOV, test_s_operand_rax_helper(8), Vm::operand_imm(Vm::value((uint64_t)3, 8), 8)));
    Vm::vm_emit(vm, Vm::vm_instr(&loc_none, Vm::OP_SYSCALL));
    Vm::vm_emit(vm, Vm::vm_instr(&loc_none, Vm::OP_HLT));
    obj = compile_vm_instrs_to_obj_helper(vm, 0);
    eval_ret = Vm::eval(obj, Vm::VM_FLAGS_NONE);
    if (eval_ret == 0) { printf("OK (RAX=0)\n"); }
    else { printf("FAIL (RAX=%llu)\n", (unsigned long long)eval_ret); success = false; }
    
    printf("======================================\n");
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

    bool success = true;

#if 1
    success = test_lex()  && success;
    success = test_expr() && success;
    success = test_stmt() && success;
    success = test_syscalls() && success; // Add this line
#else
    for ( int i = 0; i < test_procs_count; ++i ) {
        Test_Proc *t = test_procs[i];
        success = t(false) && success;
    }
#endif

    return !success;
}

