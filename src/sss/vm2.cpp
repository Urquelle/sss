namespace Vm2 {

struct Cpu;

void rip_inc(Cpu *cpu);
void vm_stmt(Stmt *stmt);

enum Vm_Op {
    OP_HLT,
    OP_DATA,

    OP_ADD,
    OP_SUB,
    OP_MUL,
    OP_DIV,
    OP_IMUL,
    OP_IDIV,
    OP_CMP,
    OP_MOV,
    OP_LEA,
    OP_PUSH,
    OP_POP,
    OP_SETE,
    OP_SETL,
    OP_SETLE,
    OP_SETG,
    OP_SETGE,
    OP_SETNE,

    OP_CALL,
    OP_RET,
};

#define REGS  \
    X(RAX)    \
    X(RBX)    \
    X(RCX)    \
    X(RDX)    \
    X(RBP)    \
    X(RSI)    \
    X(RDI)    \
    X(RSP)    \
    X(R8)     \
    X(R9)     \
    X(R10)    \
    X(R11)    \
    X(R12)    \
    X(R13)    \
    X(R14)    \
    X(R15)    \
    X(RIP)    \
    X(RFLAGS)

enum Vm_Reg64 {
#define X(Reg) REG_ ## Reg,
    REGS
#undef X
    REG_COUNT,
};

enum Vm_Reg32 {
    REG_EAX,
    REG_EBX,
    REG_ECX,
    REG_EDX,
};

enum Vm_Reg16 {
    REG_AX,
    REG_BX,
    REG_CX,
    REG_DX,
};

enum Vm_Reg8h {
    REG_AH,
    REG_BH,
    REG_CH,
    REG_DH,
};

enum Vm_Reg8l {
    REG_AL,
    REG_BL,
    REG_CL,
    REG_DL,
};

enum Vm_Rflag {
    RFLAG_CF = 0,
    RFLAG_PF = 2,
    RFLAG_AF = 4,
    RFLAG_ZF = 6,
    RFLAG_SF = 7,
    RFLAG_OF = 11,
    RFLAG_DF = 10,
    RFLAG_ID = 21,
};

enum Value_Kind {
    VAL_BOOL,
    VAL_CHAR,
    VAL_U64,
    VAL_S64,
    VAL_F32,
    VAL_STR,
};
struct Value {
    Value_Kind kind;
    uint32_t   size;

    union {
        bool       b;
        char       c;
        uint64_t   u64;
        int64_t    s64;
        float      f32;
        char     * str;
    };
};

enum Operand_Kind {
    OPERAND_NONE,
    OPERAND_IMM,
    OPERAND_REG64,
    OPERAND_REG32,
    OPERAND_REG16,
    OPERAND_REG8L,
    OPERAND_REG8H,
    OPERAND_ADDR,
    OPERAND_NAME,
};
struct Operand {
    Operand_Kind kind;

    bool         with_displacement;
    int32_t      displacement;
    uint32_t     size;

    union {
        uint64_t  addr;
        Vm_Reg64 reg64;
        Vm_Reg32 reg32;
        Vm_Reg16 reg16;
        Vm_Reg8l reg8l;
        Vm_Reg8h reg8h;
        Value     val;
    };
};

struct Instr : Loc {
    char    * label;
    char    * comment;
    Vm_Op     op;
    uint32_t  addr;

    union {
        struct {
            Operand   operand1;
            Operand   operand2;
        };

        struct {
            Operand   dst;
            Operand   src;
        };
    };
};

typedef Instr ** Instrs;

struct Mem {
    uint8_t  * mem;
    uint32_t   used;
    uint32_t   size;
};

struct Cpu {
    Instrs     instrs;
    uint32_t   num_instrs;

    uint64_t   regs[REG_COUNT];

    Mem      * mem;
    uint32_t   stack_size;
};

Instrs   vm_instrs;
Instrs   vm_instrs_labeled;
Vm_Reg64 regs[] = { REG_RCX, REG_RDX, REG_R8,  REG_R9 };

uint32_t
addr_lookup(Value val) {
    assert(val.kind == VAL_STR);

    char *label = intern_str(val.str);

    for ( int i = 0; i < buf_len(vm_instrs_labeled); ++i ) {
        Instr *instr = vm_instrs_labeled[i];

        if ( instr->label == label ) {
            return instr->addr;
        }
    }

    report_error(&loc_none, "keine passenden bezeichner für %s gefunden", label);
    return 0;
}

uint64_t
reg_read(Cpu *cpu, Vm_Reg64 reg) {
    uint64_t result = cpu->regs[reg];

    return result;
}

uint32_t
reg_read(Cpu *cpu, Vm_Reg32 reg) {
    uint32_t result = (uint32_t)(cpu->regs[reg] & 0xffffffff);

    return result;
}

uint16_t
reg_read(Cpu *cpu, Vm_Reg16 reg) {
    uint16_t result = (uint16_t)(cpu->regs[reg] & 0xffff);

    return result;
}

uint16_t
reg_read(Cpu *cpu, Vm_Reg8l reg) {
    uint8_t result = (uint8_t)(cpu->regs[reg] & 0xff);

    return result;
}

uint16_t
reg_read(Cpu *cpu, Vm_Reg8h reg) {
    uint8_t result = (uint8_t)((cpu->regs[reg] >> 2) & 0xff00);

    return result;
}

uint64_t
reg_read(Cpu *cpu, Operand op) {
    switch ( op.kind ) {
        case OPERAND_REG64: {
            return reg_read(cpu, op.reg64);
        } break;

        case OPERAND_REG32: {
            return reg_read(cpu, op.reg32);
        } break;

        case OPERAND_REG16: {
            return reg_read(cpu, op.reg16);
        } break;

        case OPERAND_REG8L: {
            return reg_read(cpu, op.reg8l);
        } break;

        case OPERAND_REG8H: {
            return reg_read(cpu, op.reg8h);
        } break;

        default: {
            assert(0);
            return 0;
        } break;
    }
}

void
reg_write(Cpu *cpu, Vm_Reg64 reg, uint64_t val) {
    cpu->regs[reg] = val;
}

void
reg_write(Cpu *cpu, Vm_Reg32 reg, uint32_t val) {
    cpu->regs[reg] = (cpu->regs[reg] & 0xffffffff00000000) | val;
}

void
reg_write(Cpu *cpu, Vm_Reg16 reg, uint16_t val) {
    cpu->regs[reg] = (cpu->regs[reg] & 0xffffffffffff0000) | val;
}

void
reg_write(Cpu *cpu, Vm_Reg8l reg, uint8_t val) {
    cpu->regs[reg] = (cpu->regs[reg] & 0xffffffffffffff00) | val;
}

void
reg_write(Cpu *cpu, Vm_Reg8h reg, uint8_t val) {
    uint16_t temp = val;

    cpu->regs[reg] = (cpu->regs[reg] & 0xffffffffffff00ff) | ((temp << 2) & 0xff00);
}

void
reg_write(Cpu *cpu, Operand op, uint64_t val) {
    switch ( op.kind ) {
        case OPERAND_REG64: {
            reg_write(cpu, op.reg64, val);
        } break;
        case OPERAND_REG32: {
            reg_write(cpu, op.reg32, (uint32_t)val);
        } break;
        case OPERAND_REG16: {
            reg_write(cpu, op.reg16, (uint16_t)val);
        } break;
        case OPERAND_REG8L: {
            reg_write(cpu, op.reg8l, (uint8_t)val);
        } break;
        case OPERAND_REG8H: {
            reg_write(cpu, op.reg8h, (uint8_t)val);
        } break;
    }
}

void
flags_set(Cpu *cpu, uint32_t flags) {
    reg_write(cpu, REG_RFLAGS, reg_read(cpu, REG_RFLAGS) | flags);
}

void
flags_clear(Cpu *cpu, uint64_t flags) {
    reg_write(cpu, REG_RFLAGS, reg_read(cpu, REG_RFLAGS) & ~(flags));
}

uint32_t
flag_state(Cpu *cpu, Vm_Rflag flag) {
    uint32_t result = (reg_read(cpu, REG_RFLAGS) & flag) == flag;

    return result;
}

enum { PAGE_SIZE = 1024*4 };
Mem *
mem_new(uint32_t size) {
    Mem *result = urq_allocs(Mem);

    result->size = size;
    result->used = 0;
    result->mem  = (uint8_t *)urq_alloc(result->size);

    return result;
}

uint32_t
mem_alloc(Mem *mem, uint32_t size) {
    assert(mem->size > (mem->used + size));
    uint32_t result = mem->used;

    mem->used += size;

    return result;
}

void
mem_write(Mem *mem, uint64_t addr, uint64_t val) {
    assert(addr < mem->size);
    *(uint64_t *)(mem->mem + addr) = val;
}

uint8_t
mem_read8(Mem *mem, uint32_t addr) {
    assert(addr < mem->size);

    uint8_t result = *(uint8_t *)(mem->mem + addr);

    return result;
}

uint16_t
mem_read16(Mem *mem, uint32_t addr) {
    assert(addr < mem->size);

    uint16_t result = *(uint16_t *)(mem->mem + addr);

    return result;
}

uint32_t
mem_read32(Mem *mem, uint32_t addr) {
    assert(addr < mem->size);

    uint32_t result = *(uint32_t *)(mem->mem + addr);

    return result;
}

uint64_t
mem_read64(Mem *mem, uint32_t addr) {
    assert(addr < mem->size);

    uint64_t result = *(uint64_t *)(mem->mem + addr);

    return result;
}

Cpu *
cpu_new(Instrs instrs, uint32_t mem_size, uint32_t start = 0) {
    Cpu *result = urq_allocs(Cpu);

    result->instrs     = instrs;
    result->num_instrs = buf_len(instrs);

    result->stack_size = 1024;
    result->mem        = mem_new(mem_size);

    reg_write(result, REG_RIP, start);
    reg_write(result, REG_RSP, result->mem->size-1);

    return result;
}

Value
value(bool val) {
    Value result = {};

    result.kind = VAL_BOOL;
    result.b    = val;

    return result;
}

Value
value(char val) {
    Value result = {};

    result.kind = VAL_CHAR;
    result.c    = val;

    return result;
}

Value
value(uint64_t val, uint32_t size) {
    Value result = {};

    result.kind = VAL_U64;
    result.size = size;
    result.u64  = val;

    return result;
}

Value
value(int64_t val, uint32_t size) {
    Value result = {};

    result.kind = VAL_U64;
    result.size = size;
    result.s64  = val;

    return result;
}

Value
value(float val) {
    Value result = {};

    result.kind = VAL_F32;
    result.f32  = val;

    return result;
}

Value
value(char *val) {
    Value result = {};

    result.kind = VAL_STR;
    result.str  = val;

    return result;
}

Operand
operand_imm(Value val) {
    Operand result = {};

    result.kind = OPERAND_IMM;
    result.val  = val;

    return result;
}

Operand
operand_reg(Vm_Reg64 reg, int32_t displacement) {
    Operand result = {};

    result.kind  = OPERAND_REG64;
    result.displacement = displacement;
    result.with_displacement = true;
    result.reg64 = reg;

    return result;
}

Operand
operand_reg(Vm_Reg64 reg) {
    Operand result = {};

    result.kind  = OPERAND_REG64;
    result.reg64 = reg;

    return result;
}

Operand
operand_reg(Vm_Reg32 reg, int32_t displacement) {
    Operand result = {};

    result.kind  = OPERAND_REG32;
    result.displacement = displacement;
    result.with_displacement = true;
    result.reg32 = reg;

    return result;
}

Operand
operand_reg(Vm_Reg32 reg) {
    Operand result = {};

    result.kind  = OPERAND_REG32;
    result.reg32 = reg;

    return result;
}

Operand
operand_reg(Vm_Reg16 reg, int32_t displacement) {
    Operand result = {};

    result.kind  = OPERAND_REG16;
    result.displacement = displacement;
    result.with_displacement = true;
    result.reg16 = reg;

    return result;
}

Operand
operand_reg(Vm_Reg16 reg) {
    Operand result = {};

    result.kind  = OPERAND_REG16;
    result.reg16 = reg;

    return result;
}

Operand
operand_reg(Vm_Reg8l reg, int32_t displacement) {
    Operand result = {};

    result.kind  = OPERAND_REG8L;
    result.displacement = displacement;
    result.with_displacement = true;
    result.reg8l = reg;

    return result;
}

Operand
operand_reg(Vm_Reg8l reg) {
    Operand result = {};

    result.kind  = OPERAND_REG8L;
    result.reg8l = reg;

    return result;
}

Operand
operand_reg(Vm_Reg8h reg, int32_t displacement) {
    Operand result = {};

    result.kind  = OPERAND_REG8H;
    result.displacement = displacement;
    result.with_displacement = true;
    result.reg8h = reg;

    return result;
}

Operand
operand_reg(Vm_Reg8h reg) {
    Operand result = {};

    result.kind  = OPERAND_REG8H;
    result.reg8h = reg;

    return result;
}

Operand
operand_name(Value name) {
    Operand result = {};

    result.kind = OPERAND_NAME;
    result.val  = name;

    return result;
}

Instr *
vm_instr(Loc *loc, Vm_Op op, Operand operand1, Operand operand2, char *label = NULL, char *comment = NULL) {
    Instr *result = urq_allocs(Instr);

    loc_copy(loc, result);

    result->label    = label;
    result->comment  = comment;
    result->op       = op;
    result->operand1 = operand1;
    result->operand2 = operand2;

    return result;
}

Instr *
vm_instr(Loc *loc, Vm_Op op, Operand operand1, char *label = NULL, char *comment = NULL) {
    Instr *result = vm_instr(loc, op, operand1, {}, label, comment);

    return result;
}

Instr *
vm_instr(Loc *loc, Vm_Op op, char *label = NULL, char *comment = NULL) {
    Instr *result = vm_instr(loc, op, {}, {}, label, comment);

    return result;
}

Instr *
vm_instr_fetch(Cpu *cpu) {
    Instr *result = cpu->instrs[reg_read(cpu, REG_RIP)];
    rip_inc(cpu);

    return result;
}

uint32_t
vm_emit(Instr *instr) {
    uint32_t result = buf_len(vm_instrs);
    buf_push(vm_instrs, instr);
    instr->addr = result;

    if ( instr->label ) {
        buf_push(vm_instrs_labeled, instr);
    }

    return result;
}

void
rsp_dec(Cpu *cpu, uint8_t by_how_much) {
    reg_write(cpu, REG_RSP, reg_read(cpu, REG_RSP) - by_how_much*8);
}

void
rsp_dec(Cpu *cpu) {
    rsp_dec(cpu, 1);
}

void
rsp_inc(Cpu *cpu, uint8_t by_how_much) {
    reg_write(cpu, REG_RSP, reg_read(cpu, REG_RSP) + by_how_much*8);
}

void
rsp_inc(Cpu *cpu) {
    rsp_inc(cpu, 1);
}

void
rip_inc(Cpu *cpu) {
    reg_write(cpu, REG_RIP, reg_read(cpu, REG_RIP) + 1);
}

void
stack_push(Cpu *cpu, uint64_t val) {
    uint64_t addr = reg_read(cpu, REG_RSP);
    assert(addr > cpu->stack_size);

    mem_write(cpu->mem, addr, val);
    rsp_dec(cpu);
}

uint64_t
stack_pop(Cpu *cpu) {
    rsp_inc(cpu);
    uint64_t result = mem_read64(cpu->mem, (uint32_t)reg_read(cpu, REG_RSP));

    return result;
}

void
stack_pop(Cpu *cpu, Vm_Reg64 reg) {
    rsp_inc(cpu);
    reg_write(cpu, reg, mem_read64(cpu->mem, (uint32_t)reg_read(cpu, REG_RSP)));
}

void
stack_pop(Cpu *cpu, Vm_Reg32 reg) {
    rsp_inc(cpu);
    reg_write(cpu, reg, mem_read32(cpu->mem, (uint32_t)reg_read(cpu, REG_RSP)));
}

void
stack_pop(Cpu *cpu, Vm_Reg16 reg) {
    rsp_inc(cpu);
    reg_write(cpu, reg, mem_read16(cpu->mem, (uint32_t)reg_read(cpu, REG_RSP)));
}

void
stack_pop(Cpu *cpu, Vm_Reg8l reg) {
    rsp_inc(cpu);
    reg_write(cpu, reg, mem_read8(cpu->mem, (uint32_t)reg_read(cpu, REG_RSP)));
}

void
stack_pop(Cpu *cpu, Vm_Reg8h reg) {
    rsp_inc(cpu);
    reg_write(cpu, reg, mem_read8(cpu->mem, (uint32_t)reg_read(cpu, REG_RSP)));
}

void
stack_pop(Cpu *cpu, Operand op) {
    switch ( op.kind ) {
        case OPERAND_REG64: {
            stack_pop(cpu, op.reg64);
        } break;
        case OPERAND_REG32: {
            stack_pop(cpu, op.reg32);
        } break;
        case OPERAND_REG16: {
            stack_pop(cpu, op.reg16);
        } break;
        case OPERAND_REG8L: {
            stack_pop(cpu, op.reg8l);
        } break;
        case OPERAND_REG8H: {
            stack_pop(cpu, op.reg8h);
        } break;
    }
}

void
vm_expr(Expr *expr) {
    switch ( expr->kind ) {
        case EXPR_INT: {
            vm_emit(vm_instr(expr, OP_MOV, operand_reg(REG_RAX), operand_imm(value(EINT(expr)->val, expr->type->size))));
        } break;

        case EXPR_FLOAT: {
            vm_emit(vm_instr(expr, OP_MOV, operand_reg(REG_RAX), operand_imm(value(EFLOAT(expr)->val))));
        } break;

        case EXPR_BOOL: {
            vm_emit(vm_instr(expr, OP_MOV, operand_reg(REG_RAX), operand_imm(value(EBOOL(expr)->val))));
        } break;

        case EXPR_STR: {
            static uint32_t string_num = 0;

            char *name = NULL;
            name = buf_printf(name, ".string.%d", string_num);

            vm_emit(vm_instr(expr, OP_DATA, operand_name(value(".string")), operand_imm(value(ESTR(expr)->val)), name));
            vm_emit(vm_instr(expr, OP_LEA, operand_name(value(name)), operand_reg(REG_RAX)));

            string_num++;
        } break;

        case EXPR_IDENT: {
            assert(EIDENT(expr)->sym);
            Sym *sym = EIDENT(expr)->sym;

            if (sym->decl->is_global) {
                vm_emit(vm_instr(expr, OP_LEA, operand_reg(REG_RAX), operand_name(value(EIDENT(expr)->val))));
            } else {
                vm_emit(vm_instr(expr, OP_LEA, operand_reg(REG_RAX), operand_reg(REG_RBP, -(int32_t)sym->decl->offset)));
            }
        } break;

        case EXPR_BIN: {
            vm_expr(EBIN(expr)->right);
            vm_emit(vm_instr(expr, OP_PUSH, operand_reg(REG_RAX)));

            vm_expr(EBIN(expr)->left);
            vm_emit(vm_instr(expr, OP_POP, operand_reg(REG_RDI)));

            if ( EBIN(expr)->op == BIN_ADD ) {
                vm_emit(vm_instr(expr, OP_ADD, operand_reg(REG_RAX), operand_reg(REG_RDI)));
            } else if ( EBIN(expr)->op == BIN_SUB ) {
                vm_emit(vm_instr(expr, OP_SUB, operand_reg(REG_RAX), operand_reg(REG_RDI)));
            } else if ( EBIN(expr)->op == BIN_MUL ) {
                vm_emit(vm_instr(expr, OP_IMUL, operand_reg(REG_RAX), operand_reg(REG_RDI)));
            } else if ( EBIN(expr)->op == BIN_DIV ) {
                if ( EBIN(expr)->right->type->is_signed ) {
                    vm_emit(vm_instr(expr, OP_DIV, operand_reg(REG_RDI)));
                } else {
                    vm_emit(vm_instr(expr, OP_IDIV, operand_reg(REG_RDI)));
                }
            } else if ( EBIN(expr)->op == BIN_LT ) {
                vm_emit(vm_instr(expr, OP_CMP, operand_reg(REG_RAX), operand_reg(REG_RDI)));
                vm_emit(vm_instr(expr, OP_SETL, operand_reg(REG_RAX)));
            } else if ( EBIN(expr)->op == BIN_LTE ) {
                vm_emit(vm_instr(expr, OP_CMP, operand_reg(REG_RAX), operand_reg(REG_RDI)));
                vm_emit(vm_instr(expr, OP_SETLE, operand_reg(REG_RAX)));
            } else if ( EBIN(expr)->op == BIN_EQ ) {
                vm_emit(vm_instr(expr, OP_CMP, operand_reg(REG_RAX), operand_reg(REG_RDI)));
                vm_emit(vm_instr(expr, OP_SETE, operand_reg(REG_RAX)));
            } else if ( EBIN(expr)->op == BIN_GTE ) {
                vm_emit(vm_instr(expr, OP_CMP, operand_reg(REG_RAX), operand_reg(REG_RDI)));
                vm_emit(vm_instr(expr, OP_SETGE, operand_reg(REG_RAX)));
            } else if ( EBIN(expr)->op == BIN_GT ) {
                vm_emit(vm_instr(expr, OP_CMP, operand_reg(REG_RAX), operand_reg(REG_RDI)));
                vm_emit(vm_instr(expr, OP_SETG, operand_reg(REG_RAX)));
            } else {
                assert(0);
            }
        } break;

        case EXPR_PAREN: {
            vm_expr(EPAREN(expr)->expr);
        } break;

        case EXPR_CALL: {
            for ( int i = (int32_t)ECALL(expr)->num_args; i > 0; --i ) {
                vm_expr(ECALL(expr)->args[i-1]);
                vm_emit(vm_instr(expr, OP_PUSH, operand_reg(REG_RAX)));
            }

            for ( int i = 0; i < ECALL(expr)->num_args; ++i ) {
                /* @INFO: die ersten 4 argumente werden in die register gepackt. die übrigen werden auf
                 *        dem stack gelassen
                 */
                if ( i == 4 ) {
                    break;
                }

                vm_emit(vm_instr(expr, OP_POP, operand_reg(regs[i])));
            }

            vm_expr(ECALL(expr)->base);
            vm_emit(vm_instr(expr, OP_CALL, operand_reg(REG_RAX)));
        } break;

        default: {
            assert(0);
        } break;
    }
}

void
vm_decl(Decl *decl) {
    switch ( decl->kind ) {
        case DECL_VAR: {
            if ( decl->is_global ) {
                assert(0);
            } else {
                if ( DVAR(decl)->expr ) {
                    vm_expr(DVAR(decl)->expr);
                    vm_emit(vm_instr(decl, OP_MOV, operand_reg(REG_RBP, -(int32_t)decl->offset), operand_reg(REG_RAX)));
                }
            }
        } break;

        case DECL_PROC: {
            vm_emit(vm_instr(decl, OP_DATA, operand_name(value(".text"))));
            vm_emit(vm_instr(decl, OP_DATA, operand_name(value(".global")), operand_name(value(decl->name))));
            vm_emit(vm_instr(decl, OP_PUSH, operand_reg(REG_RBP), decl->name));
            vm_emit(vm_instr(decl, OP_MOV, operand_reg(REG_RBP), operand_reg(REG_RSP)));

            if ( DPROC(decl)->scope->frame_size ) {
                vm_emit(vm_instr(decl, OP_SUB, operand_reg(REG_RSP), operand_imm(value((uint64_t)DPROC(decl)->scope->frame_size, 8))));
            }

            uint8_t reg = 0;
            for ( uint32_t i = 0; i < DPROC(decl)->sign->num_params; ++i ) {
                Decl_Var *param = DPROC(decl)->sign->params[i];

                if ( i < 4 ) {
                    vm_emit(vm_instr(decl, OP_MOV, operand_reg(REG_RBP, -(int32_t)param->offset), operand_reg(regs[reg++])));
                } else {
                    vm_emit(vm_instr(decl, OP_POP, operand_reg(REG_RBP, -(int32_t)param->offset)));
                }
            }

            vm_stmt(DPROC(decl)->block);

            char *label = NULL;
            label = buf_printf(label, "%s.end", decl->name);

            vm_emit(vm_instr(decl, OP_MOV, operand_reg(REG_RSP), operand_reg(REG_RBP), label));
            vm_emit(vm_instr(decl, OP_POP, operand_reg(REG_RBP)));
            vm_emit(vm_instr(decl, OP_RET));
        } break;

        default: {
            assert(0);
        } break;
    }
}

void
vm_stmt(Stmt *stmt) {
    switch ( stmt->kind ) {
        case STMT_BLOCK: {
            for ( int i = 0; i < SBLOCK(stmt)->num_stmts; ++i ) {
                vm_stmt(SBLOCK(stmt)->stmts[i]);
            }
        } break;

        case STMT_EXPR: {
            vm_expr(SEXPR(stmt)->expr);
        } break;

        case STMT_DECL: {
            vm_decl(SDECL(stmt)->decl);
        } break;

        default: {
            assert(0);
        } break;
    }
}

void
compile_procs(Stmts stmts) {
    for ( int i = 0; i < buf_len(stmts); ++i ) {
        Stmt *stmt = stmts[i];

        if ( stmt->kind == STMT_DECL && SDECL(stmt)->decl->kind == DECL_PROC ) {
            vm_stmt(stmt);
        }
    }
}

bool
operand_is_reg(Operand op) {
    bool result = op.kind >= OPERAND_REG64 && op.kind <= OPERAND_REG8H;

    return result;
}

bool
step(Cpu *cpu) {
    if ( cpu->num_instrs <= reg_read(cpu, REG_RIP) ) {
        return false;
    }

    Instr *instr = vm_instr_fetch(cpu);

    switch ( instr->op ) {
        case OP_DATA: {
            //
        } break;

        case OP_LEA: {
            if ( operand_is_reg(instr->dst) ) {
                if ( operand_is_reg(instr->src) ) {
                    if ( instr->src.with_displacement ) {
                        reg_write(cpu, instr->dst, mem_read64(cpu->mem, (uint32_t)(reg_read(cpu, instr->src) + instr->src.displacement)));
                    } else {
                        reg_write(cpu, instr->dst, reg_read(cpu, instr->src));
                    }
                } else if ( instr->src.kind == OPERAND_NAME ) {
                    reg_write(cpu, instr->dst, addr_lookup(instr->src.val));
                } else {
                    assert(0);
                }
            } else {
                assert(0);
            }
        } break;

        case OP_MOV: {
            if ( operand_is_reg(instr->dst) ) {
                if ( instr->dst.with_displacement ) {
                    if ( instr->src.kind == OPERAND_IMM ) {
                        mem_write(cpu->mem, reg_read(cpu, instr->dst) + instr->dst.displacement, instr->src.val.u64);
                    } else if ( operand_is_reg(instr->src) ) {
                        mem_write(cpu->mem, reg_read(cpu, instr->dst) + instr->dst.displacement, reg_read(cpu, instr->src));
                    } else {
                        assert(0);
                    }
                } else {
                    if ( instr->src.kind == OPERAND_IMM ) {
                        reg_write(cpu, instr->dst, instr->src.val.u64);
                    } else if ( operand_is_reg(instr->src) ) {
                        reg_write(cpu, instr->dst, reg_read(cpu, instr->src));
                    } else {
                        assert(0);
                    }
                }
            } else {
                assert(0);
            }
        } break;

        case OP_PUSH: {
            if ( operand_is_reg(instr->operand1) ) {
                stack_push(cpu, reg_read(cpu, instr->operand1));
            } else {
                assert(0);
            }
        } break;

        case OP_POP: {
            if ( operand_is_reg(instr->dst) ) {
                stack_pop(cpu, instr->dst);
            } else {
                assert(0);
            }
        } break;

        case OP_ADD: {
            uint64_t operand1 = 0;
            uint64_t operand2 = 0;

            if ( operand_is_reg(instr->operand1) ) {
                operand1 = reg_read(cpu, instr->operand1);
            } else if ( instr->operand1.kind == OPERAND_IMM ) {
                operand1 = instr->operand1.val.u64;
            } else {
                assert(0);
            }

            if ( operand_is_reg(instr->operand2) ) {
                operand2 = reg_read(cpu, instr->operand2);
            } else if ( instr->operand2.kind == OPERAND_IMM ) {
                operand2 = instr->operand2.val.u64;
            } else {
                assert(0);
            }

            reg_write(cpu, REG_RAX, operand1 + operand2);
        } break;

        case OP_SUB: {
            uint64_t operand1 = 0;
            uint64_t operand2 = 0;

            if ( operand_is_reg(instr->operand1) ) {
                operand1 = reg_read(cpu, instr->operand1);
            } else if ( instr->operand1.kind == OPERAND_IMM ) {
                operand1 = instr->operand1.val.u64;
            } else {
                assert(0);
            }

            if ( operand_is_reg(instr->operand2) ) {
                operand2 = reg_read(cpu, instr->operand2);
            } else if ( instr->operand2.kind == OPERAND_IMM ) {
                operand2 = instr->operand2.val.u64;
            } else {
                assert(0);
            }

            if ( operand_is_reg(instr->dst) ) {
                reg_write(cpu, instr->dst, operand1 - operand2);
            } else {
                assert(0);
            }

            if ( operand2 > operand1 ) {
                flags_set(cpu, RFLAG_CF);
                flags_clear(cpu, RFLAG_ZF);
            } else if ( operand1 == operand2 ) {
                flags_set(cpu, RFLAG_ZF);
                flags_clear(cpu, RFLAG_CF);
            } else {
                flags_clear(cpu, RFLAG_CF | RFLAG_ZF);
            }
        } break;

        case OP_IMUL: {
            uint64_t operand1 = 0;
            uint64_t operand2 = 0;

            if ( operand_is_reg(instr->operand1) ) {
                operand1 = reg_read(cpu, instr->operand1);
            } else if ( instr->operand1.kind == OPERAND_IMM ) {
                operand1 = instr->operand1.val.u64;
            } else {
                assert(0);
            }

            if ( operand_is_reg(instr->operand2) ) {
                operand2 = reg_read(cpu, instr->operand2);
            } else if ( instr->operand2.kind == OPERAND_IMM ) {
                operand2 = instr->operand2.val.u64;
            } else {
                assert(0);
            }

            reg_write(cpu, REG_RAX, operand1 * operand2);

            if ( reg_read(cpu, REG_RAX) == 0 ) {
                flags_set(cpu, RFLAG_ZF);
            } else {
                flags_clear(cpu, RFLAG_ZF);
            }
        } break;

        case OP_IDIV: {
            uint64_t dividend = reg_read(cpu, REG_RAX);
            uint64_t divisor = 0;

            if ( operand_is_reg(instr->operand1) ) {
                divisor = reg_read(cpu, instr->operand1);
            } else if ( instr->operand1.kind == OPERAND_IMM ) {
                divisor = instr->operand1.val.u64;
            } else {
                assert(0);
            }

            uint64_t quotient  = (uint64_t)(dividend / divisor);
            uint64_t remainder = dividend % divisor;

            reg_write(cpu, REG_RAX, quotient);
            reg_write(cpu, REG_RDX, remainder);
        } break;

        case OP_CMP: {
            uint64_t operand1 = 0;
            uint64_t operand2 = 0;

            if ( operand_is_reg(instr->operand1) ) {
                operand1 = reg_read(cpu, instr->operand1);
            } else if ( instr->operand1.kind == OPERAND_IMM ) {
                operand1 = instr->operand1.val.u64;
            } else {
                assert(0);
            }

            if ( operand_is_reg(instr->operand2) ) {
                operand2 = reg_read(cpu, instr->operand2);
            } else if ( instr->operand2.kind == OPERAND_IMM ) {
                operand2 = instr->operand2.val.u64;
            } else {
                assert(0);
            }

            if ( operand2 > operand1 ) {
                flags_set(cpu, RFLAG_OF);
            } else if ( operand1 == operand2 ) {
                flags_set(cpu, RFLAG_ZF);
            } else {
                flags_clear(cpu, RFLAG_OF | RFLAG_ZF);
            }
        } break;

        case OP_SETL: {
            assert(operand_is_reg(instr->dst));

            // SF≠OF
            reg_write(cpu, instr->dst, flag_state(cpu, RFLAG_SF) != flag_state(cpu, RFLAG_OF));
        } break;

        case OP_SETLE: {
            assert(operand_is_reg(instr->dst));

            // ZF=1 or SF≠OF
            reg_write(cpu, instr->dst, flag_state(cpu, RFLAG_ZF) || (flag_state(cpu, RFLAG_SF) != flag_state(cpu, RFLAG_OF)));
        } break;

        case OP_SETE: {
            assert(operand_is_reg(instr->dst));

            // ZF=1
            reg_write(cpu, instr->dst, flag_state(cpu, RFLAG_ZF));
        } break;

        case OP_SETGE: {
            assert(operand_is_reg(instr->dst));

            // SF=OF
            reg_write(cpu, instr->dst, flag_state(cpu, RFLAG_SF) == flag_state(cpu, RFLAG_OF));
        } break;

        case OP_SETG: {
            assert(operand_is_reg(instr->dst));

            // ZF=0 and SF≠OF
            reg_write(cpu, instr->dst, !flag_state(cpu, RFLAG_ZF) && flag_state(cpu, RFLAG_SF) == flag_state(cpu, RFLAG_OF));
        } break;

        case OP_SETNE: {
            assert(operand_is_reg(instr->dst));

            // ZF=0
            reg_write(cpu, instr->dst, !flag_state(cpu, RFLAG_ZF));
        } break;

        case OP_RET: {
            stack_pop(cpu, REG_R15);
            stack_pop(cpu, REG_R14);
            stack_pop(cpu, REG_R13);
            stack_pop(cpu, REG_R12);
            stack_pop(cpu, REG_RDI);
            stack_pop(cpu, REG_RDX);
            stack_pop(cpu, REG_RCX);
            stack_pop(cpu, REG_RBX);
            stack_pop(cpu, REG_RIP);
        } break;

        case OP_CALL: {
            stack_push(cpu, reg_read(cpu, REG_RIP)+1);
            stack_push(cpu, reg_read(cpu, REG_RBX));
            stack_push(cpu, reg_read(cpu, REG_RCX));
            stack_push(cpu, reg_read(cpu, REG_RDX));
            stack_push(cpu, reg_read(cpu, REG_RDI));
            stack_push(cpu, reg_read(cpu, REG_R12));
            stack_push(cpu, reg_read(cpu, REG_R13));
            stack_push(cpu, reg_read(cpu, REG_R14));
            stack_push(cpu, reg_read(cpu, REG_R15));

            if ( operand_is_reg(instr->operand1) ) {
                reg_write(cpu, REG_RIP, reg_read(cpu, instr->operand1));
            } else if ( instr->operand1.kind == OPERAND_NAME ) {
                reg_write(cpu, REG_RIP, addr_lookup(instr->operand1.val));
            } else {
                assert(0);
            }
        } break;

        default: {
            assert(0);
        } break;
    }

    return true;
}

Instrs
compile(Parsed_File *file) {
    compile_procs(file->stmts);

    for ( int i = 0; i < buf_len(file->stmts); ++i ) {
        Stmt *stmt = file->stmts[i];

        if ( stmt->kind != STMT_DECL || SDECL(stmt)->decl->kind != DECL_PROC ) {
            vm_stmt(stmt);
        }
    }

    return vm_instrs;
}

uint64_t
eval(Instrs instrs) {
    Cpu *cpu = cpu_new(instrs, 1024*1024, 30);

    for (;;) {
        if ( !step(cpu) ) {
            break;
        }
    }

    return reg_read(cpu, REG_RAX);
}

}
