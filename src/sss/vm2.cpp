namespace Vm2 {

struct Cpu;

void rip_inc(Cpu *cpu);

enum Vm_Op {
    OP_HLT,
    OP_DATA,

    OP_CDQ,

    OP_ADD,
    OP_SUB,
    OP_IMUL,
    OP_IDIV,

    OP_CMP,

    OP_MOV,
    OP_MOVZX,

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
};

struct Vm_Reg {
    uint64_t val;
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

enum Vm_Regs64 {
#define X(Reg) REG_ ## Reg,
    REGS
#undef X
    REG_COUNT,
};

enum Vm_Regs32 {
    REG_EAX,
    REG_EBX,
    REG_ECX,
    REG_EDX,
};

enum Vm_Regs16 {
    REG_AX,
    REG_BX,
    REG_CX,
    REG_DX,
};

enum Vm_Regs8h {
    REG_AH,
    REG_BH,
    REG_CH,
    REG_DH,
};

enum Vm_Regs8l {
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

    union {
        uint64_t  addr;
        Vm_Regs64 reg64;
        Vm_Regs32 reg32;
        Vm_Regs16 reg16;
        Vm_Regs8l reg8l;
        Vm_Regs8h reg8h;
        Value     val;
    };
};

struct Instr : Loc {
    char    * label;
    Vm_Op     op;

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

enum { MAX_STACK_SIZE = 1024*1024 };
struct Cpu {
    Instrs   instrs;
    uint32_t num_instrs;

    Vm_Reg   regs[REG_COUNT];
    uint8_t  stack[MAX_STACK_SIZE];
};

Instrs vm_instrs;

uint64_t
reg_read(Cpu *cpu, Vm_Regs64 reg) {
    uint64_t result = cpu->regs[reg].val;

    return result;
}

uint32_t
reg_read(Cpu *cpu, Vm_Regs32 reg) {
    uint32_t result = (uint32_t)(cpu->regs[reg].val & 0xffffffff);

    return result;
}

uint16_t
reg_read(Cpu *cpu, Vm_Regs16 reg) {
    uint16_t result = (uint16_t)(cpu->regs[reg].val & 0xffff);

    return result;
}

uint16_t
reg_read(Cpu *cpu, Vm_Regs8l reg) {
    uint8_t result = (uint8_t)(cpu->regs[reg].val & 0xff);

    return result;
}

uint16_t
reg_read(Cpu *cpu, Vm_Regs8h reg) {
    uint8_t result = (uint8_t)((cpu->regs[reg].val >> 2) & 0xff00);

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
reg_write(Cpu *cpu, Vm_Regs64 reg, uint64_t val) {
    cpu->regs[reg].val = val;
}

void
reg_write(Cpu *cpu, Vm_Regs32 reg, uint32_t val) {
    cpu->regs[reg].val = (cpu->regs[reg].val & 0xffffffff00000000) | val;
}

void
reg_write(Cpu *cpu, Vm_Regs16 reg, uint16_t val) {
    cpu->regs[reg].val = (cpu->regs[reg].val & 0xffffffffffff0000) | val;
}

void
reg_write(Cpu *cpu, Vm_Regs8l reg, uint8_t val) {
    cpu->regs[reg].val = (cpu->regs[reg].val & 0xffffffffffffff00) | val;
}

void
reg_write(Cpu *cpu, Vm_Regs8h reg, uint8_t val) {
    uint16_t temp = val;

    cpu->regs[reg].val = (cpu->regs[reg].val & 0xffffffffffff00ff) | ((temp << 2) & 0xff00);
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

Cpu *
cpu_new(Instrs instrs) {
    Cpu *result = urq_allocs(Cpu);

    result->instrs     = instrs;
    result->num_instrs = buf_len(instrs);

    reg_write(result, REG_RSP, MAX_STACK_SIZE-1);

    return result;
}

Value
val_bool(bool val) {
    Value result = {};

    result.kind = VAL_BOOL;
    result.b    = val;

    return result;
}

Value
val_char(char val) {
    Value result = {};

    result.kind = VAL_CHAR;
    result.c    = val;

    return result;
}

Value
val_u64(uint64_t val) {
    Value result = {};

    result.kind = VAL_U64;
    result.u64  = val;

    return result;
}

Value
val_s64(int64_t val) {
    Value result = {};

    result.kind = VAL_U64;
    result.s64  = val;

    return result;
}

Value
val_f32(float val) {
    Value result = {};

    result.kind = VAL_F32;
    result.f32  = val;

    return result;
}

Value
val_str(char *val) {
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
operand_reg(Vm_Regs64 reg) {
    Operand result = {};

    result.kind  = OPERAND_REG64;
    result.reg64 = reg;

    return result;
}

Operand
operand_reg(Vm_Regs32 reg) {
    Operand result = {};

    result.kind  = OPERAND_REG32;
    result.reg32 = reg;

    return result;
}

Operand
operand_reg(Vm_Regs16 reg) {
    Operand result = {};

    result.kind  = OPERAND_REG16;
    result.reg16 = reg;

    return result;
}

Operand
operand_reg(Vm_Regs8l reg) {
    Operand result = {};

    result.kind  = OPERAND_REG8L;
    result.reg8l = reg;

    return result;
}

Operand
operand_reg(Vm_Regs8h reg) {
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
vm_instr(Loc *loc, Vm_Op op, Operand operand1, Operand operand2, char *label = NULL) {
    Instr *result = urq_allocs(Instr);

    loc_copy(loc, result);

    result->label    = label;
    result->op       = op;
    result->operand1 = operand1;
    result->operand2 = operand2;

    return result;
}

Instr *
vm_instr(Loc *loc, Vm_Op op, Operand operand1) {
    Instr *result = vm_instr(loc, op, operand1, {});

    return result;
}

Instr *
vm_instr(Loc *loc, Vm_Op op) {
    Instr *result = vm_instr(loc, op, {}, {});

    return result;
}

Instr *
vm_instr_fetch(Cpu *cpu) {
    Instr *result = cpu->instrs[reg_read(cpu, REG_RIP)];
    rip_inc(cpu);

    return result;
}

void
vm_emit(Instr *instr) {
    buf_push(vm_instrs, instr);
}

void
rsp_dec(Cpu *cpu, uint8_t by_how_much) {
    reg_write(cpu, REG_RSP, reg_read(cpu, REG_RSP) - by_how_much);
}

void
rsp_dec(Cpu *cpu) {
    rsp_dec(cpu, 1);
}

void
rsp_inc(Cpu *cpu, uint8_t by_how_much) {
    reg_write(cpu, REG_RSP, reg_read(cpu, REG_RSP) + by_how_much);
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
vm_stack_push(Cpu *cpu, uint64_t val) {
    uint8_t size = 8;

    /* @INFO: der rsp zeigt auf das nächste freie byte. da der wert aber 8 bytes groß ist, müßen
     *        wir den rsp um weitere 7 bytes verschieben um platz für den wert zu machen.
     */
    rsp_dec(cpu, size - 1);
    *(uint64_t *)(cpu->stack + reg_read(cpu, REG_RSP)) = val;
    /* @INFO: danach schreiben wir die größe des wertes, damit wir später wissen wie weit wir beim popen
     *        entnehmen müssen
     */
    rsp_dec(cpu);
    cpu->stack[reg_read(cpu, REG_RSP)] = size;

    /* @INFO: nach dem schreiben des wertes, müssen wir den rsp wieder auf das nächste freie byte
     *        zeigen lassen
     */
    rsp_dec(cpu);
}

void
vm_stack_pop(Cpu *cpu, Vm_Regs64 reg) {
    assert(reg_read(cpu, REG_RSP) < MAX_STACK_SIZE);

    rsp_inc(cpu);
    uint8_t size = cpu->stack[reg_read(cpu, REG_RSP)];
    rsp_inc(cpu);
    assert(size == 8);
    reg_write(cpu, reg, *(uint64_t *)(cpu->stack + reg_read(cpu, REG_RSP)));
    rsp_inc(cpu, size - 1);
}

void
vm_stack_pop(Cpu *cpu, Vm_Regs32 reg) {
    assert(reg_read(cpu, REG_RSP) < MAX_STACK_SIZE);

    rsp_inc(cpu);
    uint8_t size = cpu->stack[reg_read(cpu, REG_RSP)];
    rsp_inc(cpu);
    assert(size == 8);
    reg_write(cpu, reg, *(uint32_t *)(cpu->stack + reg_read(cpu, REG_RSP)));
    rsp_inc(cpu, size - 1);
}

void
vm_stack_pop(Cpu *cpu, Vm_Regs16 reg) {
    assert(reg_read(cpu, REG_RSP) < MAX_STACK_SIZE);

    rsp_inc(cpu);
    uint8_t size = cpu->stack[reg_read(cpu, REG_RSP)];
    rsp_inc(cpu);
    assert(size == 8);
    reg_write(cpu, reg, *(uint16_t *)(cpu->stack + reg_read(cpu, REG_RSP)));
    rsp_inc(cpu, size - 1);
}

void
vm_stack_pop(Cpu *cpu, Vm_Regs8l reg) {
    assert(reg_read(cpu, REG_RSP) < MAX_STACK_SIZE);

    rsp_inc(cpu);
    uint8_t size = cpu->stack[reg_read(cpu, REG_RSP)];
    rsp_inc(cpu);
    assert(size == 8);
    reg_write(cpu, reg, *(uint8_t *)(cpu->stack + reg_read(cpu, REG_RSP)));
    rsp_inc(cpu, size - 1);
}

void
vm_stack_pop(Cpu *cpu, Vm_Regs8h reg) {
    assert(reg_read(cpu, REG_RSP) < MAX_STACK_SIZE);

    rsp_inc(cpu);
    uint8_t size = cpu->stack[reg_read(cpu, REG_RSP)];
    rsp_inc(cpu);
    assert(size == 8);
    reg_write(cpu, reg, *(uint8_t *)(cpu->stack + reg_read(cpu, REG_RSP)));
    rsp_inc(cpu, size - 1);
}

void
vm_stack_pop(Cpu *cpu, Operand op) {
    switch ( op.kind ) {
        case OPERAND_REG64: {
            vm_stack_pop(cpu, op.reg64);
        } break;
        case OPERAND_REG32: {
            vm_stack_pop(cpu, op.reg32);
        } break;
        case OPERAND_REG16: {
            vm_stack_pop(cpu, op.reg16);
        } break;
        case OPERAND_REG8L: {
            vm_stack_pop(cpu, op.reg8l);
        } break;
        case OPERAND_REG8H: {
            vm_stack_pop(cpu, op.reg8h);
        } break;
    }
}

void
vm_expr(Expr *expr) {
    switch ( expr->kind ) {
        case EXPR_INT: {
            vm_emit(vm_instr(expr, OP_MOV, operand_reg(REG_RAX), operand_imm(val_u64(EINT(expr)->val))));
        } break;

        case EXPR_FLOAT: {
            vm_emit(vm_instr(expr, OP_MOV, operand_reg(REG_RAX), operand_imm(val_f32(EFLOAT(expr)->val))));
        } break;

        case EXPR_BOOL: {
            vm_emit(vm_instr(expr, OP_MOV, operand_reg(REG_RAX), operand_imm(val_bool(EBOOL(expr)->val))));
        } break;

        case EXPR_STR: {
            static uint32_t string_num = 0;

            char *name = NULL;
            name = buf_printf(name, ".string.%d", string_num);

            vm_emit(vm_instr(expr, OP_DATA, operand_name(val_str(".string")), operand_imm(val_str(ESTR(expr)->val)), name));
            vm_emit(vm_instr(expr, OP_LEA, operand_name(val_str(name)), operand_reg(REG_RAX)));

            string_num++;
        } break;

        case EXPR_IDENT: {
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
                vm_emit(vm_instr(expr, OP_CDQ));
                vm_emit(vm_instr(expr, OP_IDIV, operand_reg(REG_RDI)));
            } else if ( EBIN(expr)->op == BIN_LT ) {
                vm_emit(vm_instr(expr, OP_CMP, operand_reg(REG_RAX), operand_reg(REG_RDI)));
                vm_emit(vm_instr(expr, OP_SETL, operand_reg(REG_RAX)));
            } else if ( EBIN(expr)->op == BIN_LTE ) {
                vm_emit(vm_instr(expr, OP_CMP, operand_reg(REG_RAX), operand_reg(REG_RDI)));
                vm_emit(vm_instr(expr, OP_SETLE, operand_reg(REG_RAX)));
            } else if ( EBIN(expr)->op == BIN_EQ ) {
                vm_emit(vm_instr(expr, OP_CMP, operand_reg(REG_RAX), operand_reg(REG_RDI)));
                vm_emit(vm_instr(expr, OP_SETE, operand_reg(REG_AL)));
                vm_emit(vm_instr(expr, OP_MOVZX, operand_reg(REG_AL), operand_reg(REG_EAX)));
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

        default: {
            assert(0);
        } break;
    }
}

void
vm_decl(Decl *decl) {
    switch ( decl->kind ) {
        case DECL_VAR: {
            //
        } break;

        default: {
            assert(0);
        } break;
    }
}

void
vm_stmt(Stmt *stmt) {
    switch ( stmt->kind ) {
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

Instrs
vm_compile(Parsed_File *file) {
    for ( int i = 0; i < buf_len(file->stmts); ++i ) {
        vm_stmt(file->stmts[i]);
    }

    return vm_instrs;
}

bool
operand_is_reg(Operand op) {
    bool result = op.kind >= OPERAND_REG64 && op.kind <= OPERAND_REG8H;

    return result;
}

bool
vm_step(Cpu *cpu) {
    if ( cpu->num_instrs <= reg_read(cpu, REG_RIP) ) {
        return false;
    }

    Instr *instr = vm_instr_fetch(cpu);

    switch ( instr->op ) {
        case OP_CDQ: {
            //
        } break;

        case OP_MOV: {
            if ( operand_is_reg(instr->dst) ) {
                if ( instr->src.kind == OPERAND_IMM ) {
                    reg_write(cpu, instr->dst, instr->src.val.u64);
                }
            }
        } break;

        case OP_MOVZX: {
            uint64_t src = 0;

            if ( operand_is_reg(instr->src) ) {
                src = reg_read(cpu, instr->src);
            } else if ( instr->src.kind == OPERAND_IMM ) {
                src = instr->src.val.u64;
            } else {
                assert(0);
            }

            if ( operand_is_reg(instr->dst) ) {
                reg_write(cpu, instr->dst, src);
            } else {
                assert(0);
            }
        } break;

        case OP_PUSH: {
            if ( operand_is_reg(instr->operand1) ) {
                vm_stack_push(cpu, reg_read(cpu, instr->operand1));
            }
        } break;

        case OP_POP: {
            if ( operand_is_reg(instr->operand1) ) {
                vm_stack_pop(cpu, instr->operand1);
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

            reg_write(cpu, REG_RAX, operand1 - operand2);

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

            // SF≠OF
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

        default: {
            assert(0);
        } break;
    }

    return true;
}

void
vm_eval(Instrs instrs) {
    Cpu *cpu = cpu_new(instrs);

    for (;;) {
        if ( !vm_step(cpu) ) {
            break;
        }
    }

    printf("rax: %lld\n", reg_read(cpu, REG_RAX));
}

}
