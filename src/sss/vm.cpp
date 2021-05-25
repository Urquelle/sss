namespace Vm {

enum { STACK_SIZE = 1024 };

struct Cpu;
struct Operand;
struct Mem;

void      vm_expr(Expr *expr, Mem *mem, bool assign = false);
void      vm_stmt(Stmt *stmt, Mem *mem);

enum Eval_Flags {
    EVAL_NONE,
    EVAL_REPL,
    EVAL_WITH_ENTRY_POINT,
};

enum Vm_Op : uint8_t {
    OP_HLT,

    OP_ADD,
    OP_CALL,
    OP_CMP,
    OP_DIV,
    OP_ENTER,
    OP_IDIV,
    OP_IMUL,
    OP_JE,
    OP_JMP,
    OP_JNE,
    OP_JNZ,
    OP_JZ,
    OP_LEA,
    OP_LEAVE,
    OP_MOV,
    OP_MUL,
    OP_NEG,
    OP_NOP,
    OP_NOT,
    OP_POP,
    OP_PUSH,
    OP_RET,
    OP_SETE,
    OP_SETG,
    OP_SETGE,
    OP_SETL,
    OP_SETLE,
    OP_SETNE,
    OP_SUB,

    OP_COUNT,
};

#undef REG_NONE
enum Reg_Kind : uint8_t {
    REG_NONE,

    REG_RAX,
    REG_RBX,
    REG_RCX,
    REG_RDX,
    REG_RBP,
    REG_RSI,
    REG_RDI,
    REG_RSP,
    REG_R8,
    REG_R9,
    REG_R10,
    REG_R11,
    REG_R12,
    REG_R13,
    REG_R14,
    REG_R15,
    REG_RIP,
    REG_RFLAGS,
    REG_RDS,

    REG_COUNT,
};

struct Reg {
    Reg_Kind kind;
    uint32_t size;
};

struct Addr {
    Reg_Kind base;
    Reg_Kind index;
    int32_t  scale;
    int32_t  displacement;
};

enum Vm_Rflag : uint8_t {
    RFLAG_CF = 1<<0,
    RFLAG_PF = 1<<1,
    RFLAG_AF = 1<<2,
    RFLAG_ZF = 1<<3,
    RFLAG_SF = 1<<4,
    RFLAG_OF = 1<<5,
    RFLAG_DF = 1<<6,
    RFLAG_ID = 1<<7,
};

enum Value_Kind : uint8_t {
    VAL_BOOL,
    VAL_CHAR,

    VAL_U8,
    VAL_U16,
    VAL_U32,
    VAL_U64,

    VAL_S8,
    VAL_S16,
    VAL_S32,
    VAL_S64,

    VAL_F32,
    VAL_STR,
};
struct Value {
    Value_Kind kind;

    union {
        bool       b;
        char       c;

        uint8_t    u8;
        uint16_t   u16;
        uint32_t   u32;
        uint64_t   u64;

        int8_t     s8;
        int16_t    s16;
        int32_t    s32;
        int64_t    s64;

        float      f32;
        char     * str;
    };
};

enum Operand_Kind : uint8_t {
    OPERAND_NONE,

    OPERAND_ADDR,
    OPERAND_IMM,
    OPERAND_LABEL,
    OPERAND_REG,
};
struct Operand {
    Operand_Kind kind;
    uint32_t     size;

    union {
        char    * label;
        Reg       reg;
        Value     val;
        Operand * op;
        Addr      addr;
    };
};

struct Instr : Loc {
    char    * label;
    char    * comment;
    Vm_Op     op;
    uint32_t  addr;

    union {
        struct {
            Operand *operand1;
            Operand *operand2;
        };

        struct {
            Operand *dst;
            Operand *src;
        };
    };
};

typedef Instr ** Instrs;

struct Section {
    char     * name;
    Instrs     instrs;
    uint32_t   num_instrs;
    uint32_t   offset;
};

struct Mem {
    uint8_t  * mem;
    uint32_t   used;
    uint32_t   size;
};

struct Vm {
    Section * data_section;
    Section * text_section;
};

struct Cpu {
    Instrs     instrs;
    uint32_t   num_instrs;

    uint64_t   regs[REG_COUNT];

    Mem      * mem;
    uint32_t   stack_size;
};

Instrs    vm_instrs;
Instrs    vm_instrs_labeled;

Section ** vm_import_sections;
Section  * vm_data_section;
Section  * vm_text_section;
Section  * vm_curr_section;

Reg_Kind regs[] = { REG_RCX, REG_RDX, REG_R8, REG_R9 };

Value value0 = { VAL_U64, 0 };
Value value1 = { VAL_U64, 1 };

char *
make_label(char *fmt, uint32_t num) {
    char *result = NULL;

    result = buf_printf(result, fmt, num);

    return result;
}

char *
make_label(char *fmt, char *str) {
    char *result = NULL;

    result = buf_printf(result, fmt, str);

    return result;
}

uint32_t
addr_lookup(char *val) {
    char *label = intern_str(val);

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
reg_read64(Cpu *cpu, Reg_Kind reg) {
    uint64_t result = cpu->regs[reg];

    return result;
}

uint32_t
reg_read32(Cpu *cpu, Reg_Kind reg) {
    uint32_t result = (uint32_t)(cpu->regs[reg] & 0xffffffff);

    return result;
}

uint16_t
reg_read16(Cpu *cpu, Reg_Kind reg) {
    uint16_t result = (uint16_t)(cpu->regs[reg] & 0xffff);

    return result;
}

uint16_t
reg_read8(Cpu *cpu, Reg_Kind reg) {
    uint8_t result = (uint8_t)(cpu->regs[reg] & 0xff);

    return result;
}

uint64_t
reg_read(Cpu *cpu, Operand *op) {
    assert(op->kind == OPERAND_REG);

    switch ( op->reg.size ) {
        case 1: {
            return reg_read8(cpu, op->reg.kind);
        } break;

        case 2: {
            return reg_read16(cpu, op->reg.kind);
        } break;

        case 4: {
            return reg_read32(cpu, op->reg.kind);
        } break;

        case 8: {
            return reg_read64(cpu, op->reg.kind);
        } break;

        default: {
            assert(0);
            return 0;
        } break;
    }
}

uint64_t
reg_read(Cpu *cpu, Reg_Kind reg, uint32_t size) {
    switch ( size ) {
        case 1: {
            return reg_read8(cpu, reg);
        } break;

        case 2: {
            return reg_read16(cpu, reg);
        } break;

        case 4: {
            return reg_read32(cpu, reg);
        } break;

        case 8: {
            return reg_read64(cpu, reg);
        } break;

        default: {
            assert(0);
            return 0;
        } break;
    }
}

uint32_t
effective_addr(Cpu *cpu, Operand *op) {
    assert(op->kind == OPERAND_ADDR);

    uint64_t base = 0;
    if ( op->addr.base != REG_NONE ) {
        base = reg_read(cpu, op->addr.base, op->size);
    }

    uint64_t index = 0;
    if ( op->addr.index != REG_NONE ) {
        index = reg_read(cpu, op->addr.index, op->size);
    }

    uint64_t scale = op->addr.scale;
    uint64_t displacement = op->addr.displacement;

    uint32_t result = (uint32_t)(base + index*scale + displacement);

    return result;
}

void
reg_write8(Cpu *cpu, Reg_Kind reg, uint8_t val) {
    cpu->regs[reg] = (cpu->regs[reg] & 0xffffffffffffff00) | val;
}

void
reg_write16(Cpu *cpu, Reg_Kind reg, uint16_t val) {
    cpu->regs[reg] = (cpu->regs[reg] & 0xffffffffffff0000) | val;
}

void
reg_write32(Cpu *cpu, Reg_Kind reg, uint32_t val) {
    cpu->regs[reg] = (cpu->regs[reg] & 0xffffffff00000000) | val;
}

void
reg_write64(Cpu *cpu, Reg_Kind reg, uint64_t val) {
    cpu->regs[reg] = val;
}

void
reg_write(Cpu *cpu, Operand *op, uint64_t val) {
    assert(op->kind == OPERAND_REG);
    switch ( op->reg.size ) {
        case 1: {
            reg_write8(cpu, op->reg.kind, (uint8_t)val);
        } break;

        case 2: {
            reg_write16(cpu, op->reg.kind, (uint16_t)val);
        } break;

        case 4: {
            reg_write32(cpu, op->reg.kind, (uint32_t)val);
        } break;

        case 8: {
            reg_write64(cpu, op->reg.kind, val);
        } break;
    }
}

void
flags_set(Cpu *cpu, uint32_t flags) {
    reg_write64(cpu, REG_RFLAGS, reg_read64(cpu, REG_RFLAGS) | flags);
}

void
flags_clear(Cpu *cpu) {
    reg_write64(cpu, REG_RFLAGS, 0);
}

void
flags_clear(Cpu *cpu, uint64_t flags) {
    reg_write64(cpu, REG_RFLAGS, reg_read64(cpu, REG_RFLAGS) & ~(flags));
}

uint32_t
flag_state(Cpu *cpu, Vm_Rflag flag) {
    uint32_t result = (reg_read64(cpu, REG_RFLAGS) & flag) == flag;

    return result;
}

Section *
section_new(char *name) {
    Section *result = urq_allocs(Section);

    result->name       = name;
    result->instrs     = NULL;
    result->num_instrs = 0;

    return result;
}

void
section_reset(Section *section) {
    section->instrs     = NULL;
    section->num_instrs = 0;
}

void
section_add_instr(Section *section, Instr *instr) {
    buf_push(section->instrs, instr);
    section->num_instrs = buf_len(section->instrs);
}

Vm *
vm_new(Section *data_section, Section *text_section) {
    Vm *result = urq_allocs(Vm);

    result->data_section = data_section;
    result->text_section = text_section;

    return result;
}

void
vm_reset(Vm *vm) {
    section_reset(vm->data_section);
    section_reset(vm->text_section);

    vm_instrs = NULL;
    vm_instrs_labeled = NULL;
}

Mem *
mem_new(uint32_t size) {
    Mem *result = urq_allocs(Mem);

    result->size = size;
    result->used = 0;
    result->mem  = (uint8_t *)urq_alloc(result->size);

    return result;
}

void
mem_reset(Mem *mem) {
    for ( uint32_t i = 0; i < mem->size; ++i ) {
        mem->mem[i] = 0;
    }

    mem->used = 0;
}

int32_t
mem_alloc(Mem *mem, uint32_t size) {
    assert(mem->size > (mem->used + size));
    assert((mem->used + size) < (mem->size - STACK_SIZE));

    int32_t result = mem->used;

    mem->used += size;

    return result;
}

void
mem_copy(Mem *mem, uint32_t addr, uint32_t size, void *data) {
    memcpy(mem->mem + addr, data, size);
}

void
mem_write(Mem *mem, uint64_t addr, uint64_t val, uint32_t size) {
    assert(addr < mem->size);

    switch ( size ) {
        case 1: {
            *(uint8_t *)(mem->mem + addr) = (uint8_t)val;
        } break;

        case 2: {
            *(uint16_t *)(mem->mem + addr) = (uint16_t)val;
        } break;

        case 4: {
            *(uint32_t *)(mem->mem + addr) = (uint32_t)val;
        } break;

        case 8: {
            *(uint64_t *)(mem->mem + addr) = val;
        } break;
    }
}

void
mem_write(Mem *mem, uint64_t addr, uint64_t val) {
    assert(addr < mem->size);
    *(uint64_t *)(mem->mem + addr) = val;
}

void
mem_write(Mem *mem, uint64_t addr, uint32_t val) {
    assert(addr < mem->size);
    *(uint32_t *)(mem->mem + addr) = val;
}

void
mem_write(Mem *mem, uint64_t addr, uint16_t val) {
    assert(addr < mem->size);
    *(uint16_t *)(mem->mem + addr) = val;
}

void
mem_write(Mem *mem, uint64_t addr, uint8_t val) {
    assert(addr < mem->size);
    *(uint8_t *)(mem->mem + addr) = val;
}

uint64_t
mem_read(Mem *mem, uint32_t addr, uint32_t size) {
    uint64_t result = 0;

    switch ( size ) {
        case 1: {
            result = *(uint8_t *)(mem->mem + addr);
        } break;

        case 2: {
            result = *(uint16_t *)(mem->mem + addr);
        } break;

        case 4: {
            result = *(uint32_t *)(mem->mem + addr);
        } break;

        case 8: {
            result = *(uint64_t *)(mem->mem + addr);
        } break;

        default: {
            assert(0);
        } break;
    }

    return result;
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
cpu_new(Instrs instrs, Mem *mem, uint32_t start = 0) {
    Cpu *result = urq_allocs(Cpu);

    result->instrs     = instrs;
    result->num_instrs = buf_len(instrs);

    result->stack_size = STACK_SIZE;
    result->mem        = mem;

    reg_write64(result, REG_RIP, start);
    reg_write64(result, REG_RSP, result->mem->size);

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
value(int64_t val, uint32_t size) {
    Value result = {};

    switch ( size ) {
        case 1: {
            result.kind = VAL_S8;
            result.s8   = (int8_t)val;
        } break;

        case 2: {
            result.kind = VAL_S16;
            result.s16  = (int16_t)val;
        } break;

        case 4: {
            result.kind = VAL_S32;
            result.s32  = (int32_t)val;
        } break;

        case 8: {
            result.kind = VAL_S64;
            result.s64  = val;
        } break;

        default: {
            assert(0);
        } break;
    }

    return result;
}

Value
value(uint64_t val, uint32_t size) {
    Value result = {};

    switch ( size ) {
        case 1: {
            result.kind = VAL_U8;
            result.u8   = (uint8_t)val;
        } break;

        case 2: {
            result.kind = VAL_U16;
            result.u16  = (uint16_t)val;
        } break;

        case 4: {
            result.kind = VAL_U32;
            result.u32  = (uint32_t)val;
        } break;

        case 8: {
            result.kind = VAL_U64;
            result.u64  = val;
        } break;

        default: {
            assert(0);
        } break;
    }

    return result;
}

Value
value(uint32_t val) {
    Value result = {};

    result.kind = VAL_U32;
    result.u32  = val;

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

Operand *
operand_addr(Reg_Kind base, Reg_Kind index, int32_t scale, int32_t displacement, int32_t size) {
    Operand *result = urq_allocs(Operand);

    result->kind              = OPERAND_ADDR;
    result->size              = size;
    result->addr.base         = base;
    result->addr.index        = index;
    result->addr.scale        = scale;
    result->addr.displacement = displacement;

    return result;
}

Operand *
operand_addr(Reg_Kind base, int32_t displacement, int32_t size) {
    Operand *result = urq_allocs(Operand);

    result->kind              = OPERAND_ADDR;
    result->size              = size;
    result->addr.base         = base;
    result->addr.index        = REG_NONE;
    result->addr.scale        = 0;
    result->addr.displacement = displacement;

    return result;
}

Operand *
operand_imm(Value val, uint32_t size) {
    Operand *result = urq_allocs(Operand);

    result->kind = OPERAND_IMM;
    result->size = size;
    result->val  = val;

    return result;
}

Operand *
operand_reg(Reg_Kind reg, uint32_t size) {
    Operand *result = urq_allocs(Operand);

    assert(size >= 1 && size <= 8);

    result->kind = OPERAND_REG;
    result->size = size;
    result->reg  = { reg, size };

    return result;
}

Operand *
operand_rax(int32_t size) {
    return operand_reg(REG_RAX, size);
}

Operand *
operand_rsi(int32_t size) {
    return operand_reg(REG_RSI, size);
}

Operand *
operand_rdi(int32_t size) {
    return operand_reg(REG_RDI, size);
}

Operand *
operand_rbp(int32_t size, int32_t displacement) {
    return operand_addr(REG_RBP, displacement, size);
}

Operand *
operand_args(int32_t size, int32_t count) {
    return operand_reg(regs[count], size);
}

Operand *
operand_label(char *label) {
    Operand *result = urq_allocs(Operand);

    result->kind  = OPERAND_LABEL;
    result->label = label;

    return result;
}

Instr *
vm_instr(Loc *loc, Vm_Op op, Operand *operand1, Operand *operand2, char *label = NULL, char *comment = NULL) {
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
vm_instr(Loc *loc, Vm_Op op, Operand *operand1, char *label = NULL, char *comment = NULL) {
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
    uint64_t rip            = reg_read64(cpu, REG_RIP);
    uint64_t section_offset = reg_read64(cpu, REG_RDS);

    Instr *result = cpu->instrs[rip - section_offset];
    reg_write64(cpu, REG_RIP, reg_read64(cpu, REG_RIP) + 1);

    return result;
}

void
vm_instr_patch(uint32_t index, uint32_t displacement) {
    vm_instrs[index]->operand1->addr.displacement = displacement;
}

uint32_t
vm_emit(Instr *instr) {
    uint32_t result = buf_len(vm_instrs);
    buf_push(vm_instrs, instr);
    instr->addr = result;

    if ( instr->label ) {
        instr->label = intern_str(instr->label);
        buf_push(vm_instrs_labeled, instr);
    }

    section_add_instr(vm_curr_section, instr);

    return result;
}

void
rsp_dec(Cpu *cpu, uint32_t by_how_much) {
    reg_write64(cpu, REG_RSP, reg_read64(cpu, REG_RSP) - by_how_much);
}

void
rsp_inc(Cpu *cpu, uint32_t by_how_much) {
    reg_write64(cpu, REG_RSP, reg_read64(cpu, REG_RSP) + by_how_much);
}

void
stack_push(Cpu *cpu, uint64_t val, uint32_t size) {
    switch ( size ) {
        case 1: {
            rsp_dec(cpu, size);
            uint64_t addr = reg_read64(cpu, REG_RSP);
            assert(addr > cpu->stack_size);
            mem_write(cpu->mem, addr, (uint8_t)val);
        } break;

        case 2: {
            rsp_dec(cpu, size);
            uint64_t addr = reg_read64(cpu, REG_RSP);
            assert(addr > cpu->stack_size);
            mem_write(cpu->mem, addr, (uint16_t)val);
        } break;

        case 4: {
            rsp_dec(cpu, size);
            uint64_t addr = reg_read64(cpu, REG_RSP);
            assert(addr > cpu->stack_size);
            mem_write(cpu->mem, addr, (uint32_t)val);
        } break;

        case 8: {
            rsp_dec(cpu, size);
            uint64_t addr = reg_read64(cpu, REG_RSP);
            assert(addr > cpu->stack_size);
            mem_write(cpu->mem, addr, val);
        } break;

        default: {
            assert(0);
        } break;
    }
}

void
stack_pop(Cpu *cpu, Reg_Kind reg, uint32_t size) {
    switch ( size ) {
        case 1: {
            assert(cpu->mem->size >= reg_read64(cpu, REG_RSP) + 1);
            reg_write8(cpu, reg, mem_read8(cpu->mem, (uint32_t)reg_read64(cpu, REG_RSP)));
            rsp_inc(cpu, 1);
        } break;

        case 2: {
            assert(cpu->mem->size >= reg_read64(cpu, REG_RSP) + 2);
            reg_write16(cpu, reg, mem_read16(cpu->mem, (uint32_t)reg_read64(cpu, REG_RSP)));
            rsp_inc(cpu, 2);
        } break;

        case 4: {
            assert(cpu->mem->size >= reg_read64(cpu, REG_RSP) + 4);
            reg_write32(cpu, reg, mem_read32(cpu->mem, (uint32_t)reg_read64(cpu, REG_RSP)));
            rsp_inc(cpu, 4);
        } break;

        case 8: {
            assert(cpu->mem->size >= reg_read64(cpu, REG_RSP) + 8);
            reg_write64(cpu, reg, mem_read64(cpu->mem, (uint32_t)reg_read64(cpu, REG_RSP)));
            rsp_inc(cpu, 8);
        } break;
    }
}

void
stack_pop(Cpu *cpu, Operand *op) {
    assert(op->kind == OPERAND_REG);

    stack_pop(cpu, op->reg.kind, op->reg.size);
}

void
vm_emit_and(Expr_Bin *expr, Mem *mem) {
    vm_expr(expr->left, mem);
    vm_emit(vm_instr(expr, OP_CMP, operand_rax(expr->type->size), operand_imm(value1, expr->type->size)));
    int32_t jmp1_instr = vm_emit(vm_instr(expr, OP_JNE, operand_addr(REG_NONE, 0, expr->type->size)));

    vm_expr(expr->right, mem);
    vm_emit(vm_instr(expr, OP_CMP, operand_rax(expr->type->size), operand_imm(value1, expr->type->size)));

    vm_instr_patch(jmp1_instr, buf_len(vm_instrs));
}

void
vm_emit_or(Expr_Bin *expr, Mem *mem) {
    vm_expr(expr->left, mem);
    vm_emit(vm_instr(expr, OP_CMP, operand_rax(expr->type->size), operand_imm(value1, expr->type->size)));
    int32_t jmp1_instr = vm_emit(vm_instr(expr, OP_JE, operand_addr(REG_NONE, 0, expr->type->size)));

    vm_expr(expr->right, mem);
    vm_emit(vm_instr(expr, OP_CMP, operand_rax(expr->type->size), operand_imm(value1, expr->type->size)));

    vm_instr_patch(jmp1_instr, buf_len(vm_instrs));
}

void
vm_expr(Expr *expr, Mem *mem, bool assign) {
    switch ( expr->kind ) {
        case EXPR_BOOL: {
            vm_emit(vm_instr(expr, OP_MOV, operand_rax(expr->type->size), operand_imm(value(EBOOL(expr)->val), expr->type->size)));
        } break;

        case EXPR_BIN: {
            uint32_t rhs_size = EBIN(expr)->right->type->size;
            uint32_t lhs_size = EBIN(expr)->left->type->size;

            if ( EBIN(expr)->op == BIN_AND ) {
                vm_emit_and(EBIN(expr), mem);
            } else if ( EBIN(expr)->op == BIN_OR ) {
                vm_emit_and(EBIN(expr), mem);
            } else {
                vm_expr(EBIN(expr)->right, mem);
                vm_emit(vm_instr(expr, OP_PUSH, operand_rax(rhs_size)));

                vm_expr(EBIN(expr)->left, mem);
                vm_emit(vm_instr(expr, OP_POP, operand_rdi(rhs_size)));

                if ( EBIN(expr)->op == BIN_ADD ) {
                    vm_emit(vm_instr(expr, OP_ADD, operand_rax(expr->type->size), operand_rdi(expr->type->size)));
                } else if ( EBIN(expr)->op == BIN_SUB ) {
                    vm_emit(vm_instr(expr, OP_SUB, operand_rax(expr->type->size), operand_rdi(expr->type->size)));
                } else if ( EBIN(expr)->op == BIN_MUL ) {
                    if ( type_issigned(expr->type) ) {
                        vm_emit(vm_instr(expr, OP_IMUL, operand_rax(expr->type->size), operand_rdi(expr->type->size)));
                    } else {
                        vm_emit(vm_instr(expr, OP_MUL, operand_rax(expr->type->size), operand_rdi(expr->type->size)));
                    }
                } else if ( EBIN(expr)->op == BIN_DIV ) {
                    if ( type_issigned(EBIN(expr)->right->type) ) {
                        vm_emit(vm_instr(expr, OP_DIV, operand_rdi(expr->type->size)));
                    } else {
                        vm_emit(vm_instr(expr, OP_IDIV, operand_rdi(expr->type->size)));
                    }
                } else if ( EBIN(expr)->op == BIN_LT ) {
                    vm_emit(vm_instr(expr, OP_CMP, operand_rax(expr->type->size), operand_rdi(expr->type->size)));
                    vm_emit(vm_instr(expr, OP_SETL, operand_rax(expr->type->size)));
                } else if ( EBIN(expr)->op == BIN_LTE ) {
                    vm_emit(vm_instr(expr, OP_CMP, operand_rax(expr->type->size), operand_rdi(expr->type->size)));
                    vm_emit(vm_instr(expr, OP_SETLE, operand_rax(expr->type->size)));
                } else if ( EBIN(expr)->op == BIN_EQ ) {
                    vm_emit(vm_instr(expr, OP_CMP, operand_rax(expr->type->size), operand_rdi(expr->type->size)));
                    vm_emit(vm_instr(expr, OP_SETE, operand_rax(expr->type->size)));
                } else if ( EBIN(expr)->op == BIN_NEQ ) {
                    vm_emit(vm_instr(expr, OP_CMP, operand_rax(expr->type->size), operand_rdi(expr->type->size)));
                    vm_emit(vm_instr(expr, OP_SETNE, operand_rax(expr->type->size)));
                } else if ( EBIN(expr)->op == BIN_GTE ) {
                    vm_emit(vm_instr(expr, OP_CMP, operand_rax(expr->type->size), operand_rdi(expr->type->size)));
                    vm_emit(vm_instr(expr, OP_SETGE, operand_rax(expr->type->size)));
                } else if ( EBIN(expr)->op == BIN_GT ) {
                    vm_emit(vm_instr(expr, OP_CMP, operand_rax(expr->type->size), operand_rdi(expr->type->size)));
                    vm_emit(vm_instr(expr, OP_SETG, operand_rax(expr->type->size)));
                } else {
                    assert(0);
                }
            }
        } break;

        case EXPR_CALL: {
            for ( int i = (int32_t)ECALL(expr)->num_args; i > 0; --i ) {
                Expr *arg = ECALL(expr)->args[i-1];

                vm_expr(arg, mem);
                vm_emit(vm_instr(expr, OP_PUSH, operand_rax(arg->type->size)));
            }

            /* @INFO: die ersten 4 argumente werden in die register gepackt. die übrigen werden auf
             *        dem stack gelassen
             */
            int32_t num_args = (int32_t)MIN(ECALL(expr)->num_args, 4);
            for ( int i = 0; i < num_args; ++i ) {
                Expr *arg = ECALL(expr)->args[i];
                vm_emit(vm_instr(expr, OP_POP, operand_args(arg->type->size, i)));
            }

            vm_expr(ECALL(expr)->base, mem);

            if ( ECALL(expr)->base->type->flags & TYPE_FLAG_SYS_CALL ) {
                report_error(expr, "syscalls werden in der vm noch nicht unterstützt");
            } else {
                vm_emit(vm_instr(expr, OP_CALL, operand_reg(REG_RAX, 8)));
            }
        } break;

        case EXPR_CHAR: {
            vm_emit(vm_instr(expr, OP_MOV, operand_rax(expr->type->size), operand_imm(value((uint64_t)ECHR(expr)->val, 1), 1)));
        } break;

        case EXPR_DEREF: {
            vm_expr(EDEREF(expr)->base, mem);

            if ( !assign ) {
                vm_emit(vm_instr(expr, OP_MOV, operand_reg(REG_RAX, 8), operand_addr(REG_RAX, 0, 8)));
            }
        } break;

        case EXPR_FIELD: {
            /* @INFO: sonderbehandlung */
            if ( EFIELD(expr)->base->type->kind == TYPE_PTR ) {
                // für ptr muß ein mov anstatt eines lea erzeugt werden
                vm_expr(EFIELD(expr)->base, mem, false);
            } else {
                vm_expr(EFIELD(expr)->base, mem, true);
            }

            Expr      * base       = EFIELD(expr)->base;
            Type      * type       = base->type;
            int32_t     num_fields = 0;
            Decl_Vars   fields     = NULL;

            if ( type->kind == TYPE_PTR ) {
                type = TPTR(type)->base;
            }

            if ( type->kind == TYPE_STRUCT ) {
                num_fields = (int32_t)TSTRUCT(type)->num_fields;
                fields     = TSTRUCT(type)->fields;
            } else if ( type->kind == TYPE_ENUM ) {
                num_fields = (int32_t)TENUM(type)->num_fields;
                fields     = TENUM(type)->fields;
            } else if ( type->kind == TYPE_UNION ) {
                num_fields = (int32_t)TENUM(type)->num_fields;
                fields     = TENUM(type)->fields;
            } else {
                assert(0);
            }

            Decl_Var *field = NULL;
            for ( int i = 0; i < num_fields; ++i ) {
                Decl_Var *f = fields[i];

                if ( f->name == EFIELD(expr)->field ) {
                    field = f;
                    break;
                }
            }

            assert(field);
            if ( type->kind == TYPE_STRUCT ) {
                vm_emit(vm_instr(expr, OP_ADD, operand_rax(field->type->size), operand_imm(value((int64_t)field->offset, 4), 4)));

                if ( !assign ) {
                    vm_emit(vm_instr(expr, OP_MOV, operand_rax(field->type->size), operand_addr(REG_RAX, 0, field->type->size)));
                }
            } else if ( type->kind == TYPE_ENUM ) {
                assert(!assign);
                vm_expr(field->expr, mem);
            } else if ( type->kind == TYPE_UNION ) {
                if ( !assign ) {
                    vm_emit(vm_instr(expr, OP_MOV, operand_rax(field->type->size), operand_addr(REG_RAX, 0, field->type->size)));
                }
            } else {
                assert(0);
            }
        } break;

        case EXPR_FLOAT: {
            /* @AUFGABE: floats in entsprechende xmm register schieben */
            vm_emit(vm_instr(expr, OP_MOV, operand_reg(REG_RAX, 8), operand_imm(value(EFLOAT(expr)->val), expr->type->size)));
        } break;

        case EXPR_IDENT: {
            assert(EIDENT(expr)->sym);
            Sym *sym = EIDENT(expr)->sym;

            if (sym->decl->is_global) {
                if ( sym->decl->kind == DECL_PROC ) {
                    if ( !(sym->decl->type->flags & TYPE_FLAG_SYS_CALL) ) {
                        vm_emit(vm_instr(expr, OP_LEA, operand_rax(sym->type->size), operand_label(sym->name)));
                    }
                } else {
                    /* @INFO: sonderbehandlung */
                    if ( assign && expr->type->kind != TYPE_ARRAY ) {
                        vm_emit(vm_instr(expr, OP_LEA, operand_rax(sym->type->size), operand_addr(REG_NONE, sym->decl->offset, sym->type->size)));
                    } else {
                        vm_emit(vm_instr(expr, OP_MOV, operand_rax(sym->type->size), operand_addr(REG_NONE, sym->decl->offset, sym->type->size)));
                    }
                }
            } else {
                /* @INFO: sonderbehandlung */
                if ( assign && expr->type->kind != TYPE_ARRAY ) {
                    vm_emit(vm_instr(expr, OP_LEA, operand_rax(sym->type->size), operand_addr(REG_RBP, sym->decl->offset, sym->type->size)));
                } else {
                    vm_emit(vm_instr(expr, OP_MOV, operand_rax(sym->type->size), operand_addr(REG_RBP, sym->decl->offset, sym->type->size)));
                }
            }
        } break;

        case EXPR_INDEX: {
            uint32_t index_size = EINDEX(expr)->index->type->size;
            uint32_t num_elems  = TARRAY(EINDEX(expr)->base->type)->num_elems;
            uint32_t elem_size  = TARRAY(EINDEX(expr)->base->type)->base->size;

            vm_expr(EINDEX(expr)->base, mem, true);
            vm_emit(vm_instr(expr, OP_PUSH, operand_rax(index_size)));

            vm_expr(EINDEX(expr)->index, mem);
            vm_emit(vm_instr(expr, OP_MUL, operand_rax(index_size), operand_imm(value(elem_size), index_size)));

            vm_emit(vm_instr(expr, OP_POP, operand_rsi(index_size)));
            vm_emit(vm_instr(expr, OP_ADD, operand_rax(index_size), operand_rsi(index_size)));

            if ( !assign ) {
                vm_emit(vm_instr(expr, OP_MOV, operand_rax(index_size), operand_addr(REG_RAX, 0, index_size)));
            }
        } break;

        case EXPR_INT: {
            if ( type_issigned(expr->type) ) {
                vm_emit(vm_instr(expr, OP_MOV, operand_rax(expr->type->size), operand_imm(value((int64_t)EINT(expr)->val, expr->type->size), expr->type->size)));
            } else {
                vm_emit(vm_instr(expr, OP_MOV, operand_rax(expr->type->size), operand_imm(value((uint64_t)EINT(expr)->val, expr->type->size), expr->type->size)));
            }
        } break;

        case EXPR_NOT: {
            vm_expr(ENOT(expr)->expr, mem);
            vm_emit(vm_instr(expr, OP_NOT, operand_rax(expr->type->size)));
        } break;

        case EXPR_PAREN: {
            vm_expr(EPAREN(expr)->expr, mem, assign);
        } break;

        case EXPR_PTR: {
            vm_expr(EPTR(expr)->base, mem, true);
        } break;

        case EXPR_STR: {
            uint32_t str_size = utf8_str_size(ESTR(expr)->val);

            expr->offset = mem_alloc(mem, str_size);
            mem_copy(mem, expr->offset, str_size, ESTR(expr)->val);

            vm_emit(vm_instr(expr, OP_LEA, operand_rax(expr->type->size), operand_addr(REG_NONE, expr->offset, expr->type->size)));
        } break;

        case EXPR_UNARY: {
            vm_expr(EUNARY(expr)->expr, mem);
            vm_emit(vm_instr(expr, OP_NEG, operand_rax(expr->type->size)));
        } break;

        default: {
            assert(0);
        } break;
    }
}

void
vm_decl(Decl *decl, Mem *mem) {
    switch ( decl->kind ) {
        case DECL_PROC: {
            if ( DPROC(decl)->sign->sys_call ) {
                return;
            }

            vm_emit(vm_instr(decl, OP_ENTER, operand_imm(value((uint64_t)DPROC(decl)->scope->frame_size, 4), 4), decl->name));

            uint8_t reg = 0;
            for ( uint32_t i = 0; i < DPROC(decl)->sign->num_params; ++i ) {
                Decl_Var *param = DPROC(decl)->sign->params[i];

                if ( i < 4 ) {
                    vm_emit(vm_instr(decl, OP_MOV, operand_rbp(param->type->size, param->offset), operand_args(param->type->size, reg++)));
                }
            }

            vm_stmt(DPROC(decl)->block, mem);
            vm_emit(vm_instr(decl, OP_LEAVE, make_label("%s.end", decl->name)));
            vm_emit(vm_instr(decl, OP_RET));
        } break;

        case DECL_ENUM:
        case DECL_STRUCT:
        case DECL_TYPE:
        case DECL_UNION:
        {
            // nichts zu tun
        } break;

        case DECL_VAR: {
            Expr *expr = DVAR(decl)->expr;

            if ( decl->is_global ) {
                decl->offset = mem_alloc(mem, decl->type->size);

                if ( expr ) {
                    vm_expr(expr, mem);
                    vm_emit(vm_instr(decl, OP_MOV, operand_addr(REG_NONE, decl->offset, decl->type->size), operand_rax(decl->type->size)));
                }
            } else {
                if ( expr ) {
                    vm_expr(expr, mem);
                    vm_emit(vm_instr(decl, OP_MOV, operand_rbp(expr->type->size, decl->offset), operand_rax(expr->type->size)));
                }
            }
        } break;

        default: {
            assert(0);
        } break;
    }
}

void
vm_stmt(Stmt *stmt, Mem *mem) {
    switch ( stmt->kind ) {
        case STMT_ASSIGN: {
            uint32_t lhs_size = SASSIGN(stmt)->lhs->type->size;
            uint32_t rhs_size = SASSIGN(stmt)->rhs->type->size;

            vm_expr(SASSIGN(stmt)->lhs, mem, true);
            vm_emit(vm_instr(stmt, OP_PUSH, operand_rax(lhs_size)));

            vm_expr(SASSIGN(stmt)->rhs, mem);

            vm_emit(vm_instr(stmt, OP_POP, operand_rdi(4)));
            vm_emit(vm_instr(stmt, OP_MOV, operand_addr(REG_RDI, 0, 4), operand_rax(SASSIGN(stmt)->rhs->type->size)));
        } break;

        case STMT_BLOCK: {
            Stmt **deferred_stmts = NULL;
            for ( int i = 0; i < SBLOCK(stmt)->num_stmts; ++i ) {
                Stmt *s = SBLOCK(stmt)->stmts[i];

                if ( s->kind == STMT_RET ) {
                    continue;
                }

                if ( s->kind == STMT_DEFER ) {
                    buf_push(deferred_stmts, s);
                } else {
                    vm_stmt(s, mem);
                }
            }

            for ( int i = buf_len(deferred_stmts); i > 0; --i ) {
                Stmt_Defer *s = SDEFER(deferred_stmts[i-1]);
                vm_stmt(s->stmt, mem);
            }

            for ( int i = 0; i < SBLOCK(stmt)->num_stmts; ++i ) {
                Stmt *s = SBLOCK(stmt)->stmts[i];

                if ( s->kind != STMT_RET ) {
                    continue;
                }

                vm_stmt(s, mem);
            }
        } break;

        case STMT_BREAK: {
            int32_t addr = vm_emit(vm_instr(stmt, OP_JMP, operand_addr(REG_NONE, 0, 4), (char *)NULL, "break"));

            buf_push(SFOR(SBREAK(stmt)->parent)->break_instrs, addr);
        } break;

        case STMT_CONTINUE: {
            int32_t addr = vm_emit(vm_instr(stmt, OP_JMP, operand_addr(REG_NONE, 0, 4), (char *)NULL, "continue"));

            buf_push(SFOR(SBREAK(stmt)->parent)->continue_instrs, addr);
        } break;

        case STMT_DECL: {
            vm_decl(SDECL(stmt)->decl, mem);
        } break;

        case STMT_EXPR: {
            vm_expr(SEXPR(stmt)->expr, mem);
        } break;

        case STMT_FOR: {
            vm_stmt(SFOR(stmt)->init, mem);
            int32_t loop_start = buf_len(vm_instrs);
            vm_expr(SFOR(stmt)->cond, mem);
            vm_emit(vm_instr(stmt, OP_CMP, operand_rax(SFOR(stmt)->cond->type->size), operand_imm(value1, SFOR(stmt)->cond->type->size)));
            int32_t jmpnz_instr = vm_emit(vm_instr(stmt, OP_JNZ, operand_addr(REG_NONE, 0, SFOR(stmt)->cond->type->size)));
            vm_stmt(SFOR(stmt)->block, mem);
            vm_stmt(SFOR(stmt)->step, mem);
            vm_emit(vm_instr(stmt, OP_JMP, operand_addr(REG_NONE, loop_start, SFOR(stmt)->cond->type->size)));

            /* @AUFGABE: sonst zweig für schleife */
            if ( SFOR(stmt)->stmt_else ) {
                report_error(stmt, "%s wird für die %s schleife noch nicht unterstützt", keyword_else, keyword_for);
            }

            int32_t loop_end = buf_len(vm_instrs);

            vm_instr_patch(jmpnz_instr, loop_end);

            /* @INFO: break anweisungen patchen */
            for ( int i = 0; i < buf_len(SFOR(stmt)->break_instrs); ++i ) {
                vm_instr_patch(SFOR(stmt)->break_instrs[i], loop_end);
            }

            /* @INFO: continue anweisungen patchen */
            for ( int i = 0; i < buf_len(SFOR(stmt)->continue_instrs); ++i ) {
                vm_instr_patch(SFOR(stmt)->continue_instrs[i], loop_start);
            }
        } break;

        case STMT_IF: {
            static uint32_t loop_count = 0;
            char *label = make_label("if.%d", loop_count++);

            vm_expr(SIF(stmt)->cond, mem);
            vm_emit(vm_instr(stmt, OP_CMP, operand_rax(SIF(stmt)->cond->type->size), operand_imm(value1, SIF(stmt)->cond->type->size)));
            int32_t jmp1_instr = vm_emit(vm_instr(stmt, OP_JNE, operand_addr(REG_NONE, 0, SIF(stmt)->cond->type->size), label));
            vm_stmt(SIF(stmt)->stmt, mem);
            int32_t jmp2_instr = vm_emit(vm_instr(stmt, OP_JMP, operand_addr(REG_NONE, 0, SIF(stmt)->cond->type->size)));

            vm_instr_patch(jmp1_instr, buf_len(vm_instrs));

            if ( SIF(stmt)->stmt_else ) {
                vm_stmt(SIF(stmt)->stmt_else, mem);
            }

            vm_instr_patch(jmp2_instr, buf_len(vm_instrs));
        } break;

        case STMT_MATCH: {
            int32_t *jmp_exit_instrs = NULL;

            for ( int i = 0; i < SMATCH(stmt)->num_lines; ++i ) {
                Match_Line *line = SMATCH(stmt)->lines[i];

                vm_expr(line->cond, mem);
                int32_t jmp_next = vm_emit(vm_instr(line, OP_JNZ, operand_addr(REG_NONE, 0, line->cond->type->size)));
                vm_stmt(line->stmt, mem);
                buf_push(jmp_exit_instrs, vm_emit(vm_instr(line, OP_JMP, operand_addr(REG_NONE, 0, line->cond->type->size))));
                vm_instr_patch(jmp_next, buf_len(vm_instrs));
            }

            int32_t exit_addr = buf_len(vm_instrs);
            for ( int i = 0; i < buf_len(jmp_exit_instrs); ++i ) {
                vm_instr_patch(jmp_exit_instrs[i], exit_addr);
            }
        } break;

        case STMT_RET: {
            if ( SRET(stmt)->num_exprs ) {
                vm_expr(SRET(stmt)->exprs[0], mem);
            }

            vm_emit(vm_instr(stmt, OP_LEAVE));
            vm_emit(vm_instr(stmt, OP_RET));
        } break;

        case STMT_WHILE: {
            int32_t loop_start = buf_len(vm_instrs);
            vm_expr(SWHILE(stmt)->cond, mem);
            vm_emit(vm_instr(stmt, OP_CMP, operand_rax(SWHILE(stmt)->cond->type->size), operand_imm(value0, SWHILE(stmt)->cond->type->size)));
            int32_t jmp_instr = vm_emit(vm_instr(stmt, OP_JZ, operand_addr(REG_NONE, 0, SWHILE(stmt)->cond->type->size), operand_imm(value1, SWHILE(stmt)->cond->type->size)));
            vm_stmt(SWHILE(stmt)->block, mem);
            vm_emit(vm_instr(stmt, OP_JMP, operand_addr(REG_NONE, loop_start, SWHILE(stmt)->cond->type->size)));

            vm_instr_patch(jmp_instr, buf_len(vm_instrs));
        } break;

        default: {
            assert(0);
        } break;
    }
}

bool
step(Cpu *cpu) {
    uint64_t section_offset = reg_read64(cpu, REG_RDS);
    uint64_t num_instructions = cpu->num_instrs + section_offset;

    if ( num_instructions <= reg_read64(cpu, REG_RIP) ) {
        return false;
    }

    Instr *instr = vm_instr_fetch(cpu);

    switch ( instr->op ) {
        case OP_ADD: {
            uint64_t operand1 = 0;
            uint64_t operand2 = 0;

            if ( instr->operand1->kind == OPERAND_REG ) {
                operand1 = reg_read(cpu, instr->operand1);
            } else if ( instr->operand1->kind == OPERAND_IMM ) {
                operand1 = instr->operand1->val.u64;
            } else {
                assert(0);
            }

            if ( instr->operand2->kind == OPERAND_REG ) {
                operand2 = reg_read(cpu, instr->operand2);
            } else if ( instr->operand2->kind == OPERAND_IMM ) {
                operand2 = instr->operand2->val.u64;
            } else {
                assert(0);
            }

            reg_write(cpu, instr->dst, operand1 + operand2);
        } break;

        case OP_CALL: {
            stack_push(cpu, reg_read64(cpu, REG_RIP), 8);
            stack_push(cpu, reg_read64(cpu, REG_RBX), 8);
            stack_push(cpu, reg_read64(cpu, REG_RCX), 8);
            stack_push(cpu, reg_read64(cpu, REG_RDX), 8);
            stack_push(cpu, reg_read64(cpu, REG_RDI), 8);
            stack_push(cpu, reg_read64(cpu, REG_R12), 8);
            stack_push(cpu, reg_read64(cpu, REG_R13), 8);
            stack_push(cpu, reg_read64(cpu, REG_R14), 8);
            stack_push(cpu, reg_read64(cpu, REG_R15), 8);

            if ( instr->operand1->kind == OPERAND_REG ) {
                reg_write64(cpu, REG_RIP, reg_read(cpu, instr->operand1));
            } else if ( instr->operand1->kind == OPERAND_LABEL ) {
                reg_write64(cpu, REG_RIP, addr_lookup(instr->operand1->label));
            } else {
                assert(0);
            }
        } break;

        case OP_CMP: {
            uint64_t operand1 = 0;
            uint64_t operand2 = 0;

            if ( instr->operand1->kind == OPERAND_REG ) {
                operand1 = reg_read(cpu, instr->operand1);
            } else if ( instr->operand1->kind == OPERAND_IMM ) {
                operand1 = instr->operand1->val.u64;
            } else {
                assert(0);
            }

            if ( instr->operand2->kind == OPERAND_REG ) {
                operand2 = reg_read(cpu, instr->operand2);
            } else if ( instr->operand2->kind == OPERAND_IMM ) {
                operand2 = instr->operand2->val.u64;
            } else {
                assert(0);
            }

            flags_clear(cpu);
            if ( operand1 < operand2 ) {
                flags_set(cpu, RFLAG_CF);
            } else if ( operand1 == operand2 ) {
                flags_set(cpu, RFLAG_ZF);
            }
        } break;

        case OP_DIV: {
            uint64_t dividend = reg_read64(cpu, REG_RAX);
            uint64_t divisor = 0;

            if ( instr->operand1->kind == OPERAND_REG ) {
                divisor = reg_read(cpu, instr->operand1);
            } else if ( instr->operand1->kind == OPERAND_IMM ) {
                divisor = instr->operand1->val.u64;
            } else {
                assert(0);
            }

            uint64_t quotient  = (uint64_t)(dividend / divisor);
            uint64_t remainder = dividend % divisor;

            reg_write64(cpu, REG_RAX, quotient);
            reg_write64(cpu, REG_RDX, remainder);
        } break;

        case OP_ENTER: {
            stack_push(cpu, reg_read64(cpu, REG_RBP), 8);
            reg_write64(cpu, REG_RBP, reg_read64(cpu, REG_RSP));

            if ( instr->operand1->val.u64 ) {
                reg_write64(cpu, REG_RSP, reg_read64(cpu, REG_RSP) - instr->operand1->val.u64);
            }
        } break;

        case OP_IDIV: {
            uint64_t dividend = reg_read64(cpu, REG_RAX);
            uint64_t divisor = 0;

            if ( instr->operand1->kind == OPERAND_REG ) {
                divisor = reg_read(cpu, instr->operand1);
            } else if ( instr->operand1->kind == OPERAND_IMM ) {
                divisor = instr->operand1->val.u64;
            } else {
                assert(0);
            }

            uint64_t quotient  = (uint64_t)(dividend / divisor);
            uint64_t remainder = dividend % divisor;

            reg_write64(cpu, REG_RAX, quotient);
            reg_write64(cpu, REG_RDX, remainder);
        } break;

        case OP_IMUL: {
            uint64_t operand1 = 0;
            uint64_t operand2 = 0;

            if ( instr->operand1->kind == OPERAND_REG ) {
                operand1 = reg_read(cpu, instr->operand1);
            } else if ( instr->operand1->kind == OPERAND_IMM ) {
                operand1 = instr->operand1->val.u64;
            } else {
                assert(0);
            }

            if ( instr->operand2->kind == OPERAND_REG ) {
                operand2 = reg_read(cpu, instr->operand2);
            } else if ( instr->operand2->kind == OPERAND_IMM ) {
                operand2 = instr->operand2->val.u64;
            } else {
                assert(0);
            }

            reg_write64(cpu, REG_RAX, operand1 * operand2);

            flags_clear(cpu);
            if ( reg_read64(cpu, REG_RAX) == 0 ) {
                flags_set(cpu, RFLAG_ZF);
            }
        } break;

        case OP_JE: {
            if ( flag_state(cpu, RFLAG_ZF) ) {
                if ( instr->operand1->kind == OPERAND_ADDR ) {
                    reg_write64(cpu, REG_RIP, effective_addr(cpu, instr->operand1));
                } else if ( instr->operand1->kind == OPERAND_LABEL ) {
                    reg_write64(cpu, REG_RIP, addr_lookup(instr->operand1->label));
                } else {
                    assert(0);
                }
            }
        } break;

        case OP_JMP: {
            if ( instr->operand1->kind == OPERAND_ADDR ) {
                reg_write64(cpu, REG_RIP, effective_addr(cpu, instr->operand1));
            } else if ( instr->operand1->kind == OPERAND_LABEL ) {
                reg_write64(cpu, REG_RIP, addr_lookup(instr->operand1->label));
            } else {
                assert(0);
            }
        } break;

        case OP_JNE: {
            if ( !flag_state(cpu, RFLAG_ZF) ) {
                if ( instr->operand1->kind == OPERAND_ADDR ) {
                    reg_write64(cpu, REG_RIP, effective_addr(cpu, instr->operand1));
                } else {
                    assert(0);
                }
            }
        } break;

        case OP_JNZ: {
            if ( !flag_state(cpu, RFLAG_ZF) ) {
                if ( instr->operand1->kind == OPERAND_ADDR ) {
                    reg_write64(cpu, REG_RIP, effective_addr(cpu, instr->operand1));
                } else {
                    assert(0);
                }
            }
        } break;

        case OP_JZ: {
            if ( flag_state(cpu, RFLAG_ZF) ) {
                if ( instr->operand1->kind == OPERAND_ADDR ) {
                    reg_write64(cpu, REG_RIP, effective_addr(cpu, instr->operand1));
                } else {
                    assert(0);
                }
            }
        } break;

        case OP_LEA: {
            if ( instr->dst->kind == OPERAND_REG ) {
                if ( instr->src->kind == OPERAND_REG ) {
                    reg_write(cpu, instr->dst, reg_read(cpu, instr->src));
                } else if ( instr->src->kind == OPERAND_LABEL ) {
                    reg_write(cpu, instr->dst, addr_lookup(instr->src->label));
                } else if ( instr->src->kind == OPERAND_ADDR ) {
                    reg_write(cpu, instr->dst, effective_addr(cpu, instr->src));
                } else {
                    assert(0);
                }
            } else {
                assert(0);
            }
        } break;

        case OP_LEAVE: {
            reg_write64(cpu, REG_RSP, reg_read64(cpu, REG_RBP));
            stack_pop(cpu, REG_RBP, 8);
        } break;

        case OP_MOV: {
            if ( instr->dst->kind == OPERAND_REG ) {
                if ( instr->src->kind == OPERAND_IMM ) {
                    reg_write(cpu, instr->dst, instr->src->val.u64);
                } else if ( instr->src->kind == OPERAND_REG ) {
                    reg_write(cpu, instr->dst, reg_read(cpu, instr->src));
                } else if ( instr->src->kind == OPERAND_ADDR ) {
                    reg_write(cpu, instr->dst, mem_read(cpu->mem, effective_addr(cpu, instr->src), instr->src->size));
                } else {
                    assert(0);
                }
            } else if ( instr->dst->kind == OPERAND_ADDR ) {
                if ( instr->src->kind == OPERAND_REG ) {
                    mem_write(cpu->mem, effective_addr(cpu, instr->dst), reg_read(cpu, instr->src), instr->src->size);
                } else if ( instr->src->kind == OPERAND_IMM ) {
                    mem_write(cpu->mem, effective_addr(cpu, instr->dst), instr->src->val.u64, instr->src->size);
                } else if ( instr->src->kind == OPERAND_ADDR ) {
                    mem_write(cpu->mem, effective_addr(cpu, instr->dst), mem_read(cpu->mem, effective_addr(cpu, instr->src), instr->src->size));
                } else {
                    assert(0);
                }
            } else {
                assert(0);
            }
        } break;

        case OP_MUL: {
            uint64_t operand1 = 0;
            uint64_t operand2 = 0;

            if ( instr->operand1->kind == OPERAND_REG ) {
                operand1 = reg_read(cpu, instr->operand1);
            } else if ( instr->operand1->kind == OPERAND_IMM ) {
                operand1 = instr->operand1->val.u64;
            } else {
                assert(0);
            }

            if ( instr->operand2->kind == OPERAND_REG ) {
                operand2 = reg_read(cpu, instr->operand2);
            } else if ( instr->operand2->kind == OPERAND_IMM ) {
                operand2 = instr->operand2->val.u64;
            } else {
                assert(0);
            }

            uint64_t result = operand1 * operand2;
            reg_write(cpu, operand_rax(instr->src->size), result);

            flags_clear(cpu);
            if ( result == 0 ) {
                flags_set(cpu, RFLAG_ZF);
            }
        } break;

        case OP_NEG: {
            flags_clear(cpu);

            if ( instr->operand1->kind == OPERAND_REG ) {
                int64_t val = reg_read(cpu, instr->operand1);
                reg_write(cpu, instr->operand1, -val);

                if ( val == 0 ) {
                    flags_set(cpu, RFLAG_ZF);
                } else {
                    flags_set(cpu, RFLAG_CF);
                }
            } else {
                assert(0);
            }
        } break;

        case OP_NOP: {
            //
        } break;

        case OP_NOT: {
            if ( instr->operand1->kind == OPERAND_REG ) {
                auto val = reg_read(cpu, instr->operand1);
                reg_write(cpu, instr->operand1, !val);
            } else {
                assert(0);
            }
        } break;

        case OP_PUSH: {
            if ( instr->operand1->kind == OPERAND_REG ) {
                stack_push(cpu, reg_read(cpu, instr->operand1), instr->operand1->size);
            } else {
                assert(0);
            }
        } break;

        case OP_POP: {
            if ( instr->dst->kind == OPERAND_REG ) {
                stack_pop(cpu, instr->dst);
            } else {
                assert(0);
            }
        } break;

        case OP_RET: {
            stack_pop(cpu, REG_R15, 8);
            stack_pop(cpu, REG_R14, 8);
            stack_pop(cpu, REG_R13, 8);
            stack_pop(cpu, REG_R12, 8);
            stack_pop(cpu, REG_RDI, 8);
            stack_pop(cpu, REG_RDX, 8);
            stack_pop(cpu, REG_RCX, 8);
            stack_pop(cpu, REG_RBX, 8);
            stack_pop(cpu, REG_RIP, 8);
        } break;

        case OP_SETL: {
            assert(instr->dst->kind == OPERAND_REG);

            reg_write(cpu, instr->dst, flag_state(cpu, RFLAG_CF));
        } break;

        case OP_SETLE: {
            assert(instr->dst->kind == OPERAND_REG);

            reg_write(cpu, instr->dst, flag_state(cpu, RFLAG_ZF) || (flag_state(cpu, RFLAG_CF)));
        } break;

        case OP_SETE: {
            assert(instr->dst->kind == OPERAND_REG);

            reg_write(cpu, instr->dst, flag_state(cpu, RFLAG_ZF));
        } break;

        case OP_SETGE: {
            assert(instr->dst->kind == OPERAND_REG);

            reg_write(cpu, instr->dst, flag_state(cpu, RFLAG_ZF) || !flag_state(cpu, RFLAG_CF));
        } break;

        case OP_SETG: {
            assert(instr->dst->kind == OPERAND_REG);

            reg_write(cpu, instr->dst, !flag_state(cpu, RFLAG_ZF) && !flag_state(cpu, RFLAG_CF));
        } break;

        case OP_SETNE: {
            assert(instr->dst->kind == OPERAND_REG);

            reg_write(cpu, instr->dst, !flag_state(cpu, RFLAG_ZF));
        } break;

        case OP_SUB: {
            uint64_t operand1 = 0;
            uint64_t operand2 = 0;

            if ( instr->operand1->kind == OPERAND_REG ) {
                operand1 = reg_read(cpu, instr->operand1);
            } else if ( instr->operand1->kind == OPERAND_IMM ) {
                operand1 = instr->operand1->val.u64;
            } else {
                assert(0);
            }

            if ( instr->operand2->kind == OPERAND_REG ) {
                operand2 = reg_read(cpu, instr->operand2);
            } else if ( instr->operand2->kind == OPERAND_IMM ) {
                operand2 = instr->operand2->val.u64;
            } else {
                assert(0);
            }

            if ( instr->dst->kind == OPERAND_REG ) {
                reg_write(cpu, instr->dst, operand1 - operand2);
            } else {
                assert(0);
            }

            flags_clear(cpu);
            if ( operand2 > operand1 ) {
                flags_set(cpu, RFLAG_CF);
            } else if ( operand1 == operand2 ) {
                flags_set(cpu, RFLAG_ZF);
            }
        } break;

        default: {
            assert(0);
        } break;
    }

    return true;
}

void
compile_procs(Stmts stmts, Mem *mem) {
    for ( int i = 0; i < buf_len(stmts); ++i ) {
        Stmt *stmt = stmts[i];

        if ( stmt->kind == STMT_DECL && SDECL(stmt)->decl->kind == DECL_PROC ) {
            vm_stmt(stmt, mem);
        }
    }
}

void
compile_non_procs(Stmts stmts, Mem *mem) {
    for ( int i = 0; i < buf_len(stmts); ++i ) {
        Stmt *stmt = stmts[i];

        if ( stmt->kind != STMT_DECL || SDECL(stmt)->decl->kind != DECL_PROC ) {
            vm_stmt(stmt, mem);
        }
    }
}

void
compile_directives(Directives dirs, Mem *mem) {
}

Vm *
compile(Parsed_File *file, Mem *mem) {
    if ( !vm_data_section ) {
        vm_data_section = section_new("data");
    }

    if ( !vm_text_section ) {
        vm_text_section = section_new("text");
    }

    compile_directives(file->directives, mem);

    vm_curr_section = vm_data_section;
    compile_non_procs(file->stmts, mem);

    vm_curr_section = vm_text_section;
    compile_procs(file->stmts, mem);

    Vm *result = vm_new(vm_data_section, vm_text_section);

    vm_data_section->offset = 0;
    vm_text_section->offset = vm_data_section->num_instrs;

    return result;
}

Cpu *
eval_section(Section *section, Mem *mem, uint32_t flags) {
    Cpu *cpu = NULL;

    if ( flags & EVAL_WITH_ENTRY_POINT ) {
        uint32_t start_addr = addr_lookup(entry_point);

        cpu = cpu_new(section->instrs, mem, start_addr);

        reg_write64(cpu, REG_RDS, section->offset);

        stack_push(cpu, section->num_instrs + section->offset, 8);
        stack_push(cpu, reg_read64(cpu, REG_RBX), 8);
        stack_push(cpu, reg_read64(cpu, REG_RCX), 8);
        stack_push(cpu, reg_read64(cpu, REG_RDX), 8);
        stack_push(cpu, reg_read64(cpu, REG_RDI), 8);
        stack_push(cpu, reg_read64(cpu, REG_R12), 8);
        stack_push(cpu, reg_read64(cpu, REG_R13), 8);
        stack_push(cpu, reg_read64(cpu, REG_R14), 8);
        stack_push(cpu, reg_read64(cpu, REG_R15), 8);
    } else {
        cpu = cpu_new(section->instrs, mem, 0);
    }

    for (;;) {
        if ( !step(cpu) ) {
            break;
        }
    }

    return cpu;
}

uint64_t
eval(Vm *vm, Mem *mem, uint32_t flags = EVAL_WITH_ENTRY_POINT) {
    Cpu *cpu = NULL;

    cpu = eval_section(vm->data_section, mem, flags);
    if ( flags & EVAL_WITH_ENTRY_POINT ) {
        cpu = eval_section(vm->text_section, mem, flags);
    }

    return reg_read32(cpu, REG_RAX);
}

}
