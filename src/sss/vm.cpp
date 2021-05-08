namespace Vm {

enum { STACK_SIZE = 1024 };

struct Cpu;

void rip_inc(Cpu *cpu);
void vm_stmt(Stmt *stmt, char *proc_name = NULL);

enum Vm_Op {
    OP_HLT,
    OP_DATA,

    OP_ADD,
    OP_CALL,
    OP_CMP,
    OP_DIV,
    OP_IDIV,
    OP_IMUL,
    OP_JE,
    OP_JMP,
    OP_JNE,
    OP_JNZ,
    OP_JZ,
    OP_LEA,
    OP_MOV,
    OP_MUL,
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

enum Vm_Reg64 {
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

    REG_COUNT,
};

enum Vm_Reg32 {
    REG_EAX,
    REG_EBX,
    REG_ECX,
    REG_EDX,
    REG_EBP,
    REG_ESI,
    REG_EDI,
    REG_ESP,
    REG_R8D,
    REG_R9D,
};

enum Vm_Reg16 {
    REG_AX,
    REG_BX,
    REG_CX,
    REG_DX,
    REG_BP,
    REG_SI,
    REG_DI,
    REG_SP,
    REG_R8W,
    REG_R9W,
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
    REG_SPL = REG_SP,
    REG_R8B = REG_R8W,
    REG_R9B = REG_R9W,
};

enum Vm_Rflag {
    RFLAG_CF = 1<<0,
    RFLAG_PF = 1<<1,
    RFLAG_AF = 1<<2,
    RFLAG_ZF = 1<<3,
    RFLAG_SF = 1<<4,
    RFLAG_OF = 1<<5,
    RFLAG_DF = 1<<6,
    RFLAG_ID = 1<<7,
};

enum Value_Kind {
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

enum Operand_Kind {
    OPERAND_NONE,
    OPERAND_PTR,
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
        uint64_t addr;
        Vm_Reg64 reg64;
        Vm_Reg32 reg32;
        Vm_Reg16 reg16;
        Vm_Reg8l reg8l;
        Vm_Reg8h reg8h;
        Value    val;
        Operand  *op;
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

struct Data {
    char    * name;
    int32_t   size;
    int32_t   addr;
};

Instrs    vm_instrs;
Instrs    vm_instrs_labeled;
Data   ** vm_data;

Vm_Reg64 regs64[] = { REG_RCX, REG_RDX, REG_R8,  REG_R9  };
Vm_Reg32 regs32[] = { REG_ECX, REG_EDX, REG_R8D, REG_R9D };
Vm_Reg16 regs16[] = { REG_CX,  REG_DX,  REG_R8W, REG_R9W };
Vm_Reg8l  regs8[] = { REG_CL,  REG_DL,  REG_R8B, REG_R9B };

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

Data *
data(char *name, int32_t size, int32_t addr) {
    Data *result = urq_allocs(Data);

    result->name = name;
    result->size = size;
    result->addr = addr;

    return result;
}

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
reg_read(Cpu *cpu, Operand *op) {
    switch ( op->kind ) {
        case OPERAND_REG64: {
            return reg_read(cpu, op->reg64);
        } break;

        case OPERAND_REG32: {
            return reg_read(cpu, op->reg32);
        } break;

        case OPERAND_REG16: {
            return reg_read(cpu, op->reg16);
        } break;

        case OPERAND_REG8L: {
            return reg_read(cpu, op->reg8l);
        } break;

        case OPERAND_REG8H: {
            return reg_read(cpu, op->reg8h);
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
reg_write(Cpu *cpu, Operand *op, uint64_t val) {
    switch ( op->kind ) {
        case OPERAND_REG64: {
            reg_write(cpu, op->reg64, val);
        } break;
        case OPERAND_REG32: {
            reg_write(cpu, op->reg32, (uint32_t)val);
        } break;
        case OPERAND_REG16: {
            reg_write(cpu, op->reg16, (uint16_t)val);
        } break;
        case OPERAND_REG8L: {
            reg_write(cpu, op->reg8l, (uint8_t)val);
        } break;
        case OPERAND_REG8H: {
            reg_write(cpu, op->reg8h, (uint8_t)val);
        } break;
    }
}

void
flags_set(Cpu *cpu, uint32_t flags) {
    reg_write(cpu, REG_RFLAGS, reg_read(cpu, REG_RFLAGS) | flags);
}

void
flags_clear(Cpu *cpu) {
    reg_write(cpu, REG_RFLAGS, 0);
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

Mem *
mem_new(uint32_t size) {
    Mem *result = urq_allocs(Mem);

    result->size = size;
    result->used = 0;
    result->mem  = (uint8_t *)urq_alloc(result->size);

    return result;
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

    result->stack_size = STACK_SIZE;
    result->mem        = mem_new(mem_size);

    reg_write(result, REG_RIP, start);
    reg_write(result, REG_RSP, result->mem->size);

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
value(int8_t val) {
    Value result = {};

    result.kind = VAL_S8;
    result.s8   = val;

    return result;
}

Value
value(int16_t val) {
    Value result = {};

    result.kind = VAL_S16;
    result.s16  = val;

    return result;
}

Value
value(int32_t val) {
    Value result = {};

    result.kind = VAL_S32;
    result.s32  = val;

    return result;
}

Value
value(int64_t val) {
    Value result = {};

    result.kind = VAL_S64;
    result.s64  = val;

    return result;
}

Value
value(uint8_t val) {
    Value result = {};

    result.kind = VAL_U8;
    result.u8   = val;

    return result;
}

Value
value(uint16_t val) {
    Value result = {};

    result.kind = VAL_U16;
    result.u16  = val;

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
value(uint64_t val) {
    Value result = {};

    result.kind = VAL_U64;
    result.u64  = val;

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
operand_addr(uint32_t addr, int32_t size = 0) {
    Operand *result = urq_allocs(Operand);

    result->kind = OPERAND_ADDR;
    result->addr = addr;
    result->size = size;

    return result;
}

Operand *
operand_imm(Value val) {
    Operand *result = urq_allocs(Operand);

    result->kind = OPERAND_IMM;
    result->val  = val;

    return result;
}

Operand *
operand_ptr(Operand *op) {
    Operand *result = urq_allocs(Operand);

    result->kind = OPERAND_PTR;
    result->op   = op;

    return result;
}

Operand *
operand_reg(Vm_Reg64 reg, int32_t displacement) {
    Operand *result = urq_allocs(Operand);

    result->kind              = OPERAND_REG64;
    result->displacement      = displacement;
    result->with_displacement = true;
    result->reg64             = reg;

    return result;
}

Operand *
operand_reg(Vm_Reg64 reg) {
    Operand *result = urq_allocs(Operand);

    result->kind  = OPERAND_REG64;
    result->reg64 = reg;

    return result;
}

Operand *
operand_reg(Vm_Reg32 reg, int32_t displacement) {
    Operand *result = urq_allocs(Operand);

    result->kind              = OPERAND_REG32;
    result->displacement      = displacement;
    result->with_displacement = true;
    result->reg32             = reg;

    return result;
}

Operand *
operand_reg(Vm_Reg32 reg) {
    Operand *result = urq_allocs(Operand);

    result->kind  = OPERAND_REG32;
    result->reg32 = reg;

    return result;
}

Operand *
operand_reg(Vm_Reg16 reg, int32_t displacement) {
    Operand *result = urq_allocs(Operand);

    result->kind              = OPERAND_REG16;
    result->displacement      = displacement;
    result->with_displacement = true;
    result->reg16             = reg;

    return result;
}

Operand *
operand_reg(Vm_Reg16 reg) {
    Operand *result = urq_allocs(Operand);

    result->kind  = OPERAND_REG16;
    result->reg16 = reg;

    return result;
}

Operand *
operand_reg(Vm_Reg8l reg, int32_t displacement) {
    Operand *result = urq_allocs(Operand);

    result->kind              = OPERAND_REG8L;
    result->displacement      = displacement;
    result->with_displacement = true;
    result->reg8l             = reg;

    return result;
}

Operand *
operand_reg(Vm_Reg8l reg) {
    Operand *result = urq_allocs(Operand);

    result->kind  = OPERAND_REG8L;
    result->reg8l = reg;

    return result;
}

Operand *
operand_reg(Vm_Reg8h reg, int32_t displacement) {
    Operand *result = urq_allocs(Operand);

    result->kind              = OPERAND_REG8H;
    result->displacement      = displacement;
    result->with_displacement = true;
    result->reg8h             = reg;

    return result;
}

Operand *
operand_reg(Vm_Reg8h reg) {
    Operand *result = urq_allocs(Operand);

    result->kind  = OPERAND_REG8H;
    result->reg8h = reg;

    return result;
}

Operand *
operand_rax(int32_t size) {
    if ( size == 1 ) {
        return operand_reg(REG_AL);
    } else if ( size == 2 ) {
        return operand_reg(REG_AX);
    } else if ( size == 4 ) {
        return operand_reg(REG_EAX);
    } else if ( size == 8 ) {
        return operand_reg(REG_RAX);
    } else {
        assert(0);
        return operand_reg(REG_RAX);
    }
}

Operand *
operand_rsi(int32_t size) {
    if ( size == 1 ) {
        return operand_reg(REG_SI);
    } else if ( size == 2 ) {
        return operand_reg(REG_SI);
    } else if ( size == 4 ) {
        return operand_reg(REG_ESI);
    } else if ( size == 8 ) {
        return operand_reg(REG_RSI);
    } else {
        assert(0);
        return operand_reg(REG_RSI);
    }
}

Operand *
operand_rdi(int32_t size) {
    if ( size == 1 ) {
        return operand_reg(REG_DI);
    } else if ( size == 2 ) {
        return operand_reg(REG_DI);
    } else if ( size == 4 ) {
        return operand_reg(REG_EDI);
    } else if ( size == 8 ) {
        return operand_reg(REG_RDI);
    } else {
        assert(0);
        return operand_reg(REG_RDI);
    }
}

Operand *
operand_rbp(int32_t size, int32_t displacement) {
    if ( size == 1 ) {
        return operand_reg(REG_BP, displacement);
    } else if ( size == 2 ) {
        return operand_reg(REG_BP, displacement);
    } else if ( size == 4 ) {
        return operand_reg(REG_EBP, displacement);
    } else if ( size == 8 ) {
        return operand_reg(REG_RBP, displacement);
    } else {
        assert(0);
        return operand_reg(REG_RBP);
    }
}

Operand *
operand_args(int32_t size, int32_t count) {
    if ( size == 1 ) {
        return operand_reg(regs8[count]);
    } else if ( size == 2 ) {
        return operand_reg(regs16[count]);
    } else if ( size == 4 ) {
        return operand_reg(regs32[count]);
    } else if ( size == 8 ) {
        return operand_reg(regs64[count]);
    } else {
        assert(0);
        return operand_reg(REG_RCX);
    }
}

Operand *
operand_name(Value name) {
    Operand *result = urq_allocs(Operand);

    result->kind = OPERAND_NAME;
    result->val  = name;

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
    Instr *result = cpu->instrs[reg_read(cpu, REG_RIP)];
    rip_inc(cpu);

    return result;
}

void
vm_instr_patch(uint32_t index, uint32_t addr) {
    vm_instrs[index]->operand1->addr = addr;
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

    return result;
}

void
rsp_dec(Cpu *cpu, uint32_t by_how_much) {
    reg_write(cpu, REG_RSP, reg_read(cpu, REG_RSP) - by_how_much);
}

void
rsp_inc(Cpu *cpu, uint32_t by_how_much) {
    reg_write(cpu, REG_RSP, reg_read(cpu, REG_RSP) + by_how_much);
}

void
rip_inc(Cpu *cpu) {
    reg_write(cpu, REG_RIP, reg_read(cpu, REG_RIP) + 1);
}

void
stack_push(Cpu *cpu, uint64_t val) {
    rsp_dec(cpu, 8);
    uint64_t addr = reg_read(cpu, REG_RSP);
    assert(addr > cpu->stack_size);
    mem_write(cpu->mem, addr, val);
}

void
stack_push(Cpu *cpu, uint32_t val) {
    rsp_dec(cpu, 4);
    uint64_t addr = reg_read(cpu, REG_ESP);
    assert(addr > cpu->stack_size);
    mem_write(cpu->mem, addr, val);
}

void
stack_push(Cpu *cpu, uint16_t val) {
    rsp_dec(cpu, 2);
    uint64_t addr = reg_read(cpu, REG_SP);
    assert(addr > cpu->stack_size);
    mem_write(cpu->mem, addr, val);
}

void
stack_push(Cpu *cpu, uint8_t val) {
    rsp_dec(cpu, 1);
    uint64_t addr = reg_read(cpu, REG_SPL);
    assert(addr > cpu->stack_size);
    mem_write(cpu->mem, addr, val);
}

void
stack_pop(Cpu *cpu, Vm_Reg64 reg) {
    assert(cpu->mem->size >= reg_read(cpu, REG_RSP) + 8);
    reg_write(cpu, reg, mem_read64(cpu->mem, (uint32_t)reg_read(cpu, REG_RSP)));
    rsp_inc(cpu, 8);
}

void
stack_pop(Cpu *cpu, Vm_Reg32 reg) {
    assert(cpu->mem->size >= reg_read(cpu, REG_RSP) + 4);
    reg_write(cpu, reg, mem_read32(cpu->mem, (uint32_t)reg_read(cpu, REG_ESP)));
    rsp_inc(cpu, 4);
}

void
stack_pop(Cpu *cpu, Vm_Reg16 reg) {
    assert(cpu->mem->size >= reg_read(cpu, REG_RSP) + 2);
    reg_write(cpu, reg, mem_read16(cpu->mem, (uint32_t)reg_read(cpu, REG_SP)));
    rsp_inc(cpu, 2);
}

void
stack_pop(Cpu *cpu, Vm_Reg8l reg) {
    assert(cpu->mem->size >= reg_read(cpu, REG_RSP) + 1);
    reg_write(cpu, reg, mem_read8(cpu->mem, (uint32_t)reg_read(cpu, REG_SPL)));
    rsp_inc(cpu, 1);
}

void
stack_pop(Cpu *cpu, Vm_Reg8h reg) {
    assert(cpu->mem->size > reg_read(cpu, REG_RSP) + 1);
    reg_write(cpu, reg, mem_read8(cpu->mem, (uint32_t)reg_read(cpu, REG_SPL)));
    rsp_inc(cpu, 1);
}

void
stack_pop(Cpu *cpu, Operand *op) {
    switch ( op->kind ) {
        case OPERAND_REG64: {
            stack_pop(cpu, op->reg64);
        } break;

        case OPERAND_REG32: {
            stack_pop(cpu, op->reg32);
        } break;

        case OPERAND_REG16: {
            stack_pop(cpu, op->reg16);
        } break;

        case OPERAND_REG8L: {
            stack_pop(cpu, op->reg8l);
        } break;

        case OPERAND_REG8H: {
            stack_pop(cpu, op->reg8h);
        } break;
    }
}

Operand *
vm_addr(Expr *expr) {
    switch ( expr->kind ) {
        case EXPR_IDENT: {
            return operand_rbp(expr->type->size, EIDENT(expr)->sym->decl->offset);
        } break;

        default: {
            assert(0);
            return NULL;
        }
    }
}

Operand *
vm_expr(Expr *expr, bool assignment = false) {
    Operand *result = NULL;

    switch ( expr->kind ) {
        case EXPR_AT: {
            if ( assignment ) {
                vm_emit(vm_instr(expr, OP_MOV, operand_rsi(expr->type->size), vm_addr(EAT(expr)->expr)));
                return operand_ptr(operand_rsi(expr->type->size));
            } else {
                vm_emit(vm_instr(expr, OP_MOV, operand_rax(expr->type->size), operand_ptr(vm_addr(EAT(expr)->expr))));
            }
        } break;

        case EXPR_BOOL: {
            vm_emit(vm_instr(expr, OP_MOV, operand_rax(expr->type->size), operand_imm(value(EBOOL(expr)->val))));
        } break;

        case EXPR_BIN: {
            if ( EBIN(expr)->op == BIN_AND ) {
                vm_expr(EBIN(expr)->left);
                vm_emit(vm_instr(expr, OP_CMP, operand_rax(expr->type->size), operand_imm(value1)));
                int32_t jmp1_instr = vm_emit(vm_instr(expr, OP_JNE, operand_addr(0)));

                vm_expr(EBIN(expr)->right);
                vm_emit(vm_instr(expr, OP_CMP, operand_rax(expr->type->size), operand_imm(value1)));

                vm_instr_patch(jmp1_instr, buf_len(vm_instrs));
            } else if ( EBIN(expr)->op == BIN_OR ) {
                vm_expr(EBIN(expr)->left);
                vm_emit(vm_instr(expr, OP_CMP, operand_rax(expr->type->size), operand_imm(value1)));
                int32_t jmp1_instr = vm_emit(vm_instr(expr, OP_JE, operand_addr(0)));

                vm_expr(EBIN(expr)->right);
                vm_emit(vm_instr(expr, OP_CMP, operand_rax(expr->type->size), operand_imm(value1)));

                vm_instr_patch(jmp1_instr, buf_len(vm_instrs));
            } else {
                vm_expr(EBIN(expr)->right);
                vm_emit(vm_instr(expr, OP_PUSH, operand_rax(EBIN(expr)->right->type->size)));

                vm_expr(EBIN(expr)->left);
                vm_emit(vm_instr(expr, OP_POP, operand_rdi(EBIN(expr)->left->type->size)));

                if ( EBIN(expr)->op == BIN_ADD ) {
                    vm_emit(vm_instr(expr, OP_ADD, operand_rax(expr->type->size), operand_rdi(expr->type->size)));
                } else if ( EBIN(expr)->op == BIN_SUB ) {
                    vm_emit(vm_instr(expr, OP_SUB, operand_rax(expr->type->size), operand_rdi(expr->type->size)));
                } else if ( EBIN(expr)->op == BIN_MUL ) {
                    vm_emit(vm_instr(expr, OP_IMUL, operand_rax(expr->type->size), operand_rdi(expr->type->size)));
                } else if ( EBIN(expr)->op == BIN_DIV ) {
                    if ( EBIN(expr)->right->type->is_signed ) {
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

                vm_expr(arg);
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

            vm_expr(ECALL(expr)->base);
            vm_emit(vm_instr(expr, OP_CALL, operand_reg(REG_RAX)));
        } break;

        case EXPR_FLOAT: {
            vm_emit(vm_instr(expr, OP_MOV, operand_reg(REG_RAX), operand_imm(value(EFLOAT(expr)->val))));
        } break;

        case EXPR_IDENT: {
            assert(EIDENT(expr)->sym);
            Sym *sym = EIDENT(expr)->sym;

            if (sym->decl->is_global) {
                vm_emit(vm_instr(expr, OP_LEA, operand_reg(REG_RAX), operand_name(value(EIDENT(expr)->val))));
            } else {
                if ( assignment ) {
                    return vm_addr(expr);
                } else {
                    vm_emit(vm_instr(expr, OP_LEA, operand_rax(expr->type->size), operand_rbp(expr->type->size, sym->decl->offset)));
                }
            }
        } break;

        case EXPR_INT: {
            if ( expr->type->is_signed ) {
                if ( expr->type->size == 1 ) {
                    vm_emit(vm_instr(expr, OP_MOV, operand_rax(expr->type->size), operand_imm(value((int8_t)EINT(expr)->val))));
                } else if ( expr->type->size == 2 ) {
                    vm_emit(vm_instr(expr, OP_MOV, operand_rax(expr->type->size), operand_imm(value((int16_t)EINT(expr)->val))));
                } else if ( expr->type->size == 4 ) {
                    vm_emit(vm_instr(expr, OP_MOV, operand_rax(expr->type->size), operand_imm(value((int32_t)EINT(expr)->val))));
                } else if ( expr->type->size == 8 ) {
                    vm_emit(vm_instr(expr, OP_MOV, operand_rax(expr->type->size), operand_imm(value((int64_t)EINT(expr)->val))));
                } else {
                    assert(0);
                }
            } else {
                if ( expr->type->size == 1 ) {
                    vm_emit(vm_instr(expr, OP_MOV, operand_rax(expr->type->size), operand_imm(value((uint8_t)EINT(expr)->val))));
                } else if ( expr->type->size == 2 ) {
                    vm_emit(vm_instr(expr, OP_MOV, operand_rax(expr->type->size), operand_imm(value((uint16_t)EINT(expr)->val))));
                } else if ( expr->type->size == 4 ) {
                    vm_emit(vm_instr(expr, OP_MOV, operand_rax(expr->type->size), operand_imm(value((uint32_t)EINT(expr)->val))));
                } else if ( expr->type->size == 8 ) {
                    vm_emit(vm_instr(expr, OP_MOV, operand_rax(expr->type->size), operand_imm(value((uint64_t)EINT(expr)->val))));
                } else {
                    assert(0);
                }
            }
        } break;

        case EXPR_NOT: {
            vm_expr(ENOT(expr)->expr);
            vm_emit(vm_instr(expr, OP_NOT, operand_rax(expr->type->size)));
        } break;

        case EXPR_PAREN: {
            vm_expr(EPAREN(expr)->expr, assignment);
        } break;

        case EXPR_STR: {
            static uint32_t string_num = 0;
            char *name = make_label(".string.%d", string_num);

            vm_emit(vm_instr(expr, OP_DATA, operand_name(value(".string")), operand_imm(value(ESTR(expr)->val)), name));
            vm_emit(vm_instr(expr, OP_LEA, operand_name(value(name)), operand_rax(expr->type->size)));

            string_num++;
        } break;

        default: {
            assert(0);
        } break;
    }

    return result;
}

void
vm_decl(Decl *decl) {
    switch ( decl->kind ) {
        case DECL_VAR: {
            if ( decl->is_global ) {
                assert(0);
            } else {
                if ( DVAR(decl)->expr ) {
                    Expr *expr = DVAR(decl)->expr;

                    vm_expr(expr);
                    vm_emit(vm_instr(decl, OP_MOV, operand_rbp(expr->type->size, decl->offset), operand_rax(expr->type->size)));
                }
            }
        } break;

        case DECL_PROC: {
            vm_emit(vm_instr(decl, OP_DATA, operand_name(value(".text"))));
            vm_emit(vm_instr(decl, OP_DATA, operand_name(value(".global")), operand_name(value(decl->name))));
            vm_emit(vm_instr(decl, OP_PUSH, operand_reg(REG_RBP), decl->name));
            vm_emit(vm_instr(decl, OP_MOV, operand_reg(REG_RBP), operand_reg(REG_RSP)));

            if ( DPROC(decl)->scope->frame_size ) {
                vm_emit(vm_instr(decl, OP_SUB, operand_reg(REG_RSP), operand_imm(value((uint64_t)DPROC(decl)->scope->frame_size))));
            }

            uint8_t reg = 0;
            for ( uint32_t i = 0; i < DPROC(decl)->sign->num_params; ++i ) {
                Decl_Var *param = DPROC(decl)->sign->params[i];

                if ( i < 4 ) {
                    vm_emit(vm_instr(decl, OP_MOV, operand_rbp(param->type->size, param->offset), operand_args(param->type->size, reg++)));
                }
            }

            vm_stmt(DPROC(decl)->block, decl->name);

            vm_emit(vm_instr(decl, OP_MOV, operand_reg(REG_RSP), operand_reg(REG_RBP), make_label("%s.end", decl->name)));
            vm_emit(vm_instr(decl, OP_POP, operand_reg(REG_RBP)));
            vm_emit(vm_instr(decl, OP_RET));
        } break;

        default: {
            assert(0);
        } break;
    }
}

void
vm_stmt(Stmt *stmt, char *proc_name) {
    switch ( stmt->kind ) {
        case STMT_ASSIGN: {
            Operand *dst = vm_expr(SASSIGN(stmt)->lhs, true);
            assert(dst);

            vm_expr(SASSIGN(stmt)->rhs);
            vm_emit(vm_instr(stmt, OP_MOV, dst, operand_rax(SASSIGN(stmt)->rhs->type->size)));
        } break;

        case STMT_BLOCK: {
            for ( int i = 0; i < SBLOCK(stmt)->num_stmts; ++i ) {
                vm_stmt(SBLOCK(stmt)->stmts[i], proc_name);
            }
        } break;

        case STMT_DECL: {
            vm_decl(SDECL(stmt)->decl);
        } break;

        case STMT_EXPR: {
            vm_expr(SEXPR(stmt)->expr);
        } break;

        case STMT_FOR: {
            static int for_count = 0;
            char *label = make_label("for.%d.start", for_count++);

            vm_stmt(SFOR(stmt)->init, proc_name);
            int32_t loop_start = vm_emit(vm_instr(stmt, OP_NOP, label, "wird für die jmp anweisung benötigt"));
            vm_expr(SFOR(stmt)->cond);
            vm_emit(vm_instr(stmt, OP_CMP, operand_rax(SFOR(stmt)->cond->type->size), operand_imm(value1)));
            int32_t jmpnz_instr = vm_emit(vm_instr(stmt, OP_JNZ, operand_addr(0)));
            vm_stmt(SFOR(stmt)->block, proc_name);
            vm_stmt(SFOR(stmt)->step, proc_name);
            vm_emit(vm_instr(stmt, OP_JMP, operand_name(value(label))));

            /* @AUFGABE: sonst zweig für schleife */
#if 0
            if ( SFOR(stmt)->stmt_else ) {
            }
#endif

            vm_instr_patch(jmpnz_instr, buf_len(vm_instrs));
        } break;

        case STMT_IF: {
            static uint32_t loop_count = 0;
            char *label = make_label("if.%d", loop_count++);

            vm_expr(SIF(stmt)->cond);
            vm_emit(vm_instr(stmt, OP_CMP, operand_rax(SIF(stmt)->cond->type->size), operand_imm(value1)));
            int32_t jmp1_instr = vm_emit(vm_instr(stmt, OP_JNE, operand_addr(0), label));
            vm_stmt(SIF(stmt)->stmt, proc_name);
            int32_t jmp2_instr = vm_emit(vm_instr(stmt, OP_JMP, operand_addr(0)));

            vm_instr_patch(jmp1_instr, buf_len(vm_instrs));

            if ( SIF(stmt)->stmt_else ) {
                vm_stmt(SIF(stmt)->stmt_else, proc_name);
            }

            vm_instr_patch(jmp2_instr, buf_len(vm_instrs));
        } break;

        case STMT_RET: {
            if ( SRET(stmt)->num_exprs ) {
                vm_expr(SRET(stmt)->exprs[0]);
            }

            assert(proc_name);
            char *label = make_label("%s.end", proc_name);
            vm_emit(vm_instr(stmt, OP_JMP, operand_name(value(label)), (char *)NULL, "res"));
        } break;

        case STMT_WHILE: {
            static int while_count = 0;
            char *label = make_label("while.%d.start", while_count++);

            int32_t loop_start = vm_emit(vm_instr(stmt, OP_NOP, label, "wird für die jmp anweisung benötigt"));
            vm_expr(SWHILE(stmt)->cond);
            int32_t jmpz_instr = vm_emit(vm_instr(stmt, OP_JZ, operand_addr(0), operand_imm(value((uint64_t)1))));
            vm_stmt(SWHILE(stmt)->block, proc_name);
            vm_emit(vm_instr(stmt, OP_JMP, operand_addr(loop_start)));

            vm_instr_patch(jmpz_instr, buf_len(vm_instrs));
        } break;

        default: {
            assert(0);
        } break;
    }
}

bool
operand_is_reg(Operand *op) {
    bool result = op->kind >= OPERAND_REG64 && op->kind <= OPERAND_REG8H;

    return result;
}

bool
step(Cpu *cpu) {
    if ( cpu->num_instrs <= reg_read(cpu, REG_RIP) ) {
        return false;
    }

    Instr *instr = vm_instr_fetch(cpu);

    switch ( instr->op ) {
        case OP_ADD: {
            uint64_t operand1 = 0;
            uint64_t operand2 = 0;

            if ( operand_is_reg(instr->operand1) ) {
                operand1 = reg_read(cpu, instr->operand1);
            } else if ( instr->operand1->kind == OPERAND_IMM ) {
                operand1 = instr->operand1->val.u64;
            } else {
                assert(0);
            }

            if ( operand_is_reg(instr->operand2) ) {
                operand2 = reg_read(cpu, instr->operand2);
            } else if ( instr->operand2->kind == OPERAND_IMM ) {
                operand2 = instr->operand2->val.u64;
            } else {
                assert(0);
            }

            reg_write(cpu, REG_RAX, operand1 + operand2);
        } break;

        case OP_CALL: {
            stack_push(cpu, reg_read(cpu, REG_RIP));
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
            } else if ( instr->operand1->kind == OPERAND_NAME ) {
                reg_write(cpu, REG_RIP, addr_lookup(instr->operand1->val));
            } else {
                assert(0);
            }
        } break;

        case OP_CMP: {
            uint64_t operand1 = 0;
            uint64_t operand2 = 0;

            if ( operand_is_reg(instr->operand1) ) {
                operand1 = reg_read(cpu, instr->operand1);
            } else if ( instr->operand1->kind == OPERAND_IMM ) {
                operand1 = instr->operand1->val.u64;
            } else {
                assert(0);
            }

            if ( operand_is_reg(instr->operand2) ) {
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

        case OP_DATA: {
            //
        } break;

        case OP_IDIV: {
            uint64_t dividend = reg_read(cpu, REG_RAX);
            uint64_t divisor = 0;

            if ( operand_is_reg(instr->operand1) ) {
                divisor = reg_read(cpu, instr->operand1);
            } else if ( instr->operand1->kind == OPERAND_IMM ) {
                divisor = instr->operand1->val.u64;
            } else {
                assert(0);
            }

            uint64_t quotient  = (uint64_t)(dividend / divisor);
            uint64_t remainder = dividend % divisor;

            reg_write(cpu, REG_RAX, quotient);
            reg_write(cpu, REG_RDX, remainder);
        } break;

        case OP_IMUL: {
            uint64_t operand1 = 0;
            uint64_t operand2 = 0;

            if ( operand_is_reg(instr->operand1) ) {
                operand1 = reg_read(cpu, instr->operand1);
            } else if ( instr->operand1->kind == OPERAND_IMM ) {
                operand1 = instr->operand1->val.u64;
            } else {
                assert(0);
            }

            if ( operand_is_reg(instr->operand2) ) {
                operand2 = reg_read(cpu, instr->operand2);
            } else if ( instr->operand2->kind == OPERAND_IMM ) {
                operand2 = instr->operand2->val.u64;
            } else {
                assert(0);
            }

            reg_write(cpu, REG_RAX, operand1 * operand2);

            flags_clear(cpu);
            if ( reg_read(cpu, REG_RAX) == 0 ) {
                flags_set(cpu, RFLAG_ZF);
            }
        } break;

        case OP_JE: {
            if ( flag_state(cpu, RFLAG_ZF) ) {
                if ( instr->operand1->kind == OPERAND_ADDR ) {
                    reg_write(cpu, REG_RIP, instr->operand1->addr);
                } else if ( instr->operand1->kind == OPERAND_NAME ) {
                    reg_write(cpu, REG_RIP, addr_lookup(instr->operand1->val));
                } else {
                    assert(0);
                }
            }
        } break;

        case OP_JMP: {
            if ( instr->operand1->kind == OPERAND_ADDR ) {
                reg_write(cpu, REG_RIP, instr->operand1->addr);
            } else if ( instr->operand1->kind == OPERAND_NAME ) {
                reg_write(cpu, REG_RIP, addr_lookup(instr->operand1->val));
            } else {
                assert(0);
            }
        } break;

        case OP_JNE: {
            if ( !flag_state(cpu, RFLAG_ZF) ) {
                if ( instr->operand1->kind == OPERAND_ADDR ) {
                    reg_write(cpu, REG_RIP, instr->operand1->addr);
                } else {
                    assert(0);
                }
            }
        } break;

        case OP_JNZ: {
            if ( !flag_state(cpu, RFLAG_ZF) ) {
                if ( instr->operand1->kind == OPERAND_ADDR ) {
                    reg_write(cpu, REG_RIP, instr->operand1->addr);
                } else {
                    assert(0);
                }
            }
        } break;

        case OP_JZ: {
            if ( flag_state(cpu, RFLAG_ZF) ) {
                if ( instr->operand1->kind == OPERAND_ADDR ) {
                    reg_write(cpu, REG_RIP, instr->operand1->addr);
                } else {
                    assert(0);
                }
            }
        } break;

        case OP_LEA: {
            if ( operand_is_reg(instr->dst) ) {
                if ( operand_is_reg(instr->src) ) {
                    if ( instr->src->with_displacement ) {
                        reg_write(cpu, instr->dst, mem_read64(cpu->mem, (uint32_t)(reg_read(cpu, instr->src) + instr->src->displacement)));
                    } else {
                        reg_write(cpu, instr->dst, reg_read(cpu, instr->src));
                    }
                } else if ( instr->src->kind == OPERAND_NAME ) {
                    reg_write(cpu, instr->dst, addr_lookup(instr->src->val));
                } else {
                    if ( instr->src->size == 1 ) {
                        reg_write(cpu, instr->dst, mem_read8(cpu->mem, (uint32_t)instr->src->addr));
                    } else if ( instr->src->size == 2 ) {
                        reg_write(cpu, instr->dst, mem_read16(cpu->mem, (uint32_t)instr->src->addr));
                    } else if ( instr->src->size == 4 ) {
                        reg_write(cpu, instr->dst, mem_read32(cpu->mem, (uint32_t)instr->src->addr));
                    } else if ( instr->src->size == 8 ) {
                        reg_write(cpu, instr->dst, mem_read64(cpu->mem, (uint32_t)instr->src->addr));
                    }
                }
            } else {
                assert(0);
            }
        } break;

        case OP_MOV: {
            if ( operand_is_reg(instr->dst) ) {
                if ( instr->dst->with_displacement ) {
                    if ( instr->src->kind == OPERAND_IMM ) {
                        mem_write(cpu->mem, reg_read(cpu, instr->dst) + instr->dst->displacement, instr->src->val.u64);
                    } else if ( operand_is_reg(instr->src) ) {
                        if ( instr->src->kind == OPERAND_REG8L ) {
                            mem_write(cpu->mem, reg_read(cpu, instr->dst) + instr->dst->displacement, reg_read(cpu, instr->src->reg8l));
                        } else if ( instr->src->kind == OPERAND_REG8H ) {
                            mem_write(cpu->mem, reg_read(cpu, instr->dst) + instr->dst->displacement, reg_read(cpu, instr->src->reg8h));
                        } else if ( instr->src->kind == OPERAND_REG16 ) {
                            mem_write(cpu->mem, reg_read(cpu, instr->dst) + instr->dst->displacement, reg_read(cpu, instr->src->reg16));
                        } else if ( instr->src->kind == OPERAND_REG32 ) {
                            mem_write(cpu->mem, reg_read(cpu, instr->dst) + instr->dst->displacement, reg_read(cpu, instr->src->reg32));
                        } else if ( instr->src->kind == OPERAND_REG64 ) {
                            mem_write(cpu->mem, reg_read(cpu, instr->dst) + instr->dst->displacement, reg_read(cpu, instr->src->reg64));
                        }
                    } else {
                        assert(0);
                    }
                } else {
                    if ( instr->src->kind == OPERAND_IMM ) {
                        reg_write(cpu, instr->dst, instr->src->val.u64);
                    } else if ( operand_is_reg(instr->src) ) {
                        if ( instr->src->with_displacement ) {
                            if ( instr->src->kind == OPERAND_REG64 ) {
                                reg_write(cpu, instr->dst, mem_read64(cpu->mem, (uint32_t)reg_read(cpu, instr->src) + instr->src->displacement));
                            } else {
                                assert(0);
                            }
                        } else {
                            reg_write(cpu, instr->dst, reg_read(cpu, instr->src));
                        }
                    } else if ( instr->src->kind == OPERAND_PTR ) {
                        Operand *op = instr->src->op;

                        if ( operand_is_reg(op) ) {
                            if ( op->with_displacement ) {
                                if ( op->kind == OPERAND_REG8L ) {
                                    reg_write(cpu, instr->dst, reg_read(cpu, op->reg8l) + op->displacement);
                                } else if ( op->kind == OPERAND_REG8H ) {
                                    reg_write(cpu, instr->dst, reg_read(cpu, op->reg8h) + op->displacement);
                                } else if ( op->kind == OPERAND_REG16 ) {
                                    reg_write(cpu, instr->dst, reg_read(cpu, op->reg16) + op->displacement);
                                } else if ( op->kind == OPERAND_REG32 ) {
                                    reg_write(cpu, instr->dst, reg_read(cpu, op->reg32) + op->displacement);
                                } else if ( op->kind == OPERAND_REG64 ) {
                                    reg_write(cpu, instr->dst, reg_read(cpu, op->reg64) + op->displacement);
                                }
                            } else {
                                assert(0);
                            }
                        }
                    } else {
                        assert(0);
                    }
                }
            } else if ( instr->dst->kind == OPERAND_PTR ) {
                Operand *op = instr->dst->op;

                if ( operand_is_reg(op) ) {
                    if ( op->with_displacement ) {
                        assert(0);
                    } else {
                        assert(operand_is_reg(instr->src));

                        if ( op->kind == OPERAND_REG8L ) {
                            mem_write(cpu->mem, (uint64_t)reg_read(cpu, op->reg8l), reg_read(cpu, instr->src->reg8l));
                        } else if ( op->kind == OPERAND_REG8H ) {
                            mem_write(cpu->mem, (uint64_t)reg_read(cpu, op->reg8h), reg_read(cpu, instr->src->reg8h));
                        } else if ( op->kind == OPERAND_REG16 ) {
                            mem_write(cpu->mem, (uint64_t)reg_read(cpu, op->reg16), reg_read(cpu, instr->src->reg16));
                        } else if ( op->kind == OPERAND_REG32 ) {
                            mem_write(cpu->mem, (uint64_t)reg_read(cpu, op->reg32), reg_read(cpu, instr->src->reg32));
                        } else if ( op->kind == OPERAND_REG64 ) {
                            mem_write(cpu->mem, (uint64_t)reg_read(cpu, op->reg64), reg_read(cpu, instr->src->reg64));
                        }
                    }
                }
            } else {
                assert(0);
            }
        } break;

        case OP_NOP: {
            //
        } break;

        case OP_NOT: {
            if ( operand_is_reg(instr->operand1) ) {
                auto val = reg_read(cpu, instr->operand1);
                reg_write(cpu, instr->operand1, ~val);
            } else {
                assert(0);
            }
        } break;

        case OP_PUSH: {
            if ( operand_is_reg(instr->operand1) ) {
                if ( instr->operand1->kind == OPERAND_REG8L ) {
                    stack_push(cpu, reg_read(cpu, instr->operand1->reg8l));
                } else if ( instr->operand1->kind == OPERAND_REG8H ) {
                    stack_push(cpu, reg_read(cpu, instr->operand1->reg8h));
                } else if ( instr->operand1->kind == OPERAND_REG16 ) {
                    stack_push(cpu, reg_read(cpu, instr->operand1->reg16));
                } else if ( instr->operand1->kind == OPERAND_REG32 ) {
                    stack_push(cpu, reg_read(cpu, instr->operand1->reg32));
                } else if ( instr->operand1->kind == OPERAND_REG64 ) {
                    stack_push(cpu, reg_read(cpu, instr->operand1->reg64));
                }
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

        case OP_SETL: {
            assert(operand_is_reg(instr->dst));

            reg_write(cpu, instr->dst, flag_state(cpu, RFLAG_CF));
        } break;

        case OP_SETLE: {
            assert(operand_is_reg(instr->dst));

            reg_write(cpu, instr->dst, flag_state(cpu, RFLAG_ZF) || (flag_state(cpu, RFLAG_CF)));
        } break;

        case OP_SETE: {
            assert(operand_is_reg(instr->dst));

            reg_write(cpu, instr->dst, flag_state(cpu, RFLAG_ZF));
        } break;

        case OP_SETGE: {
            assert(operand_is_reg(instr->dst));

            reg_write(cpu, instr->dst, flag_state(cpu, RFLAG_ZF) || !flag_state(cpu, RFLAG_CF));
        } break;

        case OP_SETG: {
            assert(operand_is_reg(instr->dst));

            reg_write(cpu, instr->dst, !flag_state(cpu, RFLAG_ZF) && !flag_state(cpu, RFLAG_CF));
        } break;

        case OP_SETNE: {
            assert(operand_is_reg(instr->dst));

            reg_write(cpu, instr->dst, !flag_state(cpu, RFLAG_ZF));
        } break;

        case OP_SUB: {
            uint64_t operand1 = 0;
            uint64_t operand2 = 0;

            if ( operand_is_reg(instr->operand1) ) {
                operand1 = reg_read(cpu, instr->operand1);
            } else if ( instr->operand1->kind == OPERAND_IMM ) {
                operand1 = instr->operand1->val.u64;
            } else {
                assert(0);
            }

            if ( operand_is_reg(instr->operand2) ) {
                operand2 = reg_read(cpu, instr->operand2);
            } else if ( instr->operand2->kind == OPERAND_IMM ) {
                operand2 = instr->operand2->val.u64;
            } else {
                assert(0);
            }

            if ( operand_is_reg(instr->dst) ) {
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
compile_procs(Stmts stmts) {
    for ( int i = 0; i < buf_len(stmts); ++i ) {
        Stmt *stmt = stmts[i];

        if ( stmt->kind == STMT_DECL && SDECL(stmt)->decl->kind == DECL_PROC ) {
            vm_stmt(stmt);
        }
    }
}

Instrs
compile(Parsed_File *file) {
    compile_procs(file->stmts);

    for ( int i = 0; i < buf_len(file->stmts); ++i ) {
        Stmt *stmt = file->stmts[i];

        if ( stmt->kind != STMT_DECL && SDECL(stmt)->decl->kind != DECL_PROC ) {
            vm_stmt(stmt);
        }
    }

    return vm_instrs;
}

uint64_t
eval(Instrs instrs) {
    Cpu *cpu = cpu_new(instrs, 1024*1024, 102);

    for (;;) {
        if ( !step(cpu) ) {
            break;
        }
    }

    return reg_read(cpu, REG_RAX);
}

}
