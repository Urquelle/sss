uint8_t  mem_read(uint8_t    *mem, size_t addr);
uint16_t mem_read16(uint8_t  *mem, size_t addr);
uint8_t  mem_write(uint8_t   *mem, size_t addr, uint8_t val);
uint8_t  mem_write16(uint8_t *mem, size_t addr, uint16_t val);

enum Reg {
    REG_ACC,

    REG_R1,
    REG_R2,
    REG_R3,
    REG_R4,
    REG_R5,
    REG_R6,
    REG_R7,
    REG_R8,

    REG_COUNT,
};

enum Vm_Op {
    VMOP_HLT,
    VMOP_CONST,
    VMOP_RET,
};

/*
struct Value {
    uint32_t val;
};
*/

struct Vm {
    //Value    ** constants;
    uint32_t    num_constants;
};

struct Cpu {
    uint8_t  * mem;

    uint16_t   pc; // program counter
    uint64_t   sp; // stack pointer
    uint64_t   fp; // frame pointer
    uint64_t   mb; // memory bank
    uint64_t   im; // interrupt mask

    uint16_t   stack_frame_size;
    uint64_t   interrupt_vector_address;
    uint16_t   regs[REG_COUNT];
};

Cpu *
cpu_create(uint8_t *mem, uint64_t interrupt_vector_address = 0x1000) {
    Cpu *result = urq_allocs(Cpu);

    *result = {};

    for ( int i = 0; i < REG_COUNT; ++i ) {
        result->regs[i] = 0;
    }

    result->mem    = mem;
    result->sp     = 0xffff - 2;
    result->fp     = result->sp;
    result->mb     = 0;
    result->im     = 0xffff;
    result->interrupt_vector_address = interrupt_vector_address;

    return result;
}

uint16_t
reg_read(Cpu *cpu, uint8_t reg) {
    assert(reg < REG_COUNT);

    uint16_t result = cpu->regs[reg];

    return result;
}

void
reg_write(Cpu *cpu, uint8_t reg, uint16_t val) {
    assert(reg < REG_COUNT);

    cpu->regs[reg] = val;
}

uint8_t *
mem_create(size_t size) {
    uint8_t *result = (uint8_t *)urq_alloc(size);

    return result;
}

uint8_t
mem_read(uint8_t *mem, size_t addr) {
    uint8_t result = bit_read8(mem, addr);

    return result;
}

uint16_t
mem_read16(uint8_t *mem, size_t addr) {
    uint16_t result = bit_read16(mem, addr);

    return result;
}

uint8_t
mem_write(uint8_t *mem, size_t addr, uint8_t val) {
    uint8_t result = bit_write8(mem, addr, val);

    return result;
}

uint8_t
mem_write16(uint8_t *mem, size_t addr, uint16_t val) {
    uint8_t result = bit_write16(mem, addr, val);

    return result;
}

uint8_t
fetch(Cpu *cpu) {
    uint8_t instr = mem_read(cpu->mem, cpu->pc);
    cpu->pc += 1;

    return instr;
}

uint16_t
fetch16(Cpu *cpu) {
    uint16_t instr = mem_read16(cpu->mem, cpu->pc);
    cpu->pc += 2;

    return instr;
}

void
push(Cpu *cpu, uint16_t val) {
    mem_write16(cpu->mem, cpu->sp, val);
    cpu->sp -= 2;
    cpu->stack_frame_size += 2;
}

uint16_t
pop(Cpu *cpu) {
    cpu->sp += 2;
    uint16_t result = mem_read16(cpu->mem, cpu->sp);
    cpu->stack_frame_size -= 2;

    return result;
}

void
exec(Cpu *cpu, uint16_t instr) {
    switch ( instr ) {
        case VMOP_CONST: {
        } break;
#if 0
        case OP_MOV_IMM_REG: {
            uint16_t imm = fetch16(cpu);
            uint8_t reg = fetch(cpu);

            reg_write(cpu, reg, imm);
        } break;

        case OP_MOV_REG_REG: {
            auto src = fetch(cpu);
            auto dst = fetch(cpu);
            uint16_t val = reg_read(cpu, src);

            reg_write(cpu, dst, val);
        } break;

        case OP_MOV_IMM_MEM: {
            uint16_t imm = fetch16(cpu);
            uint16_t addr = fetch16(cpu);

            cpu->mem->write16(cpu->mem, addr, imm);
        } break;

        case OP_MOV_REG_PTR_REG: {
            uint8_t src = fetch(cpu);
            uint8_t dst = fetch(cpu);
            uint16_t addr = reg_read(cpu, src);
            uint16_t val = cpu->mem->read16(cpu->mem, addr);
            reg_write(cpu, dst, val);
        } break;

        case OP_MOV_IMM_OFF_REG: {
            uint16_t base_addr = fetch16(cpu);
            uint8_t offset_reg = fetch(cpu);
            uint8_t dst = fetch(cpu);
            uint16_t offset = reg_read(cpu, offset_reg);
            uint16_t val = cpu->mem->read16(cpu->mem, base_addr + offset);
            reg_write(cpu, dst, val);
        } break;

        case OP_MOV_REG_MEM: {
            auto reg  = fetch(cpu);
            auto addr = fetch16(cpu);
            uint16_t val = reg_read(cpu, reg);

            cpu->mem->write16(cpu->mem, addr, val);
        } break;

        case OP_MOV_MEM_REG: {
            auto addr = fetch16(cpu);
            auto reg  = fetch(cpu);
            uint16_t val = cpu->mem->read16(cpu->mem, addr);

            reg_write(cpu, reg, val);
        } break;

        case OP_ADD_REG_REG: {
            auto r1 = fetch(cpu);
            auto r2 = fetch(cpu);
            auto imm1 = reg_read(cpu, r1);
            auto imm2 = reg_read(cpu, r2);

            reg_write(cpu, REG_ACC, imm1 + imm2);
        } break;

        case OP_ADD_IMM_REG: {
            auto imm1 = fetch16(cpu);
            auto reg = fetch(cpu);
            auto imm2 = reg_read(cpu, reg);

            reg_write(cpu, REG_ACC, imm1 + imm2);
        } break;

        case OP_SUB_REG_REG: {
            auto r1 = fetch(cpu);
            auto r2 = fetch(cpu);
            auto imm1 = reg_read(cpu, r1);
            auto imm2 = reg_read(cpu, r2);

            reg_write(cpu, REG_ACC, imm1 - imm2);
        } break;

        case OP_SUB_IMM_REG: {
            auto imm1 = fetch16(cpu);
            auto reg = fetch(cpu);
            auto imm2 = reg_read(cpu, reg);

            reg_write(cpu, REG_ACC, imm1 - imm2);
        } break;

        case OP_SUB_REG_IMM: {
            auto reg  = fetch(cpu);
            auto imm1 = reg_read(cpu, reg);
            auto imm2 = fetch16(cpu);

            reg_write(cpu, REG_ACC, imm1 - imm2);
        } break;

        case OP_INC_REG: {
            auto reg = fetch(cpu);
            auto val = reg_read(cpu, reg);

            reg_write(cpu, reg, val + 1);
        } break;

        case OP_DEC_REG: {
            auto reg = fetch(cpu);
            auto val = reg_read(cpu, reg);

            reg_write(cpu, reg, val - 1);
        } break;

        case OP_MUL_IMM_REG: {
            auto imm = fetch16(cpu);
            auto reg = fetch(cpu);
            auto val = reg_read(cpu, reg);

            reg_write(cpu, REG_ACC, imm * val);
        } break;

        case OP_MUL_REG_REG: {
            auto r1 = fetch(cpu);
            auto r2 = fetch(cpu);
            auto val1 = reg_read(cpu, r1);
            auto val2 = reg_read(cpu, r2);

            reg_write(cpu, REG_ACC, val1 * val2);
        } break;

        case OP_LSF_REG_IMM: {
            auto reg = fetch(cpu);
            auto shift = fetch16(cpu);
            auto val = reg_read(cpu, reg);

            reg_write(cpu, reg, val << shift);
        } break;

        case OP_LSF_REG_REG: {
            auto r1 = fetch(cpu);
            auto r2 = fetch(cpu);

            auto val = reg_read(cpu, r1);
            auto shift = reg_read(cpu, r2);

            reg_write(cpu, r1, val << shift);
        } break;

        case OP_RSF_REG_IMM: {
            auto reg = fetch(cpu);
            auto shift = fetch16(cpu);
            auto val = reg_read(cpu, reg);

            reg_write(cpu, reg, val >> shift);
        } break;

        case OP_RSF_REG_REG: {
            auto r1 = fetch(cpu);
            auto r2 = fetch(cpu);

            auto val = reg_read(cpu, r1);
            auto shift = reg_read(cpu, r2);

            reg_write(cpu, r1, val >> shift);
        } break;

        case OP_AND_REG_IMM: {
            auto r1 = fetch(cpu);
            auto imm = fetch16(cpu);
            auto val = reg_read(cpu, r1);

            reg_write(cpu, REG_ACC, imm & val);
        } break;

        case OP_AND_REG_REG: {
            auto r1 = fetch(cpu);
            auto r2 = fetch(cpu);

            auto val1 = reg_read(cpu, r1);
            auto val2 = reg_read(cpu, r2);

            reg_write(cpu, REG_ACC, val1 & val2);
        } break;

        case OP_OR_REG_IMM: {
            auto r1 = fetch(cpu);
            auto imm = fetch16(cpu);
            auto val = reg_read(cpu, r1);

            reg_write(cpu, REG_ACC, imm | val);
        } break;

        case OP_OR_REG_REG: {
            auto r1 = fetch(cpu);
            auto r2 = fetch(cpu);

            auto val1 = reg_read(cpu, r1);
            auto val2 = reg_read(cpu, r2);

            reg_write(cpu, REG_ACC, val1 | val2);
        } break;

        case OP_XOR_REG_IMM: {
            auto r1 = fetch(cpu);
            auto imm = fetch16(cpu);
            auto val = reg_read(cpu, r1);

            reg_write(cpu, REG_ACC, imm ^ val);
        } break;

        case OP_XOR_REG_REG: {
            auto r1 = fetch(cpu);
            auto r2 = fetch(cpu);

            auto val1 = reg_read(cpu, r1);
            auto val2 = reg_read(cpu, r2);

            reg_write(cpu, REG_ACC, val1 ^ val2);
        } break;

        case OP_NOT: {
            auto reg = fetch(cpu);
            auto val = reg_read(cpu, reg);

            reg_write(cpu, REG_ACC, (~val) & 0xffff);
        } break;

        case OP_JMP: {
            auto addr = fetch16(cpu);

            cpu->pc = addr;
        } break;

        case OP_JNE_IMM: {
            auto imm = fetch16(cpu);
            auto addr = fetch16(cpu);
            auto acc = reg_read(cpu, REG_ACC);

            if ( imm != acc ) {
                cpu->pc = addr;
            }
        } break;

        case OP_JNE_REG: {
            auto reg  = fetch(cpu);
            auto val  = reg_read(cpu, reg);
            auto addr = fetch16(cpu);

            if ( val != reg_read(cpu, REG_ACC) ) {
                cpu->pc = addr;
            }
        } break;

        case OP_JEQ_REG: {
            auto reg  = fetch(cpu);
            auto val  = reg_read(cpu, reg);
            auto addr = fetch16(cpu);

            if ( val == reg_read(cpu, REG_ACC) ) {
                cpu->pc = addr;
            }
        } break;

        case OP_JEQ_IMM: {
            auto val  = fetch16(cpu);
            auto addr = fetch16(cpu);

            if ( val == reg_read(cpu, REG_ACC) ) {
                cpu->pc = addr;
            }
        } break;

        case OP_JLT_REG: {
            auto reg  = fetch(cpu);
            auto val  = reg_read(cpu, reg);
            auto addr = fetch16(cpu);

            if ( val < reg_read(cpu, REG_ACC) ) {
                cpu->pc = addr;
            }
        } break;

        case OP_JLT_IMM: {
            auto val  = fetch16(cpu);
            auto addr = fetch16(cpu);

            if ( val < reg_read(cpu, REG_ACC) ) {
                cpu->pc = addr;
            }
        } break;

        case OP_JGT_REG: {
            auto reg  = fetch(cpu);
            auto val  = reg_read(cpu, reg);
            auto addr = fetch16(cpu);

            if ( val > reg_read(cpu, REG_ACC) ) {
                cpu->pc = addr;
            }
        } break;

        case OP_JGT_IMM: {
            auto val  = fetch16(cpu);
            auto addr = fetch16(cpu);

            if ( val > reg_read(cpu, REG_ACC) ) {
                cpu->pc = addr;
            }
        } break;

        case OP_JLE_REG: {
            auto reg  = fetch(cpu);
            auto val  = reg_read(cpu, reg);
            auto addr = fetch16(cpu);

            if ( val <= reg_read(cpu, REG_ACC) ) {
                cpu->pc = addr;
            }
        } break;

        case OP_JLE_IMM: {
            auto val  = fetch16(cpu);
            auto addr = fetch16(cpu);

            if ( val <= reg_read(cpu, REG_ACC) ) {
                cpu->pc = addr;
            }
        } break;

        case OP_JGE_REG: {
            auto reg  = fetch(cpu);
            auto val  = reg_read(cpu, reg);
            auto addr = fetch16(cpu);

            if ( val >= reg_read(cpu, REG_ACC) ) {
                cpu->pc = addr;
            }
        } break;

        case OP_JGE_IMM: {
            auto val  = fetch16(cpu);
            auto addr = fetch16(cpu);

            if ( val >= reg_read(cpu, REG_ACC) ) {
                cpu->pc = addr;
            }
        } break;

        case OP_PSH_IMM: {
            auto val = fetch16(cpu);
            push(cpu, val);
        } break;

        case OP_PSH_REG: {
            auto reg = fetch(cpu);
            auto val = reg_read(cpu, reg);
            push(cpu, val);
        } break;

        case OP_POP: {
            auto reg = fetch(cpu);
            auto val = pop(cpu);
            reg_write(cpu, reg, val);
        } break;

        case OP_CAL_IMM: {
            auto addr = fetch16(cpu);
            push_state(cpu);
            cpu->pc = addr;
        } break;

        case OP_CAL_REG: {
            auto reg = fetch(cpu);
            auto addr = reg_read(cpu, reg);
            push_state(cpu);
            cpu->pc = addr;
        } break;

        case OP_INT: {
            uint16_t val = fetch16(cpu) & 0xf;
            handle_interrupt(cpu, val);
        } break;

        case OP_RET: {
            pop_state(cpu);
        } break;

        case OP_RTI: {
            cpu->in_int = false;
            pop_state(cpu);
        } break;
#endif
    }
}

uint8_t
step(Cpu *cpu) {
    auto instr = fetch(cpu);
    exec(cpu, instr);

    return instr;
}

void
run(Cpu *cpu) {
    for ( ;; ) {
        auto ret = step(cpu);

        if ( ret == VMOP_HLT ) {
            break;
        }
    }
}
