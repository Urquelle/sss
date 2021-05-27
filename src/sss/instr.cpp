namespace Vm {

struct Sss_Instr {
    uint8_t prefix;
    uint8_t opcode;
    uint8_t mod_rm;
    uint8_t sib;

    union {
        uint32_t dis;
        uint32_t imm;
    };
};

uint64_t
sss_instr(uint8_t prefix, Vm_Op opcode, Operand *operand1, Operand *operand2) {
    uint64_t result = ((uint64_t)prefix) << 56 | ((uint64_t)opcode) << 48;

    return result;
}

}

