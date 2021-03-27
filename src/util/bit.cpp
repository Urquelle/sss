uint16_t
bit_swap16(uint16_t val) {
    uint16_t result = ((val & 0xff00) >> 8) | ((val & 0x00ff) << 8);

    return result;
}

void
bit_load(uint8_t *base, size_t addr, uint8_t *data, size_t size) {
    memcpy(base + addr, data, size);
}

uint8_t
bit_write16(uint8_t *base, size_t addr, uint16_t val) {
    *(uint16_t *)(base + addr) = val;

    return 2;
}

uint8_t
bit_write8(uint8_t *base, size_t addr, uint8_t val) {
    *(base + addr) = val;

    return 1;
}

uint8_t
bit_read8(uint8_t *base, size_t addr) {
    uint8_t val = *(base + addr);

    return val;
}

uint16_t
bit_read16(uint8_t *base, size_t addr) {
    uint16_t result = *(uint16_t *)(base + addr);

    return result;
}

