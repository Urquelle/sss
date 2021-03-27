uint64_t
uint64_hash(uint64_t x) {
    x *= 0xff51afd7ed558ccd;
    x ^= x >> 32;

    return x;
}

uint64_t
ptr_hash(void *ptr) {
    return uint64_hash((uintptr_t)ptr);
}

uint64_t
mix_hash(uint64_t x, uint64_t y) {
    x ^= y;
    x *= 0xff51afd7ed558ccd;
    x ^= x >> 32;

    return x;
}

uint64_t
bytes_hash(void *ptr, size_t len) {
    uint64_t x = 0xcbf29ce484222325;
    char *buf = (char *)ptr;

    for (size_t i = 0; i < len; i++) {
        x ^= buf[i];
        x *= 0x100000001b3;
        x ^= x >> 32;
    }

    return x;
}

