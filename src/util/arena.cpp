struct Arena_Block {
    size_t        size;
    size_t        ptr;
    uint8_t     * mem;
    Arena_Block * next;
};

enum { ARENA_SIZE = 1024 };
struct Arena {
    Arena_Block * curr_block;
};

Arena_Block *
arena_block(size_t size) {
    Arena_Block *result = (Arena_Block *)malloc(sizeof(Arena_Block));

    result->size = size;
    result->mem  = (uint8_t *)Urq::Os::os_alloc(size);
    result->ptr  = 0;
    result->next = NULL;

    return result;
}

Arena *
arena_new(size_t size) {
    Arena *result = (Arena *)malloc(sizeof(Arena));

    result->curr_block = arena_block(size);

    return result;
}

void *
arena_alloc(Arena *arena, size_t size) {
    Arena_Block *curr = arena->curr_block;

    /* @AUFGABE: größe der angeforderten menge des speichers speichern, damit später
     *           realloc weiß wieviel speicher kopiert werden muß.
     */
    if ( (curr->ptr + size) > curr->size ) {
        size_t new_size = MAX(ARENA_SIZE, size*2);
        curr->next = arena_block(new_size);
        arena->curr_block = curr->next;
        curr = curr->next;
    }

    void *result = curr->mem + curr->ptr;
    curr->ptr += size;

    return result;
}

void *
arena_realloc(Arena *arena, void *mem, size_t size) {
    void *result = realloc(mem, size);

    return result;
}


