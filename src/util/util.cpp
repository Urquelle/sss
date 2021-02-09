#ifndef __URQ_UTIL__
#define __URQ_UTIL__

#include "common.cpp"
#include "utf8.cpp"

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

struct Map {
    void **vals;
    void **keys;
    size_t len;
    size_t cap;
};

void *
map_get(Map *map, void *key) {
    if (map->len == 0) {
        return NULL;
    }

    assert(IS_POW2(map->cap));
    size_t i = (size_t)ptr_hash(key);
    assert(map->len < map->cap);

    for (;;) {
        i &= map->cap - 1;

        if ( map->keys[i] == key ) {
            return map->vals[i];
        } else if ( !map->keys[i] ) {
            return NULL;
        }
        i++;
    }

    return NULL;
}

void map_put(Map *map, void *key, void *val);

void
map_grow(Map *map, size_t new_cap) {
    new_cap = MAX(16, new_cap);
    Map new_map = {};

    new_map.keys = (void **)urq_calloc(new_cap, sizeof(void *));
    new_map.vals = (void **)urq_alloc(new_cap * sizeof(void *));
    new_map.cap  = new_cap;

    for ( size_t i = 0; i < map->cap; i++ ) {
        if ( map->keys[i] ) {
            map_put(&new_map, map->keys[i], map->vals[i]);
        }
    }

    urq_dealloc(map->keys);
    urq_dealloc(map->vals);
    *map = new_map;
}

void
map_put(Map *map, void *key, void *val) {
    assert(key);
    assert(val);

    if (2*map->len >= map->cap) {
        map_grow(map, 2*map->cap);
    }

    assert(2*map->len < map->cap);
    assert(IS_POW2(map->cap));

    size_t i = (size_t)ptr_hash(key);
    for (;;) {
        i &= map->cap - 1;

        if ( !map->keys[i] ) {
            map->len++;
            map->keys[i] = key;
            map->vals[i] = val;

            return;
        } else if ( map->keys[i] == key ) {
            map->vals[i] = val;

            return;
        }

        i++;
    }
}

void
map_reset(Map *map) {
    for ( int i = 0; i < map->cap; ++i ) {
        if ( map->keys[i] ) {
            ((char **)map->keys)[i] = 0;
        }
    }
}

struct Intern {
    size_t   len;
    Intern*  next;
    char     str[1];
};

char *
intern_str(Map *interns, char *start, char *end) {
    size_t len = end - start;
    uint64_t hash = bytes_hash(start, len);
    void *key = (void *)(uintptr_t)(hash ? hash : 1);

    Intern *intern = (Intern *)map_get(interns, key);
    for (Intern *it = intern; it; it = it->next) {
        if (it->len == len && strncmp(it->str, start, len) == 0) {
            return it->str;
        }
    }

    Intern *new_intern = (Intern *)urq_alloc(offsetof(Intern, str) + len + 1);

    new_intern->len = len;
    new_intern->next   = intern;
    memcpy(new_intern->str, start, len);
    new_intern->str[len] = 0;
    map_put(interns, key, new_intern);

    return new_intern->str;
}

char *
intern_str(Map *interns, char *value) {
    size_t len = strlen(value);
    return intern_str(interns, value, value + len);
}

struct Queue_Entry {
    Queue_Entry * next;
    Queue_Entry * prev;
    void        * data;
};

struct Queue {
    Queue_Entry   root;
    Queue_Entry * curr;
    size_t        num_elems;
};

void *
queue_entry(Queue *q, size_t index) {
    if ( index >= q->num_elems ) {
        return NULL;
    }

    Queue_Entry *elem = q->root.next;
    for ( int i = 0; i < index; ++i ) {
        elem = elem->next;
    }

    return elem->data;
}

void
queue_push(Queue *q, void *data) {
    Queue_Entry *entry = urq_allocs(Queue_Entry);

    if ( !q->curr ) {
        q->curr = &q->root;
    }

    entry->data = data;
    entry->next = NULL;
    entry->prev = q->curr;

    q->curr->next = entry;
    q->curr = entry;
    q->num_elems++;
}

void *
queue_pop(Queue *q) {
    void *result = q->curr->data;

    if ( q->curr != &q->root) {
        q->curr = q->curr->prev;
    }

    q->num_elems--;

    return result;
}

void *
queue_shift(Queue *q) {
    void *result = 0;

    if ( !q->root.next ) {
        return result;
    }

    result = q->root.next->data;
    q->root.next = q->root.next->next;

    if ( q->root.next ) {
        q->root.next->prev = &q->root;
    }

    q->num_elems--;

    return result;
}

void
queue_unshift(Queue *q, void *data) {
    Queue_Entry *entry = urq_allocs(Queue_Entry);

    entry->data = data;
    entry->next = q->root.next;
    entry->prev = &q->root;

    q->root.next = entry;
    q->num_elems++;
}

void
queue_remove(Queue *q, void *data) {
    Queue_Entry *elem = q->root.next;
    for ( int i = 0; i < q->num_elems; ++i ) {
        elem = elem->next;

        if ( elem->data == data ) {
            elem->prev->next = elem->next;
            elem->next->prev = elem->prev;
            q->num_elems--;

            break;
        }
    }
}

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

typedef struct BufHdr {
    size_t len;
    size_t cap;
    char buf[1];
} BufHdr;

#define buf__hdr(b) ((BufHdr *)((char *)(b) - offsetof(BufHdr, buf)))

#define buf_len(b) ((b) ? buf__hdr(b)->len : 0)
#define buf_cap(b) ((b) ? buf__hdr(b)->cap : 0)
#define buf_end(b) ((b) + buf_len(b))
#define buf_sizeof(b) ((b) ? buf_len(b)*sizeof(*b) : 0)

#define buf_free(b) ((b) ? (free(buf__hdr(b)), (b) = NULL) : 0)
#define buf_fit(b, n) ((n) <= buf_cap(b) ? 0 : (*((void **)&(b)) = buf__grow((b), (n), sizeof(*(b)))))
#define buf_push(b, ...) (buf_fit((b), 1 + buf_len(b)), (b)[buf__hdr(b)->len++] = (__VA_ARGS__))
#define buf_printf(b, ...) ((b) = buf__printf((b), __VA_ARGS__))
#define buf_clear(b) ((b) ? buf__hdr(b)->len = 0 : 0)

void *
buf__grow(const void *buf, size_t new_len, size_t elem_size) {
    assert(buf_cap(buf) <= (SIZE_MAX - 1)/2);
    size_t new_cap = CLAMP_MIN(2*buf_cap(buf), MAX(new_len, 16));
    assert(new_len <= new_cap);
    assert(new_cap <= (SIZE_MAX - offsetof(BufHdr, buf))/elem_size);
    size_t new_size = offsetof(BufHdr, buf) + new_cap*elem_size;
    BufHdr *new_hdr;
    if (buf) {
        new_hdr = (BufHdr *)realloc(buf__hdr(buf), new_size);
    } else {
        new_hdr = (BufHdr *)malloc(new_size);
        new_hdr->len = 0;
    }
    new_hdr->cap = new_cap;
    return new_hdr->buf;
}

char *
buf__printf(char *buf, const char *fmt, ...) {
    va_list args;
    va_start(args, fmt);
    size_t cap = buf_cap(buf) - buf_len(buf);
    size_t n = 1 + vsnprintf(buf_end(buf), cap, fmt, args);
    va_end(args);
    if (n > cap) {
        buf_fit(buf, n + buf_len(buf));
        va_start(args, fmt);
        size_t new_cap = buf_cap(buf) - buf_len(buf);
        n = 1 + vsnprintf(buf_end(buf), new_cap, fmt, args);
        assert(n <= new_cap);
        va_end(args);
    }
    buf__hdr(buf)->len += n - 1;
    return buf;
}

struct Bucket {
    Bucket   * next;
    Bucket   * prev;
    uint32_t   mem_used;
    uint8_t  * data;
};

struct Bucket_Array {
    uint32_t bucket_size;
    int32_t  bucket_count;

    Bucket *curr_bucket;
};

Bucket *
add_bucket(Bucket_Array *ba, Bucket *next, Bucket *prev) {
    Bucket *result = 0;

    result = (Bucket *)  malloc(sizeof(Bucket));
    result->data     = (uint8_t *) malloc(ba->bucket_size);
    result->mem_used = 0;
    result->next     = next;
    result->prev     = prev;

    ba->bucket_count++;

    return result;
}

void
init(Bucket_Array *ba) {
    ba->bucket_size  = 64;
    ba->bucket_count = 0;
    ba->curr_bucket = add_bucket(ba, 0, 0);
}

void *
push(Bucket_Array *bucket_array, uint32_t size) {
    assert(bucket_array->curr_bucket);

    void *result = 0;

    if ((bucket_array->curr_bucket->mem_used + size) >= bucket_array->bucket_size) {
        add_bucket(bucket_array, 0, bucket_array->curr_bucket);
    }

    result = bucket_array->curr_bucket->data + bucket_array->curr_bucket->mem_used;

    bucket_array->curr_bucket->mem_used += size;

    return result;
}

#endif

