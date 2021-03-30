#ifndef __URQ_UTIL__
#define __URQ_UTIL__

#include "common.cpp"
#include "utf8.cpp"
#include "hash.cpp"
#include "bit.cpp"
#include "map.cpp"
#include "arena.cpp"
#include "queue.cpp"

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

typedef struct BufHdr {
    int32_t len;
    int32_t cap;
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
buf__grow(void *buf, int32_t new_len, int32_t elem_size) {
    assert(buf_cap(buf) <= (SIZE_MAX - 1)/2);
    int32_t new_cap = CLAMP_MIN(2*buf_cap(buf), MAX(new_len, 16));
    assert(new_len <= new_cap);
    assert(new_cap <= (SIZE_MAX - offsetof(BufHdr, buf))/elem_size);
    int32_t new_size = offsetof(BufHdr, buf) + new_cap*elem_size;
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
    int32_t cap = buf_cap(buf) - buf_len(buf);
    int32_t n = 1 + vsnprintf(buf_end(buf), cap, fmt, args);
    va_end(args);

    if (n > cap) {
        buf_fit(buf, n + buf_len(buf));
        va_start(args, fmt);
        int32_t new_cap = buf_cap(buf) - buf_len(buf);
        n = 1 + vsnprintf(buf_end(buf), new_cap, fmt, args);
        assert(n <= new_cap);
        va_end(args);
    }

    buf__hdr(buf)->len += n - 1;
    return buf;
}

#include "args.cpp"
#include "string.cpp"

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

struct Loc {
    char *file;
    size_t line;
    size_t col;
};

Loc loc_none = { "<unbekannt>", 0, 0 };

void
loc_copy(Loc *from, Loc *to) {
    to->file = from->file;
    to->line = from->line;
    to->col  = from->col;
}

void
report_error(Loc *loc, char *fmt, ...) {
    using namespace Urq::Os::api;

    va_list args = NULL;
    va_start(args, fmt);

    os_stdout_set_font(OS_COLOR_RED, OS_FLAGS_FONT_BOLD);
    printf("Fehler");
    os_stdout_set_regular();
    printf(" in %s Zeile %lld: ", loc->file, loc->line);
    os_stdout_set_bold();
    vprintf(fmt, args);
    va_end(args);
    printf("\n");

#if 0
    exit(1);
#else
    __debugbreak();
#endif
}

#endif

