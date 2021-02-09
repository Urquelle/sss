#ifndef __URQ_COMMON__
#define __URQ_COMMON__

#define ALLOCATOR(name)   void * name(size_t size)
typedef ALLOCATOR(Alloc);

#define DEALLOCATOR(name) void   name(void *mem)
typedef DEALLOCATOR(Dealloc);

#define CALLOCATOR(name)  void * name(size_t num, size_t size)
typedef CALLOCATOR(Calloc);

#define REALLOCATOR(name) void * name(void *mem, size_t size)
typedef REALLOCATOR(Realloc);

#define ARRAY_LEN(arr) (sizeof(arr) / sizeof(arr[0]))
#define IS_POW2(x) (((x) != 0) && ((x) & ((x)-1)) == 0)
#define MAX(x, y) ((x) >= (y) ? (x) : (y))
#define MIN(x, y) ((x) <= (y) ? (x) : (y))
#define CLAMP_MIN(x, min) MAX(x, min)

#define urq_allocs(name) (name *)urq_alloc(sizeof(name))

Alloc   *urq_alloc   = malloc;
Alloc   *urq_alloct  = malloc;
Realloc *urq_realloc = realloc;
Dealloc *urq_dealloc = free;
Calloc  *urq_calloc  = calloc;

#endif

