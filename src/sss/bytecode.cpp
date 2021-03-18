#define dcAllocMem urq_alloc
#define dcFreeMem urq_dealloc

#include "dyncall.h"

struct Bytecode;
struct Bytecode_Scope;
struct Obj;
struct Obj_Proc;
struct Table;
struct Vm;

#define IS_STRUCT(Value) (((Value).kind == VAL_OBJ) && ((Value).obj_val->kind == OBJ_STRUCT))
#define IS_ENUM(Value) (((Value).kind == VAL_OBJ) && ((Value).obj_val->kind == OBJ_ENUM))
#define IS_NAMESPACE(Value) (((Value).kind == VAL_OBJ) && ((Value).obj_val->kind == OBJ_NAMESPACE))

#define VALUES      \
    X(VAL_NONE)     \
    X(VAL_INT)      \
    X(VAL_FLOAT)    \
    X(VAL_BOOL)     \
    X(VAL_OBJ)

enum Value_Kind {
#define X(Elem) Elem,
    VALUES
#undef X
};

struct Value {
    Value_Kind kind;
    uint32_t   size;

    union {
        int64_t   int_val;
        float     flt_val;
        bool      bool_val;
        Obj     * obj_val;
    };
};

typedef Value* Values;

#define OBJECTS         \
    X(OBJ_STRING)       \
    X(OBJ_RANGE)        \
    X(OBJ_PROC)         \
    X(OBJ_ITER)         \
    X(OBJ_NAMESPACE)    \
    X(OBJ_STRUCT)       \
    X(OBJ_ENUM)         \
    X(OBJ_COMPOUND)     \
    X(OBJ_STRUCT_FIELD)

enum Obj_Kind {
#define X(Elem) Elem,
    OBJECTS
#undef X
};

struct Obj {
    Obj_Kind   kind;
};

struct Obj_String : Obj {
    uint32_t   size;
    char     * ptr;
    uint32_t   hash;
};

struct Obj_Compound : Obj {
    Values   elems;
    uint32_t num_elems;
};

struct Obj_Proc : Obj {
    Obj_String     * name;
    Bytecode_Scope * scope;
    Bytecode       * bc;
    uint32_t         num_params;
    bool             sys_call;
    char           * lib;
    Proc_Sign      * sign;
};

struct Obj_Namespace : Obj {
    Obj_String     * name;
    char           * scope_name;
    Obj_Proc       * proc;
    Bytecode_Scope * scope;
    Bytecode_Scope * prev_scope;
};

struct Obj_Range : Obj {
    Value left;
    Value right;
};

struct Obj_Iter : Obj {
    Value       iter;
    Obj_Range * range;
    uint32_t    curr_index;
};

struct Obj_Struct : Obj {
    Obj_String *  name;
    Table      *  fields;
    Obj_String ** fieldnames_ordered;
};

struct Obj_Struct_Field : Obj {
    Obj_String * name;
    Value        default_value;
};

struct Obj_Enum : Obj {
    Obj_String *  name;
    Table      *  fields;
    Obj_String ** fieldnames_ordered;
};

enum Bytecode_Flags {
    BYTECODEFLAG_NONE,
    BYTECODEFLAG_ASSIGNABLE,
    BYTECODEFLAG_SET_EXISTING,
};

void               bytecode_build_file(Bytecode *bc, Parsed_File *file);
bool               bytecode_sym_set(Obj_String *key, Value val, uint32_t flags = BYTECODEFLAG_NONE);
int32_t            bytecode_emit_const(Bytecode *bc, Value val);
void               bytecode_debug(Vm *vm, int32_t code);
void               bytecode_stmt(Bytecode *bc, Stmt *stmt);
void               bytecode_init(Bytecode *bc);
void               bytecode_typespec(Bytecode *bc, Typespec *typespec);
Value              val_none();
Value              val_str(char *ptr, uint32_t size);
Value              val_bool(bool val);
void               val_print(Value val);
Obj_Proc         * obj_proc();
Obj_Proc         * as_proc(Value val);
Obj_Range        * as_range(Value val);
Obj_String       * as_str(Value val);
Obj_Struct       * as_struct(Value val);
Obj_Struct_Field * as_struct_field(Value val);

#define AS_NAMESPACE(Val) ((Obj_Namespace *)(Val).obj_val)

struct Value_Array {
    uint32_t   size;
    uint32_t   cap;
    Value    * values;
};

#define TABLE_MAX_LOAD 0.75f
struct Table_Entry {
    Obj_String * key;
    Value        val;
};

struct Table {
    int32_t       size;
    int32_t       cap;
    Table_Entry * entries;
    Table_Entry ** ordered_entries;
};

struct Bytecode_Scope {
    char           * name;
    Bytecode_Scope * parent;
    Table            syms;
    Table            export_syms;
};

Bytecode_Scope   bc_sys_scope    = {"sys",    NULL,          {}};
Bytecode_Scope   bc_global_scope = {"global", &bc_sys_scope, {}};
Bytecode_Scope * bc_curr_scope   = &bc_global_scope;
Bytecode_Scope * bc_prev_scope;

struct Bytecode {
    Bytecode * parent;
    uint8_t  * code;
    uint32_t   size;
    uint32_t   cap;

    Value_Array constants;
};

#define BYTECODES                   \
    X(BYTECODEOP_HLT)               \
    X(BYTECODEOP_NEG)               \
    X(BYTECODEOP_ADD)               \
    X(BYTECODEOP_CALL)              \
    X(BYTECODEOP_CAST)              \
    X(BYTECODEOP_CMP_LT)            \
    X(BYTECODEOP_COMPOUND)          \
    X(BYTECODEOP_CONST)             \
    X(BYTECODEOP_DIV)               \
    X(BYTECODEOP_EXPORT_SYM)        \
    X(BYTECODEOP_INC)               \
    X(BYTECODEOP_INC_LOOP)          \
    X(BYTECODEOP_INIT_LOOP)         \
    X(BYTECODEOP_IMPORT_SYMS)       \
    X(BYTECODEOP_JMP)               \
    X(BYTECODEOP_JMP_FALSE)         \
    X(BYTECODEOP_LOAD_STRUCT_FIELD) \
    X(BYTECODEOP_LOAD_SYM)          \
    X(BYTECODEOP_LOAD_SYMREF)       \
    X(BYTECODEOP_MATCH_CASE)        \
    X(BYTECODEOP_MUL)               \
    X(BYTECODEOP_NAMED_COMPOUND)    \
    X(BYTECODEOP_NAMESPACE_ENTER)   \
    X(BYTECODEOP_NAMESPACE_LEAVE)   \
    X(BYTECODEOP_NONE)              \
    X(BYTECODEOP_POP)               \
    X(BYTECODEOP_PUSH)              \
    X(BYTECODEOP_PUSH_SYM)          \
    X(BYTECODEOP_PUSH_SYSSYM)       \
    X(BYTECODEOP_PUSH_TYPEVAL)      \
    X(BYTECODEOP_RANGE)             \
    X(BYTECODEOP_RET)               \
    X(BYTECODEOP_SCOPE_ENTER)       \
    X(BYTECODEOP_SCOPE_LEAVE)       \
    X(BYTECODEOP_SET_SYM)           \
    X(BYTECODEOP_STRUCT)            \
    X(BYTECODEOP_STRUCT_FIELD)      \
    X(BYTECODEOP_ENUM)              \
    X(BYTECODEOP_SUB)               \

enum Bytecode_Opcode {
#define X(Elem) Elem,
    BYTECODES
#undef X
};

struct Call_Frame {
    Obj_Proc       * proc;
    Bytecode_Scope * prev_scope;
    uint32_t         pc;
    uint32_t         sp;
};

enum { MAX_STACK_SIZE = 1024, MAX_FRAME_NUM = 100 };
struct Vm {
    Value        stack[MAX_STACK_SIZE];
    Call_Frame   frames[MAX_FRAME_NUM];
    int32_t      frame_num;
};

Table strings;

Bytecode_Scope *
bytecode_scope_new(char *name, Bytecode_Scope *parent = NULL) {
    Bytecode_Scope *result = urq_allocs(Bytecode_Scope);

    result->name   = name;
    result->parent = parent;

    return result;
}

void
bytecode_scope_enter(char *name = NULL) {
    Bytecode_Scope *scope = urq_allocs(Bytecode_Scope);

    scope->name   = name;
    scope->parent = bc_curr_scope;
    bc_curr_scope = scope;
}

void
bytecode_scope_leave() {
    assert(bc_curr_scope->parent);
    bc_curr_scope = bc_curr_scope->parent;
}

uint32_t
hash_string(char* key, int32_t size) {
    uint32_t hash = 2166136261u;

    for (int i = 0; i < size; i++) {
        hash ^= (uint8_t)key[i];
        hash *= 16777619;
    }

    return hash;
}

Table *
table_new() {
    Table *result = urq_allocs(Table);

    result->size            = 0;
    result->cap             = 0;
    result->entries         = NULL;
    result->ordered_entries = NULL;

    return result;
}

Table_Entry *
table_find(Table_Entry *entries, int32_t cap, Obj_String *key) {
    uint32_t index = key->hash % cap;
    Table_Entry* tombstone = NULL;

    for (;;) {
        Table_Entry *entry = &entries[index];

        if (entry->key == NULL) {
            if (entry->val.kind == VAL_NONE) {
                return tombstone != NULL ? tombstone : entry;
            } else {
                if (tombstone == NULL) tombstone = entry;
            }
        } else if (entry->key == key) {
            return entry;
        }

        index = (index + 1) % cap;
    }
}

Obj_String *
table_find_string(Table *table, char* chars, uint32_t size, uint32_t hash) {
    if (table->size == 0) {
        return NULL;
    }

    uint32_t index = hash % table->cap;

    for (;;) {
        Table_Entry *entry = &table->entries[index];

        if (entry->key == NULL) {
            if (entry->val.kind == VAL_NONE) {
                return NULL;
            }
        } else if (entry->key->size == size &&
                entry->key->hash == hash &&
                memcmp(entry->key->ptr, chars, size) == 0) {
            return entry->key;
        }

        index = (index + 1) % table->cap;
    }
}

void
table_adjust(Table* table, int cap) {
    Table_Entry *entries = (Table_Entry *)urq_alloc(sizeof(Table_Entry)*cap);

    for (int i = 0; i < cap; i++) {
        entries[i].key = NULL;
        entries[i].val = val_none();
    }

    table->size = 0;
    for (int i = 0; i < table->cap; i++) {
        Table_Entry *entry = &table->entries[i];
        if (entry->key == NULL) continue;

        Table_Entry *dest = table_find(entries, cap, entry->key);
        dest->key = entry->key;
        dest->val = entry->val;
        table->size += 1;
    }

    table->entries = entries;
    table->cap = cap;
}

bool
table_set(Table *table, Obj_String *key, Value val) {
    if ( table->size + 1 > table->cap * TABLE_MAX_LOAD) {
        int cap = (table->cap < 16) ? 16 : table->cap*2;
        table_adjust(table, cap);
    }

    Table_Entry *entry = table_find(table->entries, table->cap, key);

    bool is_new_key = entry->key == NULL;
    if (is_new_key && entry->val.kind == VAL_NONE) {
        table->size += 1;
    }

    entry->key = key;
    entry->val = val;

    if ( is_new_key ) {
        buf_push(table->ordered_entries, entry);
    }

    return is_new_key;
}

void
table_add_all(Table* from, Table* to) {
    for (int i = 0; i < from->cap; i++) {
        Table_Entry* entry = &from->entries[i];
        if (entry->key != NULL) {
            table_set(to, entry->key, entry->val);
        }
    }
}

bool
table_get(Table *table, Obj_String *key, Value *val) {
    if (table->size == 0) {
        return false;
    }

    Table_Entry *entry = table_find(table->entries, table->cap, key);
    if (entry->key == NULL) {
        return false;
    }

    *val = entry->val;

    return true;
}

bool
table_del(Table *table, Obj_String *key) {
  if (table->size == 0) {
      return false;
  }

  Table_Entry* entry = table_find(table->entries, table->cap, key);
  if (entry->key == NULL) {
      return false;
  }

  entry->key = NULL;
  entry->val = val_bool(true);

  return true;
}

Vm *
vm_new(Bytecode *bc) {
    Vm *result = urq_allocs(Vm);

    result->frame_num = 1;

    result->frames[0].proc = obj_proc();
    result->frames[0].pc   = 0;
    result->frames[0].sp   = 0;
    result->frames[0].proc->bc = bc;

    return result;
}

Call_Frame *
vm_curr_frame(Vm *vm) {
    Call_Frame *result = vm->frames + vm->frame_num - 1;

    return result;
}

void
vm_debug(Vm *vm) {
    Call_Frame *frame = vm_curr_frame(vm);

    printf("\n*************************************************\n\n");
    printf("frame: %d | ", vm->frame_num);
    printf("pc: %d | ", frame->pc);
    printf("sp: %d | \n", frame->sp);

    printf("\n******************** CODE ***********************\n");
    for ( int i = 0; i < 32; ++i ) {
        printf("%s%s%02d", (i % 16 == 0) ? "\n" : "", (i % 16 == 0) ? "" : " ", frame->proc->bc->code[frame->pc+i]);
    }
    printf("\n\n***************** STACK *************************\n");
    if ( frame->sp ) {
        for ( uint32_t i = 0; i < frame->sp; ++i ) {
            printf("%s%02d[", (i % 4 == 0) ? "\n" : "", i);
            val_print(vm->stack[i]);
            printf("] ");
        }
    } else {
        printf("##    LEER   ##");
    }
    printf("\n\n*************************************************\n");
}

Bytecode *
bytecode_new() {
    Bytecode *result = urq_allocs(Bytecode);

    result->parent    = NULL;
    result->code      = NULL;
    result->size      = 0;
    result->cap       = 0;
    result->constants = {};

    return result;
}

void
stack_push(Vm *vm, Value val) {
    Call_Frame *frame = vm_curr_frame(vm);

    assert( frame->sp < MAX_STACK_SIZE );
    vm->stack[frame->sp++] = val;
}

Value
stack_pop(Vm *vm) {
    Call_Frame *frame = vm_curr_frame(vm);

    assert(frame->sp > 0);
    Value result = vm->stack[--frame->sp];

    return result;
}

int32_t
value_push(Value_Array *array, Value value) {
    if ( array->size + 1 > array->cap ) {
        uint32_t cap = ( array->cap < 16 ) ? 16 : array->cap*2;
        Value *values = (Value *)urq_alloc(cap*sizeof(Value));
        memcpy(values, array->values, array->size*sizeof(Value));

        array->values = values;
        array->cap    = cap;
    }

    int32_t result = array->size;

    array->values[array->size] = value;
    array->size += 1;

    return result;
}

Value
value_get(Value_Array *array, int32_t index) {
    Value result = array->values[index];

    return result;
}

void
value_set(Value_Array *array, int32_t index, Value val) {
    array->values[index] = val;
}

void
bytecode_resize(Bytecode *bc, int32_t size) {
    if ( bc->size + size > bc->cap ) {
        uint32_t cap = ( bc->cap < 16 ) ? 16 : bc->cap*2;
        uint8_t *code = (uint8_t *)urq_alloc(cap);
        memcpy(code, bc->code, bc->size);

        bc->code = code;
        bc->cap  = cap;
    }
}

void
bytecode_write8(Bytecode *bc, uint8_t val) {
    bytecode_resize(bc, 1);

    bc->code[bc->size] = val;
    bc->size += 1;
}

void
bytecode_write16(Bytecode *bc, uint16_t val) {
    bytecode_resize(bc, 2);

    *(uint16_t *)(bc->code + bc->size) = val;
    bc->size += 2;
}

void
bytecode_write16(Bytecode *bc, int32_t addr, uint16_t val) {
    bytecode_resize(bc, 2);

    *(uint16_t *)(bc->code + addr) = val;
}

uint8_t
bytecode_read8(Vm *vm) {
    Call_Frame *frame = vm_curr_frame(vm);

    uint8_t result = frame->proc->bc->code[frame->pc];
    frame->pc += 1;

    return result;
}

uint16_t
bytecode_read16(Vm *vm) {
    Call_Frame *frame = vm_curr_frame(vm);

    uint16_t result = *(uint16_t *)(frame->proc->bc->code + frame->pc);
    frame->pc += 2;

    return result;
}

Obj_String *
obj_string(char *ptr, uint32_t size) {
    uint32_t hash = hash_string(ptr, size);
    Obj_String *interned = table_find_string(&strings, ptr, size, hash);

    if ( interned ) {
        return interned;
    }

    Obj_String *result = urq_allocs(Obj_String);

    result->kind = OBJ_STRING;
    result->ptr  = ptr;
    result->size = size;
    result->hash = hash;

    table_set(&strings, result, val_none());

    return result;
}

Obj_Compound *
obj_compound(Values elems, uint32_t num_elems) {
    Obj_Compound *result = urq_allocs(Obj_Compound);

    result->kind      = OBJ_COMPOUND;
    result->elems     = elems;
    result->num_elems = num_elems;

    return result;
}

Obj_Range *
obj_range(Value left, Value right) {
    Obj_Range *result = urq_allocs(Obj_Range);

    result->kind  = OBJ_RANGE;
    result->left  = left;
    result->right = right;

    return result;
}

Obj_Proc *
obj_proc() {
    Obj_Proc *result = urq_allocs(Obj_Proc);

    result->kind       = OBJ_PROC;
    result->scope      = bytecode_scope_new(NULL, NULL);
    result->num_params = 0;
    result->bc         = bytecode_new();
    result->name       = NULL;
    result->sys_call   = false;
    result->lib        = NULL;
    result->sign       = NULL;

    return result;
}

Obj_Namespace *
obj_namespace(char *name, char *scope_name) {
    Obj_Namespace *result = urq_allocs(Obj_Namespace);

    result->kind       = OBJ_NAMESPACE;
    result->name       = obj_string(name, (uint32_t)utf8_str_size(name));
    result->scope_name = scope_name;
    result->proc       = obj_proc();
    result->scope      = bytecode_scope_new(scope_name, &bc_sys_scope);

    return result;
}

Obj_Proc *
obj_proc(bool sys_call, char *lib) {
    Obj_Proc *result = obj_proc();

    result->sys_call = sys_call;
    result->lib      = lib;

    return result;
}

Obj_Iter *
obj_iter(Obj_Range *range, Value iter) {
    Obj_Iter *result = urq_allocs(Obj_Iter);

    result->kind       = OBJ_ITER;
    result->range      = range;
    result->iter       = iter;
    result->curr_index = 0;

    return result;
}

Obj_Struct *
obj_struct(Obj_String *name) {
    Obj_Struct *result = urq_allocs(Obj_Struct);

    result->kind   = OBJ_STRUCT;
    result->name   = name;
    result->fields = table_new();
    result->fieldnames_ordered = NULL;

    return result;
}

Obj_Struct_Field *
obj_struct_field(Obj_String *name) {
    Obj_Struct_Field *result = urq_allocs(Obj_Struct_Field);

    result->kind = OBJ_STRUCT_FIELD;
    result->name = name;

    return result;
}

Obj_Enum *
obj_enum(Obj_String *name) {
    Obj_Enum *result = urq_allocs(Obj_Enum);

    result->kind   = OBJ_ENUM;
    result->name   = name;
    result->fields = table_new();
    result->fieldnames_ordered = NULL;

    return result;
}

void
obj_print(Obj *obj) {
    switch ( obj->kind ) {
        case OBJ_STRING: {
            printf("(string: \"%s\")", ((Obj_String *)obj)->ptr);
        } break;

        case OBJ_RANGE: {
            printf("(range: [left: ");
            val_print(((Obj_Range *)obj)->left);
            printf("]..[right: ");
            val_print(((Obj_Range *)obj)->right);
            printf("])");
        } break;

        case OBJ_COMPOUND: {
            Obj_Compound *c = ((Obj_Compound *)obj);
            printf("(compound: ");
            for ( uint32_t i = 0; i < c->num_elems; ++i ) {
                printf("[%01d: ", i);
                val_print(c->elems[i]);
                printf("]");
            }
            printf(")\n");
        } break;

        case OBJ_PROC: {
            printf("(proc: ");
            obj_print(((Obj_Proc *)obj)->name);
            printf(")");
        } break;

        case OBJ_ITER: {
            printf("(iter: [");
            obj_print(((Obj_Iter *)obj)->range);
            printf("][it: ");
            val_print(((Obj_Iter *)obj)->iter);
            printf("]\n");
        } break;

        case OBJ_STRUCT: {
            printf("(struct: ");
            obj_print(((Obj_Struct *)obj)->name);
            printf(")");
        } break;

        case OBJ_ENUM: {
            printf("(enum: ");
            obj_print(((Obj_Enum *)obj)->name);
            printf(")");
        } break;

        case OBJ_STRUCT_FIELD: {
            printf("struct_field: ");
            obj_print(((Obj_Struct_Field *)obj)->name);
        } break;

        case OBJ_NAMESPACE: {
            printf("(namespace: ");
            obj_print(((Obj_Namespace *)obj)->name);
            printf(")");
        } break;

        default: {
            assert(!"unbekanntes objekt");
        } break;
    }
}

Value
val_none() {
    Value result = {};

    result.kind    = VAL_NONE;
    result.int_val = 0;

    return result;
}

Value
val_int(int64_t val, uint32_t size = 4) {
    Value result = {};

    result.kind    = VAL_INT;
    result.size    = size;
    result.int_val = val;

    return result;
}

Value
val_float(float val) {
    Value result = {};

    result.kind    = VAL_FLOAT;
    result.flt_val = val;

    return result;
}

Value
val_bool(bool val) {
    Value result = {};

    result.kind     = VAL_BOOL;
    result.bool_val = val;

    return result;
}

Value
val_obj(Obj *val) {
    Value result = {};

    result.kind    = VAL_OBJ;
    result.obj_val = val;

    return result;
}

Value
val_str(char *ptr, uint32_t size) {
    Value result = val_obj(obj_string(ptr, size));

    return result;
}

Value
val_compound(Values elems, uint32_t num_elems) {
    Value result = val_obj(obj_compound(elems, num_elems));

    return result;
}

Value
val_range(Value left, Value right) {
    Value result = val_obj(obj_range(left, right));

    return result;
}

Value
val_proc(char *ptr, uint32_t size, bool sys_call, char *lib) {
    Value result = val_obj(obj_proc(sys_call, lib));

    return result;
}

Value
val_iter(Obj_Range *range, Value iter) {
    Value result = val_obj(obj_iter(range, iter));

    return result;
}

Value
val_struct(char *name, uint32_t len) {
    Value result = val_obj(obj_struct(obj_string(name, len)));

    return result;
}

Value
val_enum(char *name, uint32_t len) {
    Value result = val_obj(obj_enum(obj_string(name, len)));

    return result;
}

Value
val_struct_field(char *name, uint32_t len) {
    Value result = val_obj(obj_struct_field(obj_string(name, len)));

    return result;
}

Value
val_namespace(Obj_Namespace *ns) {
    Value result = val_obj(ns);

    return result;
}

void
val_print(Value val) {
    switch ( val.kind ) {
        case VAL_INT: {
            printf("(int: %lld)", val.int_val);
        } break;

        case VAL_FLOAT: {
            printf("(float: %f)", val.flt_val);
        } break;

        case VAL_BOOL: {
            printf("(bool: %s)", (val.bool_val == true) ? "true" : "false");
        } break;

        case VAL_NONE: {
            printf("(none)");
        } break;

        case VAL_OBJ: {
            obj_print(val.obj_val);
        } break;

        default: {
            assert(!"unbekannter wert");
        } break;
    }
}

Value
operator-(Value val) {
    switch ( val.kind ) {
        case VAL_INT: {
            return val_int(-val.int_val);
        } break;

        case VAL_FLOAT: {
            return val_float(-val.flt_val);
        } break;

        default: {
            assert(!"operator- implementieren");
            return val_none();
        } break;
    }
}

Value
operator+(Value left, int32_t right) {
    switch ( left.kind ) {
        case VAL_INT: {
            return val_int(left.int_val + right);
        } break;

        case VAL_FLOAT: {
            return val_float(left.flt_val + right);
        } break;

        case VAL_OBJ: {
            assert(left.obj_val->kind == OBJ_ITER);
            ((Obj_Iter *)left.obj_val)->iter = ((Obj_Iter *)left.obj_val)->iter + 1;
            return left;
        } break;

        default: {
            assert(0);
        } break;
    }

    assert(0);
    return val_none();
}

Value
operator+(Value left, Value right) {
    switch ( left.kind ) {
        case VAL_INT: {
            if ( right.kind == VAL_INT ) {
                return val_int(left.int_val + right.int_val);
            } else {
                assert(right.kind == VAL_OBJ && right.obj_val->kind == OBJ_ITER);
                Value iter = ((Obj_Iter *)right.obj_val)->iter;

                return iter + (int32_t)left.int_val;
            }
        } break;

        case VAL_FLOAT: {
            assert(right.kind == VAL_FLOAT);
            return val_float(left.flt_val + right.flt_val);
        } break;

        default: {
            assert(0);
        } break;
    }

    assert(0);
    return val_none();
}

Value
operator-(Value left, Value right) {
    switch ( left.kind ) {
        case VAL_INT: {
            assert(right.kind == VAL_INT);
            return val_int(left.int_val - right.int_val);
        } break;

        case VAL_FLOAT: {
            assert(right.kind == VAL_FLOAT);
            return val_float(left.flt_val - right.flt_val);
        } break;

        default: {
            assert(0);
        } break;
    }

    assert(0);
    return val_none();
}

Value
operator*(Value left, Value right) {
    switch ( left.kind ) {
        case VAL_INT: {
            assert(right.kind == VAL_INT);
            return val_int(left.int_val * right.int_val);
        } break;

        case VAL_FLOAT: {
            assert(right.kind == VAL_FLOAT);
            return val_float(left.flt_val * right.flt_val);
        } break;

        default: {
            assert(0);
        } break;
    }

    assert(0);
    return val_none();
}

Value
operator/(Value left, Value right) {
    switch ( left.kind ) {
        case VAL_INT: {
            assert(right.kind == VAL_INT);
            return val_int(left.int_val / right.int_val);
        } break;

        case VAL_FLOAT: {
            assert(right.kind == VAL_FLOAT);
            return val_float(left.flt_val / right.flt_val);
        } break;

        default: {
            assert(0);
        } break;
    }

    assert(0);
    return val_none();
}

bool
operator<(Value left, Value right) {
    switch ( left.kind ) {
        case VAL_INT: {
            assert(right.kind == VAL_INT);
            return left.int_val < right.int_val;
        } break;

        case VAL_FLOAT: {
            assert(right.kind == VAL_FLOAT);
            return left.flt_val < right.flt_val;
        } break;

        default: {
            assert(0);
        } break;
    }

    assert(0);
    return false;
}

bool
operator>=(float left, Value right) {
    assert(right.kind == VAL_FLOAT);

    return left >= right.flt_val;
}

bool
operator>=(Value left, float right) {
    assert(left.kind == VAL_FLOAT);

    return left.flt_val >= right;
}

bool
operator>=(int64_t left, Value right) {
    assert(right.kind == VAL_INT);

    return left >= right.int_val;
}

bool
operator>=(Value left, int64_t right) {
    assert(left.kind == VAL_INT);

    return left.int_val >= right;
}

bool
operator<=(int64_t left, Value right) {
    assert(right.kind == VAL_INT);

    return left <= right.int_val;
}

bool
operator<=(Value left, int64_t right) {
    assert(left.kind == VAL_INT);

    return left.int_val <= right;
}

bool
operator<=(float left, Value right) {
    assert(right.kind == VAL_FLOAT);

    return left <= right.flt_val;
}

bool
operator<=(Value left, float right) {
    assert(left.kind == VAL_FLOAT);

    return left.flt_val <= right;
}

bool
operator>=(Value left, Value right) {
    switch ( left.kind ) {
        case VAL_INT: {
            assert(right.kind == VAL_INT);

            return left.int_val >= right.int_val;
        } break;

        default: {
            assert(0);
        } break;
    }

    assert(0);
    return false;
}

bool
operator<=(Value left, Value right) {
    switch ( left.kind ) {
        case VAL_INT: {
            assert(right.kind == VAL_INT);

            return left.int_val <= right.int_val;
        } break;

        default: {
            assert(0);
        } break;
    }

    assert(0);
    return false;
}

bool
operator!=(Value left, Value right) {
    switch ( left.kind ) {
        case VAL_INT: {
            switch ( right.kind ) {
                case VAL_INT: {
                    return left.int_val != right.int_val;
                } break;

                case VAL_FLOAT: {
                    return left.int_val != right.flt_val;
                } break;

                case VAL_OBJ: {
                    assert(right.obj_val->kind == OBJ_RANGE);

                    bool gt = !(left.int_val >= ((Obj_Range *)right.obj_val)->left);
                    bool lt = !(left.int_val <= ((Obj_Range *)right.obj_val)->right);
                    bool result = gt || lt;

                    return result;
                } break;
            }
        } break;

        case VAL_FLOAT: {
            switch ( right.kind ) {
                case VAL_INT: {
                    return left.flt_val != right.int_val;
                } break;

                case VAL_FLOAT: {
                    return left.flt_val != right.flt_val;
                } break;

                case VAL_OBJ: {
                    assert(right.obj_val->kind == OBJ_RANGE);

                    return left.flt_val >= ((Obj_Range *)right.obj_val)->left &&
                           left.flt_val <= ((Obj_Range *)right.obj_val)->right;
                } break;
            }
        } break;

        case VAL_OBJ: {
            assert(left.obj_val->kind == OBJ_RANGE);
            Obj_Range *range = (Obj_Range *)left.obj_val;

            switch ( right.kind ) {
                case VAL_INT: {
                    return range->left >= right.int_val && range->right <= right.int_val;
                } break;

                case VAL_FLOAT: {
                    return range->left >= right.flt_val &&
                           range->right <= right.flt_val;
                } break;

                case VAL_OBJ: {
                    assert(left.obj_val->kind == OBJ_RANGE);

                    Obj_Range *left_range = (Obj_Range *)left.obj_val;
                    return range->left != left_range->left && range->right != left_range->right;
                } break;
            }
        } break;

        default: {
            assert(0);
        } break;
    }

    assert(0);
    return false;
}

int32_t
bytecode_emit_jmp(Bytecode *bc) {
    bytecode_write8(bc, BYTECODEOP_JMP);
    int32_t result = bc->size;
    bytecode_write16(bc, 0); // vorübergehend eine platzhalter adresse reinschreiben

    return result;
}

int32_t
bytecode_emit_jmp_false(Bytecode *bc) {
    bytecode_write8(bc, BYTECODEOP_JMP_FALSE);
    int32_t result = bc->size;
    bytecode_write16(bc, 0); // vorübergehend eine platzhalter adresse reinschreiben

    return result;
}

void
bytecode_patch_jmp(Bytecode *bc, int32_t offset, int32_t addr) {
    bytecode_write16(bc, offset, (uint16_t)addr);
}

int32_t
bytecode_push_constant(Bytecode *bc, Value value) {
    int32_t result = value_push(&bc->constants, value);

    return result;
}

Value
bytecode_fetch_constant(Bytecode *bc, int32_t index) {
    Value result = value_get(&bc->constants, index);

    return result;
}

int32_t
bytecode_emit_const(Bytecode *bc, Value val) {
    int32_t index = bytecode_push_constant(bc, val);

    bytecode_write8(bc, BYTECODEOP_CONST);
    bytecode_write16(bc, (int16_t)index);

    return index;
}

void
bytecode_debug(Vm *vm, int32_t code) {
    Call_Frame *frame = vm_curr_frame(vm);

    switch ( code ) {
        case BYTECODEOP_CONST: {
            printf("push ");
            val_print(bytecode_fetch_constant(frame->proc->bc, frame->proc->bc->code[frame->pc]));
            printf("\n");
        } break;

        case BYTECODEOP_ADD: {
            printf("OP_ADD [left: ");
            val_print(vm->stack[frame->sp-2]);
            printf("] + [right: ");
            val_print(vm->stack[frame->sp-1]);
            printf("]\n");
        } break;

        case BYTECODEOP_SUB: {
            printf("OP_SUB [left: ");
            val_print(vm->stack[frame->sp-2]);
            printf("] - [right: ");
            val_print(vm->stack[frame->sp-1]);
            printf("]\n");
        } break;

        case BYTECODEOP_MUL: {
            printf("OP_MUL [left: ");
            val_print(vm->stack[frame->sp-2]);
            printf("] * [right: ");
            val_print(vm->stack[frame->sp-1]);
            printf("]\n");
        } break;

        case BYTECODEOP_DIV: {
            printf("OP_DIV [left: ");
            val_print(vm->stack[frame->sp-2]);
            printf("] / [right: ");
            val_print(vm->stack[frame->sp-1]);
            printf("]\n");
        } break;

        case BYTECODEOP_MATCH_CASE: {
            printf("OP_MATCH_CASE\n");
        } break;

        case BYTECODEOP_PUSH_SYSSYM: {
            printf("OP_PUSH_SYSSYM [index: %02d] (", frame->proc->bc->code[frame->pc]);
            val_print(bytecode_fetch_constant(frame->proc->bc, frame->proc->bc->code[frame->pc]));
            printf(")\n");
        } break;

        case BYTECODEOP_PUSH_SYM: {
            printf("OP_PUSH_SYM ");
            val_print(bytecode_fetch_constant(frame->proc->bc, frame->proc->bc->code[frame->pc]));
            printf("[val: ");

            Value type_val = vm->stack[frame->sp-1];
            Value expr_val = vm->stack[frame->sp-2];
            val_print(expr_val.kind == VAL_NONE ? type_val : expr_val);

            printf("]\n");
        } break;

        case BYTECODEOP_CAST: {
            printf("OP_CAST\n");
        } break;

        case BYTECODEOP_LOAD_STRUCT_FIELD: {
            printf("OP_LOAD_STRUCT_FIELD [name: ");
            val_print(value_get(&frame->proc->bc->constants, *(uint16_t *)(frame->proc->bc->code + frame->pc)));
            printf("]\n");
        } break;

        case BYTECODEOP_LOAD_SYM: {
            printf("OP_LOAD_SYM [index: %02d] (", frame->proc->bc->code[frame->pc]);
            val_print(bytecode_fetch_constant(frame->proc->bc, frame->proc->bc->code[frame->pc]));
            printf(")\n");
        } break;

        case BYTECODEOP_CMP_LT: {
            printf("OP_CMP_LT [left: ");
            val_print(vm->stack[frame->sp-2]);
            printf("] < [right: ");
            val_print(vm->stack[frame->sp-1]);
            printf("]\n");
        } break;

        case BYTECODEOP_JMP_FALSE: {
            printf("OP_JMP_FALSE [op: ");
            val_print(vm->stack[frame->sp-1]);
            printf("] -> [addr: %d]\n", *(uint16_t *)(frame->proc->bc->code + frame->pc));
        } break;

        case BYTECODEOP_RANGE: {
            printf("OP_RANGE [left: ");
            val_print(vm->stack[frame->sp-2]);
            printf("] .. [right: ");
            val_print(vm->stack[frame->sp-1]);
            printf("]\n");
        } break;

        case BYTECODEOP_COMPOUND: {
            printf("OP_COMPOUND\n");
        } break;

        case BYTECODEOP_INIT_LOOP: {
            printf("OP_INIT_LOOP [loop var: ");
            val_print(value_get(&frame->proc->bc->constants, *(uint16_t *)(frame->proc->bc->code + frame->pc)));
            printf("][val: ");
            Value val = vm->stack[frame->sp-1];
            assert(val.kind == VAL_OBJ && val.obj_val->kind == OBJ_RANGE);
            val_print(as_range(val)->left);
            printf("]\n");
        } break;

        case BYTECODEOP_CALL: {
            printf("OP_CALL (");
            val_print(vm->stack[frame->sp-1]);
            printf(")[num_args: %d]\n", *(uint16_t *)(frame->proc->bc->code + frame->pc));
        } break;

        case BYTECODEOP_RET: {
            printf("OP_RET ");

            uint32_t num_vals = *(uint16_t *)(frame->proc->bc->code + frame->pc);
            for ( uint32_t i = 0; i < num_vals; ++i ) {
                val_print(vm->stack[frame->sp-(1+i)]);
            }

            printf("\n");
        } break;

        case BYTECODEOP_LOAD_SYMREF: {
            Value val = value_get(&frame->proc->bc->constants, *(uint16_t *)(frame->proc->bc->code + frame->pc));
            printf("OP_LOAD_SYMREF ");
            val_print(val);
            printf("\n");
        } break;

        case BYTECODEOP_SET_SYM: {
            printf("OP_SET_SYM ");
            val_print(vm->stack[frame->sp-1]);
            val_print(vm->stack[frame->sp-2]);
            printf("\n");
        } break;

        case BYTECODEOP_INC_LOOP: {
            Value val = value_get(&frame->proc->bc->constants, *(uint16_t *)(frame->proc->bc->code + frame->pc));

            printf("OP_INC_LOOP ");
            val_print(val);
            printf("\n");
        } break;

        case BYTECODEOP_JMP: {
            printf("jmp %d\n", *(uint16_t *)(frame->proc->bc->code + frame->pc));
        } break;

        case BYTECODEOP_PUSH: {
            Value val = value_get(&frame->proc->bc->constants, *(uint16_t *)(frame->proc->bc->code + frame->pc));

            printf("push ");
            val_print(val);
            printf("\n");
        } break;

        case BYTECODEOP_POP: {
            printf("pop\n");
        } break;

        case BYTECODEOP_SCOPE_ENTER: {
            printf("scope enter\n");
        } break;

        case BYTECODEOP_SCOPE_LEAVE: {
            printf("scope leave\n");
        } break;

        case BYTECODEOP_STRUCT_FIELD: {
            Value val   = vm->stack[frame->sp-1];
            Value field = vm->stack[frame->sp-2];

            printf("OP_STRUCT_FIELD [name: ");
            val_print(field);
            printf("[value: ");
            val_print(val);
            printf("]\n");
        } break;

        case BYTECODEOP_HLT: {
            printf("halt\n");
        } break;

        case BYTECODEOP_NAMESPACE_ENTER: {
            Value name = vm->stack[frame->sp-1];
            printf("namespace enter");
            val_print(name);
            printf("\n");
        } break;

        case BYTECODEOP_NAMESPACE_LEAVE: {
            printf("namespace leave\n");
        } break;

        case BYTECODEOP_STRUCT: {
            printf("struct\n");
        } break;

        case BYTECODEOP_ENUM: {
            printf("enum\n");
        } break;

        case BYTECODEOP_NONE: {
            printf("push val_none\n");
        } break;

        case BYTECODEOP_PUSH_TYPEVAL: {
            printf("OP_PUSH_TYPEVAL [");
            val_print(value_get(&frame->proc->bc->constants, *(uint16_t *)(frame->proc->bc->code + frame->pc)));
            printf("]\n");
        } break;

        case BYTECODEOP_EXPORT_SYM: {
            printf("export sym\n");
        } break;

        case BYTECODEOP_NEG: {
            printf("neg ");
            val_print(vm->stack[frame->sp-1]);
            printf("\n");
        } break;

        case BYTECODEOP_IMPORT_SYMS: {
            printf("using ");
            val_print(vm->stack[frame->sp-1]);
            printf("\n");
        } break;

        default: {
            assert(!"unbekannter bytecode");
        } break;
    }
}

void
bytecode_op(Bytecode *bc, uint32_t unary_op) {
    switch ( unary_op ) {
        case OP_SUB: {
            bytecode_write8(bc, BYTECODEOP_NEG);
        } break;

        default: {
            assert(!"unary operator");
        } break;
    }
}

void
bytecode_expr(Bytecode *bc, Expr *expr, uint32_t flags = BYTECODEFLAG_NONE) {
    switch ( expr->kind ) {
        case EXPR_INT: {
            bytecode_emit_const(bc, val_int(AS_INT(expr)->val, AS_INT(expr)->type->size));
        } break;

        case EXPR_FLOAT: {
            bytecode_emit_const(bc, val_float(AS_FLOAT(expr)->val));
        } break;

        case EXPR_STR: {
            bytecode_emit_const(bc, val_str(AS_STR(expr)->val, AS_STR(expr)->len));
        } break;

        case EXPR_BOOL: {
            bytecode_emit_const(bc, val_bool(AS_BOOL(expr)->val));
        } break;

        case EXPR_COMPOUND: {
            if ( AS_COMPOUND(expr)->elems[0]->name == NULL ) {
                /* @AUFGABE: alle elemente des ausdrucks durchgehen und auf den stack holen */
                for ( int i = 0; i < AS_COMPOUND(expr)->num_elems; ++i ) {
                    bytecode_expr(bc, AS_COMPOUND(expr)->elems[i]->value);
                }

                bytecode_write8(bc, BYTECODEOP_COMPOUND);
                bytecode_write16(bc, (uint16_t)AS_COMPOUND(expr)->num_elems);
            } else {
                assert(!"benamtes compound implementieren");
                bytecode_write8(bc, BYTECODEOP_NAMED_COMPOUND);
            }
        } break;

        case EXPR_FIELD: {
            bytecode_expr(bc, AS_FIELD(expr)->base);
            int32_t index = bytecode_push_constant(bc, val_str(AS_FIELD(expr)->field, AS_FIELD(expr)->len));
            bytecode_write8(bc, BYTECODEOP_LOAD_STRUCT_FIELD);
            bytecode_write16(bc, (int16_t)index);
        } break;

        case EXPR_RANGE: {
            bytecode_expr(bc, AS_RANGE(expr)->left);
            bytecode_expr(bc, AS_RANGE(expr)->right);

            bytecode_write8(bc, BYTECODEOP_RANGE);
        } break;

        case EXPR_PAREN: {
            bytecode_expr(bc, AS_PAREN(expr)->expr);
        } break;

        case EXPR_IDENT: {
            if ( flags & BYTECODEFLAG_ASSIGNABLE ) {
                bytecode_write8(bc, BYTECODEOP_LOAD_SYMREF);
            } else {
                bytecode_write8(bc, BYTECODEOP_LOAD_SYM);
            }

            int32_t index = bytecode_push_constant(bc, val_str(AS_IDENT(expr)->val, AS_IDENT(expr)->len));
            bytecode_write16(bc, (uint16_t)index);
        } break;

        case EXPR_CAST: {
            bytecode_typespec(bc, AS_CAST(expr)->typespec);
            bytecode_expr(bc, AS_CAST(expr)->expr);
            bytecode_write8(bc, BYTECODEOP_CAST);
        } break;

        case EXPR_CALL: {
            for ( int i = 0; i < AS_CALL(expr)->num_args; ++i ) {
                bytecode_expr(bc, AS_CALL(expr)->args[i]);
                bytecode_write8(bc, BYTECODEOP_NONE);
            }

            bytecode_expr(bc, AS_CALL(expr)->base);
            bytecode_write8(bc, BYTECODEOP_CALL);
            bytecode_write16(bc, (uint16_t)AS_CALL(expr)->num_args);
        } break;

        case EXPR_UNARY: {
            bytecode_expr(bc, AS_UNARY(expr)->expr);
            bytecode_op(bc, AS_UNARY(expr)->op);
        } break;

        case EXPR_BIN: {
            bytecode_expr(bc, AS_BIN(expr)->left);
            bytecode_expr(bc, AS_BIN(expr)->right);

            if ( AS_BIN(expr)->op == OP_ADD ) {
                bytecode_write8(bc, BYTECODEOP_ADD);
            } else if ( AS_BIN(expr)->op == OP_SUB ) {
                bytecode_write8(bc, BYTECODEOP_SUB);
            } else if ( AS_BIN(expr)->op == OP_MUL ) {
                bytecode_write8(bc, BYTECODEOP_MUL);
            } else if ( AS_BIN(expr)->op == OP_DIV ) {
                bytecode_write8(bc, BYTECODEOP_DIV);
            } else if ( AS_BIN(expr)->op == OP_LT ) {
                bytecode_write8(bc, BYTECODEOP_CMP_LT);
            } else {
                assert(!"unbekannter operator");
            }
        } break;

        default: {
            assert(!"unbekannter ausdruck");
        } break;
    }
}

void
bytecode_typespec(Bytecode *bc, Typespec *typespec) {
    assert(typespec);

    switch ( typespec->kind ) {
        case TYPESPEC_NAME: {
            int32_t index = bytecode_push_constant(bc, val_str(AS_NAME(typespec)->name, AS_NAME(typespec)->len));
            bytecode_write8(bc, BYTECODEOP_PUSH_TYPEVAL);
            bytecode_write16(bc, (int16_t)index);
        } break;

        default: {
            assert(!"unbekannter typespec");
        } break;
    }
}

void
bytecode_global_decl(Bytecode *bc, Stmt *stmt) {
    Decl *decl = AS_DECL(stmt)->decl;
}

void
bytecode_decl(Bytecode *bc, Decl *decl) {
    switch ( decl->kind ) {
        case DECL_VAR: {
            if ( AS_VAR(decl)->expr ) {
                bytecode_expr(bc, AS_VAR(decl)->expr);
            } else {
                bytecode_write8(bc, BYTECODEOP_NONE);
            }

            int32_t index = bytecode_push_constant(bc, val_str(decl->name, decl->len));

            if ( AS_VAR(decl)->typespec ) {
                bytecode_typespec(bc, AS_VAR(decl)->typespec);
            } else {
                bytecode_write8(bc, BYTECODEOP_NONE);
            }

            bytecode_write8(bc, BYTECODEOP_PUSH_SYM);
            bytecode_write16(bc, (uint16_t)index);
        } break;

        case DECL_CONST: {
            if ( AS_CONST(decl)->expr ) {
                bytecode_expr(bc, AS_CONST(decl)->expr);
            } else {
                bytecode_write8(bc, BYTECODEOP_NONE);
            }

            int32_t index = bytecode_push_constant(bc, val_str(decl->name, decl->len));

            if ( AS_CONST(decl)->typespec ) {
                bytecode_typespec(bc, AS_CONST(decl)->typespec);
            } else {
                bytecode_write8(bc, BYTECODEOP_NONE);
            }

            bytecode_write8(bc, BYTECODEOP_PUSH_SYM);
            bytecode_write16(bc, (uint16_t)index);
        } break;

        case DECL_PROC: {
            Value val = val_proc(decl->name, decl->len, AS_PROC(decl)->sign->sys_call, AS_PROC(decl)->sign->sys_lib);

            int32_t index = bytecode_push_constant(bc, val);
            bytecode_write8(bc, BYTECODEOP_CONST);
            bytecode_write16(bc, (uint16_t)index);

            index = bytecode_push_constant(bc, val_str(decl->name, decl->len));
            bytecode_write8(bc, BYTECODEOP_NONE);

            bytecode_write8(bc, BYTECODEOP_PUSH_SYM);
            bytecode_write16(bc, (uint16_t)index);

            Obj_Proc *proc = as_proc(val);
            proc->name = obj_string(decl->name, decl->len);
            proc->sign = AS_PROC(decl)->sign;
            proc->scope->name = decl->name;
            proc->num_params = (uint32_t)AS_PROC(decl)->sign->num_params;

            for ( uint32_t i = 0; i < AS_PROC(decl)->sign->num_params; ++i ) {
                Proc_Param *param = AS_PROC(decl)->sign->params[i];

                index = bytecode_push_constant(proc->bc, val_str(param->name, param->len));

                bytecode_write8(proc->bc, BYTECODEOP_PUSH_SYM);
                bytecode_write16(proc->bc, (uint16_t)index);
            }

            if ( !AS_PROC(decl)->sign->sys_call ) {
                bytecode_stmt(proc->bc, AS_PROC(decl)->block);

                if ( !AS_PROC(decl)->sign->num_rets ) {
                    bytecode_write8(proc->bc, BYTECODEOP_RET);
                    bytecode_write16(proc->bc, 0);
                }
            }
        } break;

        case DECL_ENUM: {
            Value val = val_enum(decl->name, decl->len);
            int32_t index = bytecode_emit_const(bc, val);
            bytecode_write8(bc, BYTECODEOP_NONE);

            int32_t name_index = bytecode_push_constant(bc, val_str(decl->name, decl->len));
            bytecode_write8(bc, BYTECODEOP_PUSH_SYM);
            bytecode_write16(bc, (uint16_t)name_index);

            bytecode_write8(bc, BYTECODEOP_PUSH);
            bytecode_write16(bc, (uint16_t)index);

            for ( int i = 0; i < AS_ENUM(decl)->num_fields; ++i ) {
                Enum_Field *field = AS_ENUM(decl)->fields[i];

                // feldnamen generieren {
                    Value field_val = val_struct_field(field->name, (uint32_t)utf8_str_size(field->name));
                    int32_t field_index = bytecode_push_constant(bc, field_val);
                    bytecode_write8(bc, BYTECODEOP_PUSH);
                    bytecode_write16(bc, (uint16_t)field_index);
                // }

                int32_t type_index = bytecode_push_constant(bc, val_str("s32", 3));
                bytecode_write8(bc, BYTECODEOP_PUSH_TYPEVAL);
                bytecode_write16(bc, (int16_t)type_index);

                // feldwert generieren {
                    if ( field->value ) {
                        bytecode_expr(bc, field->value);
                    } else {
                        bytecode_write8(bc, BYTECODEOP_NONE);
                    }
                // }

                bytecode_write8(bc, BYTECODEOP_STRUCT_FIELD);
            }

            bytecode_write8(bc, BYTECODEOP_ENUM);
            bytecode_write16(bc, (uint16_t)AS_ENUM(decl)->num_fields);
        } break;

        case DECL_STRUCT: {
            Value val = val_struct(decl->name, decl->len);
            int32_t index = bytecode_emit_const(bc, val);
            bytecode_write8(bc, BYTECODEOP_NONE);

            int32_t name_index = bytecode_push_constant(bc, val_str(decl->name, decl->len));
            bytecode_write8(bc, BYTECODEOP_PUSH_SYM);
            bytecode_write16(bc, (uint16_t)name_index);

            bytecode_write8(bc, BYTECODEOP_PUSH);
            bytecode_write16(bc, (uint16_t)index);

            for ( int i = 0; i < AS_STRUCT(decl)->num_fields; ++i ) {
                Struct_Field *field = AS_STRUCT(decl)->fields[i];

                // feldnamen generieren {
                    Value field_val = val_struct_field(field->name, field->len);
                    int32_t field_index = bytecode_push_constant(bc, field_val);
                    bytecode_write8(bc, BYTECODEOP_PUSH);
                    bytecode_write16(bc, (uint16_t)field_index);
                // }

                if ( field->typespec ) {
                    bytecode_typespec(bc, field->typespec);
                } else {
                    bytecode_write8(bc, BYTECODEOP_NONE);
                }

                // feldwert generieren {
                    if ( field->default_value ) {
                        bytecode_expr(bc, field->default_value);
                    } else {
                        bytecode_write8(bc, BYTECODEOP_NONE);
                    }
                // }

                bytecode_write8(bc, BYTECODEOP_STRUCT_FIELD);
            }

            bytecode_write8(bc, BYTECODEOP_STRUCT);
            bytecode_write16(bc, (uint16_t)AS_STRUCT(decl)->num_fields);
        } break;

        default: {
            assert(0);
        } break;
    }
}

void
bytecode_stmt(Bytecode *bc, Stmt *stmt) {
    switch ( stmt->kind ) {
        case STMT_ASSIGN: {
            bytecode_expr(bc, AS_ASSIGN(stmt)->rhs);

            if ( AS_ASSIGN(stmt)->op->kind == T_PLUS_ASSIGN ) {
                bytecode_expr(bc, AS_ASSIGN(stmt)->lhs);
                bytecode_write8(bc, BYTECODEOP_ADD);
            }

            bytecode_expr(bc, AS_ASSIGN(stmt)->lhs, BYTECODEFLAG_ASSIGNABLE);

            bytecode_write8(bc, BYTECODEOP_SET_SYM);
        } break;

        case STMT_BLOCK: {
            for ( int i = 0; i < AS_BLOCK(stmt)->num_stmts; ++i ) {
                bytecode_stmt(bc, AS_BLOCK(stmt)->stmts[i]);
            }
        } break;

        case STMT_FOR: {
            assert(AS_FOR(stmt)->cond->kind == EXPR_RANGE);

            /* @INFO: platziert einen wert auf dem stack */
            bytecode_expr(bc, AS_FOR(stmt)->cond);

            char *it = NULL;
            uint32_t it_len = 0;
            if ( AS_FOR(stmt)->it ) {
                it = AS_IDENT(AS_FOR(stmt)->it)->val;
                it_len = AS_IDENT(AS_FOR(stmt)->it)->len;
            } else {
                it = "it";
                it_len = 2;
            }

            /* @INFO: platziert den namen der laufvariable */
            int32_t index = bytecode_push_constant(bc, val_str(it, it_len));

            bytecode_write8(bc, BYTECODEOP_SCOPE_ENTER);

            /* @INFO: anweisung holt sich den namen der laufvariable und den wert */
            bytecode_write8(bc, BYTECODEOP_INIT_LOOP);
            bytecode_write16(bc, (uint16_t)index);

            uint32_t loop_addr = bc->size;
            bytecode_write8(bc, BYTECODEOP_JMP_FALSE);
            int32_t exit_instr = bc->size;
            bytecode_write16(bc, 0);

            /* @INFO: den eigentlichen code der schleife generieren */
            bytecode_stmt(bc, AS_FOR(stmt)->block);

            /* @INFO: die schleifenvariable inkrementieren und den boolischen wert für
             *        die spätere JMP_FALSE operation auf den stack laden
             */
            bytecode_write8(bc, BYTECODEOP_INC_LOOP);
            bytecode_write16(bc, (uint16_t)index);

            /* @INFO: bedingungsloser sprung zu der JMP_FALSE operation */
            bytecode_write8(bc, BYTECODEOP_JMP);
            bytecode_write16(bc, (uint16_t)loop_addr);
            int32_t exit_addr = bc->size;

            bytecode_write8(bc, BYTECODEOP_SCOPE_LEAVE);
            bytecode_write16(bc, exit_instr, (uint16_t)exit_addr);
        } break;

        case STMT_MATCH: {
            int32_t *exit_addrs = NULL;
            for ( int i = 0; i < AS_MATCH(stmt)->num_lines; ++i ) {
                Match_Line *line = AS_MATCH(stmt)->lines[i];

                /* @INFO: den anfangswert auf den stack holen */
                bytecode_expr(bc, AS_MATCH(stmt)->expr);

                /* @INFO: den case wert auf den stack holen */
                bytecode_expr(bc, line->resolution);

                /* @INFO: die beiden werte vergleichen */
                bytecode_write8(bc, BYTECODEOP_MATCH_CASE);
                /* @INFO: platzhalter für die sprungadresse schreiben */
                int32_t addr = bc->size;
                bytecode_write16(bc, 0);

                /* @INFO: die case anweisungen kodieren */
                bytecode_stmt(bc, line->stmt);

                /* @INFO: zum schluß ans ende der gesamten match anweisung springen */
                bytecode_write8(bc, BYTECODEOP_JMP);
                buf_push(exit_addrs, bc->size);
                bytecode_write16(bc, 0);

                /* @INFO: die adresse für MATCH_CASE patchen für den fall, daß die anweisung
                 *        nicht zutrifft und zum nächsten case gesprungen werden muß
                 */
                bytecode_patch_jmp(bc, addr, bc->size);
            }

            for ( int i = 0; i < buf_len(exit_addrs); ++i ) {
                bytecode_patch_jmp(bc, exit_addrs[i], bc->size);
            }
        } break;

        case STMT_WHILE: {
            bytecode_write8(bc, BYTECODEOP_SCOPE_ENTER);

            /* @INFO: platziert den entscheidungswert auf dem stack */
            uint32_t loop_addr = bc->size;
            bytecode_expr(bc, AS_WHILE(stmt)->cond);

            bytecode_write8(bc, BYTECODEOP_JMP_FALSE);
            int32_t exit_instr = bc->size;
            bytecode_write16(bc, 0);

            /* @INFO: den eigentlichen code der schleife generieren */
            bytecode_stmt(bc, AS_WHILE(stmt)->block);

            /* @INFO: bedingungsloser sprung zu der JMP_FALSE operation */
            bytecode_write8(bc, BYTECODEOP_JMP);
            bytecode_write16(bc, (uint16_t)loop_addr);
            int32_t exit_addr = bc->size;

            bytecode_write8(bc, BYTECODEOP_SCOPE_LEAVE);
            bytecode_write16(bc, exit_instr, (uint16_t)exit_addr);
        } break;

        case STMT_DECL: {
            bytecode_decl(bc, AS_DECL(stmt)->decl);
        } break;

        case STMT_IF: {
            bytecode_expr(bc, AS_IF(stmt)->cond);

            int32_t addr = bytecode_emit_jmp_false(bc);
            bytecode_write8(bc, BYTECODEOP_SCOPE_ENTER);
            bytecode_stmt(bc, AS_IF(stmt)->stmt);
            bytecode_write8(bc, BYTECODEOP_SCOPE_LEAVE);
            bytecode_patch_jmp(bc, addr, (uint16_t)bc->size);

            if ( AS_IF(stmt)->stmt_else ) {
                addr = bytecode_emit_jmp(bc);
                bytecode_stmt(bc, AS_IF(stmt)->stmt_else);
                bytecode_patch_jmp(bc, addr, (uint16_t)bc->size);
            }
        } break;

        case STMT_RET: {
            for ( uint32_t i = 0; i < AS_RET(stmt)->num_exprs; ++i ) {
                bytecode_expr(bc, AS_RET(stmt)->exprs[i]);
            }

            bytecode_write8(bc, BYTECODEOP_RET);
            bytecode_write16(bc, (int16_t)AS_RET(stmt)->num_exprs);
        } break;

        case STMT_EXPR: {
            bytecode_expr(bc, AS_EXPR(stmt)->expr);
        } break;

        case STMT_USING: {
            bytecode_expr(bc, AS_USING(stmt)->expr);
            bytecode_write8(bc, BYTECODEOP_IMPORT_SYMS);
        } break;

        default: {
            assert(!"unbekannte anweisung");
        } break;
    }
}

char *
to_str(Obj_String *str) {
    return str->ptr;
}

Obj_String *
as_str(Value val) {
    Obj_String *result = (Obj_String *)val.obj_val;

    return result;
}

Obj_Range *
as_range(Value val) {
    Obj_Range *result = (Obj_Range *)val.obj_val;

    return result;
}

Obj_Proc *
as_proc(Value val) {
    Obj_Proc *result = (Obj_Proc *)val.obj_val;

    return result;
}

Obj_Iter *
as_iter(Value val) {
    Obj_Iter *result = (Obj_Iter *)val.obj_val;

    return result;
}

Obj_Struct *
as_struct(Value val) {
    Obj_Struct *result = (Obj_Struct *)val.obj_val;

    return result;
}

Obj *
as_obj(Value val) {
    assert(val.kind == VAL_OBJ);

    Obj *result = val.obj_val;

    return result;
}

int32_t
as_int(Value val) {
    int32_t result = (int32_t)val.int_val;

    return result;
}

bool
is_proc(Value val) {
    bool result = val.kind == VAL_OBJ && as_obj(val)->kind == OBJ_PROC;

    return result;
}

void
call_sys_proc(Vm *vm, Value val, uint32_t num_args) {
    DCCallVM *call_vm = dcNewCallVM(1024);
    dcMode(call_vm, DC_CALL_C_DEFAULT);

    auto lib = LoadLibrary(as_proc(val)->lib);
    assert(lib);
    auto proc_ptr = GetProcAddress(lib, to_str(as_proc(val)->name));
    assert(proc_ptr);

    for ( uint32_t i = 0; i < num_args; ++i ) {
        Value type_val = stack_pop(vm);
        Value expr_val = stack_pop(vm);

        Value arg = (expr_val.kind == VAL_NONE) ? type_val : expr_val;

        switch ( arg.kind ) {
            case VAL_INT: {
                dcArgLong(call_vm, (DClong)arg.int_val);
            } break;

            case VAL_FLOAT: {
                dcArgFloat(call_vm, arg.flt_val);
            } break;

            case VAL_BOOL: {
                dcArgBool(call_vm, arg.bool_val);
            } break;

            case VAL_OBJ: {
                switch ( arg.obj_val->kind ) {
                    case OBJ_STRING: {
                        dcArgPointer(call_vm, ((Obj_String *)arg.obj_val)->ptr);
                    } break;

                    case OBJ_PTR: {
                        dcArgPointer(call_vm, ((Obj_Ptr *)arg.obj_val)->ptr);
                    } break;

                    default: {
                        assert(!"unbekanntes objekt");
                    } break;
                }
            } break;

            default: {
                assert(!"unbekannter wert");
            } break;
        }
    }

    Obj_Proc *proc = as_proc(val);

    if ( proc->sign->num_rets ) {
        Type *type = proc->sign->rets[0]->type;

        switch ( type->kind ) {
            case TYPE_BOOL: {
                auto result = dcCallBool(call_vm , proc_ptr);
            } break;

            case TYPE_U8:
            case TYPE_S8: {
                auto result = dcCallChar(call_vm , proc_ptr);
            } break;

            case TYPE_U16:
            case TYPE_S16: {
                auto result = dcCallShort(call_vm , proc_ptr);
            } break;

            case TYPE_U32:
            case TYPE_S32: {
                auto result = dcCallInt(call_vm , proc_ptr);
            } break;

            case TYPE_U64:
            case TYPE_S64: {
                auto result = dcCallLong(call_vm , proc_ptr);
            } break;

            case TYPE_F32:
            case TYPE_F64: {
                auto result = dcCallFloat(call_vm , proc_ptr);
            } break;

            case TYPE_PTR: {
                auto *result = dcCallPointer(call_vm , proc_ptr);
                stack_push(vm, val_ptr(result));
            } break;

            default: {
                assert(!"");
            } break;
        }
    } else {
        dcCallVoid(call_vm , proc_ptr);
    }
}

void
call_proc(Vm *vm, Value val, uint32_t num_args) {
    assert(vm->frame_num < MAX_FRAME_NUM);

    Call_Frame *prev_frame = vm_curr_frame(vm);
    Call_Frame *frame = vm->frames + vm->frame_num++;

    assert(is_proc(val));
    Obj_Proc *proc = as_proc(val);

    frame->proc = proc;
    frame->pc   = 0;
    frame->sp   = prev_frame->sp;
    frame->prev_scope = bc_curr_scope;

    bc_curr_scope = proc->scope;
}

void
namespace_enter(Vm *vm, Obj_Namespace *ns) {
    assert(vm->frame_num < MAX_FRAME_NUM);

    Call_Frame *prev_frame = vm_curr_frame(vm);
    Call_Frame *frame = vm->frames + vm->frame_num++;

    Obj_Proc *proc = ns->proc;

    frame->proc = proc;
    frame->pc   = 0;
    frame->sp   = prev_frame->sp;

    ns->prev_scope = bc_curr_scope;
    bc_curr_scope = ns->scope;
}

void
namespace_leave(Vm *vm, Obj_Namespace *ns) {
    assert(vm->frame_num > 0);

    vm->frame_num--;

    bc_curr_scope = ns->prev_scope;
    Bytecode_Scope *prev_scope = bc_curr_scope;

    if ( ns->scope_name ) {
        Obj_Namespace *new_ns = obj_namespace(ns->scope_name, ns->scope_name);
        bytecode_sym_set(new_ns->name, val_namespace(new_ns));
        new_ns->scope->parent = NULL;
        bc_curr_scope = new_ns->scope;
    }

    for ( int i = 0; i < buf_len(ns->scope->export_syms.ordered_entries); ++i ) {
        Table_Entry *entry = ns->scope->export_syms.ordered_entries[i];

        bytecode_sym_set(entry->key, entry->val);
    }

    if ( ns->scope_name ) {
        bc_curr_scope = prev_scope;
    }
}

bool
bytecode_sym_get(Obj_String *key, Value *val) {
    Bytecode_Scope *it = bc_curr_scope;

    while ( it ) {
        if ( table_get(&it->syms, key, val) ) {
            return true;
        }

        it = it->parent;
    }

    return false;
}

bool
bytecode_sym_set(Obj_String *key, Value val, Bytecode_Scope *scope, uint32_t flags = BYTECODEFLAG_NONE) {
    if ( flags & BYTECODEFLAG_SET_EXISTING ) {
        Bytecode_Scope *it = scope;

        while ( it ) {
            Value v;
            if ( table_get(&it->syms, key, &v) ) {
                table_set(&it->syms, key, val);

                return true;
            }

            it = it->parent;
        }
    } else {
        return table_set(&scope->syms, key, val);
    }

    return false;
}

bool
bytecode_sym_set(Obj_String *key, Value val, uint32_t flags) {
    bool result = bytecode_sym_set(key, val, bc_curr_scope, flags);

    return result;
}

void
bytecode_directive(Bytecode *bc, Directive *dir) {
    switch ( dir->kind ) {
        case DIRECTIVE_IMPORT: {
            Obj_Namespace *ns = obj_namespace(dir->file, AS_IMPORT(dir)->scope_name);
            Value ns_val = val_namespace(ns);

            bytecode_build_file(ns->proc->bc, AS_IMPORT(dir)->parsed_file);

            bytecode_emit_const(ns->proc->bc, ns_val);
            bytecode_write8(ns->proc->bc, BYTECODEOP_NAMESPACE_LEAVE);

            bytecode_emit_const(bc, ns_val);
            bytecode_write8(bc, BYTECODEOP_NAMESPACE_ENTER);
        } break;

        case DIRECTIVE_EXPORT: {
            for ( int i = 0; i < AS_EXPORT(dir)->num_syms; ++i ) {
                Module_Sym *sym = AS_EXPORT(dir)->syms[i];

                bytecode_emit_const(bc, val_str(sym->name, (uint32_t)utf8_str_size(sym->name)));

                if ( sym->alias ) {
                    bytecode_emit_const(bc, val_str(sym->alias, (uint32_t)utf8_str_size(sym->alias)));
                } else {
                    bytecode_write8(bc, BYTECODEOP_NONE);
                }

                bytecode_write8(bc, BYTECODEOP_EXPORT_SYM);
            }
        } break;

        default: {
            assert(0);
        } break;
    }
}

void
bytecode_build_import_directives(Bytecode *bc, Directives dirs) {
    for ( int i = 0; i < buf_len(dirs); ++i ) {
        Directive *dir = dirs[i];

        if ( dir->kind == DIRECTIVE_IMPORT ) {
            bytecode_directive(bc, dir);
        }
    }
}

void
bytecode_build_export_directives(Bytecode *bc, Directives dirs) {
    for ( int i = 0; i < buf_len(dirs); ++i ) {
        Directive *dir = dirs[i];

        if ( dir->kind == DIRECTIVE_EXPORT ) {
            bytecode_directive(bc, dir);
        }
    }
}

void
bytecode_build_stmts(Bytecode *bc, Stmts stmts) {
    for ( int i = 0; i < buf_len(stmts); ++i ) {
        bytecode_stmt(bc, stmts[i]);
    }
}

void
bytecode_register_globals(Bytecode *bc, Parsed_File *file) {
    /* global registrieren PUSH_SYM_GLOBAL */
#if 0
    for ( int i = 0; i < buf_len(file->stmts); ++i ) {
        Stmt *stmt = file->stmts[i];

        if ( stmt->kind != STMT_DECL ) {
            continue;
        }

        bytecode_global_decl(bc, stmt);
    }
    assert(!"implementieren");
#endif
}

void
bytecode_build_file(Bytecode *bc, Parsed_File *file) {
    bytecode_register_globals(bc, file);
    bytecode_build_import_directives(bc, file->directives);
    bytecode_build_stmts(bc, file->stmts);
    bytecode_build_export_directives(bc, file->directives);
}

Bytecode *
build(Parsed_File *file) {
    Bytecode *bc = bytecode_new();

    bytecode_init(bc);
    bytecode_build_file(bc, file);

    return bc;
}

bool
step(Vm *vm) {
    Call_Frame *frame = vm_curr_frame(vm);

    if ( frame->pc >= frame->proc->bc->size ) {
        return false;
    }

    uint8_t instr = bytecode_read8(vm);
#if BYTECODE_DEBUG
    bytecode_debug(vm, instr);
#endif

    switch ( instr ) {
        case BYTECODEOP_NONE: {
            stack_push(vm, val_none());
        } break;

        case BYTECODEOP_ADD: {
            Value right = stack_pop(vm);
            Value left  = stack_pop(vm);

            stack_push(vm, left + right);
        } break;

        case BYTECODEOP_SUB: {
            Value right = stack_pop(vm);
            Value left  = stack_pop(vm);

            stack_push(vm, left - right);
        } break;

        case BYTECODEOP_MUL: {
            Value right = stack_pop(vm);
            Value left  = stack_pop(vm);

            stack_push(vm, left * right);
        } break;

        case BYTECODEOP_DIV: {
            Value right = stack_pop(vm);
            Value left  = stack_pop(vm);

            stack_push(vm, left / right);
        } break;

        case BYTECODEOP_CONST: {
            int32_t index = bytecode_read16(vm);
            Value val = value_get(&frame->proc->bc->constants, index);

            stack_push(vm, val);
        } break;

        case BYTECODEOP_PUSH_SYSSYM: {
            Bytecode_Scope *prev_scope = bc_curr_scope;
            bc_curr_scope = &bc_sys_scope;

            Obj_String *name = as_str(value_get(&frame->proc->bc->constants, bytecode_read16(vm)));
            Value val = stack_pop(vm);

            if ( !bytecode_sym_set(name, val) ) {
                assert(!"symbol konnte nicht gesetzt werden");
            }

            bc_curr_scope = prev_scope;
        } break;

        case BYTECODEOP_EXPORT_SYM: {
            Value alias = stack_pop(vm);
            Value name  = stack_pop(vm);

            Value val;
            if ( !bytecode_sym_get(as_str(name), &val) ) {
                assert(!"symbol nicht gefunden");
            }

            Value new_name = alias.kind == VAL_NONE ? name : alias;
            table_set(&bc_curr_scope->export_syms, as_str(new_name), val);
        } break;

        case BYTECODEOP_NAMESPACE_ENTER: {
            Value ns = stack_pop(vm);

            namespace_enter(vm, AS_NAMESPACE(ns));
        } break;

        case BYTECODEOP_NAMESPACE_LEAVE: {
            Value ns = stack_pop(vm);

            namespace_leave(vm, AS_NAMESPACE(ns));
        } break;

        case BYTECODEOP_PUSH_SYM: {
            Obj_String *name = as_str(value_get(&frame->proc->bc->constants, bytecode_read16(vm)));

            Value type_val = stack_pop(vm);
            Value expr_val = stack_pop(vm);

            Value val = (expr_val.kind == VAL_NONE) ? type_val : expr_val;

            if ( !bytecode_sym_set(name, val) ) {
                assert(!"symbol konnte nicht gesetzt werden");
            }

            if ( is_proc(val) ) {
                as_proc(val)->scope->parent = bc_curr_scope;
            }
        } break;

        case BYTECODEOP_INIT_LOOP: {
            Obj_String *name = as_str(value_get(&frame->proc->bc->constants, bytecode_read16(vm)));
            Value val = stack_pop(vm);

            assert(val.kind == VAL_OBJ);
            assert(val.obj_val->kind == OBJ_RANGE);

            bytecode_sym_set(name, val_iter(as_range(val), as_range(val)->left));
            stack_push(vm, val_bool(as_range(val)->left < as_range(val)->right));
        } break;

        case BYTECODEOP_INC_LOOP: {
            int32_t index = bytecode_read16(vm);
            Obj_String *name = as_str(value_get(&frame->proc->bc->constants, index));

            Value val;

            if ( !bytecode_sym_get(name, &val) ) {
                assert(!"symbol nicht gefunden");
            }

            bytecode_sym_set(name, val + 1);
            Obj_Iter *iter = ((Obj_Iter *)val.obj_val);
            Value cond = val_bool(iter->iter < ((Obj_Range *)iter->range)->right);
            stack_push(vm, cond);
        } break;

        case BYTECODEOP_LOAD_SYM: {
            Obj_String *name = as_str(value_get(&frame->proc->bc->constants, bytecode_read16(vm)));
            Value val;

            if ( !bytecode_sym_get(name, &val) ) {
                assert(!"symbol nicht gefunden");
            }

            stack_push(vm, val);
        } break;

        case BYTECODEOP_LOAD_SYMREF: {
            Value val = value_get(&frame->proc->bc->constants, bytecode_read16(vm));
            stack_push(vm, val);
        } break;

        case BYTECODEOP_LOAD_STRUCT_FIELD: {
            Value field = value_get(&frame->proc->bc->constants, bytecode_read16(vm));
            Value val = stack_pop(vm);

            if ( IS_STRUCT(val) ) {
                Obj_Struct *structure = ((Obj_Struct *)val.obj_val);

                Value field_val;
                if ( !table_get(structure->fields, as_str(field), &field_val) ) {
                    assert(!"feld konnte nicht gefunden werden");
                }

                stack_push(vm, field_val);
            } else if ( IS_ENUM(val) ) {
                Obj_Enum *enumeration = ((Obj_Enum *)val.obj_val);

                Value field_val;
                if ( !table_get(enumeration->fields, as_str(field), &field_val) ) {
                    assert(!"feld konnte nicht gefunden werden");
                }

                stack_push(vm, field_val);
            } else {
                assert(IS_NAMESPACE(val));
                Obj_Namespace *ns = ((Obj_Namespace *)val.obj_val);

                Value ns_val;
                if ( !table_get(&ns->scope->syms, as_str(field), &ns_val) ) {
                    assert(!"symbol konnte nicht gefunden werden");
                }

                stack_push(vm, ns_val);
            }
        } break;

        case BYTECODEOP_SET_SYM: {
            Obj_String *name = as_str(stack_pop(vm));
            Value val = stack_pop(vm);

            bytecode_sym_set(name, val, BYTECODEFLAG_SET_EXISTING);
        } break;

        case BYTECODEOP_CALL: {
            Value val = stack_pop(vm);
            uint16_t num_args = bytecode_read16(vm);

            if ( as_proc(val)->sys_call ) {
                call_sys_proc(vm, val, num_args);
            } else {
                call_proc(vm, val, num_args);
            }
        } break;

        case BYTECODEOP_JMP: {
            uint16_t addr = bytecode_read16(vm);
            frame->pc = addr;
        } break;

        case BYTECODEOP_JMP_FALSE: {
            Value val = stack_pop(vm);
            uint16_t addr = bytecode_read16(vm);

            if ( val.bool_val == false ) {
                frame->pc = addr;
            }
        } break;

        case BYTECODEOP_CMP_LT: {
            Value right = stack_pop(vm);
            Value left  = stack_pop(vm);

            stack_push(vm, val_bool(left < right));
        } break;

        case BYTECODEOP_INC: {
            int32_t index = bytecode_read16(vm);
            Value val = value_get(&frame->proc->bc->constants, index);
            value_set(&frame->proc->bc->constants, index, val + 1);
        } break;

        case BYTECODEOP_RANGE: {
            Value right = stack_pop(vm);
            Value left  = stack_pop(vm);

            stack_push(vm, val_range(left, right));
        } break;

        case BYTECODEOP_COMPOUND: {
            int32_t num_vals = bytecode_read16(vm);

            Values vals = NULL;
            for ( int i = 0; i < num_vals; ++i ) {
                buf_push(vals, stack_pop(vm));
            }

            stack_push(vm, val_compound(vals, num_vals));
        } break;

        case BYTECODEOP_HLT: {
            return false;
        } break;

        case BYTECODEOP_PUSH: {
            int32_t index = bytecode_read16(vm);
            Value val = value_get(&frame->proc->bc->constants, index);
            stack_push(vm, val);
        } break;

        case BYTECODEOP_CAST: {
            Value type_to_cast    = stack_pop(vm);
            Value type_to_cast_to = stack_pop(vm);

            type_to_cast.kind = type_to_cast_to.kind;

            stack_push(vm, type_to_cast);
        } break;

        case BYTECODEOP_POP: {
            stack_pop(vm);
        } break;

        case BYTECODEOP_RET: {
            int32_t num_vals = bytecode_read16(vm);

            Values vals = NULL;
            for ( int i = 0; i < num_vals; ++i ) {
                Value val = stack_pop(vm);
                buf_push(vals, val);
            }

            vm->frame_num -= 1;

            for ( int i = 0; i < num_vals; ++i ) {
                stack_push(vm, vals[i]);
            }

            bc_curr_scope = frame->prev_scope;
        } break;

        case BYTECODEOP_SCOPE_ENTER: {
            bytecode_scope_enter();
        } break;

        case BYTECODEOP_SCOPE_LEAVE: {
            bytecode_scope_leave();
        } break;

        case BYTECODEOP_STRUCT: {
            int32_t num_fields = bytecode_read16(vm);

            Values vals = NULL;
            for ( int i = 0; i < num_fields; ++i ) {
                buf_push(vals, stack_pop(vm));
            }

            Value structure = stack_pop(vm);

            for ( int i = 0; i < num_fields; ++i ) {
                Obj_Struct_Field *field = ((Obj_Struct_Field *)vals[i].obj_val);
                table_set(((Obj_Struct *)structure.obj_val)->fields, field->name, field->default_value);
                buf_push(((Obj_Struct *)structure.obj_val)->fieldnames_ordered, field->name);
            }
        } break;

        case BYTECODEOP_ENUM: {
            int32_t num_fields = bytecode_read16(vm);

            Values vals = NULL;
            for ( int i = 0; i < num_fields; ++i ) {
                buf_push(vals, stack_pop(vm));
            }

            Value enumeration = stack_pop(vm);

            for ( int i = 0; i < num_fields; ++i ) {
                Obj_Struct_Field *field = ((Obj_Struct_Field *)vals[i].obj_val);
                table_set(((Obj_Enum *)enumeration.obj_val)->fields, field->name, field->default_value);
                buf_push(((Obj_Enum *)enumeration.obj_val)->fieldnames_ordered, field->name);
            }
        } break;

        case BYTECODEOP_PUSH_TYPEVAL: {
            int32_t index = bytecode_read16(vm);
            Value val = value_get(&frame->proc->bc->constants, index);

            Value type_val;
            if ( !bytecode_sym_get(as_str(val), &type_val) ) {
                assert(!"unbekannter datentyp");
            }

            stack_push(vm, type_val);
        } break;

        case BYTECODEOP_MATCH_CASE: {
            Value   resolution = stack_pop(vm);
            Value   expr       = stack_pop(vm);
            int32_t addr       = bytecode_read16(vm);

            if ( expr != resolution ) {
                frame->pc = addr;
            }
        } break;

        case BYTECODEOP_NEG: {
            Value val = stack_pop(vm);
            stack_push(vm, -val);
        } break;

        case BYTECODEOP_IMPORT_SYMS: {
            Value val = stack_pop(vm);
            assert(IS_NAMESPACE(val));

            for ( int i = 0; i < buf_len(AS_NAMESPACE(val)->scope->syms.ordered_entries); ++i ) {
                Table_Entry *sym = AS_NAMESPACE(val)->scope->syms.ordered_entries[i];
                bytecode_sym_set(sym->key, sym->val);
            }
        } break;

        case BYTECODEOP_STRUCT_FIELD: {
            Value default_val = stack_pop(vm);
            Value type_val = stack_pop(vm);
            Value field = stack_pop(vm);

            Value val     = (default_val.kind == VAL_NONE) ? type_val : default_val;
            Value new_val = val;

            if ( val.kind == VAL_OBJ && val.obj_val->kind == OBJ_COMPOUND ) {
                Obj_Compound * compound  = ((Obj_Compound *)val.obj_val);

                assert(type_val.obj_val->kind == OBJ_STRUCT);
                Obj_Struct   * structure = (Obj_Struct *)type_val.obj_val;
                               new_val   = val_struct(((Obj_String *)structure->name)->ptr, ((Obj_String *)structure->name)->size);

                for ( uint32_t i = 0; i < compound->num_elems; ++i ) {
                    Value compound_val = compound->elems[i];
                    table_set(((Obj_Struct *)new_val.obj_val)->fields, structure->fieldnames_ordered[i], compound_val);
                }
            }

            ((Obj_Struct_Field *)field.obj_val)->default_value = new_val;
            stack_push(vm, field);
        } break;

        default: {
            assert(0);
        } break;
    }

    return true;
}

void eval(Bytecode *bc) {
    Vm *vm = vm_new(bc);

    for ( ;; ) {
#if BYTECODE_DEBUG
        vm_debug(vm);
        getchar();
#endif
        bool keep_running = step(vm);

        if ( !keep_running ) {
            break;
        }
    }
}

void
bytecode_init(Bytecode *bc) {
#define REGISTER_TYPE(Val, Name, Size)                                                 \
    do {                                                                               \
            Bytecode_Scope *prev_scope = bc_curr_scope;                                \
            bc_curr_scope = &bc_sys_scope;                                             \
            int32_t index = bytecode_push_constant(bc, val_str(Name, Size));           \
            Obj_String *name = as_str(value_get(&bc->constants, index));               \
            if ( !bytecode_sym_set(name, Val) ) {                                      \
                assert(!"symbol konnte nicht gesetzt werden");                         \
            }                                                                          \
            bc_curr_scope = prev_scope;                                                \
    } while(0)

    REGISTER_TYPE(val_none(),      "void",   4);
    REGISTER_TYPE(val_int(0),      "u8",     2);
    REGISTER_TYPE(val_int(0),      "u16",    3);
    REGISTER_TYPE(val_int(0),      "u32",    3);
    REGISTER_TYPE(val_int(0),      "u64",    3);
    REGISTER_TYPE(val_int(0),      "s8",     2);
    REGISTER_TYPE(val_int(0),      "s16",    3);
    REGISTER_TYPE(val_int(0),      "s32",    3);
    REGISTER_TYPE(val_int(0),      "s64",    3);
    REGISTER_TYPE(val_float(0.0f), "f32",    3);
    REGISTER_TYPE(val_float(0.0f), "f64",    3);
    REGISTER_TYPE(val_bool(false), "bool",   4);
    REGISTER_TYPE(val_str("", 0),  "string", 6);

#if 0
    sym_push_sys("typeid", type_typeid);
#endif
#undef REGISTER_TYPE
}
