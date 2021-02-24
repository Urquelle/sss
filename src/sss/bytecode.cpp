struct Bytecode;
struct Obj;
struct Obj_Proc;
struct Table;

#define IS_STRUCT(Value) (((Value).kind == VAL_OBJ) && ((Value).obj_val->kind == OBJ_STRUCT))

#define VALUES   \
    X(VAL_NONE)  \
    X(VAL_INT)   \
    X(VAL_FLOAT) \
    X(VAL_BOOL)  \
    X(VAL_OBJ)

enum Value_Kind {
#define X(Elem) Elem,
    VALUES
#undef X
};

struct Value {
    Value_Kind kind;

    union {
        int64_t   int_val;
        float     flt_val;
        bool      bool_val;
        Obj     * obj_val;
    };
};

#define OBJECTS         \
    X(OBJ_STRING)       \
    X(OBJ_RANGE)        \
    X(OBJ_PROC)         \
    X(OBJ_ITER)         \
    X(OBJ_STRUCT)       \
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

struct Obj_Proc : Obj {
    Obj_String * name;
    Bytecode   * bc;
    uint32_t     num_params;
    bool         sys_call;
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
    Obj_String * name;
    Table      * fields;
};

struct Obj_Struct_Field : Obj {
    Obj_String * name;
    Value        default_value;
};

typedef Value* Values;

void               bytecode_stmt(Bytecode *bc, Stmt *stmt);
void               bytecode_init(Bytecode *bc);
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
};

enum Bytecode_Flags {
    BYTECODEFLAG_NONE,
    BYTECODEFLAG_ASSIGNABLE,
    BYTECODEFLAG_SET_EXISTING,
};

struct Bytecode_Scope {
    char           * name;
    Bytecode_Scope * parent;
    Table            syms;
};

Bytecode_Scope   bc_sys_scope    = {"sys",    NULL,          {}};
Bytecode_Scope   bc_global_scope = {"global", &bc_sys_scope, {}};
Bytecode_Scope * bc_curr_scope   = &bc_global_scope;

struct Bytecode {
    Bytecode * parent;
    uint8_t  * code;
    uint32_t   size;
    uint32_t   cap;

    Value_Array constants;
};

#define BYTECODES                   \
    X(BYTECODEOP_HLT)               \
    X(BYTECODEOP_NONE)              \
    X(BYTECODEOP_ADD)               \
    X(BYTECODEOP_SUB)               \
    X(BYTECODEOP_MUL)               \
    X(BYTECODEOP_DIV)               \
    X(BYTECODEOP_INC)               \
    X(BYTECODEOP_JMP)               \
    X(BYTECODEOP_JMP_FALSE)         \
    X(BYTECODEOP_CMP_LT)            \
    X(BYTECODEOP_PRINT)             \
    X(BYTECODEOP_RANGE)             \
    X(BYTECODEOP_STRUCT)            \
    X(BYTECODEOP_STRUCT_FIELD)      \
    X(BYTECODEOP_CONST)             \
    X(BYTECODEOP_PUSH_TYPEVAL)      \
    X(BYTECODEOP_PUSH_SYSSYM)       \
    X(BYTECODEOP_PUSH_SYM)          \
    X(BYTECODEOP_LOAD_SYM)          \
    X(BYTECODEOP_LOAD_SYMREF)       \
    X(BYTECODEOP_LOAD_STRUCT_FIELD) \
    X(BYTECODEOP_SET_SYM)           \
    X(BYTECODEOP_CALL)              \
    X(BYTECODEOP_INIT_LOOP)         \
    X(BYTECODEOP_INC_LOOP)          \
    X(BYTECODEOP_POP)               \
    X(BYTECODEOP_PUSH)              \
    X(BYTECODEOP_SCOPE_ENTER)       \
    X(BYTECODEOP_SCOPE_LEAVE)       \
    X(BYTECODEOP_RET)

enum Bytecode_Opcode {
#define X(Elem) Elem,
    BYTECODES
#undef X
};

struct Call_Frame {
    Obj_Proc * proc;
    uint32_t   pc;
    uint32_t   sp;
};

enum { MAX_STACK_SIZE = 1024, MAX_frame_num = 100 };
struct Vm {
    Value        stack[MAX_STACK_SIZE];
    Call_Frame   frames[MAX_frame_num];
    int32_t      frame_num;
};

Table strings;

void
bytecode_scope_enter() {
    Bytecode_Scope *scope = urq_allocs(Bytecode_Scope);

    scope->name   = NULL;
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

    result->size    = 0;
    result->cap     = 0;
    result->entries = NULL;

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
    result->num_params = 0;
    result->bc         = bytecode_new();
    result->name       = NULL;
    result->sys_call   = false;

    return result;
}

Obj_Proc *
obj_proc(bool sys_call) {
    Obj_Proc *result = obj_proc();

    result->sys_call   = sys_call;

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

    return result;
}

Obj_Struct_Field *
obj_struct_field(Obj_String *name) {
    Obj_Struct_Field *result = urq_allocs(Obj_Struct_Field);

    result->kind = OBJ_STRUCT_FIELD;
    result->name = name;

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

        case OBJ_STRUCT_FIELD: {
            printf("struct_field: ");
            obj_print(((Obj_Struct_Field *)obj)->name);
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
val_int(int64_t val) {
    Value result = {};

    result.kind    = VAL_INT;
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
val_range(Value left, Value right) {
    Value result = val_obj(obj_range(left, right));

    return result;
}

Value
val_proc(char *ptr, uint32_t size, bool sys_call) {
    Value result = val_obj(obj_proc(sys_call));

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
val_struct_field(char *name, uint32_t len) {
    Value result = val_obj(obj_struct_field(obj_string(name, len)));

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

Value
operator<(Value left, Value right) {
    switch ( left.kind ) {
        case VAL_INT: {
            assert(right.kind == VAL_INT);
            return val_bool(left.int_val < right.int_val);
        } break;

        case VAL_FLOAT: {
            assert(right.kind == VAL_FLOAT);
            return val_bool(left.flt_val < right.flt_val);
        } break;

        default: {
            assert(0);
        } break;
    }

    assert(0);
    return val_none();
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

void
bytecode_debug(Vm *vm, int32_t code) {
    Call_Frame *frame = vm_curr_frame(vm);

    switch ( code ) {
        case BYTECODEOP_CONST: {
            printf("OP_CONST [index: %02d] (", frame->proc->bc->code[frame->pc]);
            val_print(bytecode_fetch_constant(frame->proc->bc, frame->proc->bc->code[frame->pc]));
            printf(")\n");
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

        case BYTECODEOP_PUSH_SYSSYM: {
            printf("OP_PUSH_SYSSYM [index: %02d] (", frame->proc->bc->code[frame->pc]);
            val_print(bytecode_fetch_constant(frame->proc->bc, frame->proc->bc->code[frame->pc]));
            printf(")\n");
        } break;

        case BYTECODEOP_PUSH_SYM: {
            printf("OP_PUSH_SYM ");
            val_print(bytecode_fetch_constant(frame->proc->bc, frame->proc->bc->code[frame->pc]));
            printf("[val: ");
            val_print(vm->stack[frame->sp-1]);
            printf("]\n");
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
            printf("OP_INC_LOOP ");
            Value val = value_get(&frame->proc->bc->constants, *(uint16_t *)(frame->proc->bc->code + frame->pc));
            val_print(val);
            printf("\n");
        } break;

        case BYTECODEOP_JMP: {
            printf("OP_JMP [addr: %d]\n", *(uint16_t *)(frame->proc->bc->code + frame->pc));
        } break;

        case BYTECODEOP_PUSH: {
            printf("OP_PUSH ");
            Value val = value_get(&frame->proc->bc->constants, *(uint16_t *)(frame->proc->bc->code + frame->pc));
            val_print(val);
            printf("\n");
        } break;

        case BYTECODEOP_POP: {
            printf("OP_POP\n");
        } break;

        case BYTECODEOP_SCOPE_ENTER: {
            printf("OP_SCOPE_ENTER\n");
        } break;

        case BYTECODEOP_SCOPE_LEAVE: {
            printf("OP_SCOPE_LEAVE\n");
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

        case BYTECODEOP_STRUCT: {
            printf("OP_STRUCT\n");
        } break;

        case BYTECODEOP_NONE: {
            printf("OP_NONE\n");
        } break;

        case BYTECODEOP_PUSH_TYPEVAL: {
            printf("OP_PUSH_TYPEVAL [");
            val_print(value_get(&frame->proc->bc->constants, *(uint16_t *)(frame->proc->bc->code + frame->pc)));
            printf("]\n");
        } break;

        default: {
            assert(!"unbekannter bytecode");
        } break;
    }
}

void
bytecode_expr(Bytecode *bc, Expr *expr, uint32_t flags = BYTECODEFLAG_NONE) {
    switch ( expr->kind ) {
        case EXPR_INT: {
            int32_t index = bytecode_push_constant(bc, val_int(AS_INT(expr)->val));

            bytecode_write8(bc, BYTECODEOP_CONST);
            bytecode_write16(bc, (uint16_t)index);
        } break;

        case EXPR_FLOAT: {
            int32_t index = bytecode_push_constant(bc, val_float(AS_FLOAT(expr)->val));

            bytecode_write8(bc, BYTECODEOP_CONST);
            bytecode_write16(bc, (uint16_t)index);
        } break;

        case EXPR_STR: {
            int32_t index = bytecode_push_constant(bc, val_str(AS_STR(expr)->val, AS_STR(expr)->len));

            bytecode_write8(bc, BYTECODEOP_CONST);
            bytecode_write16(bc, (uint16_t)index);
        } break;

        case EXPR_BOOL: {
            int32_t index = bytecode_push_constant(bc, val_bool(AS_BOOL(expr)->val));

            bytecode_write8(bc, BYTECODEOP_CONST);
            bytecode_write16(bc, (uint16_t)index);
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

        case EXPR_CALL: {
            for ( int i = 0; i < AS_CALL(expr)->num_args; ++i ) {
                bytecode_expr(bc, AS_CALL(expr)->args[i]);
            }

            bytecode_expr(bc, AS_CALL(expr)->base);
            bytecode_write8(bc, BYTECODEOP_CALL);
            bytecode_write16(bc, (uint16_t)AS_CALL(expr)->num_args);
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

            bytecode_write8(bc, BYTECODEOP_PUSH_SYM);
            bytecode_write16(bc, (uint16_t)index);
        } break;

        case DECL_PROC: {
            Value val = val_proc(decl->name, decl->len, AS_PROC(decl)->sign->sys_call);
            int32_t index = bytecode_push_constant(bc, val);
            bytecode_write8(bc, BYTECODEOP_CONST);
            bytecode_write16(bc, (uint16_t)index);

            index = bytecode_push_constant(bc, val_str(decl->name, decl->len));
            bytecode_write8(bc, BYTECODEOP_PUSH_SYM);
            bytecode_write16(bc, (uint16_t)index);

            Obj_Proc *proc = as_proc(val);
            proc->name = obj_string(decl->name, decl->len);
            proc->num_params = (uint32_t)AS_PROC(decl)->sign->num_params;

            bytecode_write8(proc->bc, BYTECODEOP_SCOPE_ENTER);
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
                    bytecode_write16(bc, 0);
                }
            }
            bytecode_write8(proc->bc, BYTECODEOP_SCOPE_LEAVE);
        } break;

        case DECL_STRUCT: {
            Value val = val_struct(decl->name, decl->len);
            int32_t index = bytecode_push_constant(bc, val);
            bytecode_write8(bc, BYTECODEOP_CONST);
            bytecode_write16(bc, (uint16_t)index);

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

        default: {
            assert(!"unbekannte anweisung");
        } break;
    }
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
call_proc(Vm *vm, Value val, uint32_t num_args) {
    assert(vm->frame_num < MAX_frame_num);

    Call_Frame *prev_frame = vm_curr_frame(vm);
    Call_Frame *frame = vm->frames + vm->frame_num++;

    assert(is_proc(val));
    Obj_Proc *proc = as_proc(val);

    frame->proc = proc;
    frame->pc   = 0;
    frame->sp   = prev_frame->sp;
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
bytecode_sym_set(Obj_String *key, Value val, uint32_t flags = BYTECODEFLAG_NONE) {
    if ( flags & BYTECODEFLAG_SET_EXISTING ) {
        Bytecode_Scope *it = bc_curr_scope;

        while ( it ) {
            Value v;
            if ( table_get(&it->syms, key, &v) ) {
                table_set(&it->syms, key, val);

                return true;
            }

            it = it->parent;
        }
    } else {
        return table_set(&bc_curr_scope->syms, key, val);
    }

    return false;
}

Bytecode *
build(Resolved_Stmts stmts) {
    Bytecode *bc = bytecode_new();

    bytecode_init(bc);

    for ( int i = 0; i < buf_len(stmts); ++i ) {
        Resolved_Stmt *stmt = stmts[i];

        bytecode_stmt(bc, stmt->stmt);
    }

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

        case BYTECODEOP_PUSH_SYM: {
            Obj_String *name = as_str(value_get(&frame->proc->bc->constants, bytecode_read16(vm)));
            Value val = stack_pop(vm);

            if ( !bytecode_sym_set(name, val) ) {
                assert(!"symbol konnte nicht gesetzt werden");
            }
        } break;

        case BYTECODEOP_INIT_LOOP: {
            Obj_String *name = as_str(value_get(&frame->proc->bc->constants, bytecode_read16(vm)));
            Value val = stack_pop(vm);

            assert(val.kind == VAL_OBJ);
            assert(val.obj_val->kind == OBJ_RANGE);

            bytecode_sym_set(name, val_iter(as_range(val), as_range(val)->left));
            stack_push(vm, as_range(val)->left < as_range(val)->right);
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
            Value cond = iter->iter < ((Obj_Range *)iter->range)->right;
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

            if ( !IS_STRUCT(val) ) {
                assert(!"unerlaubter feldzugriff");
            }

            Obj_Struct *structure = ((Obj_Struct *)val.obj_val);

            Value field_val;
            if ( !table_get(structure->fields, as_str(field), &field_val) ) {
                assert(!"feld konnte nicht gefunden werden");
            }

            stack_push(vm, field_val);
        } break;

        case BYTECODEOP_SET_SYM: {
            Obj_String *name = as_str(stack_pop(vm));
            Value val = stack_pop(vm);

            bytecode_sym_set(name, val, BYTECODEFLAG_SET_EXISTING);
        } break;

        case BYTECODEOP_CALL: {
            Value val = stack_pop(vm);
            uint16_t num_args = bytecode_read16(vm);

            call_proc(vm, val, num_args);
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

            stack_push(vm, left < right);
        } break;

        case BYTECODEOP_PRINT: {
            Value val = stack_pop(vm);
            val_print(val);
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

        case BYTECODEOP_HLT: {
            return false;
        } break;

        case BYTECODEOP_PUSH: {
            int32_t index = bytecode_read16(vm);
            Value val = value_get(&frame->proc->bc->constants, index);
            stack_push(vm, val);
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

        case BYTECODEOP_STRUCT_FIELD: {
            Value val = stack_pop(vm);
            Value field = stack_pop(vm);

            ((Obj_Struct_Field *)field.obj_val)->default_value = val;
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
#define REGISTER_TYPE(Val, Name, Size)                            \
    do {                                                          \
        int32_t index = bytecode_push_constant(bc, Val);          \
        bytecode_write8(bc, BYTECODEOP_CONST);                    \
        bytecode_write16(bc, (uint16_t)index);                    \
        index = bytecode_push_constant(bc, val_str(Name, Size));  \
        bytecode_write8(bc, BYTECODEOP_PUSH_SYSSYM);              \
        bytecode_write16(bc, (uint16_t)index);                    \
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
}
