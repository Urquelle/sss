namespace Vm {

uint32_t entry_point_index;

struct Bytecode;
struct Call_Frame;
struct Instr;
struct Scope;
struct Obj;
struct Obj_Array;
struct Obj_Proc;
struct Table;
struct Value;
struct Vm;

typedef Value *  Values;
typedef Instr ** Instrs;

struct Basic_Block {
    int32_t id;
    Instrs   instrs;
};

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

struct Value : Loc {
    Value_Kind kind;
    int32_t    size;

    union {
        int64_t   int_val;
        float     flt_val;
        bool      bool_val;
        Obj     * obj_val;
    };
};

#define OBJECTS         \
    X(OBJ_STRING)       \
    X(OBJ_ARRAY)        \
    X(OBJ_RANGE)        \
    X(OBJ_PROC)         \
    X(OBJ_ITER)         \
    X(OBJ_NAMESPACE)    \
    X(OBJ_STRUCT)       \
    X(OBJ_ENUM)         \
    X(OBJ_COMPOUND)     \
    X(OBJ_PTR)          \
    X(OBJ_STRUCT_FIELD) \
    X(OBJ_TYPE)         \
    X(OBJ_TYPE_FIELD)

enum Obj_Kind {
#define X(Elem) Elem,
    OBJECTS
#undef X
};

struct Obj : Loc {
    Obj_Kind   kind;
    Scope    * scope;
};

struct Obj_String : Obj {
    uint32_t   size;
    char     * ptr;
    uint32_t   hash;
};

struct Obj_Compound : Obj {
    Values  elems;
    int32_t num_elems;
};

struct Obj_Proc : Obj {
    Obj_String     * name;
    Bytecode       * bc;
    int32_t          num_params;
    bool             sys_call;
    char           * lib;
    Proc_Sign      * sign;
};

struct Obj_Namespace : Obj {
    Obj_String     * name;
    char           * scope_name;
    Obj_Proc       * proc;
    Scope          * prev_scope;
};

struct Obj_Range : Obj {
    Value left;
    Value right;
};

enum Iter_Kind {
    ITER_NONE,
    ITER_RANGE,
    ITER_ARRAY,
};
struct Obj_Iter : Obj {
    Iter_Kind   iter_kind;
    Value       iter;
    uint32_t    curr_index;
};

struct Obj_Iter_Range : Obj_Iter {
    Obj_Range * range;
};

struct Obj_Iter_Array : Obj_Iter {
    Obj_Array * array;
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

struct Obj_Ptr : Obj {
    void *ptr;
};

struct Obj_Array : Obj {
    Values   elems;
    uint32_t num_elems;
};

struct Obj_Type : Obj {
    uint32_t   id;
    char     * name;
    uint32_t   size;
    uint32_t   offset;

    Obj_Type * base;

    char     * field_name;
    Values     fields;
    uint32_t   num_fields;

    Values     params;
    uint32_t   num_params;

    Values     rets;
    uint32_t   num_rets;
};

struct Obj_Type_Field : Obj {
    uint32_t   id;
    char     * name;
};

Obj_Type **obj_types;

enum Bytecode_Flags {
    BYTECODEFLAG_NONE,
    BYTECODEFLAG_ASSIGNABLE,
    BYTECODEFLAG_SET_EXISTING,
};

void               bytecode_build_file(Bytecode *bc, Parsed_File *file);
bool               bytecode_sym_set(Obj_String *key, Value val, uint32_t flags = BYTECODEFLAG_NONE);
int32_t            bytecode_emit_const(Loc *loc, Bytecode *bc, Value val);
void               bytecode_debug(Vm *vm, int32_t code);
void               bytecode_stmt(Bytecode *bc, Stmt *stmt);
void               bytecode_init(Bytecode *bc);
void               bytecode_typespec(Bytecode *bc, Typespec *typespec);
Value              val_none();
Value              val_array(Values elems, uint32_t num_elems);
Value              val_int(int64_t val, uint32_t size = 4);
Value              val_str(char *ptr);
Value              val_bool(bool val);
Value              val_obj(Obj *val);
void               val_print(Value val);
Call_Frame       * vm_curr_frame(Vm *vm);
Obj_Proc         * obj_proc();
Obj_Proc         * as_proc(Value val);
Obj_Struct       * as_struct(Value val);
Obj_Struct_Field * as_struct_field(Value val);

#define AS_NAMESPACE(Val) ((Obj_Namespace *)(Val).obj_val)

struct Value_Array {
    int32_t   size;
    int32_t   cap;
    Value   * values;
};

#define TABLE_MAX_LOAD 0.75f
struct Table_Entry {
    Obj_String * key;
    Value        val;
};

struct Table {
    uint32_t       size;
    uint32_t       cap;
    Table_Entry  * entries;
    Table_Entry  ** ordered_entries;
};

struct Scope {
    char  * name;
    Scope * parent;
    Table   syms;
    Table   export_syms;
};

Scope   sys_scope    = {"sys",    NULL,       {}};
Scope   global_scope = {"global", &sys_scope, {}};
Scope * curr_scope   = &global_scope;

struct Bytecode {
    Bytecode *  parent;
    Instr    ** instructions;
    int32_t     curr;
    int32_t     size;
    Value_Array constants;
};

#define BYTECODES                   \
    X(BYTECODEOP_HLT)               \
    X(BYTECODEOP_ALLOC)             \
    X(BYTECODEOP_ADDR)              \
    X(BYTECODEOP_NEG)               \
    X(BYTECODEOP_ADD)               \
    X(BYTECODEOP_CALL)              \
    X(BYTECODEOP_CAST)              \
    X(BYTECODEOP_CMP_LT)            \
    X(BYTECODEOP_CMP_GT)            \
    X(BYTECODEOP_CMP_EQ)            \
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
    X(BYTECODEOP_LOAD_STRUCT_FIELD_ASSIGNABLE) \
    X(BYTECODEOP_LOAD_SYM)          \
    X(BYTECODEOP_LOAD_SYMREF)       \
    X(BYTECODEOP_LOAD_TYPEINFO)     \
    X(BYTECODEOP_LOAD_TYPEOF)     \
    X(BYTECODEOP_MATCH_CASE)        \
    X(BYTECODEOP_MUL)               \
    X(BYTECODEOP_NAMED_COMPOUND)    \
    X(BYTECODEOP_NAMESPACE_ENTER)   \
    X(BYTECODEOP_NAMESPACE_LEAVE)   \
    X(BYTECODEOP_NONE)              \
    X(BYTECODEOP_POP)               \
    X(BYTECODEOP_PUSH)              \
    X(BYTECODEOP_PUSH_SYM)          \
    X(BYTECODEOP_PUSH_DECL_SYM)     \
    X(BYTECODEOP_PUSH_SYSSYM)       \
    X(BYTECODEOP_PUSH_TYPEVAL)      \
    X(BYTECODEOP_RANGE)             \
    X(BYTECODEOP_RET)               \
    X(BYTECODEOP_SCOPE_ENTER)       \
    X(BYTECODEOP_SCOPE_LEAVE)       \
    X(BYTECODEOP_SET_SYM)           \
    X(BYTECODEOP_STRUCT)            \
    X(BYTECODEOP_STRUCT_FIELD)      \
    X(BYTECODEOP_SUB)               \
    X(BYTECODEOP_SUBSCRIPT)         \
    X(BYTECODEOP_ENUM)              \

enum Instr_Opcode {
#define X(Elem) Elem,
    BYTECODES
#undef X
};

struct Instr : Loc {
    Instr_Opcode opcode;
    int32_t      op1;
    int32_t      op2;
};

struct Call_Frame {
    Obj_Proc   * proc;
    Scope      * prev_scope;
    int32_t      pc;
    int32_t      sp;
};

enum { MAX_STACK_SIZE = 1024, MAX_FRAME_NUM = 100 };
struct Vm {
    Value        stack[MAX_STACK_SIZE];
    Call_Frame   frames[MAX_FRAME_NUM];
    int32_t      frame_num;
};

Table strings;

Basic_Block *
basic_block(int32_t id, Instrs instrs) {
    Basic_Block *result = urq_allocs(Basic_Block);

    result->id     = id;
    result->instrs = instrs;

    return result;
}

Instr *
instr_new(Loc *loc, Instr_Opcode opcode, int32_t op1) {
    Instr *result = urq_allocs(Instr);

    loc_copy(loc, result);

    result->opcode = opcode;
    result->op1    = op1;
    result->op2    = 0;

    return result;
}

void
instr_print(Instr *instr) {
}

Instr *
instr_push(Loc *loc, Bytecode *bc, Instr_Opcode opcode, int32_t op1) {
    Instr *result = instr_new(loc, opcode, op1);

    buf_push(bc->instructions, result);

    bc->curr = (uint32_t)buf_len(bc->instructions)-1;
    bc->size = (uint32_t)buf_len(bc->instructions);

    return result;
}

Instr *
instr_push(Loc *loc, Bytecode *bc, Instr_Opcode opcode) {
    return instr_push(loc, bc, opcode, 0);
}

void
instr_patch(Bytecode *bc, int32_t index, int32_t val) {
    bc->instructions[index]->op1 = val;
}

void
instr_patch2(Bytecode *bc, int32_t index, int32_t val) {
    bc->instructions[index]->op2 = val;
}

Instr *
instr_fetch(Vm *vm) {
    Call_Frame *frame = vm_curr_frame(vm);

    Instr *result = frame->proc->bc->instructions[frame->pc];
    frame->pc += 1;

    return result;
}

Scope *
bytecode_scope_new(char *name, Scope *parent = NULL) {
    Scope *result = urq_allocs(Scope);

    result->name   = name;
    result->parent = parent;

    return result;
}

void
bytecode_scope_enter(char *name = NULL) {
    Scope *scope = urq_allocs(Scope);

    scope->name   = name;
    scope->parent = curr_scope;
    curr_scope = scope;
}

void
bytecode_scope_leave() {
    assert(curr_scope->parent);
    curr_scope = curr_scope->parent;
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
                   memcmp(entry->key->ptr, chars, size) == 0)
        {
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
    for ( uint32_t i = 0; i < table->cap; i++) {
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
    for ( uint32_t i = 0; i < from->cap; i++) {
        Table_Entry* entry = &from->entries[i];
        if (entry->key != NULL) {
            table_set(to, entry->key, entry->val);
        }
    }
}

Value *
table_get_ref(Table *table, Obj_String *key) {
    if (table->size == 0) {
        return NULL;
    }

    Table_Entry *entry = table_find(table->entries, table->cap, key);
    if (entry->key == NULL) {
        return NULL;
    }

    return &entry->val;
}

bool
table_get(Table *table, Obj_String *key, Value *val) {
    Value *source_val = table_get_ref(table, key);

    if ( !source_val ) {
        return false;
    }

    *val = *source_val;

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
        instr_print(frame->proc->bc->instructions[i]);
    }
    printf("\n\n***************** STACK *************************\n");
    if ( frame->sp ) {
        for ( int i = 0; i < frame->sp; ++i ) {
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

    result->parent       = NULL;
    result->instructions = NULL;
    result->curr         = 0;
    result->constants    = {};

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

Obj_String *
obj_string(char *ptr) {
    int32_t size = utf8_str_size(ptr);
    int32_t hash = hash_string(ptr, size);
    Obj_String *interned = table_find_string(&strings, ptr, size, hash);

    if ( interned ) {
        return interned;
    }

    Obj_String *result = urq_allocs(Obj_String);

    result->kind  = OBJ_STRING;
    result->scope = bytecode_scope_new("string");
    result->ptr   = ptr;
    result->size  = size;
    result->hash  = hash;

    table_set(&strings, result, val_none());

    return result;
}

Obj_Compound *
obj_compound(Values elems, uint32_t num_elems) {
    Obj_Compound *result = urq_allocs(Obj_Compound);

    result->kind      = OBJ_COMPOUND;
    result->elems     = elems;
    result->num_elems = num_elems;
    result->scope     = bytecode_scope_new("compound");

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

Obj_Proc *
obj_proc(bool sys_call, char *lib) {
    Obj_Proc *result = obj_proc();

    result->sys_call = sys_call;
    result->lib      = lib;

    return result;
}

Obj_Namespace *
obj_namespace(char *name, char *scope_name) {
    Obj_Namespace *result = urq_allocs(Obj_Namespace);

    result->kind       = OBJ_NAMESPACE;
    result->name       = obj_string(name);
    result->scope_name = scope_name;
    result->proc       = obj_proc();
    result->scope      = bytecode_scope_new(scope_name, &sys_scope);

    return result;
}

Obj_Iter_Range *
obj_iter_range(Obj_Range *range, Value iter) {
    Obj_Iter_Range *result = urq_allocs(Obj_Iter_Range);

    result->kind       = OBJ_ITER;
    result->iter_kind  = ITER_RANGE;
    result->range      = range;
    result->iter       = iter;
    result->curr_index = 0;

    return result;
}

Obj_Iter_Array *
obj_iter_array(Obj_Array *array, Value iter) {
    Obj_Iter_Array *result = urq_allocs(Obj_Iter_Array);

    result->kind       = OBJ_ITER;
    result->iter_kind  = ITER_ARRAY;
    result->array      = array;
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

Obj_Ptr *
obj_ptr(void *ptr) {
    Obj_Ptr *result = urq_allocs(Obj_Ptr);

    result->kind = OBJ_PTR;
    result->ptr = ptr;

    return result;
}

Obj_Array *
obj_array(Values elems, uint32_t num_elems) {
    Obj_Array *result = urq_allocs(Obj_Array);

    result->kind      = OBJ_ARRAY;
    result->elems     = elems;
    result->num_elems = num_elems;
    result->scope     = bytecode_scope_new("array");

    return result;
}

Obj_Type *
obj_type(uint32_t id, char *name, uint32_t size) {
    Obj_Type *result = urq_allocs(Obj_Type);

    result->kind       = OBJ_TYPE;
    result->scope     = bytecode_scope_new("datentyp");

    result->id         = id;
    result->name       = name;
    result->size       = size;
    result->offset     = 0;

    result->base       = NULL;

    result->fields     = NULL;
    result->num_fields = 0;

    result->params     = NULL;
    result->num_params = 0;

    result->rets       = NULL;
    result->num_rets   = 0;

    table_set(&result->scope->syms, obj_string("id"), val_int(result->id));
    table_set(&result->scope->syms, obj_string("name"), val_str(result->name));
    table_set(&result->scope->syms, obj_string("größe"), val_int(result->size));
    table_set(&result->scope->syms, obj_string("versatz"), val_int(0));
    table_set(&result->scope->syms, obj_string("basis"), val_int(0));
    table_set(&result->scope->syms, obj_string("felder"), val_array(NULL, 0));
    table_set(&result->scope->syms, obj_string("parameter"), val_array(NULL, 0));
    table_set(&result->scope->syms, obj_string("rückgabewerte"), val_array(NULL, 0));

    return result;
}

Obj_Type *
obj_type(uint32_t id, char *name, uint32_t size, Obj_Type *base) {
    Obj_Type *result = obj_type(id, name, size);

    result->base = base;

    table_set(&result->scope->syms, obj_string("basis"), val_obj(base));

    return result;
}

Obj_Type *
obj_type(uint32_t id, char *name, uint32_t size, Values fields, uint32_t num_fields) {
    Obj_Type *result = obj_type(id, name, size);

    result->fields     = fields;
    result->num_fields = num_fields;

    table_set(&result->scope->syms, obj_string("felder"), val_array(fields, num_fields));

    return result;
}

Obj_Type *
obj_type(uint32_t id, char *name, uint32_t size, Values params, uint32_t num_params, Values rets, uint32_t num_rets) {
    Obj_Type *result = obj_type(id, name, size);

    result->params     = params;
    result->num_params = num_params;
    result->rets       = rets;
    result->num_rets   = num_rets;

    table_set(&result->scope->syms, obj_string("parameter"), val_array(params, num_params));
    table_set(&result->scope->syms, obj_string("rückgabewerte"), val_array(rets, num_rets));

    return result;
}

Obj_Type_Field *
obj_type_field(uint32_t id, char *name) {
    Obj_Type_Field *result = urq_allocs(Obj_Type_Field);

    result->id   = id;
    result->name = name;

    result->scope     = bytecode_scope_new("datentyp");

    table_set(&result->scope->syms, obj_string("id"), val_int(result->id));
    table_set(&result->scope->syms, obj_string("name"), val_str(result->name));

    return result;
}

void
obj_print(Obj *obj) {
    switch ( obj->kind ) {
        case OBJ_STRING: {
            printf("(string: \"%s\")", ((Obj_String *)obj)->ptr);
        } break;

        case OBJ_RANGE: {
            printf("(range: ");
            val_print(((Obj_Range *)obj)->left);
            printf("..");
            val_print(((Obj_Range *)obj)->right);
            printf(")");
        } break;

        case OBJ_COMPOUND: {
            Obj_Compound *c = ((Obj_Compound *)obj);
            printf("(compound: ");
            for ( int i = 0; i < c->num_elems; ++i ) {
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

            if ( OITER(obj)->iter_kind == ITER_RANGE ) {
                obj_print((OITERRNG(obj)->range));
            } else {
                assert(IS_OITERARRAY(obj));
                obj_print((OITERARRAY(obj)->array));
            }

            printf("][it: ");
            val_print((OITER(obj))->iter);
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

        case OBJ_PTR: {
            printf("(ptr: 0x%p)", ((Obj_Ptr *)obj)->ptr);
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
val_int(int64_t val, uint32_t size) {
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
val_ptr(void *ptr) {
    Value result = val_obj(obj_ptr(ptr));

    return result;
}

Value
val_str(char *ptr) {
    Value result = val_obj(obj_string(ptr));

    table_set(&result.obj_val->scope->syms, obj_string("größe"), val_int(VSTR(result)->size));

    return result;
}

Value
val_compound(Values elems, uint32_t num_elems) {
    Value result = val_obj(obj_compound(elems, num_elems));

    table_set(&result.obj_val->scope->syms, obj_string("größe"), val_int(num_elems));

    return result;
}

Value
val_range(Value left, Value right) {
    Value result = val_obj(obj_range(left, right));

    return result;
}

Value
val_proc(char *ptr, bool sys_call, char *lib) {
    Value result = val_obj(obj_proc(sys_call, lib));

    return result;
}

Value
val_iter_range(Obj_Range *range, Value iter) {
    Value result = val_obj(obj_iter_range(range, iter));

    return result;
}

Value
val_iter_array(Obj_Array *array, Value iter) {
    Value result = val_obj(obj_iter_array(array, iter));

    return result;
}

Value
val_struct(char *name) {
    Value result = val_obj(obj_struct(obj_string(name)));

    return result;
}

Value
val_struct(Obj_String *name) {
    Value result = val_obj(obj_struct(name));

    return result;
}

Value
val_enum(char *name) {
    Value result = val_obj(obj_enum(obj_string(name)));

    return result;
}

Value
val_struct_field(char *name) {
    Value result = val_obj(obj_struct_field(obj_string(name)));

    return result;
}

Value
val_namespace(Obj_Namespace *ns) {
    Value result = val_obj(ns);

    return result;
}

Value
val_array(Values elems, uint32_t num_elems) {
    Value result = val_obj(obj_array(elems, num_elems));

    table_set(&result.obj_val->scope->syms, obj_string("größe"), val_int(num_elems));

    return result;
}

Value
val_type(Obj_Type *type) {
    Value result = val_obj(type);

    return result;
}

Value
val_type_field(uint32_t id, char *name) {
    Value result = val_obj(obj_type_field(id, name));

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
val_to_ptr(Value val) {
    Value result = {};

    switch ( val.kind ) {
        case VAL_INT: {
            result = val_ptr(&val.int_val);
        } break;

        case VAL_FLOAT: {
            result = val_ptr(&val.flt_val);
        } break;

        case VAL_BOOL: {
            result = val_ptr(&val.bool_val);
        } break;

        case VAL_OBJ: {
            result = val_ptr(val.obj_val);
        } break;

        default: {
            assert(!"unbekannter datentyp");
        } break;
    }

    return result;
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
            OITER(left.obj_val)->curr_index += right;

            if ( IS_OITERRNG(left.obj_val) ) {
                Obj_Iter_Range *iter = OITERRNG(left.obj_val);
                iter->iter = iter->iter + 1;
            } else {
                Obj_Iter_Array *iter = OITERARRAY(left.obj_val);
                if ( iter->curr_index < iter->array->num_elems ) {
                    iter->iter = iter->array->elems[iter->curr_index];
                }
            }

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

bool
operator==(Value left, Value right) {
    bool result = !(left != right);

    return result;
}

int32_t
bytecode_emit_jmp(Loc *loc, Bytecode *bc) {
    instr_push(loc, bc, BYTECODEOP_JMP);
    int32_t result = bc->curr;

    return result;
}

int32_t
bytecode_emit_jmp_false(Loc *loc, Bytecode *bc) {
    instr_push(loc, bc, BYTECODEOP_JMP_FALSE);
    int32_t result = bc->curr;

    return result;
}

int32_t
bytecode_push_constant(Bytecode *bc, Value value) {
    int32_t result = value_push(&bc->constants, value);

    return result;
}

int32_t
bytecode_emit_const(Loc *loc, Bytecode *bc, Value val) {
    int32_t index = bytecode_push_constant(bc, val);

    instr_push(loc, bc, BYTECODEOP_CONST, index);

    return index;
}

void
bytecode_debug(Vm *vm, Instr *instr) {
    Call_Frame *frame = vm_curr_frame(vm);
    Value_Array constants = frame->proc->bc->constants;

    switch ( instr->opcode ) {
        case BYTECODEOP_CONST: {
            printf("push ");
            val_print(value_get(&constants, instr->op1));
            printf("\n");
        } break;

        case BYTECODEOP_ADD: {
            printf("BYTECODEOP_ADD [left: ");
            val_print(vm->stack[frame->sp-2]);
            printf("] + [right: ");
            val_print(vm->stack[frame->sp-1]);
            printf("]\n");
        } break;

        case BYTECODEOP_SUB: {
            printf("BYTECODEOP_SUB [left: ");
            val_print(vm->stack[frame->sp-2]);
            printf("] - [right: ");
            val_print(vm->stack[frame->sp-1]);
            printf("]\n");
        } break;

        case BYTECODEOP_SUBSCRIPT: {
            printf("subscript ");
            val_print(vm->stack[frame->sp-2]);
            printf("[");
            val_print(vm->stack[frame->sp-1]);
            printf("]\n");
        } break;

        case BYTECODEOP_MUL: {
            printf("BYTECODEOP_MUL [left: ");
            val_print(vm->stack[frame->sp-2]);
            printf("] * [right: ");
            val_print(vm->stack[frame->sp-1]);
            printf("]\n");
        } break;

        case BYTECODEOP_DIV: {
            printf("BYTECODEOP_DIV [left: ");
            val_print(vm->stack[frame->sp-2]);
            printf("] / [right: ");
            val_print(vm->stack[frame->sp-1]);
            printf("]\n");
        } break;

        case BYTECODEOP_MATCH_CASE: {
            printf("BYTECODEOP_MATCH_CASE\n");
        } break;

        case BYTECODEOP_PUSH_SYSSYM: {
            printf("BYTECODEOP_PUSH_SYSSYM [index: %02d] (", instr->op1);
            val_print(value_get(&constants, instr->op1));
            printf(")\n");
        } break;

        case BYTECODEOP_PUSH_SYM: {
            printf("BYTECODEOP_PUSH_SYM ");
            val_print(value_get(&constants, instr->op1));
            printf("[val: ");
            val_print(vm->stack[frame->sp-1]);
            printf("]\n");
        } break;

        case BYTECODEOP_PUSH_DECL_SYM: {
            printf("BYTECODEOP_PUSH_DECL_SYM ");
            val_print(value_get(&constants, instr->op1));
            printf("[val: ");

            Value type_val = vm->stack[frame->sp-1];
            Value expr_val = vm->stack[frame->sp-2];
            val_print(expr_val.kind == VAL_NONE ? type_val : expr_val);

            printf("]\n");
        } break;

        case BYTECODEOP_CAST: {
            printf("BYTECODEOP_CAST\n");
        } break;

        case BYTECODEOP_LOAD_STRUCT_FIELD: {
            printf("BYTECODEOP_LOAD_STRUCT_FIELD [name: ");
            val_print(value_get(&constants, (uint16_t)(instr->op1)));
            printf("]\n");
        } break;

        case BYTECODEOP_LOAD_SYM: {
            printf("BYTECODEOP_LOAD_SYM [index: %02d] (", instr->op1);
            val_print(value_get(&constants, instr->op1));
            printf(")\n");
        } break;

        case BYTECODEOP_CMP_LT: {
            printf("cmp ");
            val_print(vm->stack[frame->sp-2]);
            printf(" < ");
            val_print(vm->stack[frame->sp-1]);
            printf("\n");
        } break;

        case BYTECODEOP_CMP_GT: {
            printf("cmp ");
            val_print(vm->stack[frame->sp-2]);
            printf(" > ");
            val_print(vm->stack[frame->sp-1]);
            printf("\n");
        } break;

        case BYTECODEOP_CMP_EQ: {
            printf("cmp ");
            val_print(vm->stack[frame->sp-2]);
            printf(" == ");
            val_print(vm->stack[frame->sp-1]);
            printf("\n");
        } break;

        case BYTECODEOP_JMP_FALSE: {
            printf("BYTECODEOP_JMP_FALSE [op: ");
            val_print(vm->stack[frame->sp-1]);
            printf("] -> [addr: %d]\n", (uint16_t)(instr->op1));
        } break;

        case BYTECODEOP_RANGE: {
            printf("BYTECODEOP_RANGE [left: ");
            val_print(vm->stack[frame->sp-2]);
            printf("] .. [right: ");
            val_print(vm->stack[frame->sp-1]);
            printf("]\n");
        } break;

        case BYTECODEOP_COMPOUND: {
            printf("BYTECODEOP_COMPOUND\n");
        } break;

        case BYTECODEOP_INIT_LOOP: {
            printf("BYTECODEOP_INIT_LOOP [loop var: ");
            val_print(value_get(&constants, (uint16_t)(instr->op1)));
            printf("][val: ");
            Value val = vm->stack[frame->sp-1];

            if ( IS_OITERRNG(val.obj_val) ) {
                val_print(VRANGE(val)->left);
            } else {
                val_print(VARRAY(val)->elems[0]);
            }
            printf("]\n");
        } break;

        case BYTECODEOP_CALL: {
            printf("call ");
            val_print(vm->stack[frame->sp-1]);
            //printf("(num_args: %d)\n", (uint16_t)(instr->op1));
        } break;

        case BYTECODEOP_RET: {
            printf("BYTECODEOP_RET ");

            /*
            uint32_t num_elems = instr->op1;
            for ( uint32_t i = 0; i < num_elems; ++i ) {
                val_print(vm->stack[frame->sp-(1+i)]);
            }
            */

            printf("\n");
        } break;

        case BYTECODEOP_LOAD_SYMREF: {
            Value val = value_get(&constants, (uint16_t)(instr->op1));
            printf("BYTECODEOP_LOAD_SYMREF ");
            val_print(val);
            printf("\n");
        } break;

        case BYTECODEOP_SET_SYM: {
            printf("BYTECODEOP_SET_SYM ");
            val_print(vm->stack[frame->sp-1]);
            val_print(vm->stack[frame->sp-2]);
            printf("\n");
        } break;

        case BYTECODEOP_INC_LOOP: {
            Value val = value_get(&constants, (uint16_t)(instr->op1));

            printf("BYTECODEOP_INC_LOOP ");
            val_print(val);
            printf("\n");
        } break;

        case BYTECODEOP_JMP: {
            printf("jmp %d\n", (uint16_t)(instr->op1));
        } break;

        case BYTECODEOP_PUSH: {
            Value val = value_get(&constants, (uint16_t)(instr->op1));

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

            printf("BYTECODEOP_STRUCT_FIELD [name: ");
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
            printf("BYTECODEOP_PUSH_TYPEVAL [");
            val_print(value_get(&constants, (uint16_t)(instr->op1)));
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

        case BYTECODEOP_ALLOC: {
            printf("alloc ");
            val_print(vm->stack[frame->sp-1]);
            printf("\n");
        } break;

        case BYTECODEOP_ADDR: {
            printf("@");
            val_print(vm->stack[frame->sp-1]);
            printf("\n");
        } break;

        default: {
            assert(!"unbekannter bytecode");
        } break;
    }
}

void
bytecode_op(Loc *loc, Bytecode *bc, uint32_t unary_op) {
    switch ( unary_op ) {
        case OP_SUB: {
            instr_push(loc, bc, BYTECODEOP_NEG);
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
            assert(EINT(expr)->type);
            bytecode_emit_const(expr, bc, val_int(EINT(expr)->val, EINT(expr)->type->size));
        } break;

        case EXPR_FLOAT: {
            bytecode_emit_const(expr, bc, val_float(EFLOAT(expr)->val));
        } break;

        case EXPR_STR: {
            bytecode_emit_const(expr, bc, val_str(ESTR(expr)->val));
        } break;

        case EXPR_BOOL: {
            bytecode_emit_const(expr, bc, val_bool(EBOOL(expr)->val));
        } break;

        case EXPR_COMPOUND: {
            if ( ECMPND(expr)->is_named ) {
                report_error(expr, "benamtes compound implementieren");
                instr_push(expr, bc, BYTECODEOP_NAMED_COMPOUND);
            } else {
                /* @AUFGABE: alle elemente des ausdrucks durchgehen und auf den stack holen */
                for ( int i = (int32_t)ECMPND(expr)->num_elems; i > 0; --i ) {
                    bytecode_expr(bc, ECMPND(expr)->elems[i-1]->value);
                }

                instr_push(expr, bc, BYTECODEOP_COMPOUND, (int32_t)ECMPND(expr)->num_elems);
            }
        } break;

        case EXPR_NEW: {
            bytecode_expr(bc, ENEW(expr)->expr);
            instr_push(expr, bc, BYTECODEOP_ALLOC);
        } break;

        case EXPR_FIELD: {
            bytecode_expr(bc, EFIELD(expr)->base);
            int32_t index = bytecode_push_constant(bc, val_str(EFIELD(expr)->field));

            if ( flags & BYTECODEFLAG_ASSIGNABLE ) {
                instr_push(expr, bc, BYTECODEOP_LOAD_STRUCT_FIELD_ASSIGNABLE, index);
            } else {
                instr_push(expr, bc, BYTECODEOP_LOAD_STRUCT_FIELD, index);
            }
        } break;

        case EXPR_RANGE: {
            bytecode_expr(bc, ERNG(expr)->left);
            bytecode_expr(bc, ERNG(expr)->right);

            instr_push(expr, bc, BYTECODEOP_RANGE);
        } break;

        case EXPR_PAREN: {
            bytecode_expr(bc, EPAREN(expr)->expr);
        } break;

        case EXPR_IDENT: {
            int32_t index = bytecode_push_constant(bc, val_str(EIDENT(expr)->val));

            if ( flags & BYTECODEFLAG_ASSIGNABLE ) {
                instr_push(expr, bc, BYTECODEOP_LOAD_SYMREF, index);
            } else {
                instr_push(expr, bc, BYTECODEOP_LOAD_SYM, index);
            }
        } break;

        case EXPR_CAST: {
            bytecode_typespec(bc, ECAST(expr)->typespec);
            bytecode_expr(bc, ECAST(expr)->expr);
            instr_push(expr, bc, BYTECODEOP_CAST);
        } break;

        case EXPR_SIZEOF: {
            int32_t index = bytecode_push_constant(bc, val_int(ESIZEOF(expr)->typespec->type->size));
            instr_push(expr, bc, BYTECODEOP_CONST, index);
        } break;

        case EXPR_TYPEINFO: {
            bytecode_expr(bc, ETYPEINFO(expr)->expr);
            instr_push(expr, bc, BYTECODEOP_LOAD_TYPEINFO);
        } break;

        case EXPR_TYPEOF: {
            instr_push(expr, bc, BYTECODEOP_LOAD_TYPEOF, ETYPEOF(expr)->expr->type->id);
        } break;

        case EXPR_CALL: {
            for ( int i = 0; i < ECALL(expr)->num_args; ++i ) {
                bytecode_expr(bc, ECALL(expr)->args[i]);
            }

            bytecode_expr(bc, ECALL(expr)->base);
            instr_push(expr, bc, BYTECODEOP_CALL, (int32_t)ECALL(expr)->num_args);
        } break;

        case EXPR_UNARY: {
            bytecode_expr(bc, EUNARY(expr)->expr);
            bytecode_op(expr, bc, EUNARY(expr)->op);
        } break;

        case EXPR_BIN: {
            bytecode_expr(bc, EBIN(expr)->left);
            bytecode_expr(bc, EBIN(expr)->right);

            if ( EBIN(expr)->op == OP_ADD ) {
                instr_push(expr, bc, BYTECODEOP_ADD);
            } else if ( EBIN(expr)->op == OP_SUB ) {
                instr_push(expr, bc, BYTECODEOP_SUB);
            } else if ( EBIN(expr)->op == OP_MUL ) {
                instr_push(expr, bc, BYTECODEOP_MUL);
            } else if ( EBIN(expr)->op == OP_DIV ) {
                instr_push(expr, bc, BYTECODEOP_DIV);
            } else if ( EBIN(expr)->op == OP_LT ) {
                instr_push(expr, bc, BYTECODEOP_CMP_LT);
            } else if ( EBIN(expr)->op == OP_GT ) {
                instr_push(expr, bc, BYTECODEOP_CMP_GT);
            } else if ( EBIN(expr)->op == OP_EQ ) {
                instr_push(expr, bc, BYTECODEOP_CMP_EQ);
            } else {
                report_error(expr, "unbekannter operator");
            }
        } break;

        case EXPR_AT: {
            bytecode_expr(bc, EAT(expr)->expr);
            instr_push(expr, bc, BYTECODEOP_ADDR);
        } break;

        case EXPR_INDEX: {
            bytecode_expr(bc, EINDEX(expr)->base);
            bytecode_expr(bc, EINDEX(expr)->index);
            instr_push(expr, bc, BYTECODEOP_SUBSCRIPT);
        } break;

        default: {
            report_error(expr, "unbekannter ausdruck");
        } break;
    }
}

void
bytecode_typespec(Bytecode *bc, Typespec *typespec) {
    assert(typespec);

    switch ( typespec->kind ) {
        case TYPESPEC_NAME: {
            int32_t index = bytecode_push_constant(bc, val_str(TSNAME(typespec)->name));
            instr_push(typespec, bc, BYTECODEOP_PUSH_TYPEVAL, index);
        } break;

        case TYPESPEC_PTR: {
            int32_t index = bytecode_push_constant(bc, val_ptr(NULL));
            instr_push(typespec, bc, BYTECODEOP_CONST, index);
        } break;

        case TYPESPEC_ARRAY: {
            int32_t index = bytecode_push_constant(bc, val_array(NULL, 0));
            instr_push(typespec, bc, BYTECODEOP_CONST, index);
        } break;

        default: {
            report_error(typespec, "unbekannter typespec");
        } break;
    }
}

void
bytecode_global_decl(Bytecode *bc, Stmt *stmt) {
    Decl *decl = SDECL(stmt)->decl;
}

void
bytecode_decl(Bytecode *bc, Decl *decl) {
    switch ( decl->kind ) {
        case DECL_VAR: {
            if ( DVAR(decl)->expr ) {
                bytecode_expr(bc, DVAR(decl)->expr);
            } else {
                instr_push(decl, bc, BYTECODEOP_NONE);
            }

            int32_t index = bytecode_push_constant(bc, val_str(decl->name));

            if ( DVAR(decl)->typespec ) {
                bytecode_typespec(bc, DVAR(decl)->typespec);
            } else {
                instr_push(decl, bc, BYTECODEOP_NONE);
            }

            instr_push(decl, bc, BYTECODEOP_PUSH_DECL_SYM, index);
        } break;

        case DECL_CONST: {
            if ( DCONST(decl)->expr ) {
                bytecode_expr(bc, DCONST(decl)->expr);
            } else {
                instr_push(decl, bc, BYTECODEOP_NONE);
            }

            int32_t index = bytecode_push_constant(bc, val_str(decl->name));

            if ( DCONST(decl)->typespec ) {
                bytecode_typespec(bc, DCONST(decl)->typespec);
            } else {
                instr_push(decl, bc, BYTECODEOP_NONE);
            }

            instr_push(decl, bc, BYTECODEOP_PUSH_DECL_SYM, index);
        } break;

        case DECL_PROC: {
            Value val = val_proc(decl->name, DPROC(decl)->sign->sys_call, DPROC(decl)->sign->sys_lib);

            int32_t index = bytecode_push_constant(bc, val);
            instr_push(decl, bc, BYTECODEOP_CONST, index);

            if ( decl->name == intern_str(entry_point) ) {
                entry_point_index = index;
            }

            index = bytecode_push_constant(bc, val_str(decl->name));
            instr_push(decl, bc, BYTECODEOP_PUSH_SYM, index);

            Obj_Proc *proc = as_proc(val);
            proc->name = obj_string(decl->name);
            proc->sign = DPROC(decl)->sign;
            proc->scope->name = decl->name;
            proc->num_params = (uint32_t)DPROC(decl)->sign->num_params;

            for ( int32_t i = DPROC(decl)->sign->num_params-1; i >= 0; --i ) {
                Proc_Param *param = DPROC(decl)->sign->params[i];

                index = bytecode_push_constant(proc->bc, val_str(param->name));

                instr_push(param, proc->bc, BYTECODEOP_PUSH_SYM, index);
            }

            if ( !DPROC(decl)->sign->sys_call ) {
                bytecode_stmt(proc->bc, DPROC(decl)->block);

                if ( !DPROC(decl)->sign->num_rets ) {
                    instr_push(decl, proc->bc, BYTECODEOP_RET);
                }
            }
        } break;

        case DECL_ENUM: {
            Value val = val_enum(decl->name);

            int32_t index      = bytecode_emit_const(decl, bc, val);
            int32_t name_index = bytecode_push_constant(bc, val_str(decl->name));

            instr_push(decl, bc, BYTECODEOP_PUSH_SYM, name_index);
            instr_push(decl, bc, BYTECODEOP_PUSH, index);

            for ( int i = 0; i < DENUM(decl)->num_fields; ++i ) {
                Enum_Field *field = DENUM(decl)->fields[i];

                // feldnamen generieren {
                    Value field_val = val_struct_field(field->name);
                    int32_t field_index = bytecode_push_constant(bc, field_val);
                    instr_push(field, bc, BYTECODEOP_PUSH, field_index);
                // }

                int32_t type_index = bytecode_push_constant(bc, val_str("s32"));
                instr_push(field, bc, BYTECODEOP_PUSH_TYPEVAL, type_index);

                // feldwert generieren {
                    if ( field->value ) {
                        bytecode_expr(bc, field->value);
                    } else {
                        instr_push(field, bc, BYTECODEOP_NONE);
                    }
                // }

                instr_push(field, bc, BYTECODEOP_STRUCT_FIELD);
            }

            instr_push(decl, bc, BYTECODEOP_ENUM, (int32_t)DENUM(decl)->num_fields);
        } break;

        case DECL_STRUCT: {
            Value val = val_struct(decl->name);

            int32_t index      = bytecode_emit_const(decl, bc, val);
            int32_t name_index = bytecode_push_constant(bc, val_str(decl->name));

            instr_push(decl, bc, BYTECODEOP_PUSH_SYM, name_index);
            instr_push(decl, bc, BYTECODEOP_PUSH, index);

            for ( int i = 0; i < DSTRUCT(decl)->num_fields; ++i ) {
                Struct_Field *field = DSTRUCT(decl)->fields[i];

                // feldnamen generieren {
                    Value field_val = val_struct_field(field->name);
                    int32_t field_index = bytecode_push_constant(bc, field_val);
                    instr_push(field, bc, BYTECODEOP_PUSH, field_index);
                // }

                if ( field->typespec ) {
                    bytecode_typespec(bc, field->typespec);
                } else {
                    instr_push(field, bc, BYTECODEOP_NONE);
                }

                // feldwert generieren {
                    if ( field->default_value ) {
                        bytecode_expr(bc, field->default_value);
                    } else {
                        instr_push(field, bc, BYTECODEOP_NONE);
                    }
                // }

                instr_push(field, bc, BYTECODEOP_STRUCT_FIELD);
            }

            instr_push(decl, bc, BYTECODEOP_STRUCT, (int32_t)DSTRUCT(decl)->num_fields);
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
            bytecode_expr(bc, SASSIGN(stmt)->rhs);

            if ( SASSIGN(stmt)->op->kind == T_PLUS_ASSIGN ) {
                bytecode_expr(bc, SASSIGN(stmt)->lhs);
                instr_push(SASSIGN(stmt)->lhs, bc, BYTECODEOP_ADD);
            }

            bytecode_expr(bc, SASSIGN(stmt)->lhs, BYTECODEFLAG_ASSIGNABLE);
            instr_push(stmt, bc, BYTECODEOP_SET_SYM);
        } break;

        case STMT_BLOCK: {
            for ( int i = 0; i < SBLOCK(stmt)->num_stmts; ++i ) {
                bytecode_stmt(bc, SBLOCK(stmt)->stmts[i]);
            }
        } break;

        case STMT_FOR: {
            /* @INFO: platziert einen wert auf dem stack */
            bytecode_expr(bc, SFOR(stmt)->cond);

            char *it = NULL;
            if ( SFOR(stmt)->it ) {
                it = EIDENT(SFOR(stmt)->it)->val;
            } else {
                it = "it";
            }

            uint32_t it_len = utf8_str_size(it);

            /* @INFO: platziert den namen der laufvariable */
            int32_t index = bytecode_push_constant(bc, val_str(it));

            instr_push(stmt, bc, BYTECODEOP_SCOPE_ENTER);

            /* @INFO: anweisung holt sich den namen der laufvariable und den wert */
            Instr *instr = instr_push(stmt, bc, BYTECODEOP_INIT_LOOP, index);

            uint32_t loop_addr = bc->size;
            instr_push(stmt, bc, BYTECODEOP_JMP_FALSE);
            int32_t exit_instr = bc->curr;

            /* @INFO: den eigentlichen code der schleife generieren */
            bytecode_stmt(bc, SFOR(stmt)->block);

            /* @INFO: die schleifenvariable inkrementieren und den boolischen wert für
             *        die spätere JMP_FALSE operation auf den stack laden
             */
            instr_push(stmt, bc, BYTECODEOP_INC_LOOP, index);

            /* @INFO: bedingungsloser sprung zu der JMP_FALSE operation */
            instr_push(stmt, bc, BYTECODEOP_JMP, loop_addr);
            instr_push(stmt, bc, BYTECODEOP_SCOPE_LEAVE);
            int32_t exit_addr = bc->curr;

            if ( SFOR(stmt)->stmt_else ) {
                instr_push(SFOR(stmt)->stmt_else, bc, BYTECODEOP_SCOPE_LEAVE);
                instr_push(SFOR(stmt)->stmt_else, bc, BYTECODEOP_SCOPE_ENTER);
                instr->op2 = bc->curr;
                bytecode_stmt(bc, SFOR(stmt)->stmt_else);
                instr_push(SFOR(stmt)->stmt_else, bc, BYTECODEOP_SCOPE_LEAVE);
                exit_addr = bc->curr;
            } else {
                /* @INFO: nur zur sicherheit patchen wir auch op2 zu ausstiegsadresse */
                instr->op2 = exit_addr;
            }

            instr_patch(bc, exit_instr, exit_addr);
        } break;

        case STMT_MATCH: {
            int32_t *exit_addrs = NULL;
            for ( int i = 0; i < SMATCH(stmt)->num_lines; ++i ) {
                Match_Line *line = SMATCH(stmt)->lines[i];

                /* @INFO: den anfangswert auf den stack holen */
                bytecode_expr(bc, SMATCH(stmt)->expr);

                /* @INFO: den case wert auf den stack holen */
                bytecode_expr(bc, line->resolution);

                /* @INFO: die beiden werte vergleichen */
                instr_push(stmt, bc, BYTECODEOP_MATCH_CASE);
                /* @INFO: platzhalter für die sprungadresse schreiben */
                int32_t addr = bc->curr;

                /* @INFO: die case anweisungen kodieren */
                bytecode_stmt(bc, line->stmt);

                /* @INFO: zum schluß ans ende der gesamten match anweisung springen */
                instr_push(stmt, bc, BYTECODEOP_JMP);
                buf_push(exit_addrs, bc->curr);

                /* @INFO: die adresse für MATCH_CASE patchen für den fall, daß die anweisung
                 *        nicht zutrifft und zum nächsten case gesprungen werden muß
                 */
                instr_patch(bc, addr, bc->size);
            }

            for ( int i = 0; i < buf_len(exit_addrs); ++i ) {
                instr_patch(bc, exit_addrs[i], bc->size);
            }
        } break;

        case STMT_WHILE: {
            instr_push(stmt, bc, BYTECODEOP_SCOPE_ENTER);

            /* @INFO: platziert den entscheidungswert auf dem stack */
            uint32_t loop_addr = bc->size;
            bytecode_expr(bc, SWHILE(stmt)->cond);

            instr_push(stmt, bc, BYTECODEOP_JMP_FALSE);
            int32_t exit_instr = bc->curr;

            /* @INFO: den eigentlichen code der schleife generieren */
            bytecode_stmt(bc, SWHILE(stmt)->block);

            /* @INFO: bedingungsloser sprung zu der JMP_FALSE operation */
            instr_push(stmt, bc, BYTECODEOP_JMP, loop_addr);
            int32_t exit_addr = bc->size;

            instr_push(stmt, bc, BYTECODEOP_SCOPE_LEAVE);
            instr_patch(bc, exit_instr, (int32_t)exit_addr);
        } break;

        case STMT_DECL: {
            bytecode_decl(bc, SDECL(stmt)->decl);
        } break;

        case STMT_IF: {
            bytecode_expr(bc, SIF(stmt)->cond);

            int32_t addr = bytecode_emit_jmp_false(stmt, bc);
            instr_push(stmt, bc, BYTECODEOP_SCOPE_ENTER);
            bytecode_stmt(bc, SIF(stmt)->stmt);
            instr_push(stmt, bc, BYTECODEOP_SCOPE_LEAVE);
            int32_t exit_addr = bytecode_emit_jmp(stmt, bc);
            instr_patch(bc, addr, (int32_t)bc->size);

            if ( SIF(stmt)->stmt_else ) {
                bytecode_stmt(bc, SIF(stmt)->stmt_else);
            }

            instr_patch(bc, exit_addr, (int32_t)bc->size);
        } break;

        case STMT_RET: {
            for ( uint32_t i = 0; i < SRET(stmt)->num_exprs; ++i ) {
                bytecode_expr(bc, SRET(stmt)->exprs[i]);
            }

            instr_push(stmt, bc, BYTECODEOP_RET, (int32_t)SRET(stmt)->num_exprs);
        } break;

        case STMT_EXPR: {
            bytecode_expr(bc, SEXPR(stmt)->expr);
        } break;

        case STMT_USING: {
            bytecode_expr(bc, SUSING(stmt)->expr);
            instr_push(stmt, bc, BYTECODEOP_IMPORT_SYMS);
        } break;

        default: {
            report_error(stmt, "unbekannte anweisung");
        } break;
    }
}

char *
to_str(Obj_String *str) {
    return str->ptr;
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

void
call_sys_proc(Vm *vm, Value val, uint32_t num_args) {
    DCCallVM *call_vm = dcNewCallVM(1024);
    dcMode(call_vm, DC_CALL_C_DEFAULT);

    Obj_Proc *proc = as_proc(val);

    auto lib = LoadLibrary(proc->lib);
    assert(lib);
    auto proc_ptr = GetProcAddress(lib, to_str(proc->name));
    assert(proc_ptr);

    Values args = NULL;
    for ( int32_t i = num_args; i > 0; --i ) {
        buf_push(args, stack_pop(vm));
    }

    for ( int32_t i = num_args; i > 0; --i ) {
        Value arg = args[i-1];

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

    assert(IS_VPROC(val));
    Obj_Proc *proc = as_proc(val);

    frame->proc = proc;
    frame->pc   = 0;
    frame->sp   = prev_frame->sp;
    frame->prev_scope = curr_scope;

    curr_scope = proc->scope;
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

    ns->prev_scope = curr_scope;
    curr_scope = ns->scope;
}

void
namespace_leave(Vm *vm, Obj_Namespace *ns) {
    assert(vm->frame_num > 0);

    vm->frame_num--;

    curr_scope = ns->prev_scope;
    Scope *prev_scope = curr_scope;

    if ( ns->scope_name ) {
        Obj_Namespace *new_ns = obj_namespace(ns->scope_name, ns->scope_name);
        bytecode_sym_set(new_ns->name, val_namespace(new_ns));
        new_ns->scope->parent = NULL;
        curr_scope = new_ns->scope;
    }

    for ( int i = 0; i < buf_len(ns->scope->export_syms.ordered_entries); ++i ) {
        Table_Entry *entry = ns->scope->export_syms.ordered_entries[i];

        bytecode_sym_set(entry->key, entry->val);
    }

    if ( ns->scope_name ) {
        curr_scope = prev_scope;
    }
}

bool
bytecode_sym_get(Obj_String *key, Value *val) {
    Scope *it = curr_scope;

    while ( it ) {
        if ( table_get(&it->syms, key, val) ) {
            return true;
        }

        it = it->parent;
    }

    return false;
}

Value *
bytecode_sym_get_ref(Obj_String *key) {
    Scope *it = curr_scope;
    Value *result = NULL;

    while ( it ) {
        result = table_get_ref(&it->syms, key);

        if ( result ) {
            return result;
        }

        it = it->parent;
    }

    return result;
}

bool
bytecode_sym_set(Obj_String *key, Value val, Scope *scope, uint32_t flags = BYTECODEFLAG_NONE) {
    if ( flags & BYTECODEFLAG_SET_EXISTING ) {
        Scope *it = scope;

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
    bool result = bytecode_sym_set(key, val, curr_scope, flags);

    return result;
}

void
bytecode_directive(Bytecode *bc, Directive *dir) {
    switch ( dir->kind ) {
        case DIRECTIVE_IMPORT: {
            Obj_Namespace *ns = obj_namespace(dir->file, DIRIMPORT(dir)->scope_name);
            Value ns_val = val_namespace(ns);

            bytecode_build_file(ns->proc->bc, DIRIMPORT(dir)->parsed_file);

            bytecode_emit_const(dir, ns->proc->bc, ns_val);
            instr_push(dir, ns->proc->bc, BYTECODEOP_NAMESPACE_LEAVE);

            bytecode_emit_const(dir, bc, ns_val);
            instr_push(dir, bc, BYTECODEOP_NAMESPACE_ENTER);
        } break;

        case DIRECTIVE_EXPORT: {
            for ( int i = 0; i < DIREXPORT(dir)->num_syms; ++i ) {
                Module_Sym *sym = DIREXPORT(dir)->syms[i];

                bytecode_emit_const(dir, bc, val_str(sym->name));

                if ( sym->alias ) {
                    bytecode_emit_const(dir, bc, val_str(sym->alias));
                } else {
                    instr_push(dir, bc, BYTECODEOP_NONE);
                }

                instr_push(dir, bc, BYTECODEOP_EXPORT_SYM);
            }
        } break;

        case DIRECTIVE_LOAD: {
            bytecode_build_file(bc, DIRLOAD(dir)->parsed_file);
        } break;

        default: {
            assert(0);
        } break;
    }
}

bool
step(Vm *vm) {
    Call_Frame *frame = vm_curr_frame(vm);

    if ( frame->pc >= frame->proc->bc->size ) {
        return false;
    }

    Instr *instr = instr_fetch(vm);
#if BYTECODE_DEBUG
    bytecode_debug(vm, instr);
#endif

    switch ( instr->opcode ) {
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

        case BYTECODEOP_SUBSCRIPT: {
            Value index = stack_pop(vm);
            Value base  = stack_pop(vm);

            if ( IS_VCMPND(base) ) {
                base = val_array(VCMPND(base)->elems, VCMPND(base)->num_elems);
            }

            assert(IS_VARRAY(base));
            Obj_Array *arr = (Obj_Array *)base.obj_val;

            if ( index.int_val >= arr->num_elems ) {
                report_error(instr, "index liegt außerhalb der array grenzen");
            }

            stack_push(vm, arr->elems[index.int_val]);
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
            int32_t index = instr->op1;
            Value val = value_get(&frame->proc->bc->constants, index);

            stack_push(vm, val);
        } break;

        case BYTECODEOP_PUSH_SYSSYM: {
            Scope *prev_scope = curr_scope;
            curr_scope = &sys_scope;

            Obj_String *name = VSTR(value_get(&frame->proc->bc->constants, instr->op1));
            Value val = stack_pop(vm);

            if ( !bytecode_sym_set(name, val) ) {
                assert(!"symbol konnte nicht gesetzt werden");
            }

            curr_scope = prev_scope;
        } break;

        case BYTECODEOP_EXPORT_SYM: {
            Value alias = stack_pop(vm);
            Value name  = stack_pop(vm);

            Value val;
            if ( !bytecode_sym_get(VSTR(name), &val) ) {
                assert(!"symbol nicht gefunden");
            }

            Value new_name = alias.kind == VAL_NONE ? name : alias;
            table_set(&curr_scope->export_syms, VSTR(new_name), val);
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
            Obj_String *name = VSTR(value_get(&frame->proc->bc->constants, instr->op1));

            Value val = stack_pop(vm);
            bytecode_sym_set(name, val);

            if ( IS_VPROC(val) ) {
                as_proc(val)->scope->parent = curr_scope;
            }
        } break;

        case BYTECODEOP_PUSH_DECL_SYM: {
            Obj_String *name = VSTR(value_get(&frame->proc->bc->constants, instr->op1));

            Value type_val = stack_pop(vm);
            Value expr_val = stack_pop(vm);

            Value val = (expr_val.kind == VAL_NONE) ? type_val : expr_val;

            if ( IS_VCMPND(expr_val) ) {
                if ( IS_VSTRUCT(type_val) ) {
                    Value structure = val_struct(VSTRUCT(type_val)->name);

                    int32_t num_elems = buf_len(VSTRUCT(type_val)->fieldnames_ordered);

                    for ( int i = 0; i < num_elems; ++i ) {
                        Obj_String *field_name = VSTRUCT(type_val)->fieldnames_ordered[i];
                        Value field_val = VCMPND(expr_val)->elems[i];

                        table_set(VSTRUCT(structure)->fields, field_name, field_val);
                        buf_push(VSTRUCT(structure)->fieldnames_ordered, field_name);
                    }

                    val = structure;
                } else if ( IS_VARRAY(type_val) ) {
                    val = val_array(VCMPND(val)->elems, VCMPND(val)->num_elems);
                }
            }

            bytecode_sym_set(name, val);
            if ( IS_VPROC(val) ) {
                as_proc(val)->scope->parent = curr_scope;
            }
        } break;

        case BYTECODEOP_INIT_LOOP: {
            Obj_String *name = VSTR(value_get(&frame->proc->bc->constants, instr->op1));
            Value iter = stack_pop(vm);

            if ( IS_VCMPND(iter) ) {
                iter = val_array(VCMPND(iter)->elems, VCMPND(iter)->num_elems);
            }

            if ( IS_VRANGE(iter) ) {
                Value diff = VRANGE(iter)->right - VRANGE(iter)->left;
                if ( diff <= (int64_t)0 ) {
                    frame->pc = instr->op2;
                }

                bytecode_sym_set(name, val_iter_range(VRANGE(iter), VRANGE(iter)->left));
                stack_push(vm, val_bool(VRANGE(iter)->left < VRANGE(iter)->right));
            } else {
                assert(IS_VARRAY(iter));

                if ( VARRAY(iter)->num_elems == 0 ) {
                    frame->pc = instr->op2;
                } else {
                    bytecode_sym_set(name, val_iter_array(VARRAY(iter), VARRAY(iter)->elems[0]));
                    stack_push(vm, val_bool(0 < VARRAY(iter)->num_elems));
                }
            }
        } break;

        case BYTECODEOP_INC_LOOP: {
            int32_t index = instr->op1;
            Obj_String *name = VSTR(value_get(&frame->proc->bc->constants, index));

            Value val;
            if ( !bytecode_sym_get(name, &val) ) {
                assert(!"symbol nicht gefunden");
            }

            bytecode_sym_set(name, val + 1);
            Obj_Iter *iter = ((Obj_Iter *)val.obj_val);

            if ( iter->iter_kind == ITER_RANGE ) {
                Obj_Iter_Range *iter_range = OITERRNG(iter);

                Value cond = val_bool(iter_range->iter < iter_range->range->right);
                stack_push(vm, cond);
            } else {
                assert(IS_OITERARRAY(iter));

                Obj_Iter_Array *iter_array = OITERARRAY(iter);

                Value cond = val_bool(iter_array->curr_index < iter_array->array->num_elems);
                stack_push(vm, cond);
            }
        } break;

        case BYTECODEOP_LOAD_SYM: {
            Obj_String *name = VSTR(value_get(&frame->proc->bc->constants, instr->op1));
            Value val;

            if ( !bytecode_sym_get(name, &val) ) {
                assert(!"symbol nicht gefunden");
            }

            if ( IS_VOBJ(val) && IS_OITER(val.obj_val)) {
                stack_push(vm, OITER(val.obj_val)->iter);
            } else {
                stack_push(vm, val);
            }
        } break;

        case BYTECODEOP_LOAD_SYMREF: {
            Value name = value_get(&frame->proc->bc->constants, instr->op1);
            stack_push(vm, val_ptr(bytecode_sym_get_ref(VSTR(name))));
        } break;

        case BYTECODEOP_LOAD_TYPEINFO: {
            Value val = stack_pop(vm);
            stack_push(vm, val_type(obj_types[val.int_val]));
        } break;

        case BYTECODEOP_LOAD_TYPEOF: {
            stack_push(vm, val_int(instr->op1));
        } break;

        case BYTECODEOP_LOAD_STRUCT_FIELD_ASSIGNABLE: {
            Value field = value_get(&frame->proc->bc->constants, instr->op1);
            Value val = stack_pop(vm);

            assert( IS_VSTRUCT(val) );

            Obj_Struct *structure = ((Obj_Struct *)val.obj_val);
            Value *field_val = table_get_ref(structure->fields, VSTR(field));

            stack_push(vm, val_ptr(field_val));
        } break;

        case BYTECODEOP_LOAD_STRUCT_FIELD: {
            Value field = value_get(&frame->proc->bc->constants, instr->op1);
            Value val = stack_pop(vm);

            if ( IS_VSTRUCT(val) ) {
                Obj_Struct *structure = ((Obj_Struct *)val.obj_val);

                Value field_val;
                if ( !table_get(structure->fields, VSTR(field), &field_val) ) {
                    assert(!"feld konnte nicht gefunden werden");
                }

                stack_push(vm, field_val);
            } else if ( IS_VENUM(val) ) {
                Obj_Enum *enumeration = ((Obj_Enum *)val.obj_val);

                Value field_val;
                if ( !table_get(enumeration->fields, VSTR(field), &field_val) ) {
                    assert(!"feld konnte nicht gefunden werden");
                }

                stack_push(vm, field_val);
            } else if ( IS_VOBJ(val) ) {
                Obj *obj = val.obj_val;

                Value field_val;
                if ( !table_get(&obj->scope->syms, VSTR(field), &field_val) ) {
                    assert(!"feld konnte nicht gefunden werden");
                }

                stack_push(vm, field_val);
            }
        } break;

        case BYTECODEOP_SET_SYM: {
#if 0
            Obj_String *name = VSTR(stack_pop(vm));
            Value val = stack_pop(vm);

            bytecode_sym_set(name, val, BYTECODEFLAG_SET_EXISTING);
#else
            Value var = stack_pop(vm);
            Value val = stack_pop(vm);

            assert(IS_VPTR(var));

            *((Value *)VPTR(var)->ptr) = val;
#endif
        } break;

        case BYTECODEOP_CALL: {
            Value val = stack_pop(vm);
            int32_t num_args = instr->op1;

            if ( as_proc(val)->sys_call ) {
                call_sys_proc(vm, val, num_args);
            } else {
                call_proc(vm, val, num_args);
            }
        } break;

        case BYTECODEOP_JMP: {
            int32_t addr = instr->op1;
            frame->pc = addr;
        } break;

        case BYTECODEOP_JMP_FALSE: {
            Value val = stack_pop(vm);
            int32_t addr = instr->op1;

            if ( val.bool_val == false ) {
                frame->pc = addr;
            }
        } break;

        case BYTECODEOP_CMP_LT: {
            Value right = stack_pop(vm);
            Value left  = stack_pop(vm);

            stack_push(vm, val_bool(left < right));
        } break;

        case BYTECODEOP_CMP_GT: {
            Value right = stack_pop(vm);
            Value left  = stack_pop(vm);

            stack_push(vm, val_bool(right < left));
        } break;

        case BYTECODEOP_CMP_EQ: {
            Value right = stack_pop(vm);
            Value left  = stack_pop(vm);

            stack_push(vm, val_bool(right == left));
        } break;

        case BYTECODEOP_INC: {
            int32_t index = instr->op1;
            Value val = value_get(&frame->proc->bc->constants, index);
            value_set(&frame->proc->bc->constants, index, val + 1);
        } break;

        case BYTECODEOP_RANGE: {
            Value right = stack_pop(vm);
            Value left  = stack_pop(vm);

            stack_push(vm, val_range(left, right));
        } break;

        case BYTECODEOP_COMPOUND: {
            int32_t num_elems = instr->op1;

            Values elems = NULL;
            for ( int i = 0; i < num_elems; ++i ) {
                buf_push(elems, stack_pop(vm));
            }

            stack_push(vm, val_compound(elems, num_elems));
        } break;

        case BYTECODEOP_HLT: {
            return false;
        } break;

        case BYTECODEOP_PUSH: {
            int32_t index = instr->op1;
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
            int32_t num_elems = instr->op1;

            Values elems = NULL;
            for ( int i = 0; i < num_elems; ++i ) {
                Value val = stack_pop(vm);
                buf_push(elems, val);
            }

            vm->frame_num -= 1;

            for ( int i = 0; i < num_elems; ++i ) {
                stack_push(vm, elems[i]);
            }

            curr_scope = frame->prev_scope;
        } break;

        case BYTECODEOP_SCOPE_ENTER: {
            bytecode_scope_enter();
        } break;

        case BYTECODEOP_SCOPE_LEAVE: {
            bytecode_scope_leave();
        } break;

        case BYTECODEOP_STRUCT: {
            int32_t num_fields = instr->op1;

            Values elems = NULL;
            for ( int i = 0; i < num_fields; ++i ) {
                buf_push(elems, stack_pop(vm));
            }

            Value structure = stack_pop(vm);

            for ( int i = 0; i < num_fields; ++i ) {
                Obj_Struct_Field *field = ((Obj_Struct_Field *)elems[i].obj_val);
                table_set(((Obj_Struct *)structure.obj_val)->fields, field->name, field->default_value);
                buf_push(((Obj_Struct *)structure.obj_val)->fieldnames_ordered, field->name);
            }
        } break;

        case BYTECODEOP_ENUM: {
            int32_t num_fields = instr->op1;

            Values elems = NULL;
            for ( int i = 0; i < num_fields; ++i ) {
                buf_push(elems, stack_pop(vm));
            }

            Value enumeration = stack_pop(vm);

            for ( int i = 0; i < num_fields; ++i ) {
                Obj_Struct_Field *field = ((Obj_Struct_Field *)elems[i].obj_val);
                table_set(((Obj_Enum *)enumeration.obj_val)->fields, field->name, field->default_value);
                buf_push(((Obj_Enum *)enumeration.obj_val)->fieldnames_ordered, field->name);
            }
        } break;

        case BYTECODEOP_PUSH_TYPEVAL: {
            int32_t index = instr->op1;
            Value val = value_get(&frame->proc->bc->constants, index);

            Value type_val;
            if ( !bytecode_sym_get(VSTR(val), &type_val) ) {
                assert(!"unbekannter datentyp");
            }

            stack_push(vm, type_val);
        } break;

        case BYTECODEOP_MATCH_CASE: {
            Value   resolution = stack_pop(vm);
            Value   expr       = stack_pop(vm);
            int32_t addr       = instr->op1;

            if ( expr != resolution ) {
                frame->pc = addr;
            }
        } break;

        case BYTECODEOP_NEG: {
            Value val = stack_pop(vm);
            stack_push(vm, -val);
        } break;

        case BYTECODEOP_ALLOC: {
            stack_push(vm, val_to_ptr(stack_pop(vm)));
        } break;

        case BYTECODEOP_ADDR: {
            stack_push(vm, val_to_ptr(stack_pop(vm)));
        } break;

        case BYTECODEOP_IMPORT_SYMS: {
            Value val = stack_pop(vm);
            assert(IS_VNS(val));

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
                               new_val   = val_struct(((Obj_String *)structure->name)->ptr);

                for ( int i = 0; i < compound->num_elems; ++i ) {
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

Obj_Type *
obj_type_from_type(Type *type) {
    Obj_Type *result = NULL;

    switch ( type->kind ) {
        case TYPE_VOID: {
            result = obj_type(type->id, "void", type->size);
        } break;

        case TYPE_U8: {
            result = obj_type(type->id, "u8", type->size);
        } break;

        case TYPE_U16: {
            result = obj_type(type->id, "u16", type->size);
        } break;

        case TYPE_U32: {
            result = obj_type(type->id, "u32", type->size);
        } break;

        case TYPE_U64: {
            result = obj_type(type->id, "u64", type->size);
        } break;

        case TYPE_S8: {
            result = obj_type(type->id, "s8", type->size);
        } break;

        case TYPE_S16: {
            result = obj_type(type->id, "s16", type->size);
        } break;

        case TYPE_S32: {
            result = obj_type(type->id, "s32", type->size);
        } break;

        case TYPE_S64: {
            result = obj_type(type->id, "s64", type->size);
        } break;

        case TYPE_F32: {
            result = obj_type(type->id, "f32", type->size);
        } break;

        case TYPE_F64: {
            result = obj_type(type->id, "f64", type->size);
        } break;

        case TYPE_STRING: {
            result = obj_type(type->id, "string", type->size);
        } break;

        case TYPE_BOOL: {
            result = obj_type(type->id, "bool", type->size);
        } break;

        case TYPE_PTR: {
            result = obj_type(type->id, NULL, type->size);

            result->base = obj_type_from_type(TPTR(type)->base);
        } break;

        case TYPE_STRUCT: {
            uint32_t num_fields = (uint32_t)TSTRUCT(type)->num_fields;
            Values fields = (Values)urq_alloc(sizeof(Value)*num_fields);

            for ( uint32_t i = 0; i < num_fields; ++i ) {
                Struct_Field *field = TSTRUCT(type)->fields[i];

                fields[i] = val_type_field(field->type->id, field->name);
            }

            result = obj_type(type->id, TSTRUCT(type)->sym->name, type->size, fields, num_fields);
        } break;

        case TYPE_ENUM: {
            uint32_t num_fields = (uint32_t)TENUM(type)->num_fields;
            Values fields = (Values)urq_alloc(sizeof(Value)*num_fields);

            for ( uint32_t i = 0; i < num_fields; ++i ) {
                Enum_Field *field = TENUM(type)->fields[i];

                fields[i] = val_type_field(field->type->id, field->name);
            }

            result = obj_type(type->id, TENUM(type)->sym->name, type->size, fields, num_fields);
        } break;

        case TYPE_PROC: {
            uint32_t num_params = (uint32_t)TPROC(type)->num_params;
            Values params = (Values)urq_alloc(sizeof(Value)*num_params);

            for ( uint32_t i = 0; i < num_params; ++i ) {
                Proc_Param *param = TPROC(type)->params[i];

                params[i] = val_type_field(param->type->id, param->name);
            }

            uint32_t num_rets = (uint32_t)TPROC(type)->num_rets;
            Values rets = (Values)urq_alloc(sizeof(Value)*num_rets);

            for ( uint32_t i = 0; i < num_rets; ++i ) {
                Proc_Param *ret = TPROC(type)->rets[i];

                rets[i] = val_type_field(ret->type->id, ret->name);
            }

            result = obj_type(type->id, TPROC(type)->sym->name, type->size, params, num_params, rets, num_rets);
        } break;

        case TYPE_ARRAY: {
            result = obj_type(type->id, NULL, type->size);

            result->base = obj_type_from_type(TARRAY(type)->base);
        } break;

        case TYPE_NAMESPACE: {
            result = obj_type(type->id, type->name, type->size);
        } break;

        case TYPE_COMPOUND: {
            result = obj_type(type->id, NULL, type->size);

            result->num_fields = (uint32_t)TCMPND(type)->num_elems;
            result->fields = (Values)urq_alloc(sizeof(Value)*result->num_fields);
            for ( uint32_t i = 0; i < result->num_fields; ++i ) {
                result->params[i] = val_type(obj_type_from_type(TCMPND(type)->elems[i]->type));
            }
        } break;

        case TYPE_VARIADIC: {
            result = obj_type(type->id, NULL, type->size);

            result->num_fields = (uint32_t)TVARIADIC(type)->num_types;
            result->fields = (Values)urq_alloc(sizeof(Value)*result->num_fields);
            for ( uint32_t i = 0; i < result->num_fields; ++i ) {
                result->params[i] = val_type(obj_type_from_type(TVARIADIC(type)->types[i]));
            }
        } break;
    }

    return result;
}

void
bytecode_gen_typeinfo() {
    int32_t num_types = buf_len(types);
    obj_types = (Obj_Type **)urq_alloc(sizeof(Obj_Type)*num_types);

    for ( int i = 0; i < num_types; ++i ) {
        obj_types[types[i]->id] = obj_type_from_type(types[i]);
    }
}

void
bytecode_init(Bytecode *bc) {
#define REGISTER_TYPE(Val, Name)                                       \
    do {                                                                     \
            Scope *prev_scope = curr_scope;                                  \
            curr_scope = &sys_scope;                                         \
            int32_t index = bytecode_push_constant(bc, val_str(Name)); \
            Obj_String *name = VSTR(value_get(&bc->constants, index));     \
            if ( !bytecode_sym_set(name, Val) ) {                            \
                assert(!"symbol konnte nicht gesetzt werden");               \
            }                                                                \
            curr_scope = prev_scope;                                         \
    } while(0)

    REGISTER_TYPE(val_none(),      "void");
    REGISTER_TYPE(val_int(0),      "u8");
    REGISTER_TYPE(val_int(0),      "u16");
    REGISTER_TYPE(val_int(0),      "u32");
    REGISTER_TYPE(val_int(0),      "u64");
    REGISTER_TYPE(val_int(0),      "s8");
    REGISTER_TYPE(val_int(0),      "s16");
    REGISTER_TYPE(val_int(0),      "s32");
    REGISTER_TYPE(val_int(0),      "s64");
    REGISTER_TYPE(val_float(0.0f), "f32");
    REGISTER_TYPE(val_float(0.0f), "f64");
    REGISTER_TYPE(val_bool(false), "bool");
    REGISTER_TYPE(val_str(""),     "string");

    bytecode_gen_typeinfo();
#undef REGISTER_TYPE
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

Bytecode *
optimize(Bytecode *bc) {
    Basic_Block **bbs = NULL;
    Instrs instrs = NULL;

    Value val = value_get(&bc->constants, entry_point_index);
    assert(IS_VPROC(val));
    Obj_Proc *proc = VPROC(val);
    Bytecode *code = proc->bc;

    for ( int i = 0; i < code->size; ++i ) {
        Instr *instr = code->instructions[i];

        if (instr->opcode == BYTECODEOP_JMP_FALSE ||
            instr->opcode == BYTECODEOP_JMP       ||
            instr->opcode == BYTECODEOP_CALL      ||
            instr->opcode == BYTECODEOP_RET        )
        {
            buf_push(instrs, instr);

            if ( buf_len(instrs) > 0 ) {
                buf_push(bbs, basic_block(buf_len(bbs), instrs));
            }

            instrs = NULL;

            if ( instr->opcode == BYTECODEOP_CALL ) {
            }
        } else if ( instr->opcode == BYTECODEOP_NAMESPACE_ENTER ) {
            // später gucken was hier gemacht werden kann
        } else {
            buf_push(instrs, instr);
        }
    }

    return bc;
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
bytecode_build_load_directives(Bytecode *bc, Directives dirs) {
    for ( int i = 0; i < buf_len(dirs); ++i ) {
        Directive *dir = dirs[i];

        if ( dir->kind == DIRECTIVE_LOAD ) {
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
bytecode_build_file(Bytecode *bc, Parsed_File *file) {
    bytecode_build_import_directives(bc, file->directives);
    bytecode_build_load_directives(bc, file->directives);
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

}
