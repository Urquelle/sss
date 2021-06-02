enum {
    OBJ_MAGIC_NUM1         = 0x7F,
    OBJ_MAGIC_NUM2         = 0x45,
    OBJ_MAGIC_NUM3         = 0x4c,
    OBJ_MAGIC_NUM4         = 0x46,
};

enum {
    OBJ_MAG0,
    OBJ_MAG1,
    OBJ_MAG2,
    OBJ_MAG3,
};

struct Obj_Header {
    uint8_t  magic[4];
    uint64_t size;
    uint64_t file_size;
    uint64_t rdata_offset;
    uint64_t rdata_size;
    uint64_t text_offset;
    uint64_t text_size;
    uint64_t data_offset;
    uint64_t data_size;
    uint64_t entry;
};

uint8_t *
obj_create(uint8_t *rdata, uint64_t rdata_size, uint8_t *text, uint64_t text_size,
        uint8_t *data, uint64_t data_size, uint64_t entry)
{
    uint64_t stack_size = 1024;

    uint64_t result_size = sizeof(Obj_Header) + rdata_size + text_size + data_size + stack_size;
    uint8_t *result = (uint8_t *)urq_alloc(result_size);

    Obj_Header *header = (Obj_Header *)result;

    result[OBJ_MAG0] = OBJ_MAGIC_NUM1;
    result[OBJ_MAG1] = OBJ_MAGIC_NUM2;
    result[OBJ_MAG2] = OBJ_MAGIC_NUM3;
    result[OBJ_MAG3] = OBJ_MAGIC_NUM4;

    header->size         = result_size - sizeof(Obj_Header);
    header->file_size    = result_size;
    header->rdata_offset = sizeof(Obj_Header);
    header->rdata_size   = rdata_size;
    header->text_offset  = header->rdata_offset + header->rdata_size;
    header->text_size    = text_size;
    header->data_offset  = header->text_offset + header->text_size;
    header->data_size    = data_size;
    header->entry        = entry;

    uint8_t *rdata_ptr = result + header->rdata_offset;
    uint8_t *text_ptr  = result + header->text_offset;
    uint8_t *data_ptr  = result + header->data_offset;

    memcpy(rdata_ptr, rdata, rdata_size);
    memcpy(text_ptr,  text,  text_size);
    memcpy(data_ptr,  data,  data_size);

    return result;
}

bool
obj_valid(uint8_t *bin) {
    if ( !bin ) {
        return false;
    }

    if ( bin[OBJ_MAG0] != OBJ_MAGIC_NUM1 ||
         bin[OBJ_MAG1] != OBJ_MAGIC_NUM2 ||
         bin[OBJ_MAG2] != OBJ_MAGIC_NUM3 ||
         bin[OBJ_MAG3] != OBJ_MAGIC_NUM4 )
    {
        return false;
    }

    return true;
}

Obj_Header *
obj_header(uint8_t *bin) {
    if ( !obj_valid(bin) ) {
        report_error(&loc_none, "ungültige sss datei übergeben");
    }

    Obj_Header *result = (Obj_Header *)bin;

    return result;
}

uint64_t
obj_size(uint8_t *obj) {
    Obj_Header *header = obj_header(obj);

    return header->file_size;
}

uint8_t *
obj_text_section(uint8_t *bin) {
    if ( !obj_valid(bin) ) {
        report_error(&loc_none, "ungültige sss datei übergeben");
    }

    Obj_Header *header = (Obj_Header *)bin;

    uint8_t *result = (bin + header->text_offset);

    return result;
}

uint64_t
obj_text_num_entries(uint8_t *bin) {
    if ( !obj_valid(bin) ) {
        report_error(&loc_none, "ungültige sss datei übergeben");
    }

    Obj_Header *header = (Obj_Header *)bin;

    uint64_t result = header->text_size / sizeof(void *);

    return result;
}

