enum Elf_Os_Abi {
    ELF_ABI_SYSTEM_V       = 0x00,
    ELF_ABI_HP_UX          = 0x01,
    ELF_ABI_NETBSD         = 0x02,
    ELF_ABI_LINUX          = 0x03,
    ELF_ABI_GNU_HURD       = 0x04,
    ELF_ABI_SOLARIS        = 0x06,
    ELF_ABI_AIX            = 0x07,
    ELF_ABI_IRIX           = 0x08,
    ELF_ABI_FREEBSD        = 0x09,
    ELF_ABI_TRU64          = 0x0A,
    ELF_ABI_NOVELL_MODESTO = 0x0B,
    ELF_ABI_OPENBSD        = 0x0C,
    ELF_ABI_OPENVMS        = 0x0D,
    ELF_ABI_NONSTOP_KERNEL = 0x0E,
    ELF_ABI_AROS           = 0x0F,
    ELF_ABI_FENIX_OS       = 0x10,
    ELF_ABI_CLOUDABI       = 0x11,
    ELF_ABI_OPENVOS        = 0x12,
    ELF_ABI_SSS            = 0x13,
};

enum Elf_Data {
    ELF_DATA_LITTLE        = 1,
    ELF_DATA_BIG           = 2,
};

enum Elf_Class {
    ELF_CLASS_32            = 1,
    ELF_CLASS_64            = 2,
};

enum Elf_Version {
    ELF_VERSION_1          = 1,
    ELF_VERSION_CURRENT    = ELF_VERSION_1,
};

enum Elf_Type {
    ET_NONE                = 0x00,
    ET_REL                 = 0x01,
    ET_EXEC                = 0x02,
    ET_DYN                 = 0x03,
    ET_CORE                = 0x04,
    ET_LOOS                = 0xFE00,
    ET_HIOS                = 0xFEFF,
    ET_LOPROC              = 0xFF00,
    ET_HIPROC              = 0xFFFF,
};

enum Elf_Machine {
    ELF_MACHINE_X86_64     = 0x3E,
};

enum {
    ELF_MAGIC_NUM1         = 0x7F,
    ELF_MAGIC_NUM2         = 0x45,
    ELF_MAGIC_NUM3         = 0x4c,
    ELF_MAGIC_NUM4         = 0x46,
};

enum {
    EI_MAG0,
    EI_MAG1,
    EI_MAG2,
    EI_MAG3,
    EI_CLASS,
    EI_DATA,
    EI_VERSION,
    EI_OSABI,
    EI_ABIVERSION,
};

enum Elf_Program_Header_Type {
    PT_NULL                = 0x00000000,
    PT_LOAD                = 0x00000001,
    PT_DYNAMIC             = 0x00000002,
    PT_INTERP              = 0x00000003,
    PT_NOTE                = 0x00000004,
    PT_SHLIB               = 0x00000005,
    PT_PHDR                = 0x00000006,
    PT_TLS                 = 0x00000007,
    PT_LOOS                = 0x60000000,
    PT_HIOS                = 0x6FFFFFFF,
    PT_LOPROC              = 0x70000000,
    PT_HIPROC              = 0x7FFFFFFF,
};

enum Elf_Section_Header_Type {
    SHT_NULL          = 0x0,
    SHT_PROGBITS      = 0x1,
    SHT_SYMTAB        = 0x2,
    SHT_STRTAB        = 0x3,
    SHT_RELA          = 0x4,
    SHT_HASH          = 0x5,
    SHT_DYNAMIC       = 0x6,
    SHT_NOTE          = 0x7,
    SHT_NOBITS        = 0x8,
    SHT_REL           = 0x9,
    SHT_SHLIB         = 0x0A,
    SHT_DYNSYM        = 0x0B,
    SHT_INIT_ARRAY    = 0x0E,
    SHT_FINI_ARRAY    = 0x0F,
    SHT_PREINIT_ARRAY = 0x10,
    SHT_GROUP         = 0x11,
    SHT_SYMTAB_SHNDX  = 0x12,
    SHT_NUM           = 0x13,
    SHT_LOOS          = 0x60000000,
};

struct Elf_Program_Header {
    uint32_t   type;
    uint32_t   flags;
    uint64_t   offset;
    uint64_t   vaddr;
    uint64_t   paddr;
    uint64_t   filesz;
    uint64_t   memsz;
    uint64_t   align;
};

struct Elf_Section_Header {
    uint32_t   name;
    uint32_t   type;
    uint64_t   flags;
    uint64_t   addr;
    uint64_t   offset;
    uint64_t   size;
    uint32_t   link;
    uint32_t   info;
    uint64_t   addralign;
    uint64_t   entsize;
};

struct Elf_Header {
    uint8_t    ident[16];
    uint16_t   type;
    uint16_t   machine;
    uint32_t   version;
    uint64_t   entry;

    // versatz zur program-header-table
    uint64_t   phoff;

    // versatz zur section-header-table
    uint64_t   shoff;

    uint32_t   flags;

    // größe dieser datenstruktur
    uint16_t   ehsize;

    // größe der program-header-table
    uint16_t   phentsize;

    // anzahl einträge in program-header-table
    uint16_t   phnum;

    // größe der section-header-table
    uint16_t   shentsize;

    // anzahl einträge in section-header-table
    uint16_t   shnum;

    // Contains index of the section header table entry that contains the section names
    uint16_t   shstrndx;
};

uint8_t *
elf_create(uint8_t *data_section, uint32_t data_section_size, uint8_t *text_section, uint32_t text_section_size,
        uint64_t entry)
{
    uint64_t result_size = sizeof(Elf_Header) + sizeof(Elf_Program_Header) + data_section_size + text_section_size + sizeof(Elf_Section_Header);
    uint8_t *result = (uint8_t *)urq_alloc(result_size);

    Elf_Header *header      = (Elf_Header *)result;

    result[EI_MAG0]         = ELF_MAGIC_NUM1;
    result[EI_MAG1]         = ELF_MAGIC_NUM2;
    result[EI_MAG2]         = ELF_MAGIC_NUM3;
    result[EI_MAG3]         = ELF_MAGIC_NUM4;
    result[EI_CLASS]        = ELF_CLASS_64;
    result[EI_DATA]         = ELF_DATA_LITTLE;
    result[EI_VERSION]      = ELF_VERSION_CURRENT;
    result[EI_OSABI]        = ELF_ABI_SSS;
    result[EI_ABIVERSION]   = 0;

    header->type            = ET_NONE;
    header->machine         = ELF_MACHINE_X86_64;
    header->version         = ELF_VERSION_CURRENT;
    header->entry           = entry;
    header->phoff           = sizeof(Elf_Header);
    header->shoff           = header->phoff + text_section_size + data_section_size;
    header->ehsize          = sizeof(Elf_Header);
    header->phentsize       = sizeof(Elf_Program_Header);
    header->phnum           = 0;
    header->shentsize       = sizeof(Elf_Section_Header);
    header->shnum           = 0;
    header->shstrndx        = 5;

    Elf_Program_Header *ph  = (Elf_Program_Header *)(result + header->phoff);
    ph->align               = 8;

    memcpy(result + header->phoff + header->phentsize, text_section, text_section_size);
    memcpy(result + header->phoff + header->phentsize + text_section_size, data_section, data_section_size);

    return result;
}


