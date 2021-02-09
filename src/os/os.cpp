#ifndef __URQ_OS__
#define __URQ_OS__

namespace Urq { namespace Os {

static bool
os_file_read(char *filename, char **result, size_t *size = 0) {
    HANDLE file = CreateFileA(filename, GENERIC_READ, FILE_SHARE_READ, 0, OPEN_EXISTING, FILE_ATTRIBUTE_NORMAL, 0);

    if ( file == INVALID_HANDLE_VALUE ) {
        return false;
    }

    if ( size ) {
        *size = GetFileSize(file, 0);
    }

    HANDLE file_mapping = CreateFileMapping(file, 0, PAGE_WRITECOPY, 0, 0, 0);
    *result = (char *)MapViewOfFileEx(file_mapping, FILE_MAP_COPY, 0, 0, 0, 0);

    if ( !*result ) {
        return false;
    }

    return true;
}

void *
os_alloc(size_t size) {
    void *result = VirtualAlloc(NULL, size, MEM_RESERVE|MEM_COMMIT, PAGE_READWRITE);

    return result;
}

namespace api {
    using Urq::Os::os_file_read;
    using Urq::Os::os_alloc;
}

}}

#endif

