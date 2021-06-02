#ifndef __URQ_OS__
#define __URQ_OS__

#include <strsafe.h>

namespace Urq { namespace Os {

enum Os_Color {
    OS_COLOR_BLACK,
    OS_COLOR_RED,
    OS_COLOR_GREEN,
    OS_COLOR_YELLOW,
    OS_COLOR_BLUE,
    OS_COLOR_MAGENTA,
    OS_COLOR_CYAN,
    OS_COLOR_WHITE,
    OS_COLOR_EXTENDED,
    OS_COLOR_DEFAULT,
};

enum Os_Flags {
    OS_FLAGS_NONE,
    OS_FLAGS_FONT_BOLD,
};

HANDLE os_stdout;

static char *
os_env(char *name) {
    char *result = getenv(name);

    return result;
}

void
os_error(char *proc_name) {
    LPVOID lpMsgBuf;
    LPVOID lpDisplayBuf;
    DWORD dw = GetLastError();

    FormatMessage(
        FORMAT_MESSAGE_ALLOCATE_BUFFER | FORMAT_MESSAGE_FROM_SYSTEM | FORMAT_MESSAGE_IGNORE_INSERTS,
        NULL,
        dw,
        MAKELANGID(LANG_NEUTRAL, SUBLANG_DEFAULT),
        (LPTSTR) &lpMsgBuf,
        0, NULL );

    lpDisplayBuf = (LPVOID)LocalAlloc(LMEM_ZEROINIT,
        (lstrlen((LPCTSTR)lpMsgBuf) + lstrlen((LPCTSTR)proc_name) + 40) * sizeof(TCHAR));
    StringCchPrintf((LPTSTR)lpDisplayBuf,
        LocalSize(lpDisplayBuf) / sizeof(TCHAR),
        TEXT("%s failed with error %d: %s"),
        proc_name, dw, lpMsgBuf);
    MessageBox(NULL, (LPCTSTR)lpDisplayBuf, TEXT("Error"), MB_OK);

    LocalFree(lpMsgBuf);
    LocalFree(lpDisplayBuf);
}

void
os_print(char *fmt, ...) {
    va_list args = NULL;
    va_start(args, fmt);
    vprintf(fmt, args);
    va_end(args);
}

void
os_stdout_clear() {
    if ( !os_stdout ) {
        return;
    }

    WriteConsole(os_stdout, "\x1b[2J", 5, NULL, NULL);
}

void
os_stdout_set_regular() {
    if ( !os_stdout ) {
        return;
    }

    WriteConsole(os_stdout, "\x1b[0m", 5, NULL, NULL);
}

void
os_stdout_set_bold() {
    if ( !os_stdout ) {
        return;
    }

    WriteConsole(os_stdout, "\x1b[1m", 5, NULL, NULL);
}

void
os_stdout_move_to(size_t x, size_t y) {
    if ( !os_stdout ) {
        return;
    }

    printf("\x1b[%zd;%zdH", y, x);
}

void
os_stdout_set_font(uint32_t color, uint32_t flags = OS_FLAGS_NONE) {
    if ( !os_stdout ) {
        return;
    }

    if ( flags & OS_FLAGS_FONT_BOLD ) {
        printf("\x1b[9%dm", color);
    } else {
        printf("\x1b[3%dm", color);
    }
}

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

static bool
os_file_write(char *filename, void *data, size_t len) {
    HANDLE file = CreateFileA(filename, GENERIC_WRITE, FILE_SHARE_WRITE, 0, CREATE_ALWAYS, FILE_ATTRIBUTE_NORMAL, 0);

    if ( file == INVALID_HANDLE_VALUE ) {
        return false;
    }

    DWORD bytes_written = 0;
    HRESULT h = WriteFile(file, data, (DWORD)len, &bytes_written, NULL);

    CloseHandle(file);

    return SUCCEEDED(h);
}

void *
os_alloc(size_t size) {
    void *result = VirtualAlloc(NULL, size, MEM_RESERVE|MEM_COMMIT, PAGE_READWRITE);

    return result;
}

void
os_init() {
    DWORD console_mode;

    os_stdout = GetStdHandle(STD_OUTPUT_HANDLE);
    if (os_stdout == INVALID_HANDLE_VALUE) {
        os_error("GetStdHandle");
    }

    if ( !GetConsoleMode(os_stdout, &console_mode) ) {
        os_error("GetConsoleMode");
    }

    console_mode |= ENABLE_VIRTUAL_TERMINAL_PROCESSING;
    if ( !SetConsoleMode(os_stdout, console_mode) ) {
        os_error("SetConsoleMode");
    }
}

namespace api {
    using Urq::Os::os_file_read;
    using Urq::Os::os_file_write;
    using Urq::Os::os_alloc;
    using Urq::Os::os_env;
    using Urq::Os::os_init;
    using Urq::Os::os_error;
    using Urq::Os::os_print;

    using Urq::Os::os_stdout_clear;
    using Urq::Os::os_stdout_set_regular;
    using Urq::Os::os_stdout_set_bold;
    using Urq::Os::os_stdout_move_to;
    using Urq::Os::os_stdout_set_font;

    using Urq::Os::OS_COLOR_RED;
    using Urq::Os::OS_FLAGS_FONT_BOLD;
}

}}

#endif

