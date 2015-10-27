/*Copyright 2011 Alibaba Group*/
#include "dl/cdlfcn.h"
#include <windows.h>
#include "str/cstr.h"

UInt32 cdlopen(const char* name) {
    return (UInt32)LoadLibrary(name);
}

void cdlclose(UInt32 handle) {
    FreeLibrary((HMODULE)handle);
}

void* cdlsym(UInt32 handle, const char* symname) {
    return (void*)GetProcAddress((HMODULE)handle, symname);
}

const char* cdlerror() {
    static char msg[64];
    UInt32 err;

    err = GetLastError();
    cSnprintf(msg, sizeof(msg), "win32 error: %u\n", err);
    return msg;
}
