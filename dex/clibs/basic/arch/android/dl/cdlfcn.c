/*Copyright 2011 Alibaba Group*/
#include "dl/cdlfcn.h"
#include <dlfcn.h>

ULong cdlopen(const char* name) {
    return (ULong)dlopen(name, RTLD_LAZY);
}

void cdlclose(ULong handle) {
    dlclose((void*)handle);
}

void* cdlsym(ULong handle, const char* symname) {
    return dlsym((void*)handle, symname);
}

const char* cdlerror() {
    return dlerror();
}
