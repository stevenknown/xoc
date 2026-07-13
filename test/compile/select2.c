static int __builtin_strlen(char const* s);
static bool __builtin_constant_p(char const* a);
static bool __builtin_strcmp(char const* a, char const* b);
void foo()
{
    typedef unsigned int size_t;
    register int __result; size_t __s1_len, __s2_len;
    const char * p;
    const unsigned char *__s2 = (const unsigned char *)(const char *)(".");
    bool res = __builtin_constant_p(p) &&
        __builtin_constant_p(".") &&
        (__s1_len = __builtin_strlen(p),
            __s2_len = __builtin_strlen("."),
            (!((size_t)(const void *)((p)+1) - (size_t)(const void *)(p) == 1) || __s1_len >= 4) &&
            (!((size_t)(const void *)((".") + 1) - (size_t)(const void *)(".") == 1) || __s2_len >= 4)) ?
        __builtin_strcmp(p, ".") :
        __builtin_constant_p(p) &&
        ((size_t)(const void *)((p)+1) - (size_t)(const void *)(p) == 1) &&
        (__s1_len = __builtin_strlen(p), __s1_len < 4) ?
        __builtin_constant_p(".") &&
        ((size_t)(const void *)((".") + 1) - (size_t)(const void *)(".") == 1) ?
        __builtin_strcmp(p, ".") :
        __result = (((const unsigned char *)(const char *)(p))[0] - __s2[0]) :
        __builtin_constant_p(".") &&
        ((size_t)(const void *)((".") + 1) - (size_t)(const void *)(".") == 1) &&
        (__s2_len = __builtin_strlen("."), __s2_len < 4) ?
        __builtin_constant_p(p) &&
        ((size_t)(const void *)((p)+1) - (size_t)(const void *)(p) == 1) ?
        __builtin_strcmp(p, ".") : 0
        :
        __builtin_strcmp(p, ".");
}
