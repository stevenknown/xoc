void typedef_uint()
{
    typedef unsigned int wint_t;
    typedef struct {
      int __count;
      struct {
        wint_t __wch;
        unsigned char __wchb[4];
      } __value;
    } _mbstate_t;
}


void foo()
{
    typedef char * const* * restrict CHARP;
    typedef CHARP (*CHARPARRAY)[10];
    CHARPARRAY cc;

    typedef int INT;
    typedef INT const* INTP;
    typedef INTP * INTPP;
    typedef INTPP * restrict INTPPP;
    INTPPP p;
    **p = 0;
}

void bar()
{
    typedef struct { int a, b, c;} S;
    typedef S SA;
    SA a[1];
    typedef S SB;
    SB b[1];


    struct S{int a; long long b; long long c;} w;
    typedef int (*F)();
    S (*f)(S s);
    (* ((F const*restrict) (f)) )();
}
