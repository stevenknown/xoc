struct S {int a; long long b; long long c;} w;
extern void icall_outer(int * p, int * q, int * u);
void icall()
{
    typedef int (*F)();
    struct S (*f)(S s);
    (* ((F const*restrict) (f)) )();

    int c, b, *d;
    d = &c;
    icall_outer(c, &d, &b);
}
