int printf(char const*,...);
struct S {
    char buf[256];
};

int foo(struct S * p, struct S * q)
{
    struct S ts;
    ts = *p;
    *q = ts;
}

int main(struct S * p, struct S * q)
{
    struct S s1,s2;
    for (int i = 0; i < 100; i++) {
        s1.buf[i] = i;
        s2.buf[i] = 100 - i;
    }
    foo(&s1, &s2);
    for (int i = 0; i < 100; i++) {
        printf("<%d,%d>", s1.buf[i], s2.buf[i]);
    }
    return 0;
}
