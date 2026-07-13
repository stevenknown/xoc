int foo(int n)
{
    int k;
    struct S {
        int a;
        int b;
    };
    struct S s;
    for (int i = 0; i < n; i++) {
        s = 0;
        s.a = i;
    }
    return k;
}
