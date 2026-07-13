int g1, g2, g3;
int foo3()
{
    int * p, * q;
    int a;
    p = &a;
    q = &a;
    REGION_START:
    *p=10;
    *q=20;
    REGION_END:
    return a;
}



struct S {int a,b,c,d;};
struct S goo(int a);
void foo2(int * p)
{
    struct S s;
    p++;
    REGION_START:
    while (g1 < 100) {
        *p = g1;
        g1++;
        s = goo(g2);
        p++;
    }
    REGION_END:
    return;
}

int foo(int w)
{
    int a,b,c,d;
    a = g1 + b;
    b = g2 + c;
    c = g3 + b + w;
    d = g1 + a;
    g1= a - b;
    g2= d * c;
    return g1 + g2;
}
