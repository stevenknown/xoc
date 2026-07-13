//Test code gen, the order of parameter.
typedef struct _S{int a,b,c,d;}S;
extern S foo();
void test_parameter(int p1, int p2, int p3, int p4)
{
    int a;
    a = p4 + 4;
    a = a - p2;
    long long x;
    x = p1 / p3;
    S s;
    s = foo();
}
