int printf(char const*,...);
int g;
int a[100];
int * qq;
union {
    unsigned int w;
    struct {
        char cc[6];
    } s;  
    struct {
        char dd[8];
    } s2;
}u;
char * p;
int foo(int i, int j)
{
    qq = &g;
    if (i > j) {
        p = &u.s.cc[2];
        u.w = 10;
        *p = 20;
    } else {
        p = &u.s2.dd[7];
        u.w = 30;
        *p = 40;
    }
    return *p+*qq;
}
int main()
{
    g = 230;
    u.s.cc[2] = 10;
    u.s2.dd[7] = 20;
    int x = foo(1, 2);
    if (x != 270) {
        return 1;
    }
    x = foo(2, 1);
    if (x != 250) {
        return 2;
    }
    return 0;
}
