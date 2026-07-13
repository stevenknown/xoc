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
