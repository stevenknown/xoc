void koo()
{
    struct S {
        long long l;
        char s;
        char s2:1;
        char s3:4;
    } q;
    int a;
    a = (&q)->s; //=> a=q.s
}


//int bar(long long p1, short p2, char * p3);
int foo()
{
    struct  S{
        union {
            int a;
            int b;
            long long c;
        } u1;
        int w;
        int d;
    };

    //Test istore for D_MC type
    struct S x[100];
    //x[2].u1.a=0;
    x[3].d=10;

    //Test store for D_MC type
    struct S s;
    s.u1.a=0;
    s.u1.b=1;
    s.u1.c=2;
    s.w=3;
    s.d=4;
    return 0;
}
