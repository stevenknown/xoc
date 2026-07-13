void test_struct_bit()
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

void test_struct_bit2()
{
    struct A {
       unsigned int i : 6, l : 1, j : 10, k : 15;
    };
    struct A sA;
    bool x;
    x = sA.l;
}


