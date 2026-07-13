struct S {int a; int b;} p[10];
int q;
int foo(int i)
{
    //This function test Array, StoreArray's MD reference.
    q = p[1].b; //S1
    q = p[1].a; //S2
    q = p[i].b; //S3
    p[1].b = 20; //S4
    p[1].a = 20; //S5
    p[i].a = 30; //S6
    return 1;
}
