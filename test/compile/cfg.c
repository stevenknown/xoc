struct S {int a; int b;} p[10];
int foo(int i)
{
    return p[1].b; //S3
    p[1].b = 20; //S1
}
