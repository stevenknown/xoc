struct A {
  int w;
  struct S {int a; int b;} p[10];
} x;
int foo(int i)
{
    x.p[1].b = 1; //S1
    return x.p[0].a; //S2
}
