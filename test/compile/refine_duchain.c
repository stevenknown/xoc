struct S {int a; int b;} * p;
struct S s;
int foo()
{
    p = &s;
    p->a = 20; //S1
    //((char*)p)+=4; //pointer arith lead to p->a at S1 alias to p->b at S2;
    return p->b; //S2
}
