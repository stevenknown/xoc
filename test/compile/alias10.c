struct S {int a; int b; int c; int d; } p[10];
struct S s;
//int foo(int i)
//{
//    p[0].b = 1; //S1
//    return p[0].a; //S2
//}
//int foo1(int i)
//{
//    p[0].b = 1; //S1
//    return p[0].b;
//}
//int foo2(int i)
//{
//    p[1].b = 20; //S4
//    return p[0].a;
//}
//int foo3(int i)
//{
//    p[1].b = 20; //S4
//    return p[1].a;
//}
//int foo4(int i)
//{
//    p[0].b = 20; //S4
//    return p[1].b;
//}
//int foo5(int i)
//{
//    p[0].b = 20; //S4
//    return p[1].a;
//}
//int foo6(int i)
//{
//    p[0].a = 20; //S4
//    return p[1].a;
//}
//int foo7(int i)
//{
//    p[0].a = 20; //S4
//    return p[1].b;
//}
//int foo8(int i)
//{
//    p[i].a = 20; //S4
//    return p[0].a;
//}
//int foo9(int i)
//{
//    p[i].b = 20; //S4
//    return p[0].a;
//}
//int foo10(int i)
//{
//    p[i].a = 20; //S4
//    return p[1].a;
//}
int foo11(int i)
{
    p[i].a = 20; //S4
    return p[i].a;
}
//int foo12(int i)
//{
//    p[i].a = 20; //S4
//    return p[i].b;
//}
//int foo13(int i)
//{
//    p[i].b = 20; //S4
//    return p[i].a;
//}
//int foo14(int i)
//{
//    p[i].b = 20; //S4
//    return p[i].b;
//}
//struct S foo15(int i)
//{
//    p[i].c = 20; //S4
//    return p[i];
//}
//struct S foo16(int i)
//{
//    p[i] = s; //S4
//    return p[i];
//}
//int foo17(int i)
//{
//    p[i].b = 20; //S4
//    return p[i];
//}
//int foo18(int i)
//{
//    p[i] = s; //S4
//    return p[i].a;
//}
//int foo19(int i)
//{
//    p[i] = s; //S4
//    return p[i].b;
//}
