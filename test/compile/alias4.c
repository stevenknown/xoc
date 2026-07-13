//Distinguish s.a and s.b[i], they are not overlapped.
struct S {
    int a;
    int b[100];
    short c;
};
int alias4_1(int i)
{
    struct S s1;
    s1.a = 10;
    s1.b[i] = 100; //s.b[i] not alias with s.a
    return s1.a; //after CP, equal to: return 10;
}


struct Q {
    int a;
    struct W {
        longlong l;
        int b[100];
    }c;
};
int alias4_2(int i)
{
    int c[10];
    struct S s2;
    struct S * p;
    struct Q * q;
    q->c.l = 0;
    q->c.b[i] = 90;
    //c[i] = 30;
    p->b[i] = 40;
    s2.b[i] = 10;
    if (i > 0) { //should be removed by DCE
        s2.b[i] = 10; //should be removed by DCE
    } else {
        s2.b[i] = 20; //should be removed by DCE
    }
    return s2.a; //s2.a is not alias with s2.b.
}
