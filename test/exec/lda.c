struct A2 {
    long long ab1, b;
};
struct Q {
    char pad;
    struct A2 a;
    int s;
};
char refine_ild_lda(struct Q q)
{
    //do refine ((*(&q))->a).b => q.a.b

    //do refine (&q)->s => q.s
    return (&q)->s + (*(&q.a)).b;
}


long long g[11][11];
void f(int i, int j)
{
    int * p;
    int b;
    p = &( g[i][  *(&b + i) ] ); //will crashed in ir_simp.cpp
}


int case0()
{
    int * p;
    int * q;
    int a;
    p = &*q; //be convert to p = q
    p = *&a; //be convert to p = a
    return 0;
}



struct B {
    int u;
    int v;
};
struct A {
    struct B * pb;
} *pa;
void y()
{
    int * l = &(pa->pb->v);
}


struct _S {
    int a;
    int b;
} gs[100];
int gb[100];
int * gx;
struct _S * g2;
int main()
{
    //gx = &(gs[4].b);
    //gx = &gb[5];

    int * l;
    l = &(g2->b);
    return 0;
}
