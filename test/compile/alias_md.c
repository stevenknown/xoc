////////////////////
void alias_md_g()
{
    char v[100][200];
    int * p;
    int ** q;
    p = &v;
    *p = 10;
    p = v[10][2];
    p = q[10][2];
}


//////////////////
int * g1;
extern void modify_pointer();
void bar();
void fx()
{
    bar();
    int a;
    a = 20;
    g1 = &a;
    modify_pointer(); //will overide g1
    int b;
    b = *g1;
}
/////////////////////

struct {
    int a;
    int * b;
} s;
void f()
{
    int * q;
    int ** p;

    //It equals to : (lda(s) + 4) + 4
    //I has verified.
    p = (  &((&s)->b)  ) + 1;
}
///////////////////
void fz()
{
    int a,b,*p, i, c;
    if (i)
        p = &a;
    else
        p = &b;
    *p = 10;
    c = *p;
    a = 5;
    b = 4;
    *p = 20;
    p= &a;
    *p  = 10;
}



