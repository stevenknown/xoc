int *p,a,w,u;
void bar();
int *q;
int foo(int i)
{
    int x,e,y,z;
    int l1,l2,l3,l4;
    int * t=&y;
    p = &a;
    *p = 20;
//    bar();
    if (i > 0) {
        l1 = *p;
        l2 = y+z;
    } else {
        l3 = *p;
        l4 = y+z;
        *q=20;
        *t=30;
    }
    return l1+l2+l3+l4;
}
