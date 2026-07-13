int g;
int a[100];
int * t1;
int * t2;
int * t3;
int foo(int i, int j)
{
    t3 = &g;
    if (i > j) {
        t1 = &a[j];
        //t2 may point to a and g.
    } else {
        t2 = &a[i];
        //t1 may point to a and g.
    }
    return *t1+*t2+*t3;
}
