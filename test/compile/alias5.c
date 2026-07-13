int bar(int * p, int * q, int * m, int * n)
{
    *p = *q + 20;
    *m = *n - 64;
    return 0;
}

int foo(int * p, int * q, int m, int n)
{
    int a[100];
    *p = *q + 20;
    a[1] = a[m]; 
    a[n] = a[m+1]; 
    return 0;
}
