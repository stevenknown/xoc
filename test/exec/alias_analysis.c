static int g[1];
static int * p = &g[0];
static int * q = &g[0];
int foo (void)
{
    //the function should returned 0.
    p = &g[0];
    q = &g[0];
    g[0] = 1;
    *p = 0;
    *p = *q;
    return g[0];
}

int x;
int y;
int foo2(void)
{
    int *p;
    //When foo returns, y should be 11.
    for (y = 1; y < 8; y += 7) {
        p = &y;
        *p = x; //y = x!
    }
    return y;
}

int f()
{
    int x;
    int y;
    y = *(&x + 1);
    return y;
}

void printf(char*,...);
void scanf(char*, int*);
int main()
{
    int a,b,*p, i, c;
    //scanf ("%d",&i);
    i = 10;
    x = 4;
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
    printf ("\n%d\n",*p,a,b,c);
    printf ("\n%d\n",foo2());
    printf ("\n%d\n",foo());
    return 0;
}
