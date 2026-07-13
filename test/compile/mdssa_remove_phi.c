void printf(char*,...);
int foo (void);
int foo2(void);
int x;
int main()
{
    int a,b,*p, i, c;
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
    *p = 10;
    printf ("\n%d\n",*p,a,b,c);
    printf ("\n%d\n",foo2());
    printf ("\n%d\n",foo());
    return 0;
}
