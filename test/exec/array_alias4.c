int printf(char const*,...);

void foo(int ** q)
{
    int * p = *q;
    int (*bb)[10][20];
    bb = p;
    (*bb)[9][1] = 30;
}

int main()
{
    int ga[10][20];
    int x;
    int * p = &ga;
    int ** q = &p;
    ga[2][1]=5678;
    ga[9][1]=4321;
    foo(q);
    x = ga[2][1];
    if (ga[9][1] != 30) { return -1; }
    return 0;
}
