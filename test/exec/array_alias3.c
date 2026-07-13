int printf(char const*,...);

void foo(int p[10][20])
{
    p[9][1] = 30;
}

int main()
{
    int ga[10][20];
    int x;
    int * p = &ga;
    ga[2][1]=5678;
    ga[9][1]=4321;
    foo(p);
    x = ga[2][1];
    if (ga[9][1] != 30) { return -1; }
    return 0;
}
