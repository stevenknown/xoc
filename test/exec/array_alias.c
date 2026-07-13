int printf(char const*,...);

void foo(int p[10][20])
{
    p[9][1] = 30;
}

int main()
{
    int ga[10][20];
    int x;
    ga[2][1]=5678;
    ga[9][1]=4321;
    foo(&ga);
    x = ga[2][1];
    if (ga[9][1] != 30) { return 255; }
    return 0;
}
