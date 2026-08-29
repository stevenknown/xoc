int printf(char const*,...);
int foo(int a)
{
    int x = ~(a-1);
    return x;
}
int main()
{
    int w = foo(0xFFFFffff);
    if (w != 1) { return 1; }

    w = foo(0);
    if (w != 0) { return 2; }

    w = foo(1);
    if (w != -1) { return 3; }

    return 0;
}
