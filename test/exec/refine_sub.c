int printf(char const*,...);
int foo(int x, int y)
{
    //0 - (x - y) ==> y - x
    int w = 0 - (x - y);
    return w;
}

int main()
{
    int a = foo(3, 4);
    if (a != 1) { return 1; }

    a = foo(-3, -4);
    if (a != -1) { return 2; }

    a = foo(0, 1);
    if (a != 1) { return 3; }

    a = foo(1, 0);
    if (a != -1) { return 4; }

    return 0;
}
