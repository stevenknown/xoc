int g;
int printf(char const*,...);
int foo()
{
    int c = g * 40 / 20; //can NOT do reass because DIV is not associated.
    return c;
}
int main()
{
    g = 0xFFFFffff;
    int v = foo();
    if (v == -2) { return 0; }
    return 1;
}
