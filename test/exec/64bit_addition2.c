unsigned long long g;
int x;
void printf(char const*,...);
void foo(long long x)
{
    g = g + x;
    printf("\n%llu\n", g);
}
void bar()
{
    g = g + 0x1234567890abcdefllu;
    printf("\n%llu\n", g);
}
int main()
{
    g = 0x13423562456;
    foo((long long)37); //FIX: There is a bug of FE, have to insert type-convert.
    bar();
    return 0;
}
