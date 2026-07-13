unsigned long long g;
int x;
void printf(char const*,...);
void foo(int x)
{
    g = 0x13423562456;
    g = g << x;
    printf("\n%llu\n", g);
    return 0;
}
int main()
{
    foo(37);
    return 0;
}
