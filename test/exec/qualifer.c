void printf(char const*,...);
int main()
{
    char a=0xffffFFFF;
    signed b=0xffffFFFF;
    unsigned c=0xffffFFFF;
    register d=0xffffFFFF;
    volatile e=0xffffFFFF;
    static f=0xffffFFFF;
    auto g=0xffffFFFF;
    const h=0xffffFFFF;
    inline i=0xffffFFFF;
    printf("\n%d,%d,%d,%d,%d,%d,%d,%d,%d\n",
           (int)a, (int)b, (int)c, (int)d, (int)e, (int)f, (int)g,
           (int)h, (int)i);
    return 0;
}
