void printf(char const*,...);
unsigned long
gcd_ll (unsigned long long aa, unsigned long long bb)
{
    if (aa != 100) { return 1; }
    if (bb != 200) { return 2; }
    return 0;
}
int main()
{
    unsigned int xx = 100;
    unsigned int yy = 200;
    return gcd_ll(xx, yy);
}
