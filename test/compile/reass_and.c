int a;
int foo(int i, int j)
{
    int c1,c2,c3,c4;
    c1 = (a & 0xFF) & 0xF;
    c2 = (0xFF & a) & 0xF;
    c3 = 0xF & (a & 0xFF);
    c4 = 0xF & (0xFF & a);
    return c1+c2/c3*c4;
}
