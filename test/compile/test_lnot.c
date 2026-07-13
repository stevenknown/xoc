int test_lnot2(int a, int b)
{
    return a ? b &= ~0x03 : 2;
}

int test_lnot(int a)
{
    //test refine lnot: !(a != 0)  => a==0
    if (!(a != 0)) {
        return 0;
    }
    return 1;
}
