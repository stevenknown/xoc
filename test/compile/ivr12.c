void foo(int *a)
{
    unsigned i = 2;
    for (unsigned bit = 1; bit < 0x10000; bit = 2*bit) {
        a[i] = a[i] & bit;
        i++;
    }
}
