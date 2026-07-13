int foo()
{
    int i;
    int j;
    int k;
    int w;
    for (i = 1; i <= 123;) {
        w = j + 8;
        j = i + 4;
        k = j;
        i = k;
    }
    return j;
}
