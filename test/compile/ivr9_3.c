int foo(int t)
{
    int j = 3;
    int i = 1;
    for (;;) {
        if (i > 123) goto FIN;
        t = i + 7;
        j = t + j;
        i = i + 5;
    }
    FIN:
    return j;
}
