double foo()
{
    double a[100];
    for (int i = 0; i < 100; i++) {
        a[i] = 10 + 2 * i;
    }
    return a[99];
}
