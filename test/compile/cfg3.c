int tangle(int a, int b, int c, int d)
{
    if (a > b && c < d && a != d ||
        a != c && c >= d && a == d) {
        return 0;
    }
    return 1;
}
