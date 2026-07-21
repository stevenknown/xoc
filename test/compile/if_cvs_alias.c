int g;
int alias(int c)
{
    int * p = &g;
    if (c == 10) {
        g = 1;
    } else {
        *p = 2;
    }
    return g;
}
