int b[100],c[100];
int foo()
{
    int i = 10;
    do {
        b[i] = c[i];
        i--;
    } while (i >= 0);
    return i;
}
