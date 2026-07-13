int b[100],c[100];
int foo()
{
    int i = 0;
    do {
        b[-i] = c[-i];
        i--;
    } while (i >= -10);
    return i;
}
