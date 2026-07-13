int c[100];
int b[100];
int foo(int j, int h, int k)
{
    int i = 100;
START:
    if (i == 0) {
        goto END;
    }
    j = i;
    k = j + h - 2;
    i = k;
    goto START;
END:
    return i;
}
