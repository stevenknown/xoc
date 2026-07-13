int c[100];
int b[100];
int foo()
{
    int i = 100;
START:
    if (i == 0) {
        goto END;
    }
    if (i % 2 == 0) {
        c[i] = b[i];
    }
    if (i % 7 == 0) {
        c[i+1] = b[i+1];
    }
    i = i - 2;
    goto START;
END:
    return i;
}
