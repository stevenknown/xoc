int p[100];
int foo(int n)
{
    int i;
    for (i = 0; i < n; i++) {
        int k = i*n;
        p[k] = 10;
    }
    return i;
}
