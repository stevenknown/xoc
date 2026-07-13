int a[100];
int foo(int n, int k)
{
    for (int i = 0; i < n; i++) {
        a[i] = (k + i)*n;
    }
    return k;
}
