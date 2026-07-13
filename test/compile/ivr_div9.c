int a[100];
int foo(int n)
{
    int i;
    for (i = 0; i < n; i++) {
        a[i] = i*i*i+2*i*i+3*i+7;
    }
    return i;
}
