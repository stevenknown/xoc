int a[100];
int b[100];
void foo(int * n)
{
    for (int w = 0; w < n; w++) {
        a[n[w]] = b[n[w]] + 2;
    }
}
