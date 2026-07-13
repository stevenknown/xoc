int a[100];
int b[100];
void foo(int n)
{
    for (int w = 0; w < n; w++) {
        a[w * 7] = b[3 * w] + 2;
    }
}
