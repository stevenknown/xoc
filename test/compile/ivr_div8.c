int a[100];
int foo(int n, int k)
{
    int j = 1;
    for (int i = 7; i < n; i+=3, j++) {
        k = i+j; //k starts with 8, and every time increase by 4.
        a[i] = k;
    }
    return k;
}
