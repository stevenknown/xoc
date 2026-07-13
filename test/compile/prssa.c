int prssa(int c[])
{
    int i,j;
    int a[100][100];
    for (i = 1; i <= 100; i++) {
        for (j = i; j <= 100; j++) {
            a[i][j] = c[i];
        }
    }
    return 0;
}
