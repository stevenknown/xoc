int dce_mdssa(int b[], int c[])
{
    int i,j,l,n;
    int a[100][100];
    int u;
    //Whole loop should be removed.
    for (i = 1; i <= 100; i++) {
        l = i * (n + 2);
        u = i + 2;
        for (j = i; j <= 100; j++) {
            a[i][j] = 100 * n + 10 * l + j + u + a[i-1][l] + c[i];
        }
    }

    //MDSSA have to be maintained.
    i = 10;
    do {
        b[i] = c[i]; //b,c is outer pointer, should NOT be removed by DCE.
        i--;
    } while (i >= 0);
    return 0;
}
