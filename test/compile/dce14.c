void rp_loop()
{
    //All loops removed, and test MDPhi remove.
    int i,j,l,n;
    int a[100][100];
    for (i = 1; i <= 100; i++) {
        l = i * (n + 2);
        for (j = i; j <= 100; j++) {
            a[i][j] = a[i-1][l];
        }
    }
}
