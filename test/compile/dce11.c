int dce1()
{
    //All code should be removed.
    int a[100], i;
    if (i > 0)
       a[i] = 10;
    else
       a[i] = 20;
    a[i] = 30;
}

int dce2(int b[], int c[])
{
    int i,j,l,n;
    int a[100][100];
    int u;
    //while-do.
    //Whole loop should be removed.
    for (i = 1; i <= 100; i++) {
        l = i * (n + 2);
        u = i + 2;
        for (j = i; j <= 100; j++) {
            a[i][j] = 100 * n + 10 * l + j + u + a[i-1][l] + c[i];
        }
    }

    //do-while
    i = 10;
    do {
        b[i] = c[i]; //b,c is outer pointer, should NOT be removed by DCE.
        i--;
    } while (i >= 0);

    i = 100;
    for (;i>0;i-=2) {
        c[i] = b[i]; //b,c is outer pointer, should NOT be removed by DCE.
    }
    return 0;
}

//Case comes from membench.
int da;
int dy[100];
int dx[200];
void dce3(int dy_off, int dx_off, int n)
{
    for (int i = 0; i < n; i++) {
        dy[i + dy_off] += da * dx[i + dx_off]; //nothing can be removed.
    }
}
