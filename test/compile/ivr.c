int test_ivr1()
{
    int a[100], i;
    for (i = 0; i < 100; i++) {
        a[i] = 202 - 2 * i;
    }
}

int test_ivr(int b[], int c[])
{
    int i,j,l,n;
    int a[100][100];
    int u;
    //test while-do.
    for (i = 1; i <= 100; i++) {
        l = i * (n + 2);
        u = i + 2;
        for (j = i; j <= 100; j++) {
            a[i][j] = 100 * n + 10 * l + j + u + a[i-1][l];
        }
    }

    //do-while
    i = 10;
    do {
        b[i] = c[i];
        i--;
    } while (i >= 0);

    i = 100;
    for (;i>0;i-=2) {
        c[i] = b[i];
    }
    return 0;
}



//Case comes from membench.
int da;
int dy[100];
int dx[200];
void daxpy(int dy_off, int dx_off, int n)
{
    //Attemp to elim range-check operation in aoc.
    int i;
    for (i = 0; i < n; i++) {
        dy[i + dy_off] += da * dx[i + dx_off];
    }
}



