int licm_t4()
{
    //In this case, nothing can be
    //hoisted!!
    int i;
    int a;
    a = 2;
    short n,m;
    i = 0;
    n = 10;
    m = 10;
    while (i < 100) {
        if (a <= 0) {
            n = 2;
        } else {
            n = 1;
        }
        m = n;
        i = i+1;
    }
    return i;
}


int licm_t3()
{
    int i;
    short n,m;
    i = 0;
    n = 10;
    m = 10;
    while (n != 1) {
        if ((n & 1) == 0) {
            //even
            n = m/2;
        } else {
            //odd
            n = n*3 + 1;
        }
        i = i+1;
    }
    return i;
}


int licm_t2()
{
    //i,j,l,n should be promoted to PR.
    int i,j,l,n;
    int a[100][100];
    for (i = 1; i <= 100; i++) {
        l = i * (n + 2);
        for (j = i; j <= 100; j++) {
            a[i][j] = 100 * n + 10 * l + j;
        }
    }
    return 0;
}


int licm_t1()
{
    //In this case, the LICM can
    //use store motion to remove the stores from the loop.
    //or use register promotion/scalar replacement to remove the stores from the loop.
    int i;
    char C[20];
    char A[100];
    for (i = 0; i != 10; ++i) {
          C[0] = A[i];          // One byte store
          C[1] = A[9-i];        // One byte store
    }
}

