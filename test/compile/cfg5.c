int foo()
{
    //MDPhi should be maintained during CFGOPT.
    int i,j,n;
    for (i = 1; i <= 100; i++) {
        for (j = i; j <= 100; j++) {
            n = 100;
        }
    }
    return 0;
}


