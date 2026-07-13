int foo(int sum)
{
    //MDPhi should be maintained during CFGOPT.
    int i,j,n;
    for (i = 1; i <= 100; i++) {
        for (j = i; j <= 100; j++) {
            n = 100;
            if (i * j > sum) { return 1; }
        }
    }
    return 0;
}
