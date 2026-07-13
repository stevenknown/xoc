void simp_ild_ist(int *** p)
{
    static int **** q;

    ***p = **q;
}

void simp_as_demand()
{
    int a[4][5][6][7];
    int c;
    int b;
    c = a[1][2][3][4] + b;
}
