int printf(char*,...);
int array2_test()
{
    int s[3];
    s[0]=1;
    s[1]=2;
    s[2]=3;

    int * sp;
    sp = &s[1];

    int ss;
    ss = *(sp++);

    printf("\n>>%d", ss);
    ss = *(sp++);
    printf("\n>>%d", ss);
}
