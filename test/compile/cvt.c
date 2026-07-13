int cvt_case11()
{
    int ** p;
    (*(int(*)[10][20])p)[1][2] = 10; //OK, convert p from int ** to int(*)[10][20]
}

void printf(char const* format, ...);
int cvt_case2()
{
    //Test assignment between int ** and int [][]
    int q[10][20];
    int ** p;
    p=q; //OK, but p is  warning: assignment from incompatible pointer type
    q[1][2]=20;
    (*(int(*)[10][20])p)[1][2] = 10; //OK, convert p from int ** to int(*)[10][20]
    printf("\n%d\n", q[1][2]);
}

void cvt_case3()
{
    unsigned long long a;
    char b;
    a=b; //I8->I64->U64
}


