int *p;
int (*q)[10];
int foo()
{
    int a[10];
    p = a;
    //will generate code:
    // r1 = lda(a)
    // st(p) = r1
 
    p[1] = 20; //S1
    //will generate code:
    //r2 = ld(p)
    //st(r2+1) = r1

    q = &a;
    //will generate code:
    // r1 = lda(a)
    // st(q) = r1

    (*q)[2]= 40; //S2, the code of S1 is same with the code generated of S2
    //will generate code:
    //r2 = ld(q)
    //st(r2+1) = r1

    return p[1]+(*q)[2]; 
}
