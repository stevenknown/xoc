int pointer_arith(int a, int n)
{
    int w[10];
    int * p;
    p = &w;
    int b;
    while (a != n) {
        *(int*)(((int)p)<<2) = 10;
        p = p + a + b;
        p = (int*)(((int)p) << 2); //p may point to anywhwere.
        a = *(p+1); //ild's mds is <w,unbound>
        p = p+1;
    }
    return a;
}
