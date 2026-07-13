int a[100];
int c[100];
int foo(int num, int * restrict m, int * restrict n)
{
    //can be vectorized.
    for (int i=0;i<num;i++) {
        a[i] = n[i] + c[i];
        m[i] = n[i] + 7; //ld m, ld n should have different livein VN.
    }
}
