int foo(int num, int * m, int * n)
{
    for (int i=0;i<100;i++) {
        //The loop can NOT be vectorized.
        //ld m, ld n should NOT have different livein VN,
        //because m, n may overlap.
        m[i]=n[i]+7; 
    }
}
