int foo(int num, int * restrict m, int * restrict n)
{
    for (int i=0;i<100;i++) {
        m[i]=n[i]+7; //ld m, ld n should have different livein VN.
    }
}
