int foo(int num, int * restrict m, int * restrict n)
{
    for (int i=0;i<num;i++) {
        m[i]=n[i]+7;        
    }
}
