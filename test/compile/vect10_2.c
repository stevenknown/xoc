int foo(int num1, int num2, int * restrict m, int * restrict n)
{
    for (int i=num1;i<num2;i++) {
        m[i]=n[i]+7;        
    }
}
