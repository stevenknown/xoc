int g;
int foo(int n, int h, int u, int a, int b)
{
    int j,k,i=0;
    //There is no DIV.
    //i is NOT biv, i's coeff is 2, not linear.
    while (i < n) {
       j=i+i+1; //j is NOT div.
       k=j+2; //k is not div.
       h=a*k+j;
       i=k;
       u=h-i+b;
    }
    return i;
}
