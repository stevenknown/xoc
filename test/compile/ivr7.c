int g;
int foo(int n, int h, int u, int a, int b)
{
    int j,k,i=11;
    //i is BIV.
    //u,h,j,k are DIV.
    while (i < n) {
       j=i+1;
       k=j+2;
       h=a*k+j;
       i=k;
       u=h-i+b;
    }
    return i;
}
