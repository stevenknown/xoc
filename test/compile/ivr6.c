int g;
int foo(int n)
{
    int j,k; //j,k are IV, step is 3.
    int i=0; //i is IV, step is 3.
    while (i < n) {
       j=i+1;
       k=j+2;
       i=k;
    }
    return i;
}
