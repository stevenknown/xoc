int p[100];
int foo(int n)
{
    int x;
    for (x = 0; x < n; x++)            
        p[x] = x*x*x + 2*x*x + 3*x + 7;
    return x;
}
