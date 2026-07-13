int a[100];
int const g;
void foo(int p)
{
    int j = 0;
    int i = 1;
    for (i=1;i<100;i++) {
        p = g + 3; 
        a[i]=p; //g is invariant RHS, a can be vectorized.
    }
}
