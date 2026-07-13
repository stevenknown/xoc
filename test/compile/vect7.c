int a[100];
int b[100];
int c[100];
void foo()
{
    int i = 1;
    int j = 1;
    int x,y;
    int *p;
    p = &x;
    //Can be vectorized.
    for (i=1;i<80;i++,j++) {
        x = b[i];
        y = c[i];
        a[i]=*p+y;
    }
}
