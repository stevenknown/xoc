int a[100];
int b[100];
int c[100];
int d[100];
void foo(int p)
{
    int i = 1;
    int x = 5;
    //Can not vectorized.
    for (i=1;i<80;i++, x++) {
        a[i]=b[i]+c[i];
        a[x]=b[i]>>3; //a[x] has loop-carried dep to a[i].
    }
}
