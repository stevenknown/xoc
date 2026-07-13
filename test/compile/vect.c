int a[100];
int b[100];
void foo(int p)
{
    int i = 1;
    for (i=1;i<100;i++) {
        a[i]=b[i];
    }
}
