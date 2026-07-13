int c[100];
int b[100];
int foo()
{
    int i = 100;
    for (;i>0;i-=2) {
        c[i] = b[i];
    }
    return i;
}
