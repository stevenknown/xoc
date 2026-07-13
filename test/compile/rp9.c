int a[100];
int rp(short n)
{
    int i = 0;
    while (i < 100) {
        i = i+n;
        a[n]++; 
    }
    return a[n];
}
