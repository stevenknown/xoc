int g;
int a[100];
int * p;
int foo(int i)
{
    while (i < 0) {
        g = 0;
        if (i % 3 == 0) {
            *p = i;
        }
        a[i] = g;
        i++;
    }
    return 0;
}
