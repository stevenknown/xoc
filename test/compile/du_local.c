void foo(int*,int);
int g;
void main(int i)
{
    int a[100];
    a[i] = 1;
    a[1] = 2;
    a[3] = 4;
    g = 3;
    int x = a[2];
    foo(&a, a[1]);
    int y = g;
    int z = a[1];
}
