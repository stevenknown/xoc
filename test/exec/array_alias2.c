int printf(char const*,...);
void foo(int p[10][20])
{
  p[0][0] = 27;
}
int main()
{
    int ga[10][20];
    char * x;
    x = &ga;
    *x = 200;
    foo(&ga);
    long long b;
    b = ga[0][0];
    printf("\n%d,%llu,%d\n", *x, b, ga[0][0]);
    return 0;
}
