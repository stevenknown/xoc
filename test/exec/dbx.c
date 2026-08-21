void printf(char const*,...);
int main(void)
{
    unsigned long long x = 1;
    int i = -1;
    unsigned long long res;
    res = x / i;
    printf("x    = %llu\n", x);
    printf("i    = %d\n", i);
    printf("res  = %llu\n", res);
    return 0;
}
