void printf(char const*,...);
unsigned int j = 0;
long long x[10];
int main()
{
    x[0]=-1;
    x[3]=0;
    x[4]=0;
    printf("\n%llx\n", (unsigned long long)(x[0] >> j));
    return 0;
}
