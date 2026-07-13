extern void exit(int);
extern void abort();
void printf(char const*,...);
int main()
{
    unsigned long long y;
    unsigned long long x;
    //x=-1;
    //x=0x8000U;
    //x=0x80000000U;
    //x=0x800000000000;
    x=0x8000000000000000;
    //x=0;
    //x=1;
    //x=0x7fffffffffffffff;
    //x=0xffff7fffffff7fff;
    //x=0xff0000ff00000ff0;
    //x=0x800000000000;
    y=x;
    printf("\ny=%llx,x=%llx\n", y, x);
    return 0;
}
