extern void exit(int);
extern void abort();
void printf(char const*,...);
int results;
int x;
int main()
{
    x=-1;
    printf("\n --- test_16bit_lsl ()");
    results = x << 32;
    printf("\nres=%x,x=%x\n", results, x);
    return 0;
}
