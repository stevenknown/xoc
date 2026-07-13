extern void exit(int);
extern void abort();
void printf(char const*,...);
long long results;
long long x;
int main()
{
    x=0x8000;
    printf("\n --- test_64bit_asr ()");
    results = x >> 64;
    printf("\nres=%llu,x=%llu", results, x);
    return 0;
}
