extern void exit(int);
extern void abort();
void printf(char const*,...);
long long results;
long long x;
int main()
{
    x=0x80000000U; //WORKAROUND:xocfe does not able to recognize 0x80000000 to be unsigned int.
    printf("\n --- test_64bit_asr ()");
    results = x >> 1;
    printf("\nres=%llu,x=%llu", results, x);
    return 0;
}
