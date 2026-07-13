extern void exit(int);
extern void abort();
void printf(char const*,...);
unsigned long long results;
unsigned long long src;
long long results2;
long long src2;
int main()
{
    src=-1;
    results = src >> 33;
    src2=-1;
    results2 = src2 >> 33;
    printf("\nres=%llu,src=%llu,res=src>>33\n",results, src);
    printf("\nres2=%llu,src2=%llu,res2=src2>>33\n",results2, src2);
    return 0;
}
