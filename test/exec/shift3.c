extern void exit(int);
extern void abort();
void printf(char const*,...);

int main()
{
    unsigned long long x;
    x = 0x123456789ABC;
    if (x >= (x << 3)) {
      return 4;
    }
    return 0;
}
