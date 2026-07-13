extern void exit(int);
extern void abort();
void printf(char const*,...);

int main()
{
    unsigned long long x;
    x = 0x123456789ABC;
    if (x != (x >> 0)) {
      return 1;
    }
    if (x > (x >> 0)) {
      return 2;
    }
    if (x < (x >> 0)) {
      return 3;
    }
    if (x >= (x << 3)) {
      return 4;
    }
    if (x <= (x >> 1)) {
      return 5;
    }
    if (x == (x >> 0)) {
      return 0;
    }
    return 6;
}
