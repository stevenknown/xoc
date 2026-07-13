extern void exit(int);
extern void abort();
void printf(char const*,...);

unsigned int g;
unsigned int n;
unsigned short g2;
unsigned short n2;
unsigned char g3;
unsigned char n3;
int main()
{
    g = 1;
    n = 32;
    g2 = 1;
    n2 = 16;
    g3 = 1;
    n3 = 8;
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
    if (x != (x >> 0)) {
      return 6;
    }
    g = g << n;
    if (g != 0) {
        return 7;
    }
    g2 = g2 << n2;
    if (g2 != 0) {
        return 8;
    }
    g3 = g3 << n3;
    if (g3 != 0) {
        return 9;
    }
    return 0;
}
