unsigned long long g;
unsigned int g2;
int g3;
unsigned long long g4;
int x;
void printf(char const*,...);
//Find a lot of REM instruciton that should be replaced by AND with literal.
//a % 16 => a & 0xFFFF
int main()
{
    g = 23;
    g = g % 16;
    if (g != 7) {
      printf("\nfail\n");
    }

    g2 = 23;
    g2 = g2 % 16;
    if (g2 != 7) {
      printf("\nfail\n");
    }

    g3 = 23;
    g3 = g3 % 16;
    if (g3 != 7) {
      printf("\nfail\n");
    }

    g4 = 23;
    g4 = g4 % 16;
    if (g4 != 7) {
      printf("\nfail\n");
    }

    return 0;
}
