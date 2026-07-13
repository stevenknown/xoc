void printf(char const*,...);
int main()
{
    unsigned long long a,b;
    a = 0;
    b = -1;
    if (a == b) {
      printf("\na == b failed\n");
      return -1;
    }
    if (a > b) {
      printf("\na > b failed\n");
      return -1;
    }

    long long c,d;
    c = 0;
    d = -1;
    if (c == d) {
      printf("\nc == d failed\n");
      return -1;
    }
    if (c < d) {
      printf("\nc < d failed\n");
      return -1;
    }

    return 0;
}
