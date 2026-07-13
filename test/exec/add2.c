void printf(char const*,...);
int main ()
{
  unsigned long long x;
  int i;
  x = 2;
  i = -2;
  x = x + i; //i will be hoisted to unsigned long long according to C spec.
  if (i == -2) {
    printf("\n%lld\n",x);
  }
  return 0;
}
