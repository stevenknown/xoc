void printf(char const*,...);
int main ()
{
  unsigned long long x;
  int i;
  x = -1;
  i = -1;
  x = x + i; //64bit addition with carry bit.
  printf("\n%lld\n",x);

  x = x - i;
  printf("\n%lld\n",x);

  x = x * i;
  printf("\n%lld\n",x);

  x = x / i;
  printf("\n%lld\n",(unsigned long long)x);

  unsigned long long y;
  y = -2;
  x=-2;
  x = x + y; //64bit addition with carry bit.
  printf("\n%lld\n",(unsigned long long)x);

  x = x - y;
  printf("\n%lld\n",(unsigned long long)x);

  x = x * y;
  printf("\n%lld\n",(unsigned long long)x);

  x = x / y;
  printf("\n%lld\n",(unsigned long long)x);

  return 0;
}
