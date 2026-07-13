void exit(int);
void printf(char const*,...);
int main ()
{
  unsigned long long x;
  int i;
  x=-2;
  i=2;
  if ((x / i) != 0x7fffffffFFFFFFFFULL) { exit(5); }
  i = x / i; //i will be hoisted to unsigned long long according to C spec.

  //i will be truncated i32, thus its value is 0xFFFFffff, namely -1.
  if (i != -1) { exit(6); }

  i=2;
  if ((x + i) != 0x0) { exit(7); }
  return 0;
}
