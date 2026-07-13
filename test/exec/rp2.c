void abort();
void exit(int);
void printf(char const*,...);
int d2s(double d)
{
  return (int) d;
}
int test_float_to_integer()
{
 if (sizeof (double) >= 8) {
  if (d2s((double)(int)~((~0U) >> 1)) != (int)~((~0U) >> 1))
    abort();
 }
}
int main()
{
  test_float_to_integer();
  exit(0);
}

