/* Test front-end conversions, optimizer conversions, and run-time
   conversions between different arithmetic types.

   Constants are specified in a non-obvious way to make them work for
   any word size.  Their value on a 32-bit machine is indicated in the
   comments.

   Note that this code is NOT intended for testing of accuracy of fp
   conversions.  */

void abort();
void exit(int);
void printf(char const*,...);

double
s2d(int s)
{
  return s;
}

int test_integer_to_float()
{
  double v,m;
  int w;
  w = (int)~((~0U) >> 1);
  v = s2d(w);
  m = (double)w;
  printf("\n%f,%f,%d\n", v,m,w);
  if (v != m) /* 0x80000000 */
    abort();
}

int main()
{
  test_integer_to_float();
  exit(0);
}
