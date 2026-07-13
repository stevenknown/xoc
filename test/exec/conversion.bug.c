/* Test front-end conversions, optimizer conversions, and run-time
   conversions between different arithmetic types.

   Constants are specified in a non-obvious way to make them work for
   any word size.  Their value on a 32-bit machine is indicated in the
   comments.

   Note that this code is NOT intended for testing of accuracy of fp
   conversions.  */

void abort();
void exit(int);

float
u2f(unsigned int u)
{
  return u;
}

double
u2d(unsigned int u)
{
  return u;
}

long double
u2ld(unsigned int u)
{
  return u;
}

float
s2f(int s)
{
  return s;
}

double
s2d(int s)
{
  return s;
}

long double
s2ld(int s)
{
  return s;
}

int
fnear(float x, float y)
{
  float t = x - y;
  return t == 0 || x / t > 1000000.0;
}

int
dnear(double x, double y)
{
  double t = x - y;
  return t == 0 || x / t > 100000000000000.0;
}

int
ldnear(long double x, long double y)
{
  long double t = x - y;
  return t == 0 || x / t > 100000000000000000000000000000000.0;
}

int test_integer_to_float()
{
  if (u2f(0U) != (float) 0U)                /* 0 */
    abort();
  if (!fnear (u2f(~0U), (float) ~0U))            /* 0xffffffff */
    abort();
  if (!fnear (u2f((~0U) >> 1), (float) ((~0U) >> 1)))    /* 0x7fffffff */
    abort();
  if (u2f(~((~0U) >> 1)) != (float) ~((~0U) >> 1))    /* 0x80000000 */
    abort();

  if (u2d(0U) != (double) 0U)                /* 0 */
    abort();
  if (!dnear (u2d(~0U), (double) ~0U))            /* 0xffffffff */
    abort();
  if (!dnear (u2d((~0U) >> 1),(double) ((~0U) >> 1)))    /* 0x7fffffff */
    abort();
  if (u2d(~((~0U) >> 1)) != (double) ~((~0U) >> 1))    /* 0x80000000 */
    abort();

  if (u2ld(0U) != (long double) 0U)            /* 0 */
    abort();
  if (!ldnear (u2ld(~0U), (long double) ~0U))        /* 0xffffffff */
    abort();
  if (!ldnear (u2ld((~0U) >> 1),(long double) ((~0U) >> 1)))    /* 0x7fffffff */
    abort();
  if (u2ld(~((~0U) >> 1)) != (long double) ~((~0U) >> 1))    /* 0x80000000 */
    abort();

  if (s2f(0) != (float) 0)                /* 0 */
    abort();
  if (!fnear (s2f(~0), (float) ~0))            /* 0xffffffff */
    abort();
  if (!fnear (s2f((int)((~0U) >> 1)), (float)(int)((~0U) >> 1))) /* 0x7fffffff */
    abort();
  if (s2f((int)(~((~0U) >> 1))) != (float)(int)~((~0U) >> 1)) /* 0x80000000 */
    abort();

  if (s2d(0) != (double) 0)                /* 0 */
    abort();
  if (!dnear (s2d(~0), (double) ~0))            /* 0xffffffff */
    abort();
  if (!dnear (s2d((int)((~0U) >> 1)), (double)(int)((~0U) >> 1))) /* 0x7fffffff */
    abort();
  if (s2d((int)~((~0U) >> 1)) != (double)(int)~((~0U) >> 1)) /* 0x80000000 */
    abort();

  if (s2ld(0) != (long double) 0)            /* 0 */
    abort();
  if (!ldnear (s2ld(~0), (long double) ~0))        /* 0xffffffff */
    abort();
  if (!ldnear (s2ld((int)((~0U) >> 1)), (long double)(int)((~0U) >> 1))) /* 0x7fffffff */
    abort();
  if (s2ld((int)~((~0U) >> 1)) != (long double)(int)~((~0U) >> 1)) /* 0x80000000 */
    abort();
}

int main()
{
  test_integer_to_float();
  exit(0);
}
