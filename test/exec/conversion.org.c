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

unsigned int
f2u(float f)
{
  return (unsigned) f;
}

unsigned int
d2u(double d)
{
  return (unsigned) d;
}

unsigned int
ld2u(long double d)
{
  return (unsigned) d;
}

int
f2s(float f)
{
  return (int) f;
}

int
d2s(double d)
{
  return (int) d;
}

int
ld2s(long double d)
{
  return (int) d;
}

int test_float_to_integer()
{
  if (f2u(0.0) != 0)
    abort();
  if (f2u(0.999) != 0)
    abort();
  if (f2u(1.0) != 1)
    abort();
  if (f2u(1.99) != 1)
    abort();
  if (f2u((float) ((~0U) >> 1)) != (~0U) >> 1 &&    /* 0x7fffffff */
      f2u((float) ((~0U) >> 1)) != ((~0U) >> 1) + 1)
    abort();
  if (f2u((float) ~((~0U) >> 1)) != ~((~0U) >> 1))    /* 0x80000000 */
    abort();

 /* These tests require double precision, so for hosts that don't offer
    that much precision, just ignore these test.  */
 if (sizeof (double) >= 8) {
  if (d2u(0.0) != 0)
    abort();
  if (d2u(0.999) != 0)
    abort();
  if (d2u(1.0) != 1)
    abort();
  if (d2u(1.99) != 1)
    abort();
  if (d2u((double) (~0U)) != ~0U)            /* 0xffffffff */
    abort();
  if (d2u((double) ((~0U) >> 1)) != (~0U) >> 1)        /* 0x7fffffff */
    abort();
  if (d2u((double) ~((~0U) >> 1)) != ~((~0U) >> 1))    /* 0x80000000 */
    abort();
 }

 /* These tests require long double precision, so for hosts that don't offer
    that much precision, just ignore these test.  */
 if (sizeof (long double) >= 8) {
  if (ld2u(0.0) != 0)
    abort();
  if (ld2u(0.999) != 0)
    abort();
  if (ld2u(1.0) != 1)
    abort();
  if (ld2u(1.99) != 1)
    abort();
  if (ld2u((long double) (~0U)) != ~0U)            /* 0xffffffff */
    abort();
  if (ld2u((long double) ((~0U) >> 1)) != (~0U) >> 1)    /* 0x7fffffff */
    abort();
  if (ld2u((long double) ~((~0U) >> 1)) != ~((~0U) >> 1))    /* 0x80000000 */
    abort();
 }

  if (f2s(0.0) != 0)
    abort();
  if (f2s(0.999) != 0)
    abort();
  if (f2s(1.0) != 1)
    abort();
  if (f2s(1.99) != 1)
    abort();
  if (f2s(-0.999) != 0)
    abort();
  if (f2s(-1.0) != -1)
    abort();
  if (f2s(-1.99) != -1)
    abort();
  if (f2s((float)(int)~((~0U) >> 1)) != (int)~((~0U) >> 1)) /* 0x80000000 */
    abort();

 /* These tests require double precision, so for hosts that don't offer
    that much precision, just ignore these test.  */
 if (sizeof (double) >= 8) {
  if (d2s(0.0) != 0)
    abort();
  if (d2s(0.999) != 0)
    abort();
  if (d2s(1.0) != 1)
    abort();
  if (d2s(1.99) != 1)
    abort();
  if (d2s(-0.999) != 0)
    abort();
  if (d2s(-1.0) != -1)
    abort();
  if (d2s(-1.99) != -1)
    abort();
  if (d2s((double) ((~0U) >> 1)) != (~0U) >> 1)        /* 0x7fffffff */
    abort();
  if (d2s((double)(int)~((~0U) >> 1)) != (int)~((~0U) >> 1)) /* 0x80000000 */
    abort();
 }

 /* These tests require long double precision, so for hosts that don't offer
    that much precision, just ignore these test.  */
 if (sizeof (long double) >= 8) {
  if (ld2s(0.0) != 0)
    abort();
  if (ld2s(0.999) != 0)
    abort();
  if (ld2s(1.0) != 1)
    abort();
  if (ld2s(1.99) != 1)
    abort();
  if (ld2s(-0.999) != 0)
    abort();
  if (ld2s(-1.0) != -1)
    abort();
  if (ld2s(-1.99) != -1)
    abort();
  if (ld2s((long double) ((~0U) >> 1)) != (~0U) >> 1)        /* 0x7fffffff */
    abort();
  if (ld2s((long double)(int)~((~0U) >> 1)) != (int)~((~0U) >> 1)) /* 0x80000000 */
    abort();
 }
}

int main()
{
  test_integer_to_float();
  test_float_to_integer();
  exit(0);
}
