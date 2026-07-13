void abort();
void exit(int);
void printf(char const*,...);

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
  if (sizeof (double) >= 8) {
    if (d2u((double) ((~0U) >> 1)) != (~0U) >> 1)        /* 0x7fffffff */
      abort();
  }
}

int main()
{
  test_float_to_integer();
  exit(0);
}
