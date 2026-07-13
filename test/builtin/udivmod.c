unsigned long udivmodsi4(unsigned long num, unsigned long den, int modwanted);

long
__udivsi3 (long a, long b)
{
  return udivmodsi4 (a, b, 0);
}

long
__umodsi3 (long a, long b)
{
  return udivmodsi4 (a, b, 1);
}

