int main ()
{
  unsigned long long x;
  int i;
  x = -1;
  i = -1;
  x = x + i; //x=-2 //64bit addition with carry bit.
  x = x - i; //x=-1
  x = x * i; //x=1
  x = x / i; //i should be 0xFFFFffff, so x = 1/0xFFFFffff = 0
  if (x != 0) { return -1; }
  return 0;
}
