int main ()
{
  unsigned long long pow = 0x24fd3027fe9;
  unsigned long long m = 0x7ffffff;
  pow = pow % m;
  if (pow != 0x302c9e3) {
    return -1;
  }
  return 0;
}
