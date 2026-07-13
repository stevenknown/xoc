unsigned int powmod_ll(unsigned long long aa, unsigned bb, unsigned long long cc)
{
    if (aa != 100) { return 1; }
    if (bb != 200) { return 2; }
    if (cc != 300) { return 3; }
    return 0;
}
int main ()
{
  unsigned long long ii;
  unsigned long long jj;
  ii = 100;
  jj = 300;
  return powmod_ll(ii, 200, jj);
}
