unsigned int powmod_ll(unsigned int aa, unsigned int bb, unsigned int cc)
{
    if (aa != 100) { return 1; }
    if (bb != 200) { return 2; }
    if (cc != 300) { return 3; }
    return 0;
}
int main ()
{
  unsigned long long ii;
  ii = 100;
  return powmod_ll(ii, 200, 300);
}
