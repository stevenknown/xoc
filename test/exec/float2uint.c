//test soft float operations.
void printf(char const*,...);

unsigned int f2u(float f)
{
  return (unsigned)f;
}

int main()
{
  unsigned int u;
  u = f2u(3.0);
  printf("\n%u\n",u);
  return 0;
}
