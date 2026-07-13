void printf(const char*,...);
float gf[32];
int main ()
{
  int i;
  for (i = 0; i < 32; i++)
    gf[i] = i;

  for (i = 0; i < 32; i++)
    printf("\n%f", (double)gf[i]);

  return 0;
}
