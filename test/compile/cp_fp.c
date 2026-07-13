void printf(char const*,...);
volatile double gd[100];
volatile float gf[100];
int main ()
{
  int i;
  for (i = 0; i < 100; i++)
    printf("\n%f,%f", (double)gd[i], (double)gf[i]);
  return 0;
}
