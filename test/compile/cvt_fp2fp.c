void printf(char const*,...);
volatile double gd[100];
volatile float gf[100];
int main ()
{
  printf("\n%f,%f", (double)gd[9], (double)gf[0]);
}
