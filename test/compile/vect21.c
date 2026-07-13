double gd[100];
double gf[100];
int foo(int);
int printf(char const*,...);
void exit(int);
int main ()
{
  int i;
  for (i = 0; i < 100; i++)
    gd[i] = i, gf[i] = i;
  foo (1);
  for (i = 0; i < 100; i++)
    printf("\n%f,%f", (double)gd[i], (double)gf[i]);
  exit (0);
}
