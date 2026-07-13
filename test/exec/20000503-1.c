extern void abort(void);
extern void memset(void * v, int, int);
extern void exit(int);
extern int printf(char const*,...);

unsigned long
sub (int a)
{
  return ((0 > a - 2) ? 0 : a - 2) * sizeof (long);
}

int main ()
{
  if (sub (0) != 0)
    abort ();

  exit (0);
}
