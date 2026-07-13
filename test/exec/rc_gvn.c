extern void abort();
extern void exit(int);
extern int x(int, char **);
int check = 0;
int main (int argc, char **argv)
{
  check = 0;
  char *args[] = {"a", "b", "c", "d", "e"};
  if (x (5, args) != 0 || check != 2)
    abort ();
  exit (0);
}
int x (int argc, char **argv)
{
  check = 2;
  return 0;
}
