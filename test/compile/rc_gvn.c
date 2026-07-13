extern void abort();
extern void exit(int);
extern int x(int, char **);
int check = 0;
int main (int argc, char **argv)
{
  check = 0;
  char *args[] = {"a", "b", "c", "d", "e"};
  //check may be modified in x().
  //check's VN is invalid.
  //CP of check is invalid.
  if (x (5, args) != 0 || check != 2)
    abort ();
  exit (0);
}
