extern void abort(void);
extern void exit(int);
unsigned long sub (int a);
int foo ()
{
  if (sub (0) != 0)
    abort ();
  exit (0);
}
