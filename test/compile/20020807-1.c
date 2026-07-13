/* { dg-pacdsp-not-supported { pacdsp does not support variable-length structure (a gcc extension) } }  */

int x;

static int
foo (void)
{
  return 0;
}

static void
bar (void)
{
}

static inline void
baz (void)
{
  char arr[10];

lab:
  if (foo () == -1)
    {
      bar ();
      goto lab;
    }
}

void
test (void)
{
  baz ();
}
