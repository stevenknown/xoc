/* { dg-additional-options "-Wno-implicit-int" } */

extern void foo (void *);

void
en (void)
{
}

void
p2 ()
{
  char *rl = 0;

  en ();
  foo (rl); /* { dg-warning "use of NULL 'rl' where non-null expected" } */
}
