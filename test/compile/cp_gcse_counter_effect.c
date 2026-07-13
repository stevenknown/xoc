struct A
{
  void *a;
  unsigned int b, c, d;
};
struct B
{
  struct A *e;
};
void bar (struct A *);
void baz (struct A *);
static inline unsigned int inl ();
void foo (struct B *x, int y)
{
  struct A * a = x->e;
  if (y)
     bar(a);
  else
    {
      unsigned char c;
      unsigned int b;
      c = 0;
      b = inl();
      if (a->a)
        bar(a);
   }
}
