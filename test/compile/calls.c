typedef void  *(*T)(void);
void f1 ()
{
  ((T) 0)();
}
void f2 ()
{
  ((T) 1000)();
}
void f3 ()
{
  ((T) 10000000)();
}
void f4 (int r)
{
  ((T) r)();
}
void f5 ()
{
  int (*r)() = f3;
  ((T) r)();
}

