int d;
int foo ()
{
  //d, i are promotable.
  register int i = 0;
  while (i != 1000) {
    d = i % 3;
    i++;
  }
  return i;
}
