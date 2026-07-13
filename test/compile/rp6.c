int d;
int foo ()
{
  register int i = 0;
  while (i != 1000) {
    d = d % 3;
    i++;
  }
  return i;
}
