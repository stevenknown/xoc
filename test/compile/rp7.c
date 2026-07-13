int d;
int a[100];
int foo ()
{
  register int i = 0;
  while (i != 1000) {
    a[i] = d % 3;
    i++;
  }
  return i;
}
