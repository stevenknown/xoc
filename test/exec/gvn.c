void exit(int);
void printf(char const*,...);
int main ()
{
  int x=2;
  int w;

  //NOTE:VN of i is NOT equal to -2, because its type is u16.
  unsigned short i=-2;

L101:
  w = x / i;
L102:
  w = i / x;
L103:
  if (i / x != 0x7fff) { exit(9); }
  return 0;
}
