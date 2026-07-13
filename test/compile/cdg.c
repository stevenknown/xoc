int n;
int main(int b)
{
  int i;
  switch (n) {
  case 1:
    b = 10; break;
  case 2:
    b = 12; break;
  case 3:
    b = 13; break;
  }
  n = 10; //the stmt does not have CD bb.
  return n;
}
