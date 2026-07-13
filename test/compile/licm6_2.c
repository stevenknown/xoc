int g;
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
  n = 10; 
  do {
    i = 20; //should be hoisted.
    PLACE_HOLDER_LABEL: //used to split BB.
    g = b; //should be hoisted.
  } while (n > 0);
  return g + n;
}
