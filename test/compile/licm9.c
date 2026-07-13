int g;
int n;
int main(int b)
{
  int i;
  n = 10;
  //No guard BB generated.
  while (n > 0) {
    i = 20; //should be hoisted.
    PLACE_HOLDER_LABEL: //used to split BB.
    g = b; //b should be hoisted.
  }
  return g + n;
}
