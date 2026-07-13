int g;
int n;
int main(int b)
{
  int i;
  n = 10;
  //No guard BB generated.
  do {
    i = 20; //should be hoisted.
    PLACE_HOLDER_LABEL: //used to split BB.
    g = b; //should be hoisted.
  } while (n > 0);
  return g + n;
}
