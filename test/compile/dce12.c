int g;
int main (int i)
{
  int j,m;
  if (i < 10) { //can NOT remove
    j = 1;
LABEL1:
    g = 2; //can NOT remove
  } else {
    j = 4;
LABEL3:
    g = 5; //can NOT remove
  }
  return 0; //can NOT remove
}
