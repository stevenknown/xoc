int g;
int main (int i)
{
  int j,m;
  if (i < 10) { //can NOT remove
    j = 1;
    goto LABEL1;
LABEL1:
    m = 2;
    goto LABEL2;
LABEL2:
    g = 3; //can NOT remove
  }
  if (i > 100) { //can NOT remove
     goto LABEL1; //can NOT remove
  }
  return 0; //can NOT remove
}
