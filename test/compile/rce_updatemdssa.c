int g;
int main()
{
  int j;
  static int (*sec[])() = {0};
  g = 1;
  if (g != 0)
    g=2;
  return 0;
}
