int printf(char const*,...);
int g;
void foo()
{
  g = 20;
}

int main()
{
  g = 10;
  foo();
  printf("\ng=%d\n", g);
  return 0;
}
