void printf(char const*,...);
int main()
{
  printf("\n>>%x\n\n", ((~0U) >> 1));
  return 0;
}
