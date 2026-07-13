void printf(char const*,...);
int f2s(float f, double x)
{
  return (int) f;
}

int main()
{
  if (f2s(-1.99, 2.1) != -1)
    return -1;
  return 0;
}
