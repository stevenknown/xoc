void abort();
void exit(int);
void printf(char const*,...);
float u2f(unsigned int u)
{
  return u;
}
float s2f(int s)
{
  return s;
}
int main()
{
  if (u2f(0x80000000U) != (float)0x80000000U)
    exit(1);
  if (s2f((int)0x80000000) != (float)(int)~((~0U) >> 1)) /* 0x80000000 */
    exit(2);
  return 0;
}
