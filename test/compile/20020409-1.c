int f(int i)
{
  i -= 2 * (0x7fffFFFF + 1);
  return i;
}
