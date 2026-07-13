void printchar();
static int prints(const char *string, int pad)
{
  int pc = 0;
  if (pad) { pc = '0'; }
  for ( ; *string ; ++string) {
    printchar();
  }
  return pc;
}
