int gv;
void guard(int pp)
{
  unsigned char buff[4];
  while (pp + gv > 0) {
    buff[pp] = (unsigned char)(pp&0x7F);
  }
}
