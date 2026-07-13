void crashIt(int id)
{
  char buff[5], reverse[4];
  char *bp = buff;
  char *rp = reverse;
  short int count = 0;
  while (id > 0) {
      *rp++ = id;
      id = 7;
  }
  while ((count--) > 1)
      *bp = *rp;
}
