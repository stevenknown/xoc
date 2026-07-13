extern int  printf (const char *, ...);
int flgd;
void s85()
{
  int rc;
  struct tnode
  {
    char tword[20];
    struct tnode *left;
    struct tnode *right;
  };
  struct tnode s1;
  union
  {
    char u1[30];
  } u0;
  if ((char *) &s1.right - (char *) &s1.left <= 0)
	printf("");
  if ((char *) u0.u1 - (char *) &u0 != 0
      || (char *) u0.u1 - (char *) &u0 != 0)
    {
      if (flgd != 0)
        printf("");
      rc = rc + 4;
    }
  s1.right->tword[0] += 1;
  if (s1.tword[0] != 3)
      rc = rc + 16;
}
