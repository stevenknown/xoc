extern int printf(const char *, ...);
char ppword[20];
int flgd;
int s85()
{
  int rc;
  struct tnode
  {
    char tword[20];
    struct tnode *right;
  };
  struct tnode s1;
  union
  {
    char u1[30];
  } u0;
  if ((char*)u0.u1 - (char*)&u0 != 0
      || (char*)u0.u1 - (char*)&u0 != 0)
    printf("");
  s1.right->tword[0] = 1;
  if (ppword[0] != 3)
    printf("");
  return rc;
}
