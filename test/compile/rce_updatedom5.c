extern int  printf (const char *, ...);
int flgd;
void s85 ()
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
  if ((char *) u0.u1 - (char *) &u0 != 0
      || (char *) u0.u1 - (char *) &u0 != 0)
    {
     if (flgd != 0)
        printf ("");
      rc = rc + 4;
    }
  if (sizeof u0 < sizeof u0.u1)
      rc = rc + 8;
  s1.right->tword[0] += 1;
}
