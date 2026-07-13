extern int  printf (const char *, ...);
struct defs
{
  int flgd;
};
int s85(struct defs * pd0)
{
  int rc;
  struct tnode
  {
    struct tnode *right;
  };
  struct tnode s1;
  union
  {
    char u1[30];
    unsigned u5[30];
  } u0;
  if ((char *) &s1.right - (char *) &s1.right <= 0)
      rc = 1;
  if ((char *) u0.u1 - (char *) &u0 != 0
      || (char *) u0.u5 - (char *) &u0 != 0)
    {
	  printf("");
      rc = 4;
    }
  s1.right = &s1;
  if (pd0->flgd != 0)
    rc = 16;
  return rc;
}
