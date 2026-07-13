extern int printf(const char *, ...);
void s85()
{
  struct tnode
  {
    char tword[20];
    struct tnode *right;
  };
  struct tnode s1, s2;
  union
  {
    char u1[30];
  } u0;
  if ((char *) u0.u1 - (char *) &u0 != 0
      || (char *) u0.u1 - (char *) &u0 != 0)
    printf("");
  s1.right = &s2;
  s2.tword[0] = 2;
  if (s2.tword[0] != 3)
	printf("");
}
