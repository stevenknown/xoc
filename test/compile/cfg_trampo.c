extern int  printf (const char *, ...);
int s85 ()
{
  union
  {
    char u1[30];
    unsigned u5[30];
  } u0;
  if ((char *) u0.u1 - (char *) &u0 != 0
      || (char *) u0.u5 - (char *) &u0 != 0)
    printf ("");
  return 0;
}
