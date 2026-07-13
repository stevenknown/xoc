//Test MDSSAMgr::insertDefStmt for restore-stmt.
extern int  printf (const char *, ...);
struct defs {
  int flgd;
  char rfs[8];
};
int svt;
int teste;
int s4(struct defs *pd0)
{
  static char s4er[] = "s4,er%d\n";
  int j, rc;
  unsigned target;
  unsigned int mask;
  for (j = 0; j < 3; j++) {
    if (svt != rc) {
       rc = 1;
       if (pd0->flgd != 0)
         printf(s4er, 1);
    }
  }
  //restore of 'rc' inserted here:st rc id:139
  if (teste != 0)
      rc = rc + 2;
  return rc;
}
