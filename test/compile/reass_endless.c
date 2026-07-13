void exit(int);
int tk;
int *e;
void expr(int lev)
{
  int t, *d;
  if (!tk) { ; }
  else if (tk == '"') {
    //  $7=ld e + 4
    //  st e = $7
    //  $10=$7 + 20
    //after REASS:
    //  $7=ld e + 4
    //  st e = $7
    //  $10=ld e + 20
    //then after CP:
    //  $7=ld e + 4
    //  st e = $7
    //  $10= $7 + 20
    //Cause REASS again, lead to a deadloop.
    *++e = 3;
    *++e = 2;
  }
  while (tk >= lev) {
      if (*e == 5) *e = 9; else { exit(-1); }
  }
}
