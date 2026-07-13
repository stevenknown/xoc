void exit(int);
int tk;
int *e;
void expr(int lev)
{
  if (!tk) { exit(-1); }
  else if (tk == '"') {
    *++e = 4;
    *++e = 3;
  }
  while (tk >= lev) {
      if (*e == 7) *e = 1;
      else { exit(-1); }
  }
}
