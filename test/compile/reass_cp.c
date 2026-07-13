//The case causes the reass and cp are executed over and over again.
int * e;
int tk;
void expr(int lev)
{
  int t, *d;
  while (lev) {
    if (tk) {
      *++e = 10;
      *d = e + 3;
      d = e;
    }
  }
}
