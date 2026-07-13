int *e, ty;
enum { LEA ,IMM };
enum { CHAR };
void expr(int lev)
{
  int t, *d;
  *++e = IMM; *++e = (ty == CHAR) ? 1 : 4;
}
