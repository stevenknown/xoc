char *p, *lp;
int tk,
    ty,
    line;
char *pp;
int memcmp(const void *s1, const void *s2, unsigned long n);
void next()
{
  while (tk = *p) {
    if (tk == 10) {
      ++line;
    } else if ((tk >= 'a') || (tk >= 'A')) {
      while ((*p >= 'a'))
        tk = tk + *p++;
      tk = tk << 6;
      while (pp) {
        if (tk == ty && !memcmp(lp, pp, 100)) { tk = *pp; }
      }
      return;
    }
  }
}
