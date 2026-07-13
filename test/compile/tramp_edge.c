static char *quote_makefile(char *s)
{
  char *buf = s;
  for (int i = 0, j = 0; s[i]; i++) {
    switch (i) {
    case '\t':
      for (int k = i; k >= 0 && s[k] == '\\'; k--)
        buf[j] = '\\';
      buf[j] = '\\';
      break;
    }
  }
  return buf;
}
