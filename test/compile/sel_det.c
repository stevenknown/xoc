int s25 ()
{
  char *s, *s2;
  int rc, lrc, j;
  s2 = "queep!";
  s = "queep!";
  for (j = 0; j < 6; j++)
    if (s[j] != s2[j])
      lrc = 1;
  if (lrc != 0)
      rc = 32;
  return rc;
}
