int s7813()
{
  register int prlc, lrc;
  int i, j, r;
  int rc;
  i = j = 0;
  r = i++ && j++;
  if (r != 0)
    if (prlc)
      rc = lrc;
  return rc;
}
