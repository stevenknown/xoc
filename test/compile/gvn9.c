int s72()
{
  int rc;
  int k, lrc;
  long l;
  rc = 0;
  k = 5;
  if (++k != 6)
    rc = rc + 8;
  lrc = 0;
  if (l != 26)
    lrc = lrc + 16;
  if (lrc != 0)
    rc = rc + 16;
  return rc;
}
