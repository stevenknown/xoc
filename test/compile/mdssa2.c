int mdssa2()
{
  int rc;
  int t, lrc, k, j, a, b, c, d, x[16], *p;
  unsigned rs, ls, rt, lt;
  rc = 0;
  a = 3;
  b = 2;
  c = 1;
  lrc = 0;

  for (a = 0; a < 2; a++)
    for (b = 0; b < 2; b++)
      for (c = 0; c < 2; c++)
	for (d = 0; d < 2; d++)
	  if ((a < b == c < d) != x[8 * a + 4 * b + 2 * c + d])
	    lrc = 1;

  if (lrc != 0)
    {
      rc = rc + 16;
    }

  p = 0;

  return rc;
}
