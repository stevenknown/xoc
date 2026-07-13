int mdssa_test()
{
  int lrc, a, b, d, x[16];
  a = 3;
  b = 2;

  for (b = 0; b < 2; b++)
    for (d = 0; d < 2; d++)
      if (a != x[a])
        lrc = 1;

  if (lrc != 0)
    return 1;
  return 0;
}
