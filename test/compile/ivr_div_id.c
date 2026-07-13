int g;
int s81()
{
  int k, j, prc;
  register int *rptr;
  int *nrptr;
  prc = 0;
  k = 1;
  for (j = 0; j < 50; j++) { //j is BIV
    rptr = &k; //rptr is DIV
    nrptr = &k; //nrptr is DIV
    if (rptr != nrptr)
      prc = j; //prc is DIV, because if() stmt will be removed.
    k = k << 1;
  }
  if (prc != 0)
    g=1;
}
