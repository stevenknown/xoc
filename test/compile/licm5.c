struct defs {
  int ubits;
};

int main(struct defs * pd0)
{
  char *ps, *pt;
  int rc, t, lrc, k, j;
  while (*pt = *ps) { //Because ps may alias to pt, *ps is NOT loop invariant.
                      //But *ps can be promoted by RP.
    //never quitted loop
  }
  for (k = 0; k < pd0->ubits; k++) //pd0->ubits is promoted by RP.
    for (j = 0; j < pd0->ubits; j++) //pd0->ubits is promoted by RP.
      lrc = 1;
  return rc;
}
