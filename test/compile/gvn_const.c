int * g;
int foo()
{
  int rc;
  int *p;
  g = &rc;
  if (rc != 0) {
    p = &rc;
  } else {
    p = &rc;
  }
  if (g == p) {
     return 1;
  }
  return 2; //rce removed
}
