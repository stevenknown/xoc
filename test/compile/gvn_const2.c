int * g;
int foo()
{
  int rc;
  int rc2;
  int *p;
  g = &rc2;
  if (rc != 0) {
    p = &rc;
  } else {
    p = (long long*)(char*)(void*)&rc;
  }
  if (g == p) {
     return 1; //rce removed
  }
  return 2;
}
