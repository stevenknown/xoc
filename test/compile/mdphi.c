int foo(int *p)
{
    int a = 9;
    int i = 0;
    if (i) {
        p = &a;
    }
    *p = 20;
    while (i<100) {
      a = 0; //can be removed.
      i++;
    }
    a = 7;
    while (i) {
      a = 1;
      i--;
    }
    return a;
}

int bar()
{
    int * p;
    char a[100];
    p = &a;
    int i = 0;
    while (i<100) {
      a[i] = 0; //can not be removed.
      i++;
    }
    a[0] = 7; //can not be removed.
    while (i) {
      *p = 1;
    }
    return *p;
}
