/*
 This trivial function returns always 1. But llvm can not optimize it away.
 Get a dump
 clang -c -mllvm -print-after-all -O2 foo.c
*/
int c;
int * p;
int * q;
int e;
int arr[10];
int opt_just_return_1(int a, int b, int d, int x)
{
  while (e) {
    p = &a;
    arr[2] = b;
    q = &d;
    if (c) {
      *p = 1;
      arr[2] = 1;
      d = *p;
    } else {
      *p = 2;
      arr[2] = 2;
      d = 1;
    }

    if (*p == arr[2]) {
      x = *q;
    } else {
      x = 0;
    }
  }
  return x;
}


