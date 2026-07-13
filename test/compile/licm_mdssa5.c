int * p;
int * q;
int arr[10];
void foo(int a, int d)
{
  while (a) {
    p = &a;
    arr[2] = d;
    q = &d;
    d = *p;
  }
}
