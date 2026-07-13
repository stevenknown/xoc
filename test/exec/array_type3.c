struct A {
  int ww;
  int arr[40];
} ga;

struct B {
  short vv;
  struct A * gaptr;
} gb;
int main()
{
    ga.arr[27] = 100;
    ga.arr[33] = ga.arr[27];

    int * parr = ga.arr;
    if (parr[33] != 100) { return 1; }

    struct A * gaptr = &ga;
    int * parr2 = gaptr->arr;
    if (parr2[27] != 100) { return 2; }
    return 0;
}
