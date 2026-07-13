struct A {
  int ww;
  int arr[10];
} ga;

struct B {
  short vv;
  struct A * gaptr;
} gb;
int main()
{
    ga.arr[2] = 21;
    ga.arr[3] = ga.arr[2];

    struct A * lptr = &ga;
    if (lptr->arr[3] != 21) { return 1; }

    gb.gaptr = &ga;
    if (gb.gaptr->arr[3] != 21) { return 2; }

    return 0;
}
