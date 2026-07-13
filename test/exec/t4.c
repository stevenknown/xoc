struct Date
{
  int year;
  int month;
  int day;
};
struct Date date; // = {2012, 10, 12};
int a[3] = {2012, 10, 12};
int test_struct()
{
  int day = date.day;
  int i = a[1];
  i = a[1];

  return 0;
}

int test_control()
{
  int a = 3;
  a = 3;
  if (a != 0)
    a++;
  goto L1;
  a++;
L1:
  a--;
  return a;
}


int rand();
int test_operators()
{
  int a = 5;
  int b = 2;
  int c = 0;
  int d = 0;
  a = 5;
  b = 2;
  c = 0;
  d = 0;
  int e, f, g, h, i, j, k, l = 0;
  l = 0;
  unsigned int a1 = -5, k1 = 0, f1 = 0;
  a1 = -5, k1 = 0, f1 = 0;
  c = a + b;
  d = a - b;
  e = a * b;
  f = a / b;
  f1 = a1 / b;
  g = (a & b);
  h = (a | b);
  i = (a ^ b);
  j = (a << 2);
  int j1 = (a1 << 2);
  j1 = (a1 << 2);
  k = (a >> 2);
  k1 = (a1 >> 2);

  b = !a;
  int * p = &b;
  p = &b;
  b = (b+1)%a;
  c = rand();
//  c = 12;
  b = (b+1)%c;
  return c;
}



int gI;
gI = 100;
int test_globalvar()
{
  int c = 0;

  c = gI;

  return c;
}

int main()
{
  int result = 0;
  result = 0;
  result = test_operators();
  result = test_globalvar();
  result = test_struct();
  result = test_control();
//  printf("a = %d\n", a);

  return result == 3 ? 0 : 1;
}
