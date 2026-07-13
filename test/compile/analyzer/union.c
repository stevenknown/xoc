union u
{
  int i;
  int *ptr;
};

void test_41 (void)
{
  union u u;
  u.i = 42;
}
