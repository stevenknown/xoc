void printf(char const*, ...);

/*
output:
7, 6, 5
7, 6, 5
a = 8
*/
int test2_of_sideeffect(void)
{
  int a;
  a = 5;
  printf("%d, %d, %d\n", (a++) , (a++) , (a++));
  printf("a = %d\n", a);
  return 0;
}


int test1_of_sideeffect(void)
{
  int a;
  a = 5;
  printf("%d\n", (a++) + (a++) + (a++));
  printf("a = %d\n", a);
  return 0;
}


int main(void)
{
    test1_of_sideeffect();
    test2_of_sideeffect();
    return 0;
}
