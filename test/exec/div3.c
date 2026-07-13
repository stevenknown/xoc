void exit(int);
void printf(char const*,...);
void f1()
{
  int a=5;
  int b=-2;
  int c = a/b;
  printf("\nf1:%d\n",c);
  if (c != -2) { exit(1); }
}
void f2()
{
  long long a=2;
  unsigned int b=-2;
  int c = a/b;
  printf("\nf2:%d\n",c);
  if (c != 0) { exit(2); }
}
void f3()
{
  int a=2;
  unsigned int b=-2;
  int c = a/b; //Prefer UNSIGNED.
  printf("\nf3:%d\n",c);
  if (c != 0) { exit(3); }
}
void f4()
{
  long long a=-2;
  unsigned int b=2;
  int c = a/b;
  printf("\nf4:%d\n",c);
  if (c != -1) { exit(4); }
}
void f5()
{
  long a=-2;
  unsigned int b=2;
  int c = a/b;
  printf("\nf5:0x%x\n",c);
  if (sizeof(long)==8){
    if (c != -1) { exit(5); }
  } else {
    if (c != 0x7fffFFFF) { exit(5); }
  }
}
void f5_1()
{
  long a=2;
  unsigned int b=-2;
  int c = a/b;
  printf("\nf5_1:%d\n",c);
  if (c != 0) { exit(51); }
}
void f5_2()
{
  long a=2;
  unsigned long b=-2;
  int c = a/b;
  printf("\nf5_2:%d\n",c);
  if (c != 0) { exit(51); }
}
void f6()
{
  unsigned long long x=2;
  int i=-2;
  i = x / i; //i will be hoisted to unsigned long long according to C spec.
  printf("\nf6:%d\n",i);
  if (i != 0) { exit(6); }
}
void f6_1()
{
  unsigned long long x=2;
  int i=-2;
  x = x / i; //i will be hoisted to unsigned long long according to C spec.
  printf("\nf6_1:%lld\n",x);
  if (x != 0) { exit(61); }
}

void f6_2()
{
  unsigned long long x=-2;
  int i=2;
  i = x / i; //i will be hoisted to unsigned long long according to C spec.
  printf("\nf6_2:%d\n",i);
  if (i != -1) { exit(6); }

  i=2;
  printf("\nf6_2:0x%llx\n", x / i);
}
void f6_3()
{
  unsigned long long x=-2;
  int i=2;
  x = x / i; //i will be hoisted to unsigned long long according to C spec.
  printf("\nf6_3:0x%llx\n",x);
  if (x != 0x7fffffffffffffffULL) { exit(61); }
}
void f7()
{
  unsigned int x=-2;
  int i=2;
  printf("\nf7:0x%llx\n",x / i);
  printf("\nf7:0x%llx\n",i / x);
  if (x/i != 0x7fffFFFF) { exit(7); }
  if (i/x != 0) { exit(8); }
}
void f7_1()
{
  unsigned int x=2;
  int i=-2;
  printf("\nf7_1:0x%llx\n",x / i);
  printf("\nf7_1:0x%llx\n",i / x);
  if (x/i != 0) { exit(71); }
  if (i/x != 0x7fffFFFF) { exit(78); }
}
void f8()
{
  int x=2;
  unsigned short i=-2;
  printf("\nf8:0x%x\n",x / i);
  printf("\nf8:0x%x\n",i / x);
  if (x/i != 0) { exit(8); }
  if (i/x != 0x7fff) { exit(9); }
}
void f9()
{
  int x=-2;
  unsigned short i=2;
  printf("\nf9:%d\n",x / i);
  printf("\nf9:%d\n",i / x);
  if (x/i != -1) { exit(10); }
  if (i/x != -1) { exit(11); }
}
void f10()
{
  short x=-2;
  unsigned char i=2;
  printf("\nf10:%d\n",x / i);
  printf("\nf10:%d\n",i / x);
  if (x/i != -1) { exit(12); }
  if (i/x != -1) { exit(13); }
}
void f11()
{
  short x=2;
  unsigned char i=-2;
  printf("\nf11:0x%x\n",x/i);
  printf("\nf11:0x%x\n",i/x);
  if (x/i != 0) { exit(14); }
  if (i/x != 0x7f) { exit(15); }
}
int main ()
{
  printf("\n%u,%u,%u\n",sizeof(long),sizeof(unsigned long long),sizeof(int));
  f1();
  f2();
  f3();
  f4();
  f5();
  f5_1();
  f5_2();
  f6();
  f6_1();
  f6_2();
  f6_3();
  f7();
  f7_1();
  f8();
  f9();
  f10();
  f11();
  printf("\nsucc\n");
  return 0;
}
