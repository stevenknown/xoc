void * malloc(int);
int main()
{
  int *sp;
  int *t;
  sp = malloc(1);
  *--sp = (int)t;
  return 0;
}
