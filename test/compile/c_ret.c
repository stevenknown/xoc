//In C language, this is
//NOT an error, just a warning. Thus we still give a return result.
//C warning: 'return' with a value, in function returning void.
extern void bar(void *ptrA, void *ptrB, void *ptrC);
void get_bar(void)
{
  return bar;
}
typedef void (*bar_t)(void *ptrA, void *ptrB, void *ptrC);
void foo()
{
  //TODO:report error.
  //bar_t cbb = get_bar();
  //cbb(0,0,0);
}
