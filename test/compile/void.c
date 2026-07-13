long long g;
struct {int x; long long y; double z; } a,b;
int printf(char const* a,...)
{
   return 20;
}
void *malloc(int size)
{
    return 0x1000;
}
static bb;
extern void bar(void *ptrA, void *ptrB, void *ptrC); /* { dg-message "argument 1 of 'bar' must be non-null" } */
void get_bar(void)
{
    return (long long)10;
}
void foo()
{
  int cbb = get_bar();
}

int main()
{
    //g = g % 7;
    a = b;
    printf("Edison!Hello");
    void * pp = malloc(100);
    return 0;
}
