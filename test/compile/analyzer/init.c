typedef struct obj obj;
struct obj
{
  int  base;
  char buf[];
};
int a = sizeof(obj);
void foo()
{
    int la = sizeof(obj);
}
