extern int printf(char const*,...);
void *memcpy(void *dest, const void *src, int n);
struct T
{
  unsigned e;
  unsigned d;
  unsigned f;
  unsigned g;
};

struct V
{
  long long w;
  struct T name;
};

void init(struct T * p)
{
  p->e = 20;
  p->d = 30;
  p->f = 40;
  p->g = 50;
}

void page(struct V * v, struct V * ww)
{
  v->name = (*ww).name;
}

void assign(struct V * v, struct T * t)
{
  //printf("\nassign:%d,%d,%d,%d\n", t->e, t->d,t->f,t->g);
  v->name = *t;
  //printf("\nassign:%d,%d,%d,%d\n", v->name.e, v->name.d, v->name.f, v->name.g);
}


int foo()
{
  struct V v;
  struct T t;
  t.e = 20;
  t.d = 30;
  t.f = 40;
  t.g = 50;
  v.name = t;
  printf("\n%d,%d,%d,%d\n", v.name.e, v.name.d,v.name.f,v.name.g);
  return 0;
}


int main()
{
  struct V v;
  struct T t;
  init(&t);
  printf("\n%d,%d,%d,%d\n", t.e, t.d, t.f, t.g);
  assign(&v, &t);
  printf("\n%d,%d,%d,%d\n", v.name.e, v.name.d, v.name.f, v.name.g);
  foo();

  return 0;
}
