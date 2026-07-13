struct T
{
  unsigned int b, c, *d;
  unsigned char e;
};
typedef struct T DefT;

struct S
{
  unsigned int a;
  DefT f;
};

struct U
{
  struct S g;
  struct S h;
};
typedef struct U DefU;
//struct V
//{
//  unsigned int i;
//  struct U j;
//};

int main()
{
  DefT t = { 1, 2, (void*)0x1000, 3};
  if (t.b != 1) { return 1; }
  if (t.c != 2) { return 1; }
  if (t.d != (void*)0x1000) { return 1; }
  if (t.e != 3) { return 1; }

  struct S s = { 4, {5, 6} };
  if (s.a != 4) { return 2; }
  if (s.f.b != 5) { return 2; }
  if (s.f.c != 6) { return 2; }

  DefU u = {
    {7, {8, 9} },
    {10, {11,12,(char*)13,14} }
  };
  if (u.g.a != 7) { return 3; }
  if (u.g.f.b != 8) { return 3; }
  if (u.g.f.c != 9) { return 3; }
  if (u.h.a != 10) { return 3; }
  if (u.h.f.b != 11) { return 3; }
  if (u.h.f.c != 12) { return 3; }
  if (u.h.f.d != (char*)13) { return 3; }
  if (u.h.f.e != 14) { return 3; }

  return 0;
}

