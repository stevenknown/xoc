struct T
{
  unsigned int b, c, *d;
  unsigned char e;
};
struct S
{
  unsigned int a;
  int m[10];
  struct T f;
};
struct U
{
  struct S g;
  int n[10];
  struct S h;
};

int main()
{
  struct S s = { 1, {2, 3}, {4, 5} };

  if (s.a !=  1) { return 1; }
  if (s.m[0] != 2) { return 1; }
  if (s.m[1] != 3) { return 1; }
  if (s.f.b != 4) { return 1; }
  if (s.f.c != 5) { return 1; }

  struct U u = {
    { //u.g
      7, // u.g.a
      { //u.g.m
        8, //u.g.m[0]
        9, //u.g.m[1]
      },
    },
    { //u.n
      20, //u.n[0]
      21, //u.n[1]
      22, //u.n[2]
    },
    { //u.h
      10, //u.h.a
      {
        15, //u.h.m[0]
        16, //u.h.m[1]
      },
      { //u.h.f
        11, //u.h.f.b
        12, //u.h.f.c
        (unsigned char*)13, //u.h.f.d
        14, //u.h.f.e
      },
    },
  };

  if (u.g.a != 7) { return 3; }
  if (u.g.m[0] != 8) { return 3; }
  if (u.g.m[1] != 9) { return 3; }
  if (u.n[0] != 20) { return 3; }
  if (u.n[1] != 21) { return 3; }
  if (u.n[2] != 22) { return 3; }

  if (u.h.a != 10) { return 3; }
  if (u.h.m[0] != 15) { return 3; }
  if (u.h.m[1] != 16) { return 3; }

  if (u.h.f.b != 11) { return 3; }
  if (u.h.f.c != 12) { return 3; }
  if (u.h.f.d != (unsigned char*)13) { return 3; }
  if (u.h.f.e != 14) { return 3; }

  return 0;
}
