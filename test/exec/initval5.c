struct T
{
  int b;
  char e[20];
};
struct S
{
  unsigned int a;
  struct T m[10];
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
    struct U u = {
      {  //u.g
         1, //u.g.a

         {  //u.g.m
           { //u.g.m[0]
              2, //u.g.m[0].b
              {  //u.g.m[0].e
                 3,  //u.g.m[0].e[0]
                 4,  //u.g.m[0].e[1]
              },
           },

           { //u.g.m[1]
              10, //u.g.m[1].b
              {  //u.g.m[1].e
                 5,  //u.g.m[1].e[0]
                 6,  //u.g.m[1].e[1]
              },
           },
         },

         {  //u.g.f
            7,  //u.g.f.b
            { //u.g.f.e
               8, //u.g.f.e[0]
               9,  //u.g.f.e[0]
            },
         },
      }
    };

    if (1 != u.g.a) { return 1; }
    if (2 != u.g.m[0].b) { return 2; }
    if (3 != u.g.m[0].e[0]) { return 3; }
    if (4 != u.g.m[0].e[1]) { return 4; }

    if (10 != u.g.m[1].b) { return 5; }
    if (5 != u.g.m[1].e[0]) { return 6; }

    if (6 != u.g.m[1].e[1]) { return 7; }
    if (7 != u.g.f.b) { return 8; }
    if (8 != u.g.f.e[0]) { return 9; }
    if (9 != u.g.f.e[1]) { return 10; }

    return 0;
}
