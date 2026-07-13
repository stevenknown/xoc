typedef struct Base
{
  struct Obj *ab_type;
  int ab;
} Base;

typedef struct Obj
{
  Base base;
  int cc;
} Obj;

Obj xxx = {
  { &xxx, 1},
  10
};
