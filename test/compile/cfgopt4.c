static int bar();
static int zoo();
typedef struct TK TK;
struct TK {
  TK *next;
};
static void foo(TK *tok) {
  for (TK *tok1 = tok; tok1 != 0;) {
    int kind = bar();
    TK *basety = tok1;
    if (basety > 1)
      for (TK *t = 0;;)
          zoo();
    while (tok1 == 0)
      tok1 = tok1->next;
  }
}
