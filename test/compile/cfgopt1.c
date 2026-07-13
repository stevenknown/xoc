typedef struct TKE TKE;
struct TKE {
  TKE *next;
};
void tko();
void foo(TKE *t) {
  for (TKE *t1 = t; t1 != 10;) {
    if (t1 > 1)
      for (TKE *t = 0; t == 11;)
          tko();
    while (t1 == 12)
      t1 = t1->next;
  }
}
