int printf(char const*,...);
typedef struct _atype {
    float bg[1], cg[1];
    bool ant;
} atype;

int foo(atype * a)
{
    float * p1 = a->bg;
    float (*p2)[1] = &a->bg;
    float * p3 = &a->bg[0];

    if (*p1 != (*p2)[0]) { return 1; }
    if (*p1 != *p3) { return 2; }
    if ((*p2)[0] != *p3) { return 3; }
    if (*p1 != 0.01f) { return 4; }
    return 0;
}

int main()
{
   atype s;
   s.bg[0] = 0.01f;
   return foo(&s);
}
