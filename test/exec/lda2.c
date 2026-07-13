typedef struct _atype {
    float bg[1];
    int ant;
} atype;

atype ** w;
int * n;
int foo(atype * a)
{
    w = &(a);
    n = &((*w)->ant);
    float * p1 = (*w)->bg;
    if (*p1 != 0.01f) { return 1; }
    return 0;
}

int main()
{
   atype s;
   s.bg[0] = 0.01f;
   return foo(&s);
}
