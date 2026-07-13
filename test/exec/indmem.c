int printf(char const*,...);
typedef struct _atype {
    float bg[1], cg[1];
    bool ant;
} atype;

bool foo(atype **rng_stream)
{
    typedef float ty[1];
    ty *tt1 = &((*rng_stream)->bg);
    ty *tt2 = (*rng_stream);

    //printf("\n%f,%f\n", (double)(*tt)[0],(double)0.01f);
    //printf("\n%f,%f\n", (double)(*rng_stream)->bg[0], (double)0.01f);

    return (*tt1)[0] == 0.01f;
}

int main()
{
   atype s, *ps;
   s.bg[0] = 0.01f;
   ps = &s;
   return !foo(&ps);
}
