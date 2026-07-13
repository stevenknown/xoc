typedef struct {
    char * p;
    char * q;
} U;
U extfun(char * a, char * b);
char* oo()
{
    char l1, l2;
    char * x = extfun(&l1, &l2).q; //x may point to l1, l2.
    return x;
}

