typedef struct {
    char * p;
    char * q;
    char * x;
    char * y;
    char * z;
} V;
V extfun2(char * a, char * b);
char* oo()
{
    char l1, l2;
    char * x = extfun2(&l1, &l2).z; //x may alias to l1, l2.
    return x;
}

