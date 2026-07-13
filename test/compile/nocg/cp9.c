typedef struct {
    unsigned cdr;
} cons;
void nconc(unsigned x)
{
    unsigned * ptr = &x;
    cons * z;
    while (!(*ptr))
        ptr = z->cdr;
    *ptr = x;
}

