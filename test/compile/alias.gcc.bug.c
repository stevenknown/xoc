typedef struct {
    unsigned car;
    unsigned cdr;
} cons;

void nconc(unsigned x, unsigned y)
{
    unsigned * ptr = &x;
    ptr = &x;
    while (!(*ptr & 3))
        ptr = & ((cons *)(*ptr))->cdr;
    *ptr = y;
}

