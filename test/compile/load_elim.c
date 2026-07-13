typedef struct {
    double x;
    double y;
} POINT;

POINT * p;

int bar(POINT * w);
double foo()
{
    //Twice memory load such as p->x will be gcse+cp+dce optimized.
    //But it does not work better in SSA mode, because, GCSE do not
    //consider SSA info,:(.
    bar(p);
    return p->x * p->x;
}

