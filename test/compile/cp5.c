int t1()
{
    int ** p;
    int * q;
    int * x;
    int r;
    int a;
    p = &a;
    x = &r;
    p = &r;
    q = &x;
    *p = *q; // *p = *q, for any x, r: if p->r, q->x->r, add r -> r
    return 0;
}
