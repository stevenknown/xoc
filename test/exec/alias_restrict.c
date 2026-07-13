void foo_restrict(int * restrict p1, int * restrict p2)
{
    if (*p1 > 10) {
        *p2 = 1; //p1 does not alias with p2
    }
    if (*p1 > 5) {
        *p2 = 2;
    }
}
void foo(int * p1, int * p2)
{
    if (*p1 > 10) {
        *p2 = 1; //p1 may alias with p2
    }
    if (*p1 > 5) {
        *p2 = 2;
    }
}
int main()
{
    int p1, p2;

    p1 = 11;
    foo(&p1, &p1);
    if (p1 != 1) { return 1; }

    p1 = 11;
    p2 = 6;
    foo(&p1, &p2);
    if (p1 != 11 || p2 != 2) { return 2; }

    p1 = 11;
    foo_restrict(&p1, &p1);
    if (p1 != 1) { return 3; }

    p1 = 11;
    p2 = 6;
    foo_restrict(&p1, &p2);
    if (p1 != 11 || p2 != 2) { return 4; }

    return 0;
}
