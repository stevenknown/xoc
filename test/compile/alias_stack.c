void foo(int*);
void main()
{
    int stk;
    int * sp = &stk;
    int * p = sp;
    int q2 = 0;
LOOP:
    char * r912 = p + q2;
    *r912 = 0; //should not be removed by DCE. It is used by foo().
    q2++;
    if (q2 < 98) {
        goto LOOP;
    }
    int * param0 = p + 6;
    foo(param0);
}
