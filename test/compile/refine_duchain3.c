int foo(int * p)
{
    int la[100];
    int lb[200];
    int * lq = &lb;
    *p = 10; //*p should NOT dependent to la[1].
    la[0] = 20;
    return la[1]+lb[1];
}
