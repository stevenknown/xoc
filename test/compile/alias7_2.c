long long x;
char * p;
char ** p2;
char *** p3;
int foo(int i, char * w)
{
    //x's point-to set have to equal to p's point-to set.
    if (i) {
        p3 = &x;
        p = &i;
    } else {
        p3 = w; //*p3 may alias to p2.
    }
    //p2 = &p;
    //p3 = &p2;
    //***p3 = p; //Actually, ***p3 is alias to x
    p2 = 100;
    *p3 = 20;
    return *p3;
}
