int foo()
{
    //base's point-to set have to equal to p's point-to set.
    long long x;
    char * p;
    p = &x;

    char ** p2;
    p2 = &p;

    char *** p3;
    p3 = &p2;

    ***p3 = p; //equal to: x = p;
    return ***p3; //equal to: return x;
}
