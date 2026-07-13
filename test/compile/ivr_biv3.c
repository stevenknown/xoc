int foo()
{
    int i;
    int a;
    int * p;
    for (i = 0; i < 100; i++) {
        *p++ = 22; //*p is NOT alias to i. i should be IV.
    }
    return a;
}
