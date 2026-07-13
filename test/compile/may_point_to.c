int foo(char * q, char * p)
{
    *p = *q; //*p, *q should be alias.
    return 0;
}

