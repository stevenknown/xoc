long long x;
char *** p3;
int foo()
{
    *p3 = 20;
    //Whole tree start at ILD will be replaced, thus its kid LD will be UNDEF.
    return *p3;
}
