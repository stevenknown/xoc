int foo(unsigned i)
{
    if (i < 0) {
        return 1; //dead code.
    }
    return 2;
}
