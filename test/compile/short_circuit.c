extern int bar();
int foo()
{
    int a = 0;
    !a && bar();
    return 1;
}
