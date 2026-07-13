int foo(int i)
{
    int x;
    *(float*)&x = 2.3f;
    x = 4;
    return x;
}
