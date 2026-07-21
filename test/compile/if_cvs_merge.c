int g;
int foo(int a, int b)
{
    if (a > b) {
        g = 0;
    } else {
        g =1;
    }
    return g;
}
int main()
{
    g = 3;
    foo(3, 1);
    return g;
}
