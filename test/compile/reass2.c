int g;
int foo()
{
    int c = g * 40;
    int d = c * 20;
    return d;
}

int main()
{
    g = 3;
    int v = foo();
    return !(v == 2400); 
}
