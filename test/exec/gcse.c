int a = 0;
int b = 0;
int c = 0;
int d = 0;
int e = 0;
int f = 0;
int g = 0;
int h = 0;
                                                                                
int foo(int i)
{
    int l = c * (a + b) + d * (a + b) + e;
    while (i++ < 10) {
        e++;
    }
    int x = l * 2;
    return x;
}
                                                                                
int main()
{
    a = 1;
    b = 2;
    c = 3;
    d = 4;
    e = 5;
    f = 6;
    g = 7;
    h = 8;
    int x = foo(1);
    if (x != 52) {
        return 1;
    }
    return 0;
}
