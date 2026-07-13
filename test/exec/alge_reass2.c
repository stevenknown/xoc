int printf(char const*,...);

int t1 = 3;
int t2 = 4;
int t3 = 5;
int t4 = 6;
int t5 = 7;
int t6 = 8;
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
    //c * (a + b) + d * (a + b) + e ---> (c + d) * (a + b) + e
    int l = c * (a + b) + d * (a + b) + e;
    while (i++ < 10) {
        e++;
    }
    int x = l * 2;
    return x;
}

int main()
{
    t1 = 3;
    t2 = 4;
    t3 = 5;
    t4 = 6;
    t5 = 7;
    t6 = 8;
    a = 1;
    b = 2;
    c = 3;
    d = 4;
    e = 5;
    f = 6;
    g = 7;
    h = 8;
    foo(1);
    if (a != 1 || b != 2 || c != 3 || d != 4 || e != 14 || f != 6 ||
        g != 7 || h != 8) {
        return 1;
    }
    return 0;
}
