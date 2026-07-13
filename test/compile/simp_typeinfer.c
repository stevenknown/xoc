char g = 3;

void print(int a);

int get1(int a)
{
    return a + 1234;
}

int get2(int a)
 {
    return a + 1024 * 13;
}

void run1()
{
    for (int i = 0; i < 1000; i++) {
        if (i % 2) {
            g = get1(g);
        } else {
            g = get2(g);
        }
    }
    print(g);
}

void run2()
{
    struct { long long y; short x:3; } a;
    a.x=4;
    struct { double y; unsigned char x;} b;
    //b = a;
    print(b.x);
    a.y=5;
    print(a.y);
    print(b.y);
}

void run3()
{
    int a[100];
    //a["xxx"]=30;
    //a["hello"]=30;
    a[5000]=30;
    a[5001]=30;
}
