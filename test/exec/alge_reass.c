int printf(char const*,...);

int t1 = 3;
int t2 = 4;
int t3 = 5;
int t4 = 6;
int t5 = 7;
int t6 = 8;
int c = 0;
int d = 0;
int e = 0;
int k = 0;
int g = 0;
int h = 0;

int foo(int i)
{
    //Only MUL.
    int l = t5 * t3;
    g = t6 * t4;
    int l2 = l * 7;
    h = l2 * 2;
    while (i++ < 10) {
        e++;
    }
    int x = g * h;
    x = x * 3;
    return x;
}

int foo1(int i)
{
    //Combine ADD and MUL.
    int l = t5 * t3;
    g = t6 * t4;
    int l2 = l * 7;
    h = l2 * 2;
    k = 4 + t3;
    d = (t2 + 20) + k;
    while (i++ < 10) {
        e++;
    }
    int w = t1 + d; //t1 is region live-in.
    int u = t1 + d; //t1 is region live-in.
    int x = g * h;
    return w+u+x;
}

int foo2(int i)
{
    //Only ADD.
    d = (t2 + 20) + 3;
    if (i > 0) {
        e = 20;
    }
    return t1 + d; //t1 is region live-in.
}

int foo3()
{
    c = ((10 + t1) + t2) + 20;
    return 0;
}

int foo4()
{
    c = (10 + t1) + 2 + (t2 + 20) + 3 + (t3 + 30);
    return 0;
}

int foo5(int i)
{
    d = (t2 + 20) + 3;
    if (i > 0) {
        e = 20;
    }
    c = (10 + t1) + 2 + d + (t3 + 30);
    return 0;
}

int main()
{
    t1 = 3;
    t2 = 4;
    t3 = 5;
    t4 = 6;
    t5 = 7;
    t6 = 8;
    c = 0;
    d = 0;
    e = 0;
    k = 0;
    g = 0;
    h = 0;
    foo(1);
    if (c != 0 || d != 0 || e != 9 || k != 0 || g != 48 || h != 490) {
        return 1;
    }

    foo1(0);
    if (c != 0 || d != 33 || e != 19 || k != 9 || g != 48 || h != 490) {
        return 2;
    }

    foo2(1);
    if (c != 0 || d != 27 || e != 20 || k != 9 || g != 48 || h != 490) {
        return 3;
    }

    foo3();
    if (c != 37 || d != 27 || e != 20 || k != 9 || g != 48 || h != 490) {
        return 4;
    }

    foo4();
    if (c != 77 || d != 27 || e != 20 || k != 9 || g != 48 || h != 490) {
        return 5;
    }

    foo5(1);
    if (c != 77 || d != 27 || e != 20 || k != 9 || g != 48 || h != 490) {
        return 6;
    }
    return 0;
}
