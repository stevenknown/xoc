int w1, w2;
void test_local_killing_def(int a, int b, int * p)
{
    if (a > b) {
        w1 = *p;
        p = &w2;
        a = b - 1;
        p++;
        b--;
        w1 = p;
    } else {
        b++;
    }
    return a + b;
}

int g1,g2;
long long g3;
long long f2(int * p)
{
    *p = 10;
    g1 = g2;
    return (long long)g1+g2;
}

