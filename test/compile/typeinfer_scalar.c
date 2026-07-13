int get2(int);
int get1(int);

int get3()
{
    static int g;
    int i;
    int ret;
    for (i = 0; i < 100; i++) {
        if (g % 11 == 0) {
            g = 5;
            ret = get1(g);
        } else {
            g = 3;
            ret = get2(g);
        }
    }
    return g + ret;
}

char g = 3;

int get2(int a)
{
    return a + 1024 * 13;
}

int get1(int a)
{
    a = 100;
    return a + 1234;
}


