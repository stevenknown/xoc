void printf(char const*,...);
int originfunc() {
    int a;
    int p;
    int t;

    a = 1;
    p = 0;
    t = 0;

    do {
        printf("%d\n", a);
        t = a;
        a = t + p;
        p = t;
    } while (a < 100);

    return 0;
}

int main()
{
    if (originfunc() == 0) {
        return 0;
    }
    return -1;
}
