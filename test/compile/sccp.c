void foo()
{
    int a;
    int d;
    int f;
    int g;
    a = 3;
    d = 2;
    while (1) {
        f = a + d;
        g = 5;
        a = g - d;
        if (f <= g) {
            f = g+1;
            goto L1;
        } else {
            if (g < a) {
                L1:
                d = 2;
            } else {
                break;
            }
        }
    }
}
