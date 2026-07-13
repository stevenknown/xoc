int a;
double ad;
int b;
int c;
int func1()
{
    int cnt = 0;
    //Reform '((a == a) | (b & c)) == true' into true.
    if ((a == a) | (b & c)) {
        cnt++;
    }
    if ((b & c) | (a == a)) {
        cnt++;
    }
    if ((a == a) || (b & c)) {
        cnt++;
    }
    if ((b & c) || (a == a)) {
        cnt++;
    }
    return cnt;
}
int func2()
{
    int cnt = 0;
    if ((a > 1) & (a > 5)) {
        cnt++;
    }
    if ((1 < a) & (a > 5)) {
        cnt++;
    }
    if ((1 < a) & (5 < a)) {
        cnt++;
    }
    if ((a > 1) & (5 < a)) {
        cnt++;
    }
    if ((a > 5) & (a > 1)) {
        cnt++;
    }
    if ((a > 1) && (a > 5)) {
        cnt++;
    }
    if ((a > 5) && (a > 1)) {
        cnt++;
    }
    return cnt;
}
int func3()
{
    int cnt = 0;
    if ((a < 1) & (a < 5)) {
        cnt++;
    }
    if ((1 > a) & (a < 5)) {
        cnt++;
    }
    if ((1 > a) & (5 > a)) {
        cnt++;
    }
    if ((a < 1) & (5 > a)) {
        cnt++;
    }
    if ((a < 5) & (a < 1)) {
        cnt++;
    }
    if ((a < 1) && (a < 5)) {
        cnt++;
    }
    if ((a < 5) && (a < 1)) {
        cnt++;
    }
    return cnt;
}
int func4()
{
    int cnt = 0;
    if ((ad > 1.0) & (ad > 5.0)) {
        cnt++;
    }
    if ((1.0 < ad) & (ad > 5.0)) {
        cnt++;
    }
    if ((1.0 < ad) & (5.0 < ad)) {
        cnt++;
    }
    if ((ad > 1.0) & (5.0 < ad)) {
        cnt++;
    }
    if ((ad > 5.0) & (ad > 1.0)) {
        cnt++;
    }
    if ((ad > 1.0) && (ad > 5.0)) {
        cnt++;
    }
    if ((ad > 5.0) && (ad > 1.0)) {
        cnt++;
    }
    return cnt;
}
int func5()
{
    int cnt = 0;
    if ((ad < 1.0) & (ad < 5.0)) {
        cnt++;
    }
    if ((1.0 > ad) & (ad < 5.0)) {
        cnt++;
    }
    if ((1.0 > ad) & (5.0 > ad)) {
        cnt++;
    }
    if ((ad < 1.0) & (5.0 > ad)) {
        cnt++;
    }
    if ((ad < 5.0) & (ad < 1.0)) {
        cnt++;
    }
    if ((ad < 1.0) && (ad < 5.0)) {
        cnt++;
    }
    if ((ad < 5.0) && (ad < 1.0)) {
        cnt++;
    }
    return cnt;
}
int main()
{
    a = 6;
    b = 1;
    c = 0;
    int cnt;
    cnt = func1();
    if (cnt !=  4) { return 1; }

    cnt = func2();
    if (cnt !=  7) { return 2; }

    a = -1;
    cnt = func3();
    if (cnt !=  7) { return 2; }

    ad = 6.0;
    cnt = func4();
    if (cnt !=  7) { return 3; }

    ad = -1.0;
    cnt = func5();
    if (cnt != 7) { return 4; }

    return 0;
}
