long long a,b;
int main()
{
    bool res;
    a = 0x0000fff10000ffffLL;
    b = 0x0000fff10000ffffLL;
    res = a==b;
    if (res) {
        ;
    } else {
        return 1;
    }

    a = 0x0000fff10000ffffLL;
    b = 0x0000fff10000ffffLL;
    res = a!=b;
    if (res) {
        return 2;
    }

    a = 0x0000000100000002LL;
    b = 0x0000000100000003LL;
    res = a==b;
    if (res) {
        return 3;
    }

    a = 0x0000000100000002LL;
    b = 0x0000000100000003LL;
    res = a!=b;
    if (res) {
        ;
    } else {
        return 4;
    }

    a = 0x0000000200000001LL;
    b = 0x0000000300000001LL;
    res = a!=b;
    if (res) {
        ;
    } else {
        return 5;
    }

    a = 0x0000000200000001LL;
    b = 0x0000000300000001LL;
    res = a==b;
    if (res) {
        return 6;
    }

    a = 0x0000000200000001LL;
    b = 0x0000000300000001LL;
    res = a>b;
    if (res) {
        return 7;
    }

    a = 0x0000000200000001LL;
    b = 0x0000000300000001LL;
    res = a<b;
    if (res) {
        ;
    } else {
        return 8;
    }

    a = 0x0000000200000001LL;
    b = 0x0000000200000002LL;
    res = a>b;
    if (res) {
        return 9;
    }

    a = 0x0000000200000001LL;
    b = 0x0000000200000002LL;
    if (a < b) {
        ;
    } else {
        return 10;
    }

    a = 0x0000000200000001LL;
    b = 0x0000000300000001LL;
    res = a<=b;
    if (res) {
        ;
    } else {
        return 11;
    }

    a = 0x0000000200000001LL;
    b = 0x0000000300000001LL;
    res = a>b;
    if (res) {
        return 12;
    }

    a = 0x0000000300000001LL;
    b = 0x0000000300000002LL;
    res = a>b;
    if (res) {
        return 13;
    }

    a = 0x0000000300000001LL;
    b = 0x0000000300000002LL;
    res = a<=b;
    if (res) {
        ;
    } else {
        return 14;
    }

    return 0;
}
