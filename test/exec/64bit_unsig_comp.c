unsigned long long a,b;
int main()
{
    a = 0x0000fff10000ffffLL;
    b = 0x0000fff10000ffffLL;
    if (a == b) {
        ;
    } else {
        return 1;
    }

    a = 0x0000fff10000ffffLL;
    b = 0x0000fff10000ffffLL;
    if (a != b) {
        return 2;
    }

    a = 0x0000000100000002LL;
    b = 0x0000000100000003LL;
    if (a == b) {
        return 3;
    }

    a = 0x0000000100000002LL;
    b = 0x0000000100000003LL;
    if (a != b) {
        ;
    } else {
        return 4;
    }

    a = 0x0000000200000001LL;
    b = 0x0000000300000001LL;
    if (a != b) {
        ;
    } else {
        return 5;
    }

    a = 0x0000000200000001LL;
    b = 0x0000000300000001LL;
    if (a == b) {
        return 6;
    }

    a = 0x0000000200000001LL;
    b = 0x0000000300000001LL;
    if (a > b) {
        return 7;
    }

    a = 0x0000000200000001LL;
    b = 0x0000000300000001LL;
    if (a < b) {
        ;
    } else {
        return 8;
    }

    a = 0x0000000200000001LL;
    b = 0x0000000200000002LL;
    if (a > b) {
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
    if (a <= b) {
        ;
    } else {
        return 11;
    }

    a = 0x0000000200000001LL;
    b = 0x0000000300000001LL;
    if (a >= b) {
        return 12;
    }

    a = 0x0000000300000001LL;
    b = 0x0000000300000002LL;
    if (a >= b) {
        return 13;
    }

    a = 0x0000000300000001LL;
    b = 0x0000000300000002LL;
    if (a <= b) {
        ;
    } else {
        return 14;
    }

    a = 0xf000000200000001LL;
    b = 0xf000000300000001LL;
    if (a > b) {
        return 15;
    }

    a = 0xf000000200000001LL;
    b = 0xf000000300000001LL;
    if (a < b) {
        ;
    } else {
        return 16;
    }

    a = 0xf000000200000001LL;
    b = 0xf000000200000002LL;
    if (a > b) {
        return 17;
    }

    a = 0xf000000200000001LL;
    b = 0xf000000200000002LL;
    if (a < b) {
        ;
    } else {
        return 18;
    }

    return 0;
}
