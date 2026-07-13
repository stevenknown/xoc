unsigned short test_if()
{
    long long a,c;
    a = 5;
    c = 1;
L1:
    if (c > a) {
        goto L2;
    }
    c = c + c;
    goto L1;
L2:
    a = c - a;
    c = 0;
}



