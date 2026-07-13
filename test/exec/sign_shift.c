int main()
{
    long long a;
    a = 0xFFFFf24fd3027fe9llu;
    unsigned int hi;
    hi = a >> 32;
    if (hi != 0xfffff24f) { return 1; }

    a = 0xFFFFf24fd3027fe9llu;
    hi = a >> 33;
    if (hi != 0xfffff927) { return 2; }

    a = 0xFFFFf24fd3027fe9llu;
    hi = a >> 62;
    if (hi != 0xffffffff) { return 3; }

    a = 0x8000FFFF12345678llu;
    hi = a >> 63;
    if (hi != 0xffffffff) { return 4; }

    a = 0x8000FFFF12345678llu;
    hi = a >> 64;
    if (hi != 0xffffffff) { return 5; }

    unsigned long long b;
    unsigned long long hx;

    b = 0x8FFFffff12345678llu;
    hx = b >> 63;
    if (hx != 1) { return 6; }

    b = 0x8FFFffff12345678llu;
    hx = b >> 64;
    if (hx != 0) { return 7; }

    return 0;
}
