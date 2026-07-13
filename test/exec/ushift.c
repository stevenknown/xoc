int main()
{
    unsigned long long a;
    a = 0x24fd3027fe9llu;
    unsigned int hi;
    hi = a >> 32;
    if (hi != 0x24f) { return -1; }
    return 0;
}
