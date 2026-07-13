extern unsigned int clz32 (unsigned int);

unsigned int clz64 (unsigned long long a)
{
    unsigned int hi = a >> 32;
    if (hi != 0) {
    return clz32(hi);
    }
    return (32 + clz32(a));
}
