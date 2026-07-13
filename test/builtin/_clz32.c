unsigned int clz32(unsigned int a)
{
    unsigned int marker = 1 << (32 - 1);
    unsigned int i = 0;
    for (; i < 32; i++) {
        if ((marker & a) == marker) { break; }
        marker = marker >> 1;
    }
    return i;
}
