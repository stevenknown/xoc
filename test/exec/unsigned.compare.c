int main()
{
    unsigned int x;
    x = 0xFFFFFFFF;
    x++;
    if (x > (unsigned int)0xFFFFFFFF) {
        return 1;
    }

    x = 0;
    x--;
    if (x < 0x0) {
        return 2;
    }
    return 0;
}
