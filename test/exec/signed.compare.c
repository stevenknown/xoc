int main()
{
    int x;
    x = 0xFFFFFFFF;
    x++;
    if (x > (int)0x7FFFFFFF) {
        return 1;
    }

    x = 0;
    x--;
    if (x < (int)0x80000000) {
        return 2;
    }

    x = 0x80000000;
    x--;
    if (x < (int)0x80000000) {
        return 3;
    }
    return 0;
}
