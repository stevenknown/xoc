int main()
{
    int a[4][5][6][7];
    a[1][2][3][4] = 0x1234ABCD;
    int * p;
    p = &a[0][0][0][0];
    int offset = 319 * sizeof(int);
    p = (int*)(((char*)p)+offset);
    if (*p != 0x1234ABCD) {
        return 1;
    }
    return 0;
}
