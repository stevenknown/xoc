int foo(int aa[], int bb[])
{
    //aa, bb is actually a pointer.
    int i = 5;
    do {
        bb[i] = aa[i]; //should not be removed by DCE.
        i--;
    } while (i >= 0);
    return 0;
}

int main()
{
    int a[10] = {11,12,13,14,15,16};
    int b[10] = {0,0,0,0,0,0,0,0};
    foo(a, b); //C will transfer array address as a parameter.
    if (b[0] != 11) { return 1; }
    if (b[1] != 12) { return 2; }
    if (b[2] != 13) { return 3; }
    if (b[3] != 14) { return 4; }
    if (b[4] != 15) { return 5; }
    if (b[5] != 16) { return 6; }
    return 0;
}
