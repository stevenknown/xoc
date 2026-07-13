int main()
{
    int (*arr[10])[20][30];
    if (sizeof(arr) != 40) { return 1; }

    double (*arr2[10])[20][30];
    if (sizeof(arr2) != 40) { return 2; }

    struct S { double (*arr2[10])[20][30]; } s;
    if (sizeof(s) != 40) { return 3; }

    return 0;
}
