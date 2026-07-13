int main()
{
    int *x;
    int p=0x5678;
    int q=0x1234;
    x=&(p=q);
    *x = 100;
    if (p!=100) { return 1; }
    if (q!=0x1234) { return 2; }
    return 0;
}
