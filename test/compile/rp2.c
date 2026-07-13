int main()
{
    int a[100];
    int * p;
    p  = &a;
    int idx;
    idx = 3;
    int m;
    while (1 == 1) {
        m = (p + 10)[idx];
        if (m == idx) {
            (p + 10)[idx] = 20;
        }
        int n;
        n = (p + 10)[idx];
    }
    return m;
}
