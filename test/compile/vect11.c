int foo(int num, int * restrict m, int * restrict n, int * restrict z)
{
    for (int i = 0; i < num;i++) {
        n[i] = z[i] << 3;
        m[i] = n[i] + 7;
    }
}
