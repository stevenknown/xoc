int g;
int m;
int k;
void printf(char const*,...);
void foo()
{
    for (int i = 0; i < 5; i++) {
        printf("\n11:%u\n",i);
        if (i) {
            g++;
            printf("\n22:%u\n",i);
            continue;

        }
        if (i) { m++; break; }
        printf("\n33:%u\n",i);
        k++;
    }
}
int main()
{
    g = 0;
    k = 0;
    m = 0;
    foo();
    if (g != 4) { return 1; }
    if (k != 1) { return 2; }
    if (m != 0) { return 3; }
    return 0;
}
