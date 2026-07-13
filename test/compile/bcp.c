int g;
int m;
int k;
void foo()
{
    for (int i = 0; i < 5; i++) {
        if (i) { g++; continue; }
        if (i) { m++; break; }
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
