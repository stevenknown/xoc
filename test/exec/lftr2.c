extern void exit(int);
extern void abort();
void printf(char const*,...);
int a[200];
int main()
{
    for (int j = 0; j < 200; j++) { a[j]=j; }
    int i = 1;
    int c = 49;
    int d = 1;
    int e = 2;
    while (1) {
        if (i > 5) { break; }
        a[c * 2] = a[d * 3] + 30 + a[e];
        i++;
        c = c - 2;
        d = 2*i + 3;
        e = -4 + 7*d;
    }
    for (int j = 0; j < 200; j++) { printf("%d,",a[j]); }
    return 0;
}
