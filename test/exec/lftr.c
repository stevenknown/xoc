extern void exit(int);
extern void abort();
void printf(char const*,...);
int a[100];
int main()
{
    for (int j = 0; j < 100; j++) { a[j]=j; }
    int i = 1;
    while (1) {
        if (i > 10) { break; }
        a[i * 2] = a[i * 3] + 30;
        i++;
    }
    for (int j = 0; j < 100; j++) { printf("%d,",a[j]); }
    return 0;
}
