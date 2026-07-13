extern void exit(int);
extern void abort();
void printf(char const*,...);
int a[100];
int main()
{
    for (int j = 0; j < 100; j++) { a[j]=j; }
    int c = 45;
    for (int i = 0; i < 5;) {
        a[i * 2] = a[i * 3] + 30;
        if (i < 10) { i++; }
        c = c - 2;
    }
    printf("\nsuccess\n");
    return 0;
}
