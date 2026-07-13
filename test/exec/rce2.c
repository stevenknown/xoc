extern void exit(int);
extern void abort();
void printf(char const*,...);
int a[200];
int main()
{
    for (int j = 0; j < 200; j++) { a[j]=j; }
    int i = 1;
    int c = 49;
    while (1) { //RCE replace falsebr/truebr with goto.
        if (i > 5) { break; }
        a[c * 2] = a[c];
        i++;
        c = c - 2;
    }
    printf("\nsuccess\n");
    for (int j = 0; j < 200; j++) { printf("%d,",a[j]); }
    return 0;
}
