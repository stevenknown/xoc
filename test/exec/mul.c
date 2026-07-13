extern int printf(char const*,...);
int g;
int main()
{
    g = 20;
    int i;
    for (i = 0; i < 33; i++) {
        g = g*13;
        printf("\n%d\n",g);
    }
    return 0;
}
