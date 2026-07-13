unsigned long long g;
unsigned int g2;
int g3;
int x;
void printf(char const*,...);
void foo()
{
    g = g >> 35;
    printf("\n%llu\n", g);
}

void bar()
{
    g = g << 13;
    printf("\n%llu\n", g);
}



int main()
{
    g = 0x13451304957llu;
    g2 = 0x13957lu;
    g3 = 23;
    foo();
    bar();
    return 0;
}
