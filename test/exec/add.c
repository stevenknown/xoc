unsigned long long g;
unsigned int g2;
int g3;
unsigned int g4;
int g5;
int x;
void printf(char const*,...);
void foo()
{
    g = g + 0x1234567890abcdefllu;
    printf("\n%llu\n", g);
}

void loo()
{
    g3 = g3 + g5;
    printf("\n%llu\n", (unsigned long long)g3);
}

void bar()
{
    g3 = g3 + 0x123;
    printf("\n%llu\n", (unsigned long long)g3);
    printf("\n0x%llx\n", (unsigned long long)g3);
}

void zoo()
{
    //g4 = g4 + 0xFFF;
    g4 = g4 + (unsigned int)0xffffFFF;
    printf("\n%llu\n", (unsigned long long)g4);
    g4 = g4 + g2;
    printf("\n%llu\n", (unsigned long long)g4);
}

void moo()
{
    g5 = g5 + (int)0xfffFFF;
    printf("\n%llu\n", (unsigned long long)g5);
    g5 = g5 + x;
    printf("\n%llu\n", (unsigned long long)g5);
}

int main()
{
    g = 0x13451304957llu;
    g2 = 0x13957lu;
    g3 = 23;
    g4 = 39;
    g5 = 59;
    bar();
    loo();
    foo();
    zoo();
    moo();
    return 0;
}
