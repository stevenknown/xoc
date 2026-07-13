void printf(char const*,...);
int C[100];
int foo(int i)
{
    C[i] = 5;
}

int main()
{
    C[1]=0;
    foo(1);
    printf("\n>>%u<<\n",C[1]);
    if (C[1] != 5) {
        return -1;
    }
    return 0;
}
