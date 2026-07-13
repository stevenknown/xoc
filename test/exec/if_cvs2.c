void printf(char const*,...);
int A;
int B;
int C;
int foo(int i)
{
    if (A > B) {
        C = A + B + 5;
    }
    return C;
}

int main()
{
    A=2;
    B=1;
    C=100;
    int w = foo(1);
    if (C != 8) {
        return -1;
    }
    return 0;
}
