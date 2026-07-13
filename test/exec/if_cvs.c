void printf(char const*,...);
int A[100];
int B[100];
int C[100];
int foo(int i)
{
    if (A[i] > B[i]) {
        C[i] = A[i] + B[i] + 5;
    }
    return C[i];
}

int main()
{
    A[1]=2;
    A[2]=2;
    A[3]=2;
    B[1]=1;
    B[2]=1;
    B[3]=1;
    C[1]=0;
    C[2]=0;
    C[3]=0;
    foo(1);
    foo(2);
    foo(3);
    if (C[1]+C[2]+C[3] != 24) {
        return -1;
    }
    return 0;
}
