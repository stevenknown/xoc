int A[100];
int B[100];
int C[100];
int foo(int n)
{
    unsigned sum = 0;
    for (int i = 0; i < n; ++i) {
        if (A[i] > B[i]) {
            C[i] = A[i] + B[i] + 5;
        }
    }
    return sum;
}
