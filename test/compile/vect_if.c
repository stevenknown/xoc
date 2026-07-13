int A[100];
int B[100];
int foo(int n)
{
    unsigned sum = 0;
    for (int i = 0; i < n; ++i) {
        if (A[i] > B[i]) {
            sum += A[i] + 5;
        }
    }
    return sum;
}
