int arr[10][10];
int sum = 0;
int n;
void foo()
{
    //can be vectorized.
    for (int i = 0; i < 10; i++)
    for (int j = 0; j < 10; j++) {
        arr[n*i+2][j-3] = arr[n*i+2][j-3] + sum;
    }
}
