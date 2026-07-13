int arr[10];
int sum = 0;
void foo()
{
    for (int i = 0; i < 10; i += 2) {
        //stride is 2, can not vectorized.
        arr[i+1] = arr[i+1] + sum;
    }
}
