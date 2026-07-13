int arr[10];
int sum = 0;
void foo()
{
    for (int i = 0; i < 10; i += 2) {
        sum = sum + arr[i];
        //reduce dep, can not vectorized.
        arr[i+1] = arr[i+1] + sum;
    }
} 
