int arr[10];
int sum = 0;
void foo()
{
    for (int i = 0; i < 10; i += 1) {
        sum = sum + arr[i];
        arr[i] = sum;
    }
}
