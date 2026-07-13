void bar(int arr1[], int arr2[]);
int a[10];
int main(int i)
{
    int b[10];
    a[1] = 10;
    bar(a, b);
    return b[i] + a[i];
}
