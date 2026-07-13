int arr[10];
int sum = 0;
void foo()
{
    for (int i = 0; i < 10; i += 1) {
        sum += arr[i]; //RHS 'sum' has loop-reduce dep, prevent vectoize.
        //Dependence between store of 'arr' and load of 'arr' prevents
        //vectorization.
        arr[i+1] = sum;
    }
}
