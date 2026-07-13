int arr[10];
int sum = 0;
void foo()
{
    for (int i = 0; i < 10; i += 1) {
        sum += arr[i];
        //Dependence between store of 'arr' and load of 'arr' prevents
        //vectorization.
        //The compututaion of 'sum' and load of 'sum' prevent vectorization.
        //Report the actions.
        arr[i+1] += sum;
    }
}
