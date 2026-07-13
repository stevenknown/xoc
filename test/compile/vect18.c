int arr[10][10];
int sum = 0;
void foo()
{
    for (int i = 0; i < 10; i++)
    for (int j = 0; j < 10; j++) {
        //Dependence between store of 'arr' and load of 'arr' prevents
        //vectorization.
        //Report the actions.
        arr[i][j] = arr[i+1][j] + sum;
    }
}
