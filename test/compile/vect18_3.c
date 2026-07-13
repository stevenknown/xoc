int arr[10];
int sum = 0;
int n,m,x,y,z;
void foo()
{
    //Can not vectorized.
    //subexp is not linear-rep of IV.
    for (int j = 0; j < 10; j++) {
        arr[n*j+m*7-61*x*j+19*y-z*11-3] = arr[n*j+m*7-61*x*j+19*y-z*11-3] + sum;
    }
}
