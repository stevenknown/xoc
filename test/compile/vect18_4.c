int arr[10];
int sum = 0;
int n,m,x,y,z;
void foo()
{
    //Can not vectorized.
    //stride is 8 bytes, use mask.
    for (int j = 0; j < 10; j++) {
        arr[m*7-61*x*y+2*j+19*y-z*11-3] = arr[m*7-61*x*y+2*j+19*y-z*11-3] + sum;
    }
}
