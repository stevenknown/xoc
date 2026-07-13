int arr[10];
int sum = 0;
int n,m,x,y,z;
int *p;
void foo()
{
    //Can NOT be vectorized, because *p might alias with arr.
    for (int j = 0; j < 10; j++) {
        arr[m*7-*p+61*x*y+2*j/2+19*y-z*11-3] = arr[m*7-*p+61*x*y+2*j/2+19*y-z*11-3] + sum;
    }
}
