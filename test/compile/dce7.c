void foo();
int g;
int main()
{
    char arr[100];
    int i,j;
    char * p;
    p = arr;
    arr[3] = 100; //should be removed.
    if (p == 0) {
        arr[1] = 10; //should be removed.
        arr[3] = 400; //should be removed.
        g = 500;
    } else {
        arr[2] = 20;
    }
    arr[3] = 10;
    foo();
    return arr[3] + arr[2] + g;
}
