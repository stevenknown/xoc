int main()
{
    char arr[100];
    int i,j;
    char * p;
    p = arr;
    if (p == 0) {
        arr[1] = 10;
        arr[5] = 40;
    } else {
        arr[2] = 20;
        arr[5] = 30;
    }
    arr[3] = 200;
    return arr[4] + arr[3] + arr[5];
}
