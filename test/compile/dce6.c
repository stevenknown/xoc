int main()
{
    char arr[100];
    int i,j;
    char * p;
    p = arr; //should be removed.
    arr[3] = 100; //should be removed.
    if (p == 0) { //should be removed.
        arr[1] = 10; //should be removed.
    } else {
        arr[2] = 20; //should be removed.
    }
    arr[3] = 10;
    return arr[4] + arr[3] + arr[5];
}
