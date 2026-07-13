void printf(char const*,...);
void exit(int);
int array[100][100];
void foo(int (*arr)[100][100], int d1, int d2)
{
    int size = sizeof(*arr);
    if (size != sizeof(int) * 100 * 100) {
        printf("\nfailed!\n");
        exit(1);
    }
}
int main ()
{
  foo(&array, 100, 100);
  return 0;
}
