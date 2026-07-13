void printf(char const*,...);
int main()
{
    char * p;
    char arr[10];
    arr[0]=0xffffFFFF;

    p = &arr[1];  //s1;
    *p = 20;

    p = &arr[2];  //s2; s1,s2 should not alias each other,
                  //if AA is flow sensitive.
    *p = 30;

    int i;
    for (i=3;i<10;i++) {
      p = &arr[i];
      *p = 40;
    }

    for (i=0;i<10;i++) {
      p = &arr[i];
      printf("\n%d,", (int)*p);
    }
    printf("\n");
    return 0;
}
