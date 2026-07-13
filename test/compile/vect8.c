int a[100];
int b[100];
int c[100];
void foo()
{
    int i = 1;
    int j = 1;
    int xxx,yyy;
    for (i=1;i<80;i++) {
    //for (i=1;i<80;i++,j++) {
        xxx = b[i];
        yyy = 10;
        a[i]=xxx+yyy;
    }
}
