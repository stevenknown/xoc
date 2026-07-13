int a[100];
void foo()
{
    int j = 100;
    int i = 0;
    for (;;i++) {
        if (j < i) {
            ;
        } else {
            break;
        }
        a[i]=0;
    }
}
