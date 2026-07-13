int a[100];
void foo()
{
    int j = 0;
    int i = 0;
    for (;;i++) {
        if (i < 100) {
            ;
        } else {
            break;
        }
        a[i]=0;
    }
}
