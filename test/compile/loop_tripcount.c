int a[100];
void foo(int p)
{
    int j = 0;
    int i = 1;
    for (;;i++) {
        if (i < 100) { //check i's end value is 100, via VN.
            ;
        } else {
            break;
        }
        a[i]=0;
    }
}
