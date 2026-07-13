int a[100];
void foo(int p)
{
    int b;
    if (p > 0) { b = 100; }
    else { b = 100; }
    int j = 0;
    int i = 1;
    for (;;i++) {
        if (i < b) { //b's VN is 100
            ;
        } else {
            break;
        }
        a[i]=0;
    }
}
