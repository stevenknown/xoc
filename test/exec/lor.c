int a,b,g;
int main()
{
    a=1;
    b=2;
    if (a==1||--b==1) { //--b should not execute.
        g=3;
        a=4;
    }
    if (b!=2) {
        return -1;
    }
    if (a==4||b--==1) { //b-- should not execute.
        g=4;
    }
    if (b!=2) {
        return -2;
    }
    return 0;
}
