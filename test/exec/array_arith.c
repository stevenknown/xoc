int ga[10][20];
int main()
{
    int * x;
    int i;
    x = ga[2]+1;
    *x = 4321;
    if (ga[2][1] != 4321) {
       return -1;
    }
    return 0;
}
