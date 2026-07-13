int * gp;
int x[10];
int main()
{
    int m;
    int i;
    //only i,m can be promoted.
    while (i) {
        //gp may alias to x. Thus both (gp + 10)[20] and x[i] can NOT
        //be promoted.
        m = (gp + 10)[20];
        x[i] = 3;
        (gp + 10)[20] = x[3];
    }
    //m need to restore.
    return m + (gp + 10)[20];
}
