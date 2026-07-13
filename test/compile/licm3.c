int * gp;
int x[30];
int main()
{
    int x[10];
    int i;
    int m;
    while (i) {
        m = (gp + 10)[20]; //should be hoisted outside loop.
        x[i] = 20; //should be hoisted outside loop.
    }
    while (i) {
        m = (gp + 10)[20]; //can NOT be hoisted
        x[i] = 20; //can NOT be hoisted
        i++;
    }
    return m + (gp + 10)[20];
}
