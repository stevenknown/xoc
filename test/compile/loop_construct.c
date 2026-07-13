//Construct two loop which leading by same loop head.
int g;
int f()
{
    int i,j;
    i = 1;
    j = 2;
B1:
    if (i < j) {
        goto B2;
    } else if (i > j) {
        goto B3;
    } else {
        goto B4;
    }
B2:
    g++;
    i++;
    goto B1;
B3:
    i--;
    goto B1;
B4:
    return g;
}
