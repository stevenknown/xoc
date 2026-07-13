int t1;
int t2;
int g;
void dse3()
{
    g = 0;
    int i;
    i = 0;
    while (i < t2) {
        g = 3;
        i++;
        g = 2;
    }

/*
    while (i < t2) {
        g = 3;
        i++;
        g = 2;
    }
*/
    g = 4;
}
