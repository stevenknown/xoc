int main()
{
    int a[20];
    int * q;
    q = a;

    for (int i=0,*p=q; i<5; i++, *p=i, p++) {;}

    if (a[0]==1 && a[1]==2 && a[2]==3 && a[3]==4 && a[4]==5) {
        return 0;
    }
    return 1;
}
