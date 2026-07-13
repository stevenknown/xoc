int g;
int foo(int n)
{
    int i;
    for (i=7;i<n;i++){
        if (g % i == 0) {
            i++;
        }
    }
    return i;
}
