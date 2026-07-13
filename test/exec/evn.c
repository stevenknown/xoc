int P[10];
void swap(int j, int m)
{
    int save;
    save = P[j];
    P[j] = P[m];
    P[m] = save;
    save = P[j];
    P[j] = P[m];
    P[m] = save;
}
int main()
{
    P[0] = 3;
    P[1] = 4;
    swap(0, 1);
    if (P[0] != 3) { return 1; }
    if (P[1] != 4) { return 2; }
    return 0;
}
