int y[10][10];
int main(int n, int j, int k)
{
    int sum;
    int i = 0;
    while (i < n) {
        //y[i][j] could be promoted.
        sum += y[k][j];
        y[k][j] = i;
        i++;
    }
    return sum + y[9][9];
}
