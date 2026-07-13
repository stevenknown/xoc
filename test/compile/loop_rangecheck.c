
int da;
int dy[100];
int dx[200];
void Daxpy(int dy_off, int dx_off, int n)
{
    //Attemp to elim range-check operation in aoc.
    int i;
    for (i = 0; i < n; i++) {
        dy[i + dy_off] += da * dx[i + dx_off];
    }
}

