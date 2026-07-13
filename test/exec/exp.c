/* EXP */
int n;
double x;
void printf(char const*,...);

int k;
char d[10];
int main(int argc, char* argv[])
{
    n = 5;
    x = 3.14159265;
    double y;
    y = 1.0;

    /* turn n int binary representation. */
    int t;
    t = n;
    k = -1;
    while (t != 0)
    {
        d[++k] = t & 1;
        t = t >> 1;
    }

    int j;
    for (j = k; j >= 0; j--)
    {
        y = y * y;

        if (d[j] == 1)
        {
            y = x * y;
        }
    }

    printf ("power(%f, %d) = %f\n", x, n, y);
    return 0;
}
