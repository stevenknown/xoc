/* EXPREC */

void printf(char const*,...);
int n;
double x;

double power (double x, int m)
{
    double y;

    if (m == 0)
    {
        y = 1.0;
    }
    else
    {
        y = power (x, m / 2);
        y = y * y;

        if (m & 1)
        {
            /* m is odd. */
            y = x * y;
        }
    }

    return y;
}


int main(int argc, char* argv[])
{
    double result;
    n = 5;
    x = 3.14159265;
    result = power (x, n);
    printf ("power(%f, %d) = %f\n", x, n, result);

    return 0;
}
