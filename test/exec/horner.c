/* HORNER */

int n = 3;
double a[3 + 1];
double x;

/* Pn(x) = a[n]*(x EXP(n)) + a[n-1]*(x EXP(n-1)) + ... + a[1]*(x EXP(1)) + a[0].
         = 4.4*(10.0 EXP(3)) + 3.3*(10.0 EXP(2)) + 2.2*(10.0 EXP(1)) + 1.1
         = 4400.0 + 330.0 + 22.0 + 1.1
         = 4730.0 + 23.1
         = 4753.1
 */

void printf(char const*,...);

int main(int argc, char* argv[])
{
    n = 3;
    a[0] = 1.1;
    a[1] = 2.2;
    a[2] = 3.3;
    a[3] = 4.4;
    x = 10.0;
    double p;
    p = a[n];

    int j;
    for (j = 1; j <= n; j++)
    {
        p = (x * p) + a[n - j];
    }
    printf ("p = %f\n", p);
    return 0;
}
