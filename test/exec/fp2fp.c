int printf(char const*,...);
int main()
{
    float p;
    double aa;
    double t;
    p = 3.14159267986;
    aa = 6.2841973153;
    t = (double)p;
    p = (float)aa;
    aa = t;
    printf("\n%f,%f,%f\n", (double)p, aa, t);
    return 0;
}

