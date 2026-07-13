extern double fabs(double x);
double sqrt(double num)
{
    double x = num;
    double y = (x + num / x) / 2;
    while (fabs(y - x) > 0.000000001) {
        x = y;
        y = (x + num / x) / 2;
    }
    return y;
    //double epsilon = 0.000000001;
    //double guess = x;
    //while (fabs(guess * guess - x) >= epsilon) {
    //    guess = (guess + x / guess) / 2.0;
    //}
    //return guess;
}
