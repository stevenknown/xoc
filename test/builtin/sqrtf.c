//double precison floating-point type can even produce more inaccurate results.
//extern double fabs(double x);

extern float fabsf(float x);
float sqrtf(float x)
{
    if (x <= 0) {
        return 0;
    }
    float epsilon = 0.00001;
    float guess = x;
    while (fabsf(guess * guess - x) >= epsilon) {
        guess = (guess + x / guess) / 2.0;
    }
    return guess;
}
