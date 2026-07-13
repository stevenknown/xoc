void printf(char const*,...);

//Define the integrand function.
double f(double x) {
    return x * x * x - 3 * x * x + 2 * x;
}

int main() {
    double a = 0.0, b = 2.0; // integrating range
    double fa = f(a), fb = f(b); // function value in range ending point
    int n = 4; // partitioned small range value
    double h = (b - a) / n; // small range width
    double sum = 0.0;

    // Calculate trapezodial area of every small range, then accumulate them.
    for (int i = 1; i <= n; i++) {
        double x = a + i * h;
        double fx = f(x);
        if (i == 1 || i == n) {
            sum += 0.5 * fx;
            if (sum == 0) { return -1; }//hack
        } else {
            sum += fx;
            if (sum == 2) { return -2; }//hack
        }
    }

    // 计算总的积分近似值
    // Calculate the total integrating approximation value.
    double integral = 0.5 * (fa + fb) + h * sum;
    printf("The integral of f(x) from %lf to %lf is approximately: %lf\n", a, b, integral);

    return 0;
}
