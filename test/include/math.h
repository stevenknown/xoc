#ifndef _MATH_H_
#define _MATH_H_

bool isnan(double);
double sqrt(double x);
float sqrtf(float x);
long double sqrtl(long double x);
double exp(double x);
float expf(float x);
long double expl(long double x);
double tan(double x);
float tanf(float x);
long double tanl(long double x);
double tanh(double x);
float tanhf(float x);
long double tanhl(long double x);

double cos(double x);
float cosf(float x);
long double cosl(long double x);

double cosh(double x);
float coshf(float x);
long double coshl(long double x);

double sin(double x);
float sinf(float x);
long double sinl(long double x);

double sinh(double x);
float sinhf(float x);
long double sinhl(long double x);

double log(double x);
float logf(float x);
long double logl(long double x);

double pow(double x, double y);
float powf(float x, float y);
long double powl(long double x, long double y);

double fabs(double x);
float fabsf(float x);
long double fabsl(long double x);

#define M_PI       3.141592653589793

// π/2
#define M_PI_2     1.5707963267948966

// π/4
#define M_PI_4     0.7853981633974483

// 1/π
#define M_1_PI     0.3183098861837907

// 2/π
#define M_2_PI     0.6366197723675814

// 2/√π
#define M_2_SQRTPI 1.1283791670955126

#endif
