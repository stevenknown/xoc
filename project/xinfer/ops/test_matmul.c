
#include "ops.h"

#define EPS 0.000001

int test_trans()
{
    float w[3][2];
    w[0][0] = 1.0;
    w[0][1] = 2.0;
    w[1][0] = 3.0;
    w[1][1] = 4.0;
    w[2][0] = 5.0;
    w[2][1] = 6.0;

    float out[2][3];
    trans(&out[0][0], &w[0][0], 3, 2);
    XASSERT(out[0][0] == 1);
    XASSERT(out[0][1] == 3);
    XASSERT(out[0][2] == 5);
    XASSERT(out[1][0] == 2);
    XASSERT(out[1][1] == 4);
    XASSERT(out[1][2] == 6);
    return 0;
}

int test_matmulTrans()
{
    float w[3][2];
    w[0][0] = 1.0;
    w[0][1] = 2.0;
    w[1][0] = 3.0;
    w[1][1] = 4.0;
    w[2][0] = 5.0;
    w[2][1] = 6.0;

    float hi[1*2]= {10.0,100.0};
    float out[3];
    matmulTransF32Ptr(out, hi, &w[0][0], 3, 2);
    float x1=out[0];
    float x2=out[1];
    float x3=out[2];
    if (x1 - 210.0 > EPS) { return 1; }
    if (x2 - 430.0 > EPS) { return 2; }
    if (x3 - 650.0 > EPS) { return 3; }
    return 0;
}

int main()
{
    XASSERT(test_matmulTrans() == 0);
    XASSERT(test_trans() == 0);
    return 0;
}
