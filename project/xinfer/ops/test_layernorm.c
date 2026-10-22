#include "ops.h"

void base_layernorm_f32(
    float *out, const float *in,
    const float *w, const float *b, int n_dim, float eps)
{
    //
    float mean = 0.0f;
    for(int i = 0; i < n_dim; i++){
        mean += in[i];
    }
    mean /= n_dim;

    //
    float var = 0.0f;
    for(int i = 0; i < n_dim; i++){
        float diff = in[i] - mean;
        var += diff * diff;
    }
    var /= n_dim;

    //
    float inv_std = 1.0f / sqrtf(var + eps);
    for(int i = 0; i < n_dim; i++){
        float x_norm = (in[i] - mean) * inv_std;
        out[i] = x_norm * w[i] + b[i];
    }
}

static void verify_mean_and_var(float * out, unsigned int N)
{
    //After normalization, the output mean is close to 0, and the variance is
    //close to 1. (while w=1,b=0)
    float mean_out = 0.0f;
    float var_out = 0.0f;
    for(int i=0; i<N; i++){
        mean_out += out[i];
    }
    mean_out /= N;
    for(int i=0; i<N; i++){
        float d = out[i] - mean_out;
        var_out += d*d;
    }
    var_out /= N;

    printf("\nCheck statistic after LN (w=1, b=0):\n");
    printf("output mean = %.6f (should near 0)\n", mean_out);
    printf("output var  = %.6f (should near 1)\n", var_out);

    float diff = fabsf(mean_out - 0.0f);
    if (diff > 0.000001) {
        printf("\ndiff = %.6f\n", diff);
        XASSERT(0);
    }
    diff = fabsf(var_out - 1.0f);
    if (diff > 0.000005) {
        printf("\ndiff = %.6f\n", diff);
        XASSERT(0);
    }
}

int main(void)
{
    #define N 100
    const float eps = 0.000001;
    float in[N] = {
     0.12f, -0.31f, 0.45f, 0.77f, -0.22f, 0.51f, -0.63f, 0.29f, 0.88f, -0.44f,
     0.33f, 0.19f, -0.71f, 0.55f, 0.27f, -0.15f, 0.66f, -0.39f, 0.91f, 0.04f,
     -0.52f, 0.41f, 0.72f, -0.28f, 0.37f, -0.80f, 0.21f, 0.61f, -0.48f, 0.83f,
     0.09f, -0.66f, 0.49f, 0.32f, -0.11f, 0.74f, -0.35f, 0.58f, -0.75f, 0.24f,
     0.69f, -0.41f, 0.39f, 0.16f, -0.57f, 0.86f, -0.25f, 0.44f, 0.79f, -0.60f,
     0.30f, 0.22f, -0.84f, 0.53f, 0.11f, -0.33f, 0.64f, -0.50f, 0.94f, 0.01f,
     -0.46f, 0.47f, 0.70f, -0.19f, 0.34f, -0.77f, 0.26f, 0.59f, -0.69f, 0.81f,
     0.06f, -0.54f, 0.42f, 0.36f, -0.21f, 0.76f, -0.37f, 0.50f, -0.72f, 0.28f,
     0.63f, -0.43f, 0.31f, 0.14f, -0.59f, 0.84f, -0.29f, 0.40f, 0.73f, -0.64f,
     0.35f, 0.23f, -0.82f, 0.56f, 0.17f, -0.30f, 0.67f, -0.47f, 0.90f, 0.07f
    };

    float w[N];
    float b[N];
    for(int i = 0; i < N; i++){
        w[i] = 1.0f; //scale is 1.
        b[i] = 0.0f; //shift is 0.
    }

    float out1[N];
    base_layernorm_f32(out1, in, w, b, N, eps);

    float out2[N];
    layernormF32(out2, in, w, b, N, eps);

    printf("\nLayerNorm result:\n");
    for(int i=0; i<30; i++){
        printf("out1[%u]=%.6f, out2[%u]=%.6f\n", i, out1[i], i, out2[i]);
    }
    printf("\n");

    verify_mean_and_var(out1, N);
    verify_mean_and_var(out2, N);
    return 0;
}

