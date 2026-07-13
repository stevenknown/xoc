//#include <math.h> //used by sqrt, cc heart.3D.c -lm
double sqrt(double x);
void printf(char const*,...);
float f(float x, float y, float z) {
    float a;
    a = x * x + 9.0f / 4.0f * y * y + z * z - 1;
    return a * a * a - x * x * z * z * z - 9.0f / 80.0f * y * y * z * z * z;
}

float h(float x, float z) {
    float y;
    for (y = 1.0f; y >= 0.0f; y -= 0.001f)
        if (f(x, y, z) <= 0.0f)
            return y;
    return 0.0f;
}

int main() {
    //FILE* fp = fopen("heart.ppm", "w");
    int sw, sh;
    //ORIGNIAL SETTING
    //sw = 512, sh = 512;
    //OUR SETTING
    sw = 32, sh = 64;
    printf("P3\n%d %d\n255\n", sw, sh);
    int sy;
    for (sy = 0; sy < sh; sy++) {
        float z;
        z = 1.5f - sy * 3.0f / sh;
        int sx;
        for (sx = 0; sx < sw; sx++) {
            float x,v;
            int r;
            x = sx * 3.0f / sw - 1.5f;
            v = f(x, 0.0f, z);
            r = 0;
            if (v <= 0.0f) {
                float y0,ny,nx,nz,nd,d;
                y0 = h(x, z);
                ny = 0.001f;
                nx = h(x + ny, z) - y0;
                nz = h(x, z + ny) - y0;
                nd = 1.0f / sqrt((double)(nx * nx + ny * ny + nz * nz));
                d = (nx + ny - nz) / sqrt((double)3) * nd * 0.5f + 0.5f;
                r = (int)(d * 255.0f);
            }
            printf("%d 0 0 ", r);
        }
        printf("\n");
    }
    return 0;
}

