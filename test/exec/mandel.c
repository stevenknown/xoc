#include "stdio.h"

//picture size
#define WIDTH 80
#define HEIGHT 40

//maximum iteration
#define MAX_ITER 64

//mandelbrot range scope
#define XMIN -2.2
#define XMAX 0.8
#define YMIN -1.2
#define YMAX 1.2

static int mandel(double cx, double cy)
{
    double x = 0.0, y = 0.0;
    int iter = 0;
    double x2, y2;

    while (iter < MAX_ITER)
    {
        x2 = x * x;
        y2 = y * y;
        if (x2 + y2 > 4.0)
        {
            break;
        }
        /* z = z^2 + c */
        double nx = x2 - y2 + cx;
        double ny = 2 * x * y + cy;
        x = nx;
        y = ny;
        iter++;
    }
    return iter;
}

int main(void)
{
    int px, py;
    // ASCII grey char
    char palette[] = " .:-=+*#%@";
    const int pal_len = sizeof(palette) - 1;
    for (py = 0; py < HEIGHT; py++)
    {
        double cy = YMIN + (YMAX - YMIN) * py / HEIGHT;
        for (px = 0; px < WIDTH; px++)
        {
            double cx = XMIN + (XMAX - XMIN) * px / WIDTH;
            int it = mandel(cx, cy);
            char ch;
            if (it == MAX_ITER)
            {
                ch = palette[pal_len - 1];
            }
            else
            {
                int idx = it * pal_len / MAX_ITER;
                ch = palette[idx];
            }
            //putchar(ch);
            printf("%c",ch);
        }
        printf("\n");
        //putchar('\n');
    }
    return 0;
}
