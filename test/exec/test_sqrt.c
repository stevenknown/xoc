float sqrtf(float x);
double sqrt(double x);
void printf(char const*,...);
int main()
{
  double x = sqrt((double)4.0);
  printf("\nx:%f\n", (double)x);
  printf("\nsqrtf:%f\n", (double)(sqrt(0.000086)));

  float y = sqrtf((double)4.0);
  printf("\nx:%f\n", (double)y);
  printf("\nsqrtf:%f\n", (double)(sqrtf(0.000086)));

  return 0;
}

