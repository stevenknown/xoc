int a[100];
int b[100];
int c[100];
int threshold;
void foo(int n)
{
  for (int i = 0; i < n; i++) {
    if (a[i] > threshold) {
      c[i] = a[i] + b[i];
    } else {
      c[i] = a[i] - b[i];
    }
  }
}
