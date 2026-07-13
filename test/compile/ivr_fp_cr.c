void putchar(char);
int main() {
  for (float y = 1.5f; y > -1.5f; y -= 0.1f) {
    float f = y * y;
    putchar("@"[f * -8.0f]);
  }
  return 0;
}

