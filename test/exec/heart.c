void putchar(char);
int main() {
    float y;
    for (y = 1.5f; y > -1.5f; y -= 0.1f) {
        float x;
        for (x = -1.5f; x < 1.5f; x += 0.05f) {
            float a;
            a = x * x + y * y - 1;
            putchar(a * a * a - x * x * y * y * y <= 0.0f ? '*' : ' ');
        }
        putchar('\n');
    }
    return 0;
}
