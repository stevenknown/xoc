void putchar(char);
void printf(char const*,...);
int main() {
    int f;
    f = 0;
    char d;
    char c;
    c = ".:-=+*#%@"[0];
    d = f <= 0 ? c : '$';
    putchar(d);
    printf("\nfinish\n");
    return 0;
}
