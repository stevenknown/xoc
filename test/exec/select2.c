void putchar(char);
void printf(char const*,...);
int main() {
    int f = 0;
    for (int i=0;i<9;i++){
      char c = ".:-=+*#%@"[i];
      char d = f <= 0 ? c : '$';
      putchar(d);
    }
    printf("\nfinish\n");
    return 0;
}
