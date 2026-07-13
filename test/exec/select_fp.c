void putchar(char);
void printf(char const*,...);
int main() {
    float f = 1.5f;
    int i;
    for (i=0;i<9;i++, f-=0.2){
      char d;
      char c;
      c = ".:-=+*#%@"[i];
      d = f <= 0.0f ? c : '_';
      putchar(d);
    }
    printf("\nfinish\n");
    return 0;
}


