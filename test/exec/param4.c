unsigned long long g_l;
int printf(char const*,...);
long long foo1(char const* h, int s, int b, unsigned long long c, int w) {
//      printf(
//          "\n"
//          );
      printf(
          h
          ,100
          ,200
          ,c
          ,300
          ,400
          ,500
          ,600);
}

int main()
{
    g_l = 0xff;
    g_l = g_l << 32;
    foo1("\nPTR %d,%d,0x%llx,%d,%d,%d\n", 100, 200, g_l, 300);
    return 0;
}
