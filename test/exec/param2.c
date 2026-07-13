unsigned long long g_l;
int printf(char const*,...);
long long foo1(char const* h, unsigned long long c) {
      printf(
          h
          ,100
          ,c
          ,200
          ,300
          );
}

int main()
{
    char * x;
    x = 0;
    g_l = 0xffffeeee;
    g_l = g_l << 32;
    foo1("\n%d,0x%llx,%d,%d\n", g_l);
    return 0;
}
