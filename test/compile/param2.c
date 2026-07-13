//Test parameter passing mechanism of CG.
void printf(char const*,...);
typedef struct _t{
  int x;
  int y;
  int z;
  int p;
  int q;
  int r;
  int s;
  int t;
} S;

S g_s;
long long g_l;

void foo(long long c) {
    printf(
        "\n%d,%d,%d\n",
        c);
}

void bar(S s) {
    printf(
        "\n%d,%d,%d,%d,%d,%d\n",
        s.x,
        s.y,
        s.q,
        s.r,
        s.s,
        s.t);
}

int main()
{
    g_l = 0x12345678;
    g_s.x = 20; 
    g_s.y = 30; 
    g_s.z = 50; 
    g_s.p = 10; 
    g_s.q = 40; 
    g_s.r = 60; 
    g_s.s = 70; 
    g_s.t = 80; 
    foo(g_l);
    bar(g_s); 
    return 0;
}
