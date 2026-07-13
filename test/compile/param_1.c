long long g_l;
typedef struct _t{
  int x;
  int y;
  int z;
  int p;
  int q;
} S;
S g_s;
//long long foo(int a, S s, long long c);
int printf(char const*,...);
long long foo(long long c,  S s, int a) {
    printf(
        "\n%d,%d,%d\n",
        s.x,
        s.y,
        s.q);
    printf(
        "\n%d,%d,%d\n",1,2,3,4,5,6,7,8,9,10);
}
int main()
{
    g_l = 0x12345678;
    g_s.x = 20; 
    g_s.y = 30; 
    g_s.z = 50; 
    g_s.p = 10; 
    g_s.q = 40; 
    foo(
      g_l,
      g_s, 
      sizeof(g_s));
    return 0;
}
