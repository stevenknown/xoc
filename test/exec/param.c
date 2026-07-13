unsigned long long g_l;
typedef struct _t{
  int x;
  int y;
  int z;
  int p;
  int q;
} S;
S g_s,g_s2;
int k1,k2,k3,k4,k5,k6;
//long long foo(int a, S s, long long c);
int printf(char const*,...);

long long foo1(unsigned long long c,  S s, int a) {
    int x;
    //x = c >> 31;
    //x = c;
    printf(
        //"\n0x%llx,%d,%d,%d\n",
        "\n0x%llx,%d,%d,%d,%d,%d\n",
        c
        ,100
        ,200
        ,300
        ,400
        ,500
        ,600 //bug
        );
}

long long foo2(unsigned long long c,  S s, int a) {
      printf(
          "\n0x%llx,%d,%d,%d,%d,%d,%d,%d,%d,%d,%d,%d\n",
          c
          ,s.x
          ,s.y
          ,s.z
          ,s.p
          ,s.q
          ,k1
          ,k2
          ,k3
          ,k4
          ,g_s2.x
          ,g_s2.q
          );

}

long long foo3(unsigned long long c,  S s, int a) {
      printf(
          "\n0x%llx,%d,%d,%d,%d,%d,%d,%d,%d,%d,%d,%d\n",
          c
          ,888
          );

}



int main()
{
    g_l = 0x12345678;
    g_l = g_l << 32;
    g_s.x = 20;
    g_s.y = 30;
    g_s.z = 40;
    g_s.p = 50;
    g_s.q = 60;
    g_s2 = g_s;
    k1 = 91;
    k2 = 92;
    k3 = 93;
    k4 = 94;
    k5 = 95;
    k6 = 96;
    foo1(g_l,g_s,sizeof(g_s));
    //foo2(g_l,g_s,sizeof(g_s));
    //foo3(g_l,g_s,sizeof(g_s));
    return 0;
}
