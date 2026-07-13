typedef struct _S1 {
  int i,j,k;
} S1;
typedef struct _S2 {
  int o,p,q,x,y;
} S2;

S1 s1;
S2 s2;
S1 bar1();
S2 bar2();
S1 bar1()
{
    return s1;
}

S2 bar2()
{
    return s2;
}

int main()
{
    s1.k = 3;
    s1.j = 4;
    S1 l1 = bar1();
    S1 l2 = bar1();
    if (l1.k != 3) { return -1; }
    if (l2.j != 4) { return -1; }
    s2.x = 5;
    S2 l3 = bar2();
    if (l3.x != 5) { return -1; }
    return 0;
}

