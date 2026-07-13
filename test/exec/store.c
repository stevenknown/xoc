unsigned long long g;
unsigned int g2;
int g3;
unsigned long long g4;
int x;
void printf(char const*,...);

int a0,a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12,a13,a14,a15,a16;
int b0,b1,b2,b3,b4,b5,b6,b7,b8,b9;
int c0,c1,c2,c3,c4,c5,c6,c7,c8,c9;

void foo()
{
    g = g4;
    printf("\n%llu\n", g);
}

typedef struct _S {
  char pad[8192];
  int a,b,c,d,e,f,g,h,i,j,k;
} S;

S bar()
{
    S s1;
    s1.j = 10;
    printf("\n%d\n", s1.j);
    return s1;
}

int main()
{
    g = 0x13451304957llu;
    g2 = 0x13957lu;
    g3 = 23;
    bar();
    foo();
    return 0;
}
