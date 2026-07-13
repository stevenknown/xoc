void printf(char const*,...);
typedef struct _S {
  int i,j,k;
} S;

S s1;
S bar()
{
    return s1;
}

int main()
{
    S s2;
    s1.i=1;
    s1.j=2;
    s1.k=3;
    s2 = bar();
    printf("\n%d,%d,%d\n",s2.i, s2.j, s2.k);
    return 0;
}

