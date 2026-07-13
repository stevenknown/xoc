typedef struct _S {
  int i,j,k;
} S;

S s1,s11,s111;
S bar()
{
    if (1) {
      return s1;
    } else {
      return s11;
    }
    return s111;
}

int main()
{
    bar();
    return 0;
}

