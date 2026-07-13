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
    bar();
    return 0;
}

