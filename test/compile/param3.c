//Test parameter passing mechanism of CG.
void printf(char const*,...);
typedef struct _t{
  char buf[17];
} S;

S g_s;

void bar(S s) {
}

int main()
{
    bar(g_s); 
    return 0;
}
