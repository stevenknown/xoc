int g;
int m;
int k;
void printf(char const*,...);
void foo()
{
    for (int i = 0; i < 5; i++) {
        printf("\n11:%u\n",i);
        if (i) {
            g++;
            printf("\n22:%u\n",i);
            g++;
            continue;
            g++;
            continue;
        }
        if (i) { m++; break; }
        for (int j = 0; j < 5; j++) {
              if (i != j) {
                  printf("\n33:%u\n",i);
                  g++;
                  continue;
                  g++;
              }
              switch (j) {
              case 1:
                  g++;
                  printf("\n44:%u\n",j);
                  g++;
                  continue;
                  g++;
                  break;
                  g++;
              case 2:
                  printf("\n55:%u\n",j);
                  break;
                  break;
              case 3:
                  printf("\n66:%u\n",j);
                  g++;
                  continue;
                  g++;
                  break;
              case 4:
                  printf("\n77:%u\n",j);
                  break;
                  g++;
                  continue;
                  g++;
              default:
                  g++;
                  printf("\nCC:%u\n",j);
              }
        }
    }
}
int main()
{
    g = 0;
    k = 0;
    m = 0;
    foo();
    printf("\n%d,%d,%d\n",g,k,m);
    if (g != 13) { return 1; }
    if (k != 0) { return 2; }
    if (m != 0) { return 3; }
    return 0;
}
