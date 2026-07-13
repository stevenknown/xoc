int g;
int m;
int k;
void printf(char const*,...);
void foo()
{
    for (int i = 0; i < 5; i++) {
        printf("\n00:%u\n",i);
        continue;
        printf("\n22:%u\n",i);
        continue;
        printf("\n33:%u\n",i);
        break;
        break;
        printf("\n44:%u\n",i);
        continue;
        continue;
    }
    printf("\nBB\n");
    for (int i = 0; i < 5; i++) {
        break;
        break;
        printf("\n55:%u\n",i);
        continue;
        printf("\n66:%u\n",i);
        continue;
        printf("\n77:%u\n",i);
    }
    for (int i = 0; i < 5; i++) {
        printf("\n88:%u\n",i);
        if (i) {
            g++;
            printf("\n99:%u\n",i);
            continue;
            printf("\nAA:%u\n",i);
            continue;
            printf("\nBB:%u\n",i);
            continue;
        }
        if (i) { m++; break; }
        printf("\nCC:%u\n",i);
        k++;
        continue;
        printf("\nDD:%u\n",i);
        continue;
        printf("\nEE:%u\n",i);
    }
    printf("\nFF:%u\n");
}
int main()
{
    g = 0;
    k = 0;
    m = 0;
    foo();
    printf("\n%d,%d,%d\n",g, k, m);
    if (g != 4) { return 1; }
    if (k != 1) { return 2; }
    if (m != 0) { return 3; }
    return 0;
}
