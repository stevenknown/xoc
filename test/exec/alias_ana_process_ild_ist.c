void abort();
void exit(int);
void printf(char const*,...);

void process_ild_ist_struct()
{
    struct S {
      char ch[200];
      int a,b,c;
    } a[100];
    a[13].b = 37;
    int * p;
    struct S * q;
    p = &a;
    q = &a[13];
    int i;
    i = 100;
    while (i) {
      *p = (*q).b;
      i--;
    }
    if (*p != (*q).b || a[0].ch[0] != 37) {
        exit(2);
    }
}

void process_ild_ist()
{
    int * p;
    long long * q;
    char a[100];
    p = &a;
    q = ((char*)&a) + 12;
    int i = 0;
    while (i<100) {
      a[i] = 0;
      i++;
    }
    a[0] = 27;
    a[12] = 27;
    while (i) {
      *p = (int)*q; //q's address should be aligned according to target machine.
      i--;
    }
    if (*p != *q || *q != 27) {
        exit(1);
    }
}

int main()
{
    process_ild_ist();
    process_ild_ist_struct();
    return 0;
}
