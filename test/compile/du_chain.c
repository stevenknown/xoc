int g[100];
int test_du_chain4(int i)
{
    g[i]=10;
    return g[5];
}


int g_test_val;
void aux_test_du_chain3();
int test_du_chain3(int * p)
{
    int i;
    *p = 10; //p may point to global.
    i = g_test_val;
    aux_test_du_chain3(); //call may clobber any global.
    return g_test_val;
}



int test_du_chain(int a)
{
    int d;
    d = 0;
    int i;
    for (i = 0;i < a; i++) {
      d = d + a;
    }
    return d;
}

int test_du_chain2(int i)
{
    int a[100];
    a[10] = 10;
    a[20] = 20;
    int b;
    b = a[10];
    a[i] = 20;
    return a[10];
}



