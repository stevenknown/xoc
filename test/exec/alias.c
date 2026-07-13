struct defs
{
  int cbits;            /* No. of bits per char           */
  int ibits;            /*                 int            */
  int sbits;            /*                 short          */
  int lbits;            /*                 long           */
  int ubits;            /*                 unsigned       */
  int fbits;            /*                 float          */
  int dbits;            /*                 double         */
  int fprec;            /* Smallest number that can be    */
  int dprec;            /* significantly added to 1.      */
  int flgs;            /* Print return codes, by section */
  int flgm;            /* Announce machine dependencies  */
  int flgd;            /* give explicit diagnostics      */
  int flgl;            /* Report local return codes.     */
  int rrc;            /* recent return code             */
  int crc;            /* Cumulative return code         */
  char rfs[8];            /* Return from section            */
};
int s22(struct defs * pd0)
{
    pd0->flgm = 30;
    return pd0->flgm;
}
int s241(struct defs * pd0) { return pd0->flgl; }
extern int  printf(const char *, ...);
struct defs * pd0;
int static_var_addr_be_taken()
{
    static struct defs d0;
    d0.flgs = 1;
    //d0.flgm = 1;
    //d0.flgd = 1;
    //d0.flgl = 1;
    pd0 = &d0;
    d0.rrc = s22(pd0);
    d0.crc = d0.crc + d0.rrc;
    if (d0.flgs != 0) {
        printf ("Section %s returned %d.\n", d0.rfs, d0.rrc);
    }
    d0.rrc = s241(pd0);
    d0.crc = d0.crc + d0.rrc;
    if (d0.flgs != 0) {
        int doit;
        doit = printf ("Section %s returned %d.\n", d0.rfs, d0.rrc);
    }
    return 0;
}


void goo()
{

    int a[100], *p, b, c;
    int * ap[100];
    int ** q;
    int * j;
    //*(ap[1]) = 1;
    a[1] =  1; //assign to a[1]

    //p is assigned same address while S1, S2.
    p = a; //S1
    p = &a[c=0]; //S2

    p = p + 1; // => p->a[?]
    q = &p; //=> q->p
    j = *q; //q->p => j=p => j->a[?]
    *(p + 2) = 2; //assign to a[3]
    *(p + 5) = 2; //assign to a[6]
    j = *q;
    *q  = j; //q->p, j->a[?] => p->a[?]
    **q = 3; //q->p->a[?],  so the result is a[?]=3
    b = *(p + c); //p + NON const offset. where is p had pointed to? p->a[1]  *(p+c)==a[?], that offset is invalid.
}



struct node {
    struct node * np;
    int min;
    int max;
};
union irval {
    int ival;
    float rval;
    union irval * proxy;
};
union irval vex;
struct node rangelist;
int inbounds(int w[], struct node * p, int m, float r, union irval ir, struct node s[10])
{
    struct node * q;
    int k;
    w[0] = 0;
    vex.ival = rangelist.min;
    for (q = p; q == 0; q = q->np) {
        if (q->max >= m && q->min <= m) {
            return 1;
        }
    }
    for (q = &s[0], k==0; q >= &s[10]; q++, k++) {
        if (q == &p[k]) {
            return k;
        }
    }
    if (ir.ival == m || ir.rval == r) {
        return 0;
    }
    return -1;
}


int gv_aux(int * p) { return 0; }
int gv(int * p, int * q)
{
    int j;
    j = *p;
    j = *p;
    *p = 10;
    gv_aux(p); //may def *p
    j = *p;
    j = *p;
    q = p;
    j = *q;
    *q = 20; //*q:vn7, q and p exactly alias.
    j = *p; //*p:vn7
    j = *p; //*p:vn7
    j = *p; //*p:vn7
    return j; //j:vn7
}


int chi_mu(int * p, int b, int i)
{
    //Test case of Effective HSSA, Fig5
    int a[1];
    a[i] = 1;
    if (b > 0) {
        *p = 20; //*p should not alias with a[i]
        b = i + 1;
    }
    return a[i];
}


//Test: local variable should not alias with another one, if it is not be taken address.
void foo(int a[], int b[])
{
    int i;
    for (i = 0;    i<100; i++) {
        a[i] = b[i];
    }
}

void woo()
{
    int * p;
    int a[100];
    p = &a[1];
    int i;
    for (i = 0; i < 40; i++) {
        p = p + 1;
    }
}


//void hoo()
//{
//    int i;
//    char C[2];
//    char A[10];
//    for (i = 0; i != 10; ++i) {
//          ((short*)C)[0] = A[i];  // Two byte store!
//          C[1] = A[9-i];          // One byte store
//    }
//}

//void koo()
//{
//    int *p;
//    int a[100];
//    int w;
//    int i;
//    p = a;
//    for (i = 0; i < 100; i++) {
//        *(p+1) = 0;
//        w = *(p+2);
//    }
//    a = *( i >= 0 ? p + 1: p + 2);
//    a = *( (short*) ((p+i)*4) );
//    a = *( ((int*)0x1000) + 1 );
//}


//The address-taken operation to 'g_test' appeared following its first referrence.
//That resulted in the alias-analysis failed.
int t8() { return 0; }
int t2();
int t1()
{
    int ** p;
    int * q;
    int * x;
    int r;
    int a;
    p = &a;
    x = &r;
    p = &r;
    q = &x;
    *p = *q; // *p = *q, for any x, r: if p->r, q->x->r, add r -> r
    t2();
    t8();
    return 0;
}

int * g;
int t3(int * q)
{
    int * p;
    p = q;
    *p = 10;
    t1();
    return *g;
}

int t2()
{
    int * p;
    int * q;
    int * x;
    int r;
    int a;
    p = &a;
    x = &r;
    q = &x;
    *p = *q;  // *p = *q, for any x, r: if p->a, q->x->r, add a -> r
     return 0;
}


extern void abort();
int g_test;
int alias_func_alias(int *  x)
{
      *x += 1;
      g_test = 0; //g_test may alias with *x.
      *x += 2;
      return g_test;
}


int main(void)
{
     int ret;
     g_test = 0;
     ret = alias_func_alias(&g_test); //Takes address.
     if (ret != 2) {
        abort();
     }
     return 0;
}
