int gvn_gv_aux(int * p);
int gvn_gv(int * p, int * q)
{
    int j;
    q = p;
    j = *q; //j has same value with *q.
    *q = 20; //*q:vn7, q and p exactly alias.
    j = *p; //*p:vn7
    j = *p; //*p:vn7
    return j; //j:vn7
}

int label()
{
    int a, b;
    if (a > b) {
        goto L1;
    }

    if (a < b) {
        goto L2;
    }

    if (a == b) {
        goto L3;
    }

L1:
L2:
L3:
L4:
    return 10;
}

int aloha()
{
    int a, b, c;
    goto L1;

L2:
    c = 2*a;
    if (b==c) { //<== b always equal to c.
        goto L3;
    }
    goto L4;

L1:
    b = a + a;
    goto L2;

L3:
    return 10; //<== always return 10.
L4:
    return 20;
}


int string_md()
{
    char * p;
    p = "xxx";

    char * q;
    q = "yyy";

    if (p == q) {
        return 1;
    }
    return 0;
}


int gc;
int cran(int, int, int);
inline int crankshaft(int * p, int * q)
{
    p = &gc;
    int a,b;
    a = *p;
    if (a > 0) {
        b = *p;
    } else {
        b = 20;
    }
    if (b > 30) {
        b--;
    } else {
        b = *p;
    }
    int i;
    for (i = 0; i < a; i++) {
        if (gc != *p) {
            gc = cran(*p, gc, *p);
        } else {
            gc = *p - gc;
        }
    }
    return *p;
}


int case1_aux(int * p);
struct {int a; int b; int c;} c1g;
int case1(int * p, int * q)
{
    int j;
    *p = 10;
    case1_aux(p); //may def *p
    //c1g.b'vn is different with c1g.c's vn.
    j = c1g.b; //ld with ofst 4
    j = c1g.c; //ld with ofst 5
    return j; //j:vn7
}



int case0_aux(int * p);
int case0_a[100];
int case0_g;
int case0(int * p, int * q)
{
    int j,i;
    *p = 10;
    case0_a[j] = 10;
    case0_aux(p); //may def *p
    j = case0_g;
    j = case0_g;
    j = *p;
    j = *p;
    j = case0_a[i];
    j = case0_a[i];
    return j; //j:vn7
}


typedef struct {
    int b[100];
    int a[100];
}ST;
int array(int i)
{
    int A[100];
    int x;
    ST st;
    st.a[i] = i;
    x = st.a[i];
    A[i] = 13;
    x = A[i];
    return x;
}


int ll(int a, int b, int c, int d);
int hell(int a, int b, int c, int d)
{
    int x,y,z,u;
    int * p, * q;
    q = &d;
    p = q;
    *p = 10;
    return ll(x, y, z, u);
}

int klum(int r)
{
    int x,y,z;
    if (r != 0) {
        x = 3;
        y = 2;
    } else {
        x = 2;
        y = 3;
    }
    z = x + y; //z is always 5. How to do that? Factor DefUse can do, but it is emprically,
               //I need
               //theoretical method.
    return z;
}

int d, e;
int foo();
int gb(unsigned int a, int * restrict pbuf)
{
    //c[10] and c[11] has same value.
    int * pt;
    unsigned int c[100][20];
    c[10][2] = a;
    c[11][3] = a;
    unsigned int b;
    b = c[10][2] + c[11][3];
    d = a;
    pt = &d;
    e = *pt + 0;
    //e = foo();
    if (b > e*2)  //<= always false, gvn can judge a+a==2e
        a = 4;
    else
        a = 8;
    return a + 7; //<= always return 15!
}

int g;
int foo()
{
    int a, b, c, x, y;
    c = a;
    if (g > 0) {
        x = a + b;
    } else {
        x = 0;
    }
    y = c + b;
    return y;
}


int * foo2(int * x);
void foo3()
{
    int * p;
    int x;
    p = foo2(&x);
    *p = 10;
    return x + 1;
}


