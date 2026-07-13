unsigned long long val, *p;
extern unsigned long long xtr(unsigned long long);
unsigned long long caller()
{
    //LLVM generate code that the constant value is loaded twice, once into %rax
    //and again into %rdi as the first parameter for xtr. There is no need for
    //the second constant load!
    //gcc (4.6+) generates better code~
    unsigned long long x;
    p = &val;
    x = 12345123400L; //should be removed!
    int y;
    y = *p + val;
    *p = x;
    return xtr(x);
}


int bar2(int x, int y);
int bar(short b, short a)
{
    //All x+y expression should be removed.
    //Should do GCSE first.
    //And the second is CP.
    int x,y,z;
    z = x+y;
    if (b) {
        z = b;
        z = x+y;//redundant.
    } else {
        z = a;
        z = x+y;//redundant.
    }
    z = bar2(x+y, x+y);//redundant.
    return z;
}


int gb(unsigned int a) {
    //c[10] and c[11] has same value.
    unsigned int c[100];
    c[10] = a;
    c[11] = a;
    unsigned int b;
    b = c[10] + c[11];
    if (b > a*2)  //<= always false
        a = 4;
    else
        a = 8;
    return a + 7; //<= always return 15!
}


void koo()
{
    struct S {
        long long l;
        char s;
    } q;
    int a;
    //a = (&q)->s; //=> a=q.s

    char * p;
    p = &(q.s);
}


void foo(int * p)
{
    int a,b,c,d,i;
    short * zz;
    zz = 0;
    a = c;
    if (a > b) {
        p = &a;
        b = 10;
    } else {
        d++;
    }
    *p = 30; //p may -> a, so value of 'c' is not available.
    i = a;
}

int y[10];
int z[10];
void h();
void h()
{
    //i = p
    //if(p == i) {
    //    y[i]=z[i];
    //}
    //=> propagate p
    //    i = p
    //    if(p == p) {
    //        y[i]=z[i];
    //    }
    //=> do redundant code remove.
    //    i = p
    //    {
    //        y[i]=z[i];
    //    }
    //=> prop p
    //    i = p
    //    {
    //        y[p]=z[p];
    //    }
    //=> dead code elim.
    //    {
    //        y[p]=z[p];
    //    }
    int i,p;
    double t;
    double x;
    t = (1.0+2.0+x)*(x+(1.0+2.0));
    //i = sizeof(h());
    //i = 1234567lu;
    //if(p == i) { //should be removed
        y[i]=z[i];
    //}
    return t;
}

double xoo()
{
    double t;
    double x;
    t = (1.0+2.0+x)*(x+(1.0+2.0));
    //Should
    //p1 = x+3.0
    //p2 = p1*p1
    //t = p2
    return t;
}


void foo2()
{
    int a,b,c,d,i;
    a = c;
    if (a > b) {
        b = 10;
    } else {
        d++;
    }
    c = 20; //value of 'c' redefined, so it is not available.
    i = a;
}


int hoo_call(int * p, char * h);
void hoo()
{
    int a, b;
    char * h;
    int * p;
    p = &a;
    h = &b;
    hoo_call(p, h);
}

int g()
{
    //We can NOT propagate 'c' , because p->a, and 'a' modified during the propagation-path.
    int * p;
    int * q;
    int a;
    int b;
    int c;
    p = &a;
    q = &b;
    b = 10; //can not propagate q, but we can propagate p. :)
    c = *p + *q;
    a = 10;
    double z;
    z = z * 2.0;
    return c; //<--can not propagate 'c'
}

void f()
{
    int * p;
    int w, b, x;
    p = &w;
    b = 10;
    w = b;
    x = b + *p;
}
