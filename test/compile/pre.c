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
    z = x+y;//redundant.
    return z;
}

int N = 100;
int g_a, g_b;
//Test case of wangfan.
/*
For the case, we can not hoist x+y always up, because we
should consider the variable life time of x and y.
(e.g: hoist x+y to BB1, of cause, the BB1 is the
best position which x+y should be moved to just only at this case.)
So you must use ANTI-IN, and AVAIABLE-IN info to compute the
EARLIEST.
Or you must design a new method that either hoist expression or
considering the variable's life time, that is what the LCM did.
*/
int test_pre1()
{
    //loop invariant motion
    int x, y, z;
    x = 10;
    y = 20;
    if (g_a > g_b) {
        if (x != y) {
            goto L1;
        }
        g_a = 2;
        do {
            z = x+y;
            N--;
        } while (N>0);
        z = z << 2;
    } else {
        g_a = 1;
        L1:
        z = x+y;
    }
    return z;
}

//Test for loop invariant motion.
int test_pre()
{
    int a;
    int b,c;
    a=10;
    do {
        c = a+b;
        N--;
    } while (N>0);
    return 0;
}

void test_pre2(int w)
{
    static int A,B,C;
    if (w) {
        A = w + 1;
    } else {
        B = w + 1;
    }
    C = w + 1;
}


