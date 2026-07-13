struct S {int w; short e[100];int a,b,c,d; };
struct S s[100][200];
int ** p;
int foo(int (q)[20][30])
{
    //s[1].a = 0;
    //s[3][2].d = 1;
    //p[4][3] = 2; //p is not array
    //q[7][8] = 3; //q is not array, it is pointer, point to an array.
                 //q's type is int q->pointer->array[20]->array[30]

    int i,j,k;
    struct S ll;
    ll.e[j] = 22;

    int local[100][200][300];
    local[i][j][k] = 0;


    short a1;
    a1 = s[5][6].e[7];
    a1 = s[5][6].b;


    struct W {int e1; short e2[100];  long long e3;};
    struct W w;
    a1 = w.e2[2];
    return 0;
}
