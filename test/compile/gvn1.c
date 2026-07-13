extern void case10_help(int * a);
int case10(int * p, int b)
{
    int a[100];
    *p = 10; //*p has vn2
    while (b-- > 0) {
        a[100] = *p; //*p has no vn, a[100] has vn11
        *p  = a[100]; //*p has no vn, a[100] has vn11
    }
    case10_help(p);
    return b; //b has vn1
}


extern void case9_help(int * a);
int case9(int * p, int b)
{
    *p = 10; //*p has vn2
    case9_help(p);
    b = *p; //*p has no vn6, b has vn6
    return b; //b has vn6
}


extern void case8_help(int * a);
int case8(int * p, int b)
{
    if (b > 0) { //b has vn1
        case8_help(p);  //p has vn4
    } else {
        *p = 10;    //*p has vn5, p:vn4
    }
    b = *p; //*p has no vn, b has vn7
    return b; //b has vn7
}


extern void case7_help(int * a);
int case7(int * p, int b)
{
    *p = 10; //*p has vn2, p:vn1
    if (b > 0) { //b:vn3
        case7_help(p); //p:vn1
    }
    b = *p; //*p has no vn, p:vn1, b:vn8

    case7_help(p); //p:vn1
    if (b > 0) { //b:vn8
        *p = 20; //*p has vn2, p:vn1
    }
    return *p; //*p has no vn,p:vn1
}


int case6(int * p)
{
    *p = 20; //*p has vn2, p:vn1
    *p = 10; //*p has vn3, p:vn1
    return *p; //*p has vn3, p:vn1
}


extern void case5_help(int * a);
int case5(int * p)
{
    int a;
    a = case5_help(p); //a has vn2, p:vn1
    if (p != 0) {//p:vn1
        a = a + 10; //opnd 'a' has vn2, result 'a':vn6
    }
    return a; //a has no vn
}


int case4(int * a, int b)
{
    int c[10];
    *(a + b) = 20; //*(a+b) has vn1
    c[b] = 0;
    return *(a + b); //*(a+b) has vn1
}


extern void case3_help(int * a);
int case3(int * a, int b)
{
    case3_help(a);
    if (b > 0) {
        *a = 2;     //*a has vn1
    } else {
        *a = 3;     //*a has vn2
    }
    b = *a; //*a has no vn

    case3_help(a);
    *(a + b) = 20; //*(a+b) has vn1
    return *(a + b); //*(a+b) has vn1
}


extern void case2_help(int * a);
int case2(int * a, int b)
{
    int c[10];
    *a = 0; //*a has vn1
    if (b > 0) {
        *a = 2;     //*a has vn1
    } else {
        case2_help(a);
    }
    *a = 3;     //*a has vn2
    c[b] = 20; //AA ensure it will not interfered *a memory.
    return *a; //*a has vn2
}


extern void case1_help(int * a);
int case1(int * a, int b)
{
    *a = 0; //*a has vn
    if (b > 0) {
        *a = 2;     //*a has vn
    } else {
        case1_help(a);
    }
    return *a; //*a has no vn
}


int case0(int a, int b)
{
    if (a > 0) {
        b = 2; //ExactDef is partial available.
    }
    return b; //b has no vn.
}



