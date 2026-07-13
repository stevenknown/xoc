//Test different anonym domdef for same expresion
int case5aux();
int case5(int g[], int * p)
{
    int i;
    g[3] = 0;
    i = *p; //s1
    i = *p;
    case5aux();
    i = *p; //s2
    i = *p;
    return i;

    //NOTE: s1's vn should not same with s2.
}


//Test accessing order , Whether if RPO or BFS is correct?
int case4g;
int case4aux();
int case4(int i)
{
    case4aux();
    if (i > 0) {
        L1:
        case4g = 2;
        L2:
        case4g = 3;
        L3:
        case4g = 4;
        case4aux();    //When we use BFS order, this stmt will be
                    //regard as domdef, it is wrong!!!
                    //It seemd we only could use RPO order to
                    //determine which Def is the last.
    }
    case4aux();    //this is domdef.
    L4:
    return case4g;
}


int case3(int * p, int i)
{
    int b;
    *p = 10;
    while (--i > 0) {
        i = i + 1;
        i = i / 2;
        b = *p; //*p:no vn
        i = i << 3;
        i = i >> 8;
        *p = 20;
    }
    return b; //b:no vn
}


int case2(int * p, int i)
{
    int b;
    if (i % 4) { //i:vn1
        b = *p; //*p:vn8
    } else {
        *p = 20; //*p:vn7
    }
    *p = 30; //*p:vn9
    if (b > 0) {
        b--; //S1, b:vn11, the exact DEF of b will reach the S2.
    } else {
        return *p; //*p:vn9
    }
    return b; //S2, b:vn11, good! S1 reach here.
}


int case1(int * p, int i)
{
    int b;
    *p = 10;
    while (--i > 0) {
        if (i % 4 == 0) { //i:no vn
            b = *p; //*p:no vn
        }
        *p = 20; //*p:vn8
    }
    return b; //b:no vn
}


int case0(int * p, int i)
{
    int b;
    *p = 10;
    while (--i > 0) {
        if (i % 4 == 0) { //i:no vn
            *p = 20;
        } else {
            b = *p; //*p:no vn
        }
    }
    return b; //b:no vn
}
