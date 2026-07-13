int x[10];
int * p;
int * q;
void printf(char const*,...);
int main()
{
    x[0] = 11;
    x[1] = 12;
    x[2] = 13;
    x[3] = 14;
    x[4] = 15;
    x[5] = 16;
    x[6] = 17;
    x[7] = 18;
    x[8] = 19;
    x[9] = 20;
    int i;
    p = &x[0];
    q = &x[2];
    //Promote x[2], *q, and *(p+2) into same PR.
    for (i = 0; i < 10; i++) {
        x[2] = *q + 11; //*q is exact alias with x[2]
        *p = 13; //*p is exact alias with x[0]
        //*(p+2) = x[3] + x[2] + *q; //*(p+2) exactly alias with x[2]
                                     //*q is exact alias with x[2]
    }
    printf("\n%d+%d=%d\n", x[2], x[0], x[2]+x[0]);
    return 0;
}

