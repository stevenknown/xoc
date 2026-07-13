char in0;
int main(char * in1)
{
    int r3 = *in1;
    char * r1 = &in0; 
    char * r2 = r1 + 4;
    char * r4 = 0;
    if (r3 != 5) {
        goto USER_L3;
    }
    goto USER_L2;

USER_L1:
    r2 = r2 + 4;
    *r2 = r3 + 4;
    r3++;
    if (r3 == 5) {
        goto USER_L3;
    }

USER_L2:
    r4 = r2 + 8;
    *r4 = r3;

USER_L3:
    r4 = r2 + 8;
    *r4 = r3;
    if (r3 == 2) {
        goto USER_L4;
    }
    goto USER_L1;

USER_L4:
    return 0;
}
