int f()
{
    //The case will cause IR_CCP failed.
    //Compute exp_vr round-robin incur the value of a,i incremented always
    //at each iteration.
    int i;
    int a;
    a = 0;
    i = 0;
    for (; a <=30; i++) {
        a+=8;
    }
    return i;
}

/*
int f()
{
    int i;
    int a;
    int w;
    w = 0;
    for (a = 0, i = 0; w <= 100, a <=30; i++) {
        a+=2;
    }
    return i;
}
*/
