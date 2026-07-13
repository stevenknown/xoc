void foo() {
    typedef struct _tag {
        int pad;
        int opnd[2];
    } S;
    S * ir;
    int * k1; //k1 has type with 'int*'
    int k2;
    k1 = *(ir->opnd + 2);
    k2 = ir->opnd[2];

    int * k3; //k2 has type with 'int (*)[]'
    //k2 = &ir->opnd;
}
