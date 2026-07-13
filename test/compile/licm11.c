int delt[100];
int delt_x[100];
int anchor[256];
int anchor_x[256];
void foo(int offset, int upper_bound) {
    //upper_bound should NOT be LICM, becase LICM does not consider kid IR.
    //upper_bound will be optimized by RP. 
    for (int i=0;i<256;i++){
        if (i<offset) {
            delt[i + 1*256]=delt_x[upper_bound+ 1*offset + i];
            delt[i + 2*256]=delt_x[upper_bound+ 2*offset + i];
            delt[i + 3*256]=delt_x[upper_bound+ 3*offset + i];
            anchor[i]=anchor_x[upper_bound+i];
            anchor[i+1*256]=anchor_x[upper_bound + 1*offset + i];
        } else {
            delt[i + 0*256]=0;
            delt[i + 1*256]=0;
            delt[i + 2*256]=0;
            delt[i + 3*256]=0;
            anchor[i + 0*256]=0;
            anchor[i + 1*256]=0;
            anchor[i + 2*256]=0;
            anchor[i + 3*256]=0;
        }
    }
}
