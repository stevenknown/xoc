struct GTS {
    int d;
    int i;
    double x[10];
} this;
void load_elim3(int i)
{
    //Whole loop is unpromotable, except wild!
    //Test IR result processing.
    //this.x[2] and this.x[4] can not be promoted and
    //can not be moved out of loop.
    //Because this.x[i] may def them.
    double p;
    double * pthis;
    double * wild;
    pthis = &this.x[2];
    for (this.d = 1; this.d <= this.i; this.d = (1 + this.d)) {
        this.x[2] = 0.4 * this.x[4];
        *wild = 0.0; //this IST will prevent scalarization.
        *pthis = 20.0;
    }
    //In this case, if we only scan opnd of each ir, we can
    //find this.x[2]. Assume this.x[i] is not exist, then this.x[2]
    //is promotable.
}
