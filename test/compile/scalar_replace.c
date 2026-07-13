struct GTS {
    int d;
    int i;
    double x[10];
} this;
//////////////////////////////////////
//Add test function after this line
//////////////////////////////////////

void load_elim5(int i)
{
    //Test IR result processing.
    //this.x[2] and this.x[4] can not be promoted and
    //can not be moved out of loop.
    //Because this.x[i] may def them.
    double p;
    double * pthis;
    pthis = &this.x[2];
    for (this.d = 1; this.d <= this.i; this.d = (1 + this.d)) {
        this.x[2] = 0.4 * this.x[4];
        *pthis = 20.0;
    }
    //In this case, if we only scan opnd of each ir, we can
    //find this.x[2]. Assume this.x[i] is not exist, then this.x[2]
    //is promotable.
}


void load_elim_org()
{
    for (this.d = 1; this.d <= this.i; this.d = (1 + this.d)) {
        this.x[0] = (0.499975 * (this.x[0] + this.x[1] + this.x[2] - this.x[3]));
        this.x[1] = (0.499975 * (this.x[0] + this.x[1] - this.x[2] + this.x[3]));
        this.x[2] = (0.499975 * (this.x[0] - this.x[1] + this.x[2] + this.x[3]));
        this.x[3] = (0.499975 * (-this.x[0] + this.x[1] + this.x[2] + this.x[3]));
    }
}


//a and b may be aliased. But IR_GVN given the answer
//is that a has VN4 and b has VN5, they are
//definitly not same memory!
//That may incur incorrect analysis result.
//Check it out!
void load_elim6(long long a[], long long b[])
{
    int i;
    for (i = 0; i <= 100; i++) {
        a[2] = 0.4 * b[4];
    }
}



void load_elim4(int i)
{
    //Test IR result processing.
    //In this case, nothing can be promoted.
    //this.x[2] and this.x[4] can not be promoted and
    //can not be moved out of loop.
    //Because this.x[i] may def them.
    double p;
    double * pthis;
    pthis = &this.x[2];
    for (this.d = 1; this.d <= this.i; this.d = (1 + this.d)) {
        this.x[2] = 0.4 * this.x[4];
        this.x[i] = 0.8; //this IST will prevent scalarization.
        *pthis = 20.0;
    }
    //In this case, if we only scan opnd of each ir, we can
    //find this.x[2]. Assume this.x[i] is not exist, then this.x[2]
    //is promotable.
}


void load_elim3(int i)
{
    //Whole loop is unpromotable!
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



void load_elim2(int i)
{
    //Test IR opnd processing.
    //this.x[3] and this.x[4] can not be promoted and
    //can not be moved out of loop.
    //Because this.x[i] may def them.
    double p;
    for (this.d = 1; this.d <= this.i; this.d = (1 + this.d)) {
        this.x[i] = 0.4 * this.x[4];
        p = this.x[3];
        this.x[3] = 0.8;
        p = this.x[3];
    }
}

void load_elim()
{
    this.x[0] = 0.101;
    for (this.d = 1; this.d <= this.i; this.d = (1 + this.d)) {
        this.x[0] = (0.499975 * (this.x[0] + this.x[1] + this.x[2] - this.x[3]));
        if (this.d > 100) {
            break;
        }
        this.x[1] = (0.499975 * (this.x[0] + this.x[1] - this.x[2] + this.x[3]));
        this.x[2] = (0.499975 * (this.x[0] - this.x[1] + this.x[2] + this.x[3]));
        this.x[3] = (0.499975 * (-this.x[0] + this.x[1] + this.x[2] + this.x[3]));
        this.x[4] = (double)this.d;
    }
}
