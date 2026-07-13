int a;
int test_int()
{
    int ** p;
    int * q;
    int * x;
    int r;
    //int a;
    /*
    add X,0 => X
    sub X,X => 0
    and X,-1 => X
    mod X,1 => 0
    xor X,X => 0
    add (add X,1),1 => add X,2
    X<<64 => 0
    */

    {
        //a = **p - **p;
        //a = *p + 0;
        //a = r & -1;
        //q = ((char*)(*q + 1)) * 2;
        q = ((char*)(*q + 1)) - 2;
    }
     return q;
}


/*
double test_fp()
{
    long long a, b, c;
    c = a * b;

    float t;
    t = t / 244140625.0; //t=t*0.0
    t = t / 65536.0; //t=t*0.000015
    t = t / 1023.0; //no change
    t = t / 1.0; //t = t
     return t;
}
*/

