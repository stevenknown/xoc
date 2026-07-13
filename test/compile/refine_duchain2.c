int foo(char * pchar, char * qchar)
{
    int res, res2;
    pchar = 100;
    qchar = 101;

    //*pchar is not dependent to *qchar.
    *pchar = 300; //S1, USE is S4.

    res = *qchar; //S2, HAS NO DEF.

    //*(int*)pchar is actually access 4bytes, it is overlap to *qchar.
    *(int*)pchar = 400; //S3, USE is S4.

    res2 = *(short*)qchar; //S4, DEF is {S1,S3}.

    return res+res2; 
}
