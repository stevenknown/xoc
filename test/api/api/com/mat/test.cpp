#include "../../../../../src/opt/targ_const_info.h"
#include "../../../../../src/com/xcominc.h"

void testMat()
{
    IMatMgr im;
    {
        IMat m(2,1);
        m.growRowAndCol(1,2,9);
        m.dumps();
        IMat tm(3,3);
        tm.setPartialElem(9,
            0,9,9,
            0,9,9,
            9,9,9);
        ASSERT0(m == tm);
    }
    {
        RMat m(2,1);
        m.growRowAndCol(1,2,9);
        m.dumps();
        RMat tm(3,3);
        tm.sete(9,
            0,9,9,
            0,9,9,
            9,9,9);
        ASSERT0(m == tm);
    }
    {
        FMat m(2,1);
        m.growRowAndCol(1,2,9);
        m.dumps();
        FMat tm(3,3);
        tm.sete(9,
            (double)0,(double)9,(double)9,
            (double)0,(double)9,(double)9,
            (double)9,(double)9,(double)9);
        ASSERT0(m == tm);
    }
    {
        IMat m(1,2);
        m.growRowAndCol(0,3,9);
        m.dumps();
        IMat tm(1,5);
        tm.setPartialElem(5,
            0,0,9,9,9);
        ASSERT0(m == tm);
    }
    {
        IMat m(1,2);
        m.growRowAndCol(2,0,9);
        m.dumps();
        IMat tm(3,2);
        tm.setPartialElem(6,
            0,0,
            9,9,
            9,9);
        ASSERT0(m == tm);
    }
    {
        IMat m(1,2);
        m.growRowAndCol(1,1,9);
        m.dumps();
        IMat tm(2,3);
        tm.setPartialElem(6,
            0,0,9,
            9,9,9);
        ASSERT0(m == tm);
    }
    {
        IMat m(1,2);
        m.growRowAndCol(2,1,9);
        m.dumps();
        IMat tm(3,3);
        tm.setPartialElem(9,
            0,0,9,
            9,9,9,
            9,9,9);
        ASSERT0(m == tm);
    }
    {
        IMat m(1,2);
        m.padTo(im, 0,2,0,1,9);
        m.dumps();
        IMat tm(3,3);
        tm.setPartialElem(9,
            0,0,9,
            9,9,9,
            9,9,9);
        ASSERT0(m == tm);
    }
    {
        IMat m(1,2);
        m.padTo(im, 1,2,0,1,9);
        m.dumps();
        IMat tm(4,3);
        tm.setPartialElem(12,
            9,9,9,
            0,0,9,
            9,9,9,
            9,9,9);
        ASSERT0(m == tm);
    }
    {
        IMat m(1,2);
        m.padTo(im, 1,2,2,1,9);
        m.dumps();
        IMat tm(4,5);
        tm.setPartialElem(20,
            9,9,9,9,9,
            9,9,0,0,9,
            9,9,9,9,9,
            9,9,9,9,9);
        ASSERT0(m == tm);
    }
    {
        IMat m(1,2);
        m.padTo(im, 1,0,2,0,9);
        m.dumps();
        IMat tm(2,4);
        tm.setPartialElem(8,
            9,9,9,9,
            9,9,0,0);
        ASSERT0(m == tm);
    }
}

int main()
{
    testMat();
    return 0;
}
