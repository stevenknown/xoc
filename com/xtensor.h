/*@
Copyright (c) 2013-2021, Su Zhenyu steven.known@gmail.com
All rights reserved.

Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions are met:

    * Redistributions of source code must retain the above copyright
      notice, this list of conditions and the following disclaimer.
    * Redistributions in binary form must reproduce the above copyright
      notice, this list of conditions and the following disclaimer in the
      documentation and/or other materials provided with the distribution.
    * Neither the name of the Su Zhenyu nor the names of its contributors
      may be used to endorse or promote products derived from this software
      without specific prior written permission.

THIS SOFTWARE IS PROVIDED "AS IS" AND ANY
EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE
FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,
OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE
USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
@*/
#ifndef __XTENSOR_H__
#define __XTENSOR_H__

namespace xcom {

template <class T> class Ten2D;
template <class T> class Ten2DMgr;
class FTen2D;

//Float
#define DEFAULT_SD            6
#define USE_FAST_BUT_LOW_PRECISION_SQRT

typedef MatWrap<Float> FTen2DWrap;

class FTen2D : public Ten2D<Float> {
protected:
    virtual void copyTBuf(Float * dst, Float const* src, UINT elemnum) override
    { ::memcpy(dst, src, elemnum * sizeof(Float)); }
public:
    FTen2D() {}
    FTen2D(UINT dim0, UINT dim1) : Ten2D<Float>(dim0, dim1) {}
    ~FTen2D() {}

    virtual void dumpT2S(Float const& t) const override
    { printf("%f, ", t.val()); }
    virtual void dumpT2Buf(Float const& t, OUT StrBuf & buf) const override
    { buf.strcat("%f, ", t.val()); }
};


class FTen2DMgr : public Ten2DMgr<Float> {
    COPY_CONSTRUCTOR(FTen2DMgr);
public:
    FTen2DMgr() {}
    ~FTen2DMgr() {}
    virtual Matrix<Float> * allocMat() override
    {
        ASSERT0(incMatCnt());
        FTen2D * m = new FTen2D();
        ASSERT0(m->setId(m_matcnt));
        return m;
    }
    virtual Matrix<Float> * allocMat(UINT row, UINT col) override
    {
        ASSERT0(incMatCnt());
        FTen2D * m = new FTen2D(row, col);
        ASSERT0(m->setId(m_matcnt));
        return m;
    }
    FTen2D * allocTen() { return (FTen2D*)allocMat(); }
    FTen2D * allocTen(UINT dim0, UINT dim1)
    { return (FTen2D*)allocMat(dim0, dim1); }
    FTen2D * allocTenAndRecord()
    { return (FTen2D*)Ten2DMgr<Float>::allocTenAndRecord(); }
    FTen2D * allocTenAndRecord(UINT dim0, UINT dim1)
    { return (FTen2D*)Ten2DMgr<Float>::allocTenAndRecord(dim0, dim1); }
};


class ITen2D : public Ten2D<INT> {
protected:
    virtual void copyTBuf(INT * dst, INT const* src, UINT elemnum) override
    { ::memcpy(dst, src, elemnum * sizeof(INT)); }
public:
    ITen2D() {}
    ITen2D(INT) {} //Used by template call of T(0) in Vector<Mat>.
    ITen2D(UINT dim0, UINT dim1) : Ten2D<INT>(dim0, dim1) {}
    ~ITen2D() {}

    virtual void dumpT2S(INT const& t) const  override { printf("%d, ", t); }
    virtual void dumpT2Buf(INT const& t, OUT StrBuf & buf) const override
    { buf.sprint("%d", t); }
};


class ITen2DMgr : public Ten2DMgr<INT> {
    COPY_CONSTRUCTOR(ITen2DMgr);
public:
    ITen2DMgr() {}
    ~ITen2DMgr() {}
    virtual Matrix<INT> * allocMat() override
    {
        ASSERT0(incMatCnt());
        ITen2D * m = new ITen2D();
        ASSERT0(m->setId(m_matcnt));
        return m;
    }
    virtual Matrix<INT> * allocMat(UINT row, UINT col) override
    {
        ASSERT0(incMatCnt());
        ITen2D * m = new ITen2D(row, col);
        ASSERT0(m->setId(m_matcnt));
        return m;
    }
    ITen2D * allocTen() { return (ITen2D*)allocMat(); }
    ITen2D * allocTen(UINT dim0, UINT dim1)
    { return (ITen2D*)allocMat(dim0, dim1); }
    ITen2D * allocTenAndRecord()
    { return (ITen2D*)Ten2DMgr<INT>::allocTenAndRecord(); }
    ITen2D * allocTenAndRecord(UINT dim0, UINT dim1)
    { return (ITen2D*)Ten2DMgr<INT>::allocTenAndRecord(dim0, dim1); }
};

} //namespace xcom

#endif
