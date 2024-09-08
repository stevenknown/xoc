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
#ifndef __TENSOR2D_H__
#define __TENSOR2D_H__

namespace xcom {

template <class T> class Ten2D;
template <class T> class Ten2DMgr;

template <class T> class Ten2D : public Tensor<T>, public Matrix<T> {
protected:
    static PRECISION_TYPE convertVal(T t) { return PRECISION_TYPE(t); }
    static PRECISION_TYPE convertVal(Float const& t)
    { return PRECISION_TYPE(t.val()); }

    static T dotInputAndWeight(
        Ten2D<T> const& input, Ten2D<T> const& weight,
        UINT tile_row_start, UINT tile_col_start);
public:
    Ten2D() {}
    Ten2D(INT) {} //Used by template call of T(0) in Vector<Mat>
    Ten2D(UINT dim0, UINT dim1) : Matrix<T>(dim0, dim1) {}
    ~Ten2D() {}

    //Convolution.
    //Note the function does NOT support in-place operation, res can not be
    //same memory with 'input' or 'weight'.
    //Record the result in 'res' Ten2D.
    static Ten2D<T> & conv(Ten2D<T> const& input, Ten2D<T> const& weight,
                           OUT Ten2D<T> & res);
    static Ten2D<T> & conv(Ten2D<T> const& input, Ten2D<T> const& weight,
                           UINT input_dim0_stride, UINT input_dim1_stride,
                           OUT Ten2D<T> & res);
    static void computeConvResSize(
        Ten2D<T> const& input, Ten2D<T> const& weight,
        UINT input_dim0_stride, UINT input_dim1_stride,
        OUT UINT & res_row, OUT UINT & res_col);
    static Ten2D<T> & cos(MOD Ten2D<T> & input, OUT Ten2D<T> & res);

    //The function performs division for 'v' and elements in input, and restore
    //the result to 'res', which 'v' is dividend, element value is divisor.
    //The function operates on each elements in input.
    //Note the function supports in-place operation.
    //Record the result in 'res' Ten2D.
    static Ten2D<T> & div(T v, MOD Ten2D<T> & input, OUT Ten2D<T> & res);

    //The function performs division for 'v' and elements in input, and restore
    //the result to 'res', which element value is dividend, 'v' is divisor.
    //The function operates on each elements in input.
    //Note the function supports in-place operation.
    //Record the result in 'res' Ten2D.
    static Ten2D<T> & div(MOD Ten2D<T> & input, T v, OUT Ten2D<T> & res);

    virtual UINT getDim() const override { return 2; }
    virtual UINT getDegreeOfDim(UINT dim) const override
    { return dim == 0 ? this->getRowSize() : this->getColSize(); }
    virtual T getElem(UINT dim, UINT degree) const override
    { return this->get(dim, degree); }
    virtual T * getElemBuf() const override
    { return Matrix<T>::getElemBuf(); }

    //The function multiplies element's value in input by 'v'.
    //The function operates on each elements in input.
    //Note the function supports in-place operation.
    //Record the result in 'res' Ten2D.
    static Ten2D<T> & mul(MOD Ten2D<T> & input, T v, OUT Ten2D<T> & res);

    //The function computes elements in input to the power 'v'.
    //The function operates on each elements in input.
    //Note the function supports in-place operation.
    //Record the result in 'res' Ten2D.
    static Ten2D<T> & pow(MOD Ten2D<T> & input, T v, OUT Ten2D<T> & res);

    //Trigonometric Function.
    //Note the function supports in-place operation.
    //Record the result in 'res' Ten2D.
    static Ten2D<T> & tan(MOD Ten2D<T> & input, OUT Ten2D<T> & res);

    //Trigonometric Function.
    //Note the function supports in-place operation.
    //Record the result in 'res' Ten2D.
    static Ten2D<T> & tanh(MOD Ten2D<T> & input, OUT Ten2D<T> & res);

    //Reshape current tensor to be the same shape as 'src'.
    virtual void reshapeTo(Tensor<T> const& src) override
    {
        this->reinit(((Ten2D<T>&)src).getRowSize(),
                     ((Ten2D<T>&)src).getColSize());
    }

    virtual void setElem(UINT dim, UINT degree, T t) override
    { this->set(dim, degree, t); }

    //Calculate Absolute Value.
    //Note the function supports in-place operation.
    //Record the result in 'res' Ten2D.
    static Ten2D<T> & abs(MOD Ten2D<T> & input, OUT Ten2D<T> & res);

    //Trigonometric Function.
    //Note the function supports in-place operation.
    //Record the result in 'res' Ten2D.
    static Ten2D<T> & sin(MOD Ten2D<T> & input, OUT Ten2D<T> & res);

    //The function substracts input2 from input1.
    //Note the function supports in-place operation, and input1 and input2
    //must be homogeneous.
    //Record the result in 'res' Ten2D.
    static Ten2D<T> & sub(MOD Ten2D<T> & input1, MOD Ten2D<T> & input2,
                          OUT Ten2D<T> & res);

    //The function substracts 'v' from element's value in input, namely,
    //element is minuend, v is subtrahend.
    //The function operates on each elements in input.
    //Note the function supports in-place operation.
    //Record the result in 'res' Ten2D.
    static Ten2D<T> & sub(MOD Ten2D<T> & input, T v, OUT Ten2D<T> & res);

    //The function substracts element's value in input from 'v', namely,
    //v is minuend, element is subtrahend.
    //The function operates on each elements in input.
    //Note the function supports in-place operation.
    //Record the result in 'res' Ten2D.
    static Ten2D<T> & sub(T v, MOD Ten2D<T> & input, OUT Ten2D<T> & res);
};


template <class T>
T Ten2D<T>::dotInputAndWeight(
    Ten2D<T> const& input, Ten2D<T> const& weight,
    UINT tile_row_start, UINT tile_col_start)
{
    ASSERT0(tile_row_start < input.getRowSize());
    ASSERT0(tile_col_start < input.getColSize());
    T mac = T(0);
    UINT tile_row_size = MIN(input.getRowSize() - tile_row_start,
                             weight.getRowSize());
    UINT tile_col_size = MIN(input.getColSize() - tile_col_start,
                             weight.getColSize());
    for (UINT ti = tile_row_start, wi = 0; wi < tile_row_size; ti++, wi++) {
        for (UINT tj = tile_col_start, wj = 0; wj < tile_col_size; tj++, wj++) {
            mac = mac + input.get(ti, tj) * weight.get(wi, wj);
        }
    }
    return mac;
}


template <class T>
Ten2D<T> & Ten2D<T>::cos(MOD Ten2D<T> & input, OUT Ten2D<T> & res)
{
    ASSERTN(input.is_init(), ("not yet initialize."));
    ASSERTN(input.getSize() > 0, ("invalid Ten2D"));
    res.reinit(input.getRowSize(), input.getColSize());
    for (UINT i = 0; i < input.getRowSize(); i++) {
        for (UINT j = 0; j < input.getColSize(); j++) {
            PRECISION_TYPE t = ::cos((PRECISION_TYPE)input.get(i, j));
            res.set(i, j, T(t));
        }
    }
    return res;
}


template <class T>
Ten2D<T> & Ten2D<T>::abs(MOD Ten2D<T> & input, OUT Ten2D<T> & res)
{
    ASSERTN(input.is_init(), ("not yet initialize."));
    ASSERTN(input.getSize() > 0, ("invalid Ten2D"));
    res.reinit(input.getRowSize(), input.getColSize());
    for (UINT i = 0; i < input.getRowSize(); i++) {
        for (UINT j = 0; j < input.getColSize(); j++) {
            PRECISION_TYPE t = ::fabs((PRECISION_TYPE)input.get(i, j));
            res.set(i, j, T(t));
        }
    }
    return res;
}


template <class T>
Ten2D<T> & Ten2D<T>::sin(MOD Ten2D<T> & input, OUT Ten2D<T> & res)
{
    ASSERTN(input.is_init(), ("not yet initialize."));
    ASSERTN(input.getSize() > 0, ("invalid Ten2D"));
    res.reinit(input.getRowSize(), input.getColSize());
    for (UINT i = 0; i < input.getRowSize(); i++) {
        for (UINT j = 0; j < input.getColSize(); j++) {
            PRECISION_TYPE t = ::sin((PRECISION_TYPE)input.get(i, j));
            res.set(i, j, T(t));
        }
    }
    return res;
}


template <class T>
Ten2D<T> & Ten2D<T>::sub(MOD Ten2D<T> & input1, MOD Ten2D<T> & input2,
                         OUT Ten2D<T> & res)
{
    ASSERTN(input1.is_init(), ("not yet initialize."));
    ASSERTN(input1.is_init(), ("not yet initialize."));
    ASSERTN(input1.getSize() > 0, ("invalid Ten2D"));
    ASSERTN(input2.getSize() > 0, ("invalid Ten2D"));
    ASSERTN(input1.is_homo(input2), ("invalid Ten2D"));
    res.reinit(input1.getRowSize(), input1.getColSize());
    for (UINT i = 0; i < input1.getRowSize(); i++) {
        for (UINT j = 0; j < input1.getColSize(); j++) {
            res.set(i, j, input1.get(i, j) - input2.get(i, j));
        }
    }
    return res;
}


template <class T>
Ten2D<T> & Ten2D<T>::sub(MOD Ten2D<T> & input, T v, OUT Ten2D<T> & res)
{
    ASSERTN(input.is_init(), ("not yet initialize."));
    ASSERTN(input.getSize() > 0, ("invalid Ten2D"));
    res.reinit(input.getRowSize(), input.getColSize());
    for (UINT i = 0; i < input.getRowSize(); i++) {
        for (UINT j = 0; j < input.getColSize(); j++) {
            res.set(i, j, input.get(i, j) - v);
        }
    }
    return res;
}


template <class T>
Ten2D<T> & Ten2D<T>::sub(T v, MOD Ten2D<T> & input, OUT Ten2D<T> & res)
{
    ASSERTN(input.is_init(), ("not yet initialize."));
    ASSERTN(input.getSize() > 0, ("invalid Ten2D"));
    res.reinit(input.getRowSize(), input.getColSize());
    for (UINT i = 0; i < input.getRowSize(); i++) {
        for (UINT j = 0; j < input.getColSize(); j++) {
            res.set(i, j, v - input.get(i, j));
        }
    }
    return res;
}


template <class T>
Ten2D<T> & Ten2D<T>::mul(MOD Ten2D<T> & input, T v, OUT Ten2D<T> & res)
{
    ASSERTN(input.is_init(), ("not yet initialize."));
    ASSERTN(input.getSize() > 0, ("invalid Ten2D"));
    res.reinit(input.getRowSize(), input.getColSize());
    for (UINT i = 0; i < input.getRowSize(); i++) {
        for (UINT j = 0; j < input.getColSize(); j++) {
            res.set(i, j, input.get(i, j) * v);
        }
    }
    return res;
}


template <class T>
Ten2D<T> & Ten2D<T>::div(MOD Ten2D<T> & input, T v, OUT Ten2D<T> & res)
{
    ASSERTN(input.is_init(), ("not yet initialize."));
    ASSERTN(input.getSize() > 0, ("invalid Ten2D"));
    res.reinit(input.getRowSize(), input.getColSize());
    for (UINT i = 0; i < input.getRowSize(); i++) {
        for (UINT j = 0; j < input.getColSize(); j++) {
            res.set(i, j, input.get(i, j) / v);
        }
    }
    return res;
}


template <class T>
Ten2D<T> & Ten2D<T>::div(T v, MOD Ten2D<T> & input, OUT Ten2D<T> & res)
{
    ASSERTN(input.is_init(), ("not yet initialize."));
    ASSERTN(input.getSize() > 0, ("invalid Ten2D"));
    res.reinit(input.getRowSize(), input.getColSize());
    for (UINT i = 0; i < input.getRowSize(); i++) {
        for (UINT j = 0; j < input.getColSize(); j++) {
            res.set(i, j, v / input.get(i, j));
        }
    }
    return res;
}


template <class T>
Ten2D<T> & Ten2D<T>::pow(MOD Ten2D<T> & input, T v, OUT Ten2D<T> & res)
{
    ASSERTN(input.is_init(), ("not yet initialize."));
    ASSERTN(input.getSize() > 0, ("invalid Ten2D"));
    res.reinit(input.getRowSize(), input.getColSize());
    for (UINT i = 0; i < input.getRowSize(); i++) {
        for (UINT j = 0; j < input.getColSize(); j++) {
            res.set(i, j, (T)::pow((PRECISION_TYPE)input.get(i, j),
                                   (PRECISION_TYPE)v));
        }
    }
    return res;
}


template <class T>
Ten2D<T> & Ten2D<T>::tanh(MOD Ten2D<T> & input, OUT Ten2D<T> & res)
{
    ASSERTN(input.is_init(), ("not yet initialize."));
    ASSERTN(input.getSize() > 0, ("invalid Ten2D"));
    res.reinit(input.getRowSize(), input.getColSize());
    for (UINT i = 0; i < input.getRowSize(); i++) {
        for (UINT j = 0; j < input.getColSize(); j++) {
            PRECISION_TYPE t = ::tanh((PRECISION_TYPE)input.get(i, j));
            res.set(i, j, T(t));
        }
    }
    return res;
}


template <class T>
Ten2D<T> & Ten2D<T>::tan(MOD Ten2D<T> & input, OUT Ten2D<T> & res)
{
    ASSERTN(input.is_init(), ("not yet initialize."));
    ASSERTN(input.getSize() > 0, ("invalid Ten2D"));
    res.reinit(input.getRowSize(), input.getColSize());
    for (UINT i = 0; i < input.getRowSize(); i++) {
        for (UINT j = 0; j < input.getColSize(); j++) {
            PRECISION_TYPE t = ::tan((PRECISION_TYPE)input.get(i, j));
            res.set(i, j, T(t));
        }
    }
    return res;
}


template <class T>
Ten2D<T> & Ten2D<T>::conv(Ten2D<T> const& input, Ten2D<T> const& weight,
                          OUT Ten2D<T> & res)
{
    ASSERTN(input.is_init() && weight.is_init(), ("not yet initialize."));
    ASSERTN(input.getSize() > 0 && weight.getSize() > 0, ("invalid Ten2D"));
    ASSERT0(input.getRowSize() >= weight.getRowSize());
    ASSERT0(input.getColSize() >= weight.getColSize());
    ASSERTN(&input != &res && &weight != &res,
            ("not support in-place operation"));
    UINT tile_row_start = 0;
    UINT tile_col_start = 0;
    UINT tile_row_stride = 1;
    UINT tile_col_stride = 1;
    return conv(input, weight, tile_row_stride, tile_col_stride, res);
}


template <class T>
void Ten2D<T>::computeConvResSize(
    Ten2D<T> const& input, Ten2D<T> const& weight,
    UINT input_dim0_stride, UINT input_dim1_stride,
    OUT UINT & res_row, OUT UINT & res_col)
{
    ASSERTN(input.is_init() && weight.is_init(), ("not yet initialize."));
    ASSERTN(input.getSize() > 0 && weight.getSize() > 0, ("invalid Ten2D"));
    ASSERT0(input.getRowSize() >= weight.getRowSize());
    ASSERT0(input.getColSize() >= weight.getColSize());
    ASSERT0(input_dim0_stride >= 1 && input_dim1_stride >= 1);
    UINT tile_row_stride = input_dim0_stride;
    UINT tile_col_stride = input_dim1_stride;
    bool is_compute_partial = false;
    res_row = 1; //At least one computation.
    res_row += is_compute_partial ?
        input.getRowSize() : input.getRowSize() - weight.getRowSize();

    //Consider stride.
    res_row = xceiling(res_row, tile_row_stride);
    res_col = 1; //At least one computation.
    res_col += is_compute_partial ?
        input.getColSize() : input.getColSize() - weight.getColSize();

    //Consider stride.
    res_col = xceiling(res_col, tile_col_stride);
}


template <class T>
Ten2D<T> & Ten2D<T>::conv(Ten2D<T> const& input, Ten2D<T> const& weight,
                          UINT input_dim0_stride, UINT input_dim1_stride,
                          OUT Ten2D<T> & res)
{
    ASSERTN(input.is_init() && weight.is_init(), ("not yet initialize."));
    ASSERTN(input.getSize() > 0 && weight.getSize() > 0, ("invalid Ten2D"));
    ASSERT0(input.getRowSize() >= weight.getRowSize());
    ASSERT0(input.getColSize() >= weight.getColSize());
    ASSERTN(&input != &res && &weight != &res,
            ("not support in-place operation"));
    ASSERT0(input_dim0_stride >= 1 && input_dim1_stride >= 1);
    UINT res_row, res_col;
    computeConvResSize(input, weight, input_dim0_stride, input_dim1_stride,
                       res_row, res_col);
    res.reinit(res_row, res_col);
    UINT tile_row_stride = input_dim0_stride;
    UINT tile_col_stride = input_dim1_stride;
    for (UINT i = 0, resi = 0; resi < res_row;
         i += tile_row_stride, resi++) {
        for (UINT j = 0, resj = 0; resj < res_col;
             j += tile_col_stride, resj++) {
            T mac = dotInputAndWeight(input, weight, i, j);
            res.set(resi, resj, mac);
        }
    }
    return res;
}


//
//START Ten2DMgr
//
template <class T> class Ten2DMgr : public MatMgr<T> {
    COPY_CONSTRUCTOR(Ten2DMgr);
protected:
    List<Ten2D<T>*> m_tlst;
public:
    Ten2DMgr() {}
    virtual ~Ten2DMgr()
    {
        for (Ten2D<T> * t = m_tlst.get_head();
             t != nullptr; t = m_tlst.get_next()) {
            this->freeMat(t);
        }
    }
    Ten2D<T> * allocTenAndRecord()
    {
        //Use 'this' to invoke the function that is in current object
        //virtual functions. The usage will make compiler more clearer.
        Ten2D<T> * t = (Ten2D<T>*)this->allocMat();
        m_tlst.append_tail(t);
        return t;
    }
    Ten2D<T> * allocTenAndRecord(UINT dim0, UINT dim1)
    {
        Ten2D<T> * t = (Ten2D<T>*)this->allocMat(dim0, dim1);
        m_tlst.append_tail(t);
        return t;
    }
};
//END Ten2DMgr

} //namespace xcom
#endif
