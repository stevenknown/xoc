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
#ifndef __TENSOR_H__
#define __TENSOR_H__

namespace xcom {

template <class T> class Tensor {
protected:
    void mulElemWiseDimX(UINT dim, T * input1buf, T * input2buf, T * resbuf);
public:
    //Dump tensor shape in given buffer.
    void dumpShapeBuf(OUT StrBuf & buf) const;

    //Return the dimension of current tensor.
    virtual UINT getDim() const = 0;

    //Return the degree of given dimension of current tensor.
    virtual UINT getDegreeOfDim(UINT dim) const = 0;

    //Get the element by given dimension 'dim' and degree in 'dim'.
    virtual T getElem(UINT dim, UINT degree) const = 0;

    //Return the start address of element buffer.
    virtual T * getElemBuf() const = 0;

    //Get the address pointer of element buffer by given dimension 'dim' and
    //degree in 'dim'.
    //e.g: given tensor is <2x3x5>, 'dim' is 0, the function return the address
    //of 0th dimension which degree is 2.
    T * getDimBuf(UINT dim) const
    {
        ASSERT0(this->getDim() >= 1 && dim < this->getDim());
        UINT addend = 1;
        for (UINT i = this->getDim() - 1; i > dim; i--) {
            addend *= this->getDegreeOfDim(i);
        }
        return getElemBuf() + addend;
    }

    //Return true if 'src' is homogeneous to current tensor's shape.
    bool is_homo_shape(Tensor<T> const& src) const
    {
        if (getDim() != src.getDim()) { return false; }
        for (UINT i = 0; i < getDim(); i++) {
            if (getDegreeOfDim(i) != src.getDegreeOfDim(i)) { return false; }
        }
        return true;
    }

    //The function multiplies element's value in current tensor and input2.
    //The function operates on each elements in current tensor and input2.
    //Note the function supports in-place operation.
    //Record the result in 'res' tensor.
    virtual Tensor<T> & mulElemWise(MOD Tensor<T> & input2,
                                    OUT Tensor<T> & res);

    //Reshape current tensor to be the same shape as 'src'.
    virtual void reshapeTo(Tensor<T> const& src) = 0;

    virtual void setElem(UINT dim, UINT degree, T t) = 0;
};


template <class T>
void Tensor<T>::mulElemWiseDimX(
    UINT dim, T * input1buf, T * input2buf, T * resbuf)
{
    ASSERT0(this->getDim() >= 1 && dim < this->getDim());
    UINT degree = this->getDegreeOfDim(dim);
    if (dim == this->getDim() - 1) {
        for (UINT j = 0; j < degree; j++) {
            resbuf[j] = input1buf[j] * input2buf[j];
        }
        return;
    }
    UINT addend = 1;
    for (UINT i = this->getDim() - 1; i > dim; i--) {
        addend *= this->getDegreeOfDim(i);
    }
    for (UINT j = 0; j < degree; j++) {
        mulElemWiseDimX(dim + 1, input1buf, input2buf, resbuf);
        input1buf = input1buf + addend;
        input2buf = input2buf + addend;
        resbuf = resbuf + addend;
    }
}


template <class T>
Tensor<T> & Tensor<T>::mulElemWise(MOD Tensor<T> & input2, OUT Tensor<T> & res)
{
    ASSERTN(this->is_homo_shape(input2), ("invalid Tensor"));
    res.reshapeTo(*this);
    T * input1buf = this->getElemBuf();
    T * input2buf = input2.getElemBuf();
    T * resbuf = res.getElemBuf();
    mulElemWiseDimX(0, input1buf, input2buf, resbuf);
    return res;
}


template <class T>
void Tensor<T>::dumpShapeBuf(OUT StrBuf & buf) const
{
    buf.strcat("<");
    for (UINT i = 0; i < getDim(); i++) {
        if (i != 0) {
            buf.strcat("x");
        }
        buf.strcat("%u", getDegreeOfDim(i));
    }
    buf.strcat(">");
}

} //namespace xcom
#endif
