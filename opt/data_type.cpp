/*@
XOC Release License

Copyright (c) 2013-2014, Alibaba Group, All rights reserved.

    compiler@aliexpress.com

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

THIS SOFTWARE IS PROVIDED "AS IS" AND ANY EXPRESS OR IMPLIED
WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES
OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS
BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT
OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR
BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY,
WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR
OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN
IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

author: Su Zhenyu
@*/
#include "cominc.h"

namespace xoc {

TypeDesc const g_type_desc[] = {
    {D_UNDEF, "undef",  0},
    {D_B, "bool", 8}, //bool
    {D_I8, "i8", 8}, //signed integer 8 bits
    {D_I16, "i16", 16},
    {D_I32, "i32", 32},
    {D_I64, "i64", 64},
    {D_I128, "i128", 128},

    {D_U8, "u8", 8}, //unsigned integer 8 bits
    {D_U16, "u16", 16},
    {D_U32, "u32", 32},
    {D_U64, "u64", 64},
    {D_U128, "u128", 128},

    {D_F4X2, "f4x2", 8}, //8-bit container storing two 4-bit FP values (F4).
    {D_F8, "f8", 8}, //8-bit floating point
    {D_F8E4M3, "f8e4m3", 8}, //8-bit float (E4M3 format)
    {D_F8E5M2, "f8e5m2", 8}, //8-bit float (E5M2 format)
    {D_F8_MIX, "f8_mix", 8}, //8-bit float (mixed format).
    {D_BF16, "bf16", 16}, //bfloat16 (16-bit floating point)
    {D_F16, "f16", 16}, //float point 16 bits
    {D_F32, "f32", 32}, //float point 32 bits

    //TensorFloat-32 (32-bit floating point, reduced precision for tensor ops)
    {D_TF32, "tf32", 32},
    {D_F64, "f64", 64},
    {D_F80, "f80", 80},
    {D_F128, "f128", 128},

    //In D_B<n>, n is the ratio of the width of an element to
    //the number of registers used. D_B<n>expresses using n bits
    //to represent a BOOL value. For example, if the element
    //width is 32 bits and the number of registers used is 1,
    //then n=32/1=32.
    {D_B1, "bool1", 8},
    {D_B2, "bool2", 8},
    {D_B4, "bool4", 8},
    {D_B8, "bool8", 8},
    {D_B16, "bool16", 8},
    {D_B32, "bool32", 8},
    {D_B64, "bool64", 8},

    //This type of D_U16M1 is an scalable data type, indicating that
    //the number of vector elements it represents can be scalable.
    //The M<n> or MF<n> after the basic data type represents the
    //group multiplication coefficient, which is the number of
    //physical registers used.
    //For example, M1 represents using one physical register,
    //M2 represents using two physical registers, and MF2 represents
    //using half of one physical register to store data.
    {D_U8M1, "u8m1", 8},
    {D_U8M2, "u8m2", 8},
    {D_U8M4, "u8m4", 8},
    {D_U8M8, "u8m8", 8},
    {D_U8MF2, "u8mf2", 8},
    {D_U8MF4, "u8mf4", 8},
    {D_U8MF8, "u8mf8", 8},

    {D_I8M1, "i8m1", 8},
    {D_I8M2, "i8m2", 8},
    {D_I8M4, "i8m4", 8},
    {D_I8M8, "i8m8", 8},
    {D_I8MF2, "i8mf2", 8},
    {D_I8MF4, "i8mf4", 8},
    {D_I8MF8, "i8mf8", 8},

    {D_U16M1, "u16m1", 16},
    {D_U16M2, "u16m2", 16},
    {D_U16M4, "u16m4", 16},
    {D_U16M8, "u16m8", 16},
    {D_U16MF2, "u16mf2", 16},
    {D_U16MF4, "u16mf4", 16},

    {D_I16M1, "i16m1", 16},
    {D_I16M2, "i16m2", 16},
    {D_I16M4, "i16m4", 16},
    {D_I16M8, "i16m8", 16},
    {D_I16MF2, "i16mf2", 16},
    {D_I16MF4, "i16mf4", 16},

    {D_U32M1, "u32m1", 32},
    {D_U32M2, "u32m2", 32},
    {D_U32M4, "u32m4", 32},
    {D_U32M8, "u32m8", 32},
    {D_U32MF2, "u32mf2", 32},

    {D_I32M1, "i32m1", 32},
    {D_I32M2, "i32m2", 32},
    {D_I32M4, "i32m4", 32},
    {D_I32M8, "i32m8", 32},
    {D_I32MF2, "i32mf2", 32},

    {D_U64M1, "u64m1", 64},
    {D_U64M2, "u64m2", 64},
    {D_U64M4, "u64m4", 64},
    {D_U64M8, "u64m8", 64},

    {D_I64M1, "i64m1", 64},
    {D_I64M2, "i64m2", 64},
    {D_I64M4, "i64m4", 64},
    {D_I64M8, "i64m8", 64},

    {D_BF16M1, "bf16m1", 16},
    {D_BF16M2, "bf16m2", 16},
    {D_BF16M4, "bf16m4", 16},
    {D_BF16M8, "bf16m8", 16},
    {D_BF16MF2, "bf16mf2", 16},
    {D_BF16MF4, "bf16mf4", 16},

    {D_F16M1, "f16m1", 16},
    {D_F16M2, "f16m2", 16},
    {D_F16M4, "f16m4", 16},
    {D_F16M8, "f16m8", 16},
    {D_F16MF2, "f16mf2", 16},
    {D_F16MF4, "f16mf4", 16},

    {D_F32M1, "f32m1", 32},
    {D_F32M2, "f32m2", 32},
    {D_F32M4, "f32m4", 32},
    {D_F32M8, "f32m8", 32},
    {D_F32MF2, "f32mf2", 32},

    {D_F64M1, "f64m1", 64},
    {D_F64M2, "f64m2", 64},
    {D_F64M4, "f64m4", 64},
    {D_F64M8, "f64m8", 64},

    {D_MC, "mc", 0}, //memory chunk, for structures, default bitsize is 0
    {D_STR, "str", BYTE_PER_POINTER * BIT_PER_BYTE}, //string is pointer
    {D_PTR, "*", BYTE_PER_POINTER * BIT_PER_BYTE}, //pointer
    {D_VEC, "vec", 0}, //vector, default bitsize is 0

    //void type, default bitsize is as long as pointer type.
    {D_ANY, "any", BYTE_PER_POINTER * BIT_PER_BYTE},
    {D_TENSOR, "tensor", 0}, //tensor type, default bitsize is 0
    {D_STREAM, "stream", 0}, //stream type, default bitsize is 0

    //Add it to ensure that the length of g_type_desc is consistent with the
    //length of the DATA_TYPE enumeration type to prevent out of bounds access.
    {D_LAST, "none", 0},
};


#ifdef _DEBUG_
Type const* checkType(Type const* ty, DATA_TYPE dt)
{
    ASSERTN(TY_dtype(ty) == dt, ("type is not '%s'", DTNAME(dt)));
    return ty;
}
#endif


//
//START TensorType
//
//Return byte size of total tensor.
UINT TensorType::getByteSize(TypeMgr const* mgr) const
{
    UINT size = mgr->getDTypeByteSize(getElemDataType());
    for (UINT i = 0; i < degree_of_dimension.get_capacity(); i++) {
        ASSERT0(degree_of_dimension[i] > 0);
        size *= degree_of_dimension[i];
    }
    return size;
}


//Set degree to given dimension.
//Note degree should not be 0.
void TensorType::setDegreeOfDim(UINT dim, UINT degree, TypeMgr * mgr)
{
    ASSERTN(degree != 0, ("Degree of dim%d is 0.", dim));
    degree_of_dimension.set(dim, degree, mgr->get_pool());
}


bool TensorType::is_homo(TensorType const& src) const
{
    if (getDim() != src.getDim()) { return false; }
    for (UINT i = 0; i < getDim(); i++) {
        if (getDegreeOfDim(i) != src.getDegreeOfDim(i)) { return false; }
    }
    return true;
}


void TensorType::copy(TensorType const& src, TypeMgr * mgr)
{
    Type::copy(src);
    tensor_elem_type = src.tensor_elem_type;
    degree_of_dimension.copy(src.degree_of_dimension, mgr->get_pool());
}
//END TensorType


UINT TypeMgr::getNumbitsOfBoolTypeInMask(DATA_TYPE dt)
{
    ASSERT0(IS_SCALABLE(dt));
    switch (dt) {
    case D_I8M8:
    case D_U8M8:
    case D_B1: return 1;
    case D_I8M4:
    case D_U8M4:
    case D_I16M8:
    case D_U16M8:
    case D_F16M8:
    case D_BF16M8:
    case D_B2: return 2;
    case D_I8M2:
    case D_U8M2:
    case D_I16M4:
    case D_U16M4:
    case D_F16M4:
    case D_BF16M4:
    case D_I32M8:
    case D_U32M8:
    case D_F32M8:
    case D_B4: return 4;
    case D_I8M1:
    case D_U8M1:
    case D_I16M2:
    case D_U16M2:
    case D_F16M2:
    case D_BF16M2:
    case D_I32M4:
    case D_U32M4:
    case D_F32M4:
    case D_I64M8:
    case D_U64M8:
    case D_F64M8:
    case D_B8: return 8;
    case D_I8MF2:
    case D_U8MF2:
    case D_I16M1:
    case D_U16M1:
    case D_F16M1:
    case D_BF16M1:
    case D_I32M2:
    case D_U32M2:
    case D_F32M2:
    case D_I64M4:
    case D_U64M4:
    case D_F64M4:
    case D_B16: return 16;
    case D_I8MF4:
    case D_U8MF4:
    case D_I16MF2:
    case D_U16MF2:
    case D_F16MF2:
    case D_BF16MF2:
    case D_I32M1:
    case D_U32M1:
    case D_F32M1:
    case D_I64M2:
    case D_U64M2:
    case D_F64M2:
    case D_B32: return 32;
    case D_I8MF8:
    case D_U8MF8:
    case D_I16MF4:
    case D_U16MF4:
    case D_F16MF4:
    case D_BF16MF4:
    case D_I32MF2:
    case D_U32MF2:
    case D_F32MF2:
    case D_I64M1:
    case D_U64M1:
    case D_F64M1:
    case D_B64: return 64;
    default: UNREACHABLE(); break;
    }
    return 0;
}


DATA_TYPE TypeMgr::getScalableDType(DATA_TYPE basic_dt, LMUL lmul)
{
    switch (basic_dt) {
    case D_U8: {
        switch (lmul) {
        case LMUL_M1: return D_U8M1;
        case LMUL_M2: return D_U8M2;
        case LMUL_M4: return D_U8M4;
        case LMUL_M8: return D_U8M8;
        case LMUL_MF2: return D_U8MF2;
        case LMUL_MF4: return D_U8MF4;
        case LMUL_MF8: return D_U8MF8;
        default: UNREACHABLE(); break;
        }
    }
    case D_I8: {
        switch (lmul) {
        case LMUL_M1: return D_I8M1;
        case LMUL_M2: return D_I8M2;
        case LMUL_M4: return D_I8M4;
        case LMUL_M8: return D_I8M8;
        case LMUL_MF2: return D_I8MF2;
        case LMUL_MF4: return D_I8MF4;
        case LMUL_MF8: return D_I8MF8;
        default: UNREACHABLE(); break;
        }
    }
    case D_U16: {
        switch (lmul) {
        case LMUL_M1: return D_U16M1;
        case LMUL_M2: return D_U16M2;
        case LMUL_M4: return D_U16M4;
        case LMUL_M8: return D_U16M8;
        case LMUL_MF2: return D_U16MF2;
        case LMUL_MF4: return D_U16MF4;
        default: UNREACHABLE(); break;
        }
    }
    case D_I16: {
        switch (lmul) {
        case LMUL_M1: return D_I16M1;
        case LMUL_M2: return D_I16M2;
        case LMUL_M4: return D_I16M4;
        case LMUL_M8: return D_I16M8;
        case LMUL_MF2: return D_I16MF2;
        case LMUL_MF4: return D_I16MF4;
        default: UNREACHABLE(); break;
        }
    }
    case D_F16: {
        switch (lmul) {
        case LMUL_M1: return D_F16M1;
        case LMUL_M2: return D_F16M2;
        case LMUL_M4: return D_F16M4;
        case LMUL_M8: return D_F16M8;
        case LMUL_MF2: return D_F16MF2;
        case LMUL_MF4: return D_F16MF4;
        default: UNREACHABLE(); break;
        }
    }
    case D_BF16: {
        switch (lmul) {
        case LMUL_M1: return D_BF16M1;
        case LMUL_M2: return D_BF16M2;
        case LMUL_M4: return D_BF16M4;
        case LMUL_M8: return D_BF16M8;
        case LMUL_MF2: return D_BF16MF2;
        case LMUL_MF4: return D_BF16MF4;
        default: UNREACHABLE(); break;
        }
    }
    case D_U32: {
        switch (lmul) {
        case LMUL_M1: return D_U32M1;
        case LMUL_M2: return D_U32M2;
        case LMUL_M4: return D_U32M4;
        case LMUL_M8: return D_U32M8;
        case LMUL_MF2: return D_U32MF2;
        default: UNREACHABLE(); break;
        }
    }
    case D_I32: {
        switch (lmul) {
        case LMUL_M1: return D_I32M1;
        case LMUL_M2: return D_I32M2;
        case LMUL_M4: return D_I32M4;
        case LMUL_M8: return D_I32M8;
        case LMUL_MF2: return D_I32MF2;
        default: UNREACHABLE(); break;
        }
    }
    case D_F32: {
        switch (lmul) {
        case LMUL_M1: return D_F32M1;
        case LMUL_M2: return D_F32M2;
        case LMUL_M4: return D_F32M4;
        case LMUL_M8: return D_F32M8;
        case LMUL_MF2: return D_F32MF2;
        default: UNREACHABLE(); break;
        }
    }
    case D_U64: {
        switch (lmul) {
        case LMUL_M1: return D_U64M1;
        case LMUL_M2: return D_U64M2;
        case LMUL_M4: return D_U64M4;
        case LMUL_M8: return D_U64M8;
        default: UNREACHABLE(); break;
        }
    }
    case D_I64: {
        switch (lmul) {
        case LMUL_M1: return D_I64M1;
        case LMUL_M2: return D_I64M2;
        case LMUL_M4: return D_I64M4;
        case LMUL_M8: return D_I64M8;
        default: UNREACHABLE(); break;
        }
    }
    case D_F64: {
        switch (lmul) {
        case LMUL_M1: return D_F64M1;
        case LMUL_M2: return D_F64M2;
        case LMUL_M4: return D_F64M4;
        case LMUL_M8: return D_F64M8;
        default: UNREACHABLE(); break;
        }
    }
    default: UNREACHABLE(); break;
    }
    UNREACHABLE();
    return D_UNDEF;
}


class TypeMgrIntlImpl {
public:
    static Type const* hoistDTypeForSameSize(
        DATA_TYPE t0, DATA_TYPE t1, MOD TypeMgr * tm)
    {
        ASSERT0(IS_INT(t0) || IS_INT(t1));
        ASSERT0(tm->getDTypeBitSize(t0) == tm->getDTypeBitSize(t1));
        if (IS_SINT(t0)) {
            ASSERT0(IS_SIMPLEX(t1));
            //Prefer USINGED if one of them are UNSINGED.
            return tm->getSimplexType(t1);
        }
        if (IS_SINT(t1)) {
            ASSERT0(IS_SIMPLEX(t0));
            //Prefer USINGED if one of them are UNSINGED.
            return tm->getSimplexType(t0);
        }
        //Both of them are UNSIGNED.
        return tm->getSimplexType(t0);
    }
};


Type const* TypeMgr::hoistDTypeForBinOpWithINTType(
    Type const* d0, Type const* d1)
{
    ASSERT0(!d0->is_any() && !d1->is_any());
    ASSERTN(!d0->is_vector() && !d1->is_vector(),
            ("Can not hoist vector type."));
    ASSERTN(!d0->is_pointer() && !d1->is_pointer(),
            ("Can not hoist pointer type."));
    DATA_TYPE t0 = TY_dtype(d0);
    DATA_TYPE t1 = TY_dtype(d1);
    ASSERT0(IS_INT(t0) || IS_INT(t1));

    //Always hoist to longest integer type.
    //t0 = hoistDtype(t0);
    //t1 = hoistDtype(t1);

    //Integer Conversion Rank:
    //1. The greater the bitsize is, the higher the rank is.
    //2. The bitsize is the same, the rank is the same.
    //Generic data type.
    UINT bitsize0 = getDTypeBitSize(t0);
    UINT bitsize1 = getDTypeBitSize(t1);
    if (bitsize0 == bitsize1) {
        return TypeMgrIntlImpl::hoistDTypeForSameSize(t0, t1, this);
    }
    UINT maxbitsize = MAX(bitsize0, bitsize1);
    bool is_signed = false;
    if (bitsize0 == maxbitsize) {
        is_signed = IS_SINT(t0);
    } else {
        is_signed = IS_SINT(t1);
    }
    DATA_TYPE res = getIntDType(maxbitsize, is_signed);
    ASSERT0(res != D_UNDEF);
    ASSERT0(IS_SIMPLEX(res));
    return getSimplexType(res);
}


Type const* TypeMgr::hoistDTypeForBinOpWithFPType(
    Type const* d0, Type const* d1)
{
    ASSERT0(!d0->is_any() && !d1->is_any());
    ASSERTN(!d0->is_vector() && !d1->is_vector(),
            ("Can not hoist vector type."));
    ASSERTN(!d0->is_pointer() && !d1->is_pointer(),
            ("Can not hoist pointer type."));
    DATA_TYPE t0 = TY_dtype(d0);
    DATA_TYPE t1 = TY_dtype(d1);
    ASSERT0(IS_FP(t0) || IS_FP(t1));
    //Always hoist to longest integer type.
    //t0 = hoistDtype(t0);
    //t1 = hoistDtype(t1);

    //Generic data type.
    UINT bitsize = MAX(getDTypeBitSize(t0), getDTypeBitSize(t1));
    DATA_TYPE res = getFPDType(bitsize, false);
    ASSERT0(res != D_UNDEF);
    ASSERT0(IS_SIMPLEX(res));
    return getSimplexType(res);
}


//The function handles cases that at least one of operands
//is of MC type.
Type const* TypeMgr::hoistDTypeForBinOpWithMCType(
    Type const* d0, Type const* d1)
{
    ASSERT0(!d0->is_any() && !d1->is_any());
    ASSERTN(!d0->is_vector() && !d1->is_vector(),
            ("Can not hoist vector type."));
    ASSERTN(!d0->is_pointer() && !d1->is_pointer(),
            ("Can not hoist pointer type."));
    DATA_TYPE t0 = TY_dtype(d0);
    DATA_TYPE t1 = TY_dtype(d1);
    if (t0 == D_MC && t1 == D_MC) {
        ASSERT0(TY_mc_size(d0) == TY_mc_size(d1));
        return d0;
    }
    if (t0 == D_MC) {
        ASSERT0(TY_mc_size(d0) != 0);
        UINT ty_size = MAX(TY_mc_size(d0), getByteSize(d1));
        if (ty_size == TY_mc_size(d0)) {
            return d0;
        }
        return d1;
    }
    if (t1 == D_MC) {
        ASSERT0(TY_mc_size(d1) != 0);
        UINT ty_size = MAX(TY_mc_size(d1), getByteSize(d0));
        if (ty_size == TY_mc_size(d1)) {
            return d1;
        }
        return d0;
    }
    UNREACHABLE();
    return nullptr;
}


Type const* TypeMgr::hoistDTypeForBinOp(IR const* opnd0, IR const* opnd1)
{
    Type const* d0 = opnd0->getType();
    Type const* d1 = opnd1->getType();
    if (d0 == d1) { return d0; }
    ASSERT0(!d0->is_any() && !d1->is_any());
    ASSERTN(!d0->is_vector() && !d1->is_vector(),
            ("Can not hoist vector type."));
    ASSERTN(!d0->is_pointer() && !d1->is_pointer(),
            ("Can not hoist pointer type."));
    DATA_TYPE t0 = TY_dtype(d0);
    DATA_TYPE t1 = TY_dtype(d1);
    if (t0 == D_MC || t1 == D_MC) {
        return hoistDTypeForBinOpWithMCType(d0, d1);
    }
    if (IS_FP(t0) || IS_FP(t1)) {
        return hoistDTypeForBinOpWithFPType(d0, d1);
    }
    return hoistDTypeForBinOpWithINTType(d0, d1);
}


//Hoist DATA_TYPE up to upper bound of given bit length.
DATA_TYPE TypeMgr::hoistDtype(UINT data_size, OUT UINT * hoisted_data_size)
{
    DATA_TYPE dt = D_UNDEF;
    if (data_size > getDTypeBitSize(D_I128)) {
        //Memory chunk
        dt = D_MC;
        *hoisted_data_size = data_size;
    } else {
        dt = hoistBSdtype(data_size, false);
        *hoisted_data_size = getDTypeByteSize(dt);
    }
    return dt;
}


//Hoist DATA_TYPE up to upper bound of given type.
DATA_TYPE TypeMgr::hoistDtype(IN DATA_TYPE dt) const
{
    if (IS_INT(dt) &&
        getDTypeBitSize(dt) < (BYTE_PER_INT * BIT_PER_BYTE)) {
        //Hoist to longest INT type.
        return hoistBSdtype(BYTE_PER_INT * BIT_PER_BYTE, IS_SINT(dt));
    }
    return dt;
}


DATA_TYPE TypeMgr::hoistBSdtype(UINT bit_size, bool is_signed) const
{
    DATA_TYPE m = D_UNDEF;
    if (bit_size > 1 && bit_size <= 8) {
        m = is_signed ? D_I8 : D_U8;
    } else if (bit_size > 8 && bit_size <= 16) {
        m = is_signed ? D_I16 : D_U16;
    } else if (bit_size > 16 && bit_size <= 32) {
        m = is_signed ? D_I32 : D_U32;
    } else if (bit_size > 32 && bit_size <= 64) {
        m = is_signed ? D_I64 : D_U64;
    } else if (bit_size > 64 && bit_size <= 128) {
        m = is_signed ? D_I128 : D_U128;
    } else if (bit_size == 1) {
        m = D_B;
    } else if (bit_size > 128) {
        m = D_MC;
    }
    return m;
}


//Register a pointer data type.
TypeContainer const* TypeMgr::registerPointer(Type const* type)
{
    ASSERT0(type && type->is_pointer());
    //Insertion Sort by ptr-base-size in incrmental order.
    //e.g: Given PTR, base_size=32,
    //    PTR, base_size=24
    //    PTR, base_size=128
    //    ...
    //=> after insertion.
    //    PTR, base_size=24
    //    PTR, base_size=32  //insert into here.
    //    PTR, base_size=128
    //    ...
    TypeContainer const* entry = m_pointer_type_tab.get(type);
    if (entry != nullptr) {
        return entry;
    }

    //Add new item into table.
    TypeContainer * x = newTC();
    PointerType * pt = newPointerType();
    TC_type(x) = pt;
    pt->copy((PointerType const&)*type);
    m_pointer_type_tab.set((Type const*)pt, x);
    TC_typeid(x) = m_type_count++;
    m_type_tab.set(TC_typeid(x), pt);
    return x;
}


//'type': it must be D_MC type, and the vector-element-type can not D_UNDEF,
//e.g: vector<I8,I8,I8,I8> type, which mc_size is 32 byte, vec-type is D_I8.
TypeContainer const* TypeMgr::registerVector(Type const* type)
{
    ASSERT0(type->is_vector() && TY_vec_ety(type) != D_UNDEF);
    ASSERT0(TY_vec_size(type) >= getDTypeByteSize(TY_vec_ety(type)) &&
            TY_vec_size(type) % getDTypeByteSize(TY_vec_ety(type)) == 0);

    VectorElemTypeTab * elemtab = m_vector_type_tab.get(type);
    if (elemtab != nullptr) {
        TypeContainer const* entry = elemtab->get(type);
        if (entry != nullptr) {
            return entry;
        }
    }

    //Add new element type into vector.
    //e.g:
    //    VEC,size=100,vec_ty=D_UNDEF
    //    VEC,size=200,vec_ty=D_UNDEF
    //        VEC,size=200,vec_ty=D_I8
    //        VEC,size=200,vec_ty=D_U8 //I8<U8
    //        VEC,size=200,vec_ty=D_F32
    //    VEC,size=300,vec_ty=D_UNDEF
    //        VEC,size=300,vec_ty=D_F32
    //    ...
    TypeContainer * x = newTC();
    VectorType * ty = newVectorType();
    TC_type(x) = ty;
    ty->copy((VectorType const&)*type);
    if (elemtab == nullptr) {
        //Add new vector into table.
        elemtab = new VectorElemTypeTab();
        m_vector_type_tab.set(ty, elemtab);
    }
    elemtab->set(ty, x);
    TC_typeid(x) = m_type_count++;
    m_type_tab.set(TC_typeid(x), ty);
    return x;
}


//Register a stream data type.
//ty: it must be D_STREAM type, and the stream-element-type can not D_UNDEF,
//e.g: stream<I8> type, which stream-element-type is D_I8.
TypeContainer const* TypeMgr::registerStream(Type const* type)
{
    ASSERT0(type->is_stream() && TY_stream_ety(type) != D_UNDEF);
    TypeContainer const* entry = m_stream_type_tab.get(type);
    if (entry != nullptr) {
        return entry;
    }

    //Add new element type into vector.
    //e.g:
    //    STREAM,vec_ty=D_UNDEF
    //        STREAM,vec_ty=D_I8
    //        STREAM,vec_ty=D_U8 //I8<U8
    //        STREAM,vec_ty=D_F32
    //    ...
    TypeContainer * x = newTC();
    StreamType * ty = newStreamType();
    TC_type(x) = ty;
    ty->copy((StreamType const&)*type);
    //Add new vector into table.
    m_stream_type_tab.set(ty, x);
    TC_typeid(x) = m_type_count++;
    m_type_tab.set(TC_typeid(x), ty);
    return x;
}


//'type': it must be D_TENSOR type, and the tensor-element-type can not D_UNDEF,
TypeContainer const* TypeMgr::registerTensor(Type const* type)
{
    ASSERT0(type->is_tensor() && TY_tensor_ety(type) != D_UNDEF);
    ASSERT0(((TensorType const*)type)->getByteSize(this) >=
            getDTypeByteSize(TY_tensor_ety(type)));
    ASSERT0(((TensorType const*)type)->getByteSize(this) %
            getDTypeByteSize(TY_tensor_ety(type)) == 0);

    TensorElemTypeTab * elemtab = m_tensor_type_tab.get(type);
    if (elemtab != nullptr) {
        TypeContainer const* entry = elemtab->get(type);
        if (entry != nullptr) {
            return entry;
        }
    }

    //Add new element type into table.
    //e.g:
    //    TENSOR,ety=D_U8
    //        TENSOR,ety=D_U8,dim=<2,3>
    //        TENSOR,ety=D_U8,dim=<2,3,4>
    //        TENSOR,ety=D_U8,dim=<2,3,5>
    //    TENSOR,ety=D_U16
    //        TENSOR,ety=D_U16,dim=<2,3>
    //        TENSOR,ety=D_U16,dim=<2,3,4>
    //        TENSOR,ety=D_U16,dim=<2,3,5>
    TypeContainer * x = newTC();
    TensorType * ty = newTensorType();
    TC_type(x) = ty;
    ty->copy((TensorType const&)*type, this);
    if (elemtab == nullptr) {
        //Add new tensor into table.
        elemtab = new TensorElemTypeTab();
        m_tensor_type_tab.set(ty, elemtab);
    }
    elemtab->set(ty, x);
    TC_typeid(x) = m_type_count++;
    m_type_tab.set(TC_typeid(x), ty);
    return x;
}


//Return tensor type, total byte size of tensor =
//degree_of_dim0 * degree_of_dim1 * ...  * degree_of_dimN * elem_byte_size.
//e.g: Get tensor with type D_F32<2x3x4x5x1>.
//Type const* tensor = getTensorType(D_F32, 4, 2, 3, 5, 1);
//Return type indicates there are 120 elements in tensor,
//each element is D_F32, the degree of dimension 0 is 2, and degree of
//dimenson 1 is 3, and so on. Total size of tensor is 480 bytes.
Type const* TypeMgr::getTensorType(DATA_TYPE elem_ty, UINT dim, ...)
{
    ASSERT0(dim != 0 && elem_ty != D_UNDEF);
    TensorType d;
    TY_dtype(&d) = D_TENSOR;
    TY_tensor_ety(&d) = elem_ty;
    va_list args;
    va_start(args, dim);
    UINT i = 0;
    while (i < dim) {
        UINT degree = (UINT)va_arg(args, UINT);
        d.setDegreeOfDim(i, degree, this);
        i++;
    }
    va_end(args);
    return TC_type(registerTensor(&d));
}


TypeContainer const* TypeMgr::registerMC(Type const* type)
{
    ASSERT0(type);
    //Insertion Sort by mc-size in incrmental order.
    //e.g:Given MC, mc_size=32
    //MC, mc_size=24
    //MC, mc_size=32 <= insert into here.
    //MC, mc_size=128
    //...
    if (type->is_vector()) {
        return registerVector(type);
    }

    ASSERT0(type->is_mc());
    TypeContainer const* entry = m_memorychunk_type_tab.get(type);
    if (entry != nullptr) {
        return entry;
    }

    //Add new item into table.
    TypeContainer * x = newTC();
    MCType * ty = newMCType();
    TC_type(x) = ty;
    ty->copy((MCType const&)*type);
    m_memorychunk_type_tab.set(ty, x);
    TC_typeid(x) = m_type_count++;
    m_type_tab.set(TC_typeid(x), ty);
    return x;
}


//Register simplex type, e.g:INT, UINT, FP, bool.
TypeContainer const* TypeMgr::registerSimplex(Type const* type)
{
    ASSERT0(type);
    TypeContainer ** head = &m_simplex_type[TY_dtype(type)];
    if (*head == nullptr) {
        *head = newTC();
        TypeContainer * x = *head;
        Type * ty = newType();
        TC_type(x) = ty;
        m_simplex_type[TY_dtype(type)] = x;
        ty->copy(*type);
        TC_typeid(x) = m_type_count++;
        m_type_tab.set(TC_typeid(x), ty);
        return x;
    }
    return *head;
}


//Return ty-idx in m_type_tab.
Type * TypeMgr::registerType(Type const* type)
{
    ASSERT0(type);

    #ifdef _DEBUG_
    if (type->is_pointer()) {
        //Some pointer is never participate pointer-arithmetic, it may be
        //used as just a representation of pointer. Moreover, the base-size of
        //pointer is always useful when the pointer is regarded as base of
        //ILD, IST, pointer-arith as well. Thus, we will allow the base-size
        //is zero here for conveninence.
        //ASSERT0(TY_ptr_base_size(type) != 0);
    }

    if (type->is_vector()) {
        ASSERT0(TY_dtype(type) == D_MC && TY_mc_size(type) != 0);
    }
    #endif

    if (type->is_simplex()) {
        return TC_type(registerSimplex(type));
    }

    if (type->is_pointer()) {
        return TC_type(registerPointer(type));
    }

    if (type->is_mc()) {
        return TC_type(registerMC(type));
    }

    if (type->is_vector()) {
        return TC_type(registerMC(type));
    }

    if (type->is_tensor()) {
        return TC_type(registerTensor(type));
    }

    ASSERTN(0, ("unsupport data type"));

    return nullptr;
}


UINT TypeMgr::getByteSizeIfItExist(Type const* ty) const
{
    if (ty->is_any()) { return 0; }
    return getByteSize(ty);
}


UINT TypeMgr::getByteSize(Type const* type) const
{
    ASSERT0(type);
    DATA_TYPE dt = TY_dtype(type);
    switch (dt) {
    SWITCH_CASE_SCALABLE_ELEM_DTYPE:
    SWITCH_CASE_INT_DTYPE:
    SWITCH_CASE_FP_DTYPE:
    case D_B:
    case D_STR:
        return getDTypeByteSize(dt);
    case D_ANY:
        ASSERTN(0, ("can not query bytesize for ANY type"));
        //We intend to give the same size as PTR type as the placeholder size
        //of ANY type, because ANY type always be represented by Object Pointer
        //in runtime system.
        return 0;
    case D_PTR: return getPointerByteSize();
    case D_MC: return ((MCType const*)type)->getByteSize();
    case D_VEC: return ((VectorType const*)type)->getByteSize();
    case D_TENSOR: return ((TensorType const*)type)->getByteSize(this);
    default: ASSERTN(0, ("unsupport"));
    }
    return 0;
}


void TypeMgr::dump_type(UINT tyid) const
{
    dump_type(getType(tyid));
}


void TypeMgr::dump_type(Type const* type) const
{
    if (!getRegionMgr()->isLogMgrInit()) { return; }
    StrBuf buf(64);
    prt(getRegionMgr(), "%s", dump_type(type, buf));
}


void TypeMgr::dump() const
{
    RegionMgr const* rm = getRegionMgr();
    ASSERT0(rm);
    if (!rm->isLogMgrInit()) { return; }
    xcom::StrBuf buf(64);
    note(rm, "\n==---- DUMP TypeMgr ----==");
    for (VecIdx i = UNDEF_TYID + 1; i <= m_type_tab.get_last_idx(); i++) {
        buf.clean();
        Type const* d = m_type_tab.get(i);
        ASSERT0(d);
        note(rm, "\ntyid:%u %s bytesize:%u ", i,
             dump_type(d, buf), getByteSizeIfItExist(d));
        if (d->is_scalar()) { prt(rm, "scalar "); }
        if (d->is_signed()) { prt(rm, "signed "); }
        if (d->is_unsigned()) { prt(rm, "unsigned "); }
        if (d->is_scalable_elem_ty()) { prt(rm, "scalable_elem "); }
        if (d->is_vector_with_scalable_elem_type()) {
            prt(rm, "vector_with_scalable_elem ");
        }
    }
}


//
//START Type
//
bool Type::verify(TypeMgr const* tm) const
{
    if (is_pointer()) {
        //Some pointer is never participate pointer-arithmetic, it may be
        //used as just a representation of pointer. Moreover, the base-size of
        //pointer is always useful when the pointer is regarded as base of
        //ILD, IST, pointer-arith as well. Thus, we will allow the base-size
        //is zero here for conveninence.
        //ASSERT0(TY_ptr_base_size(this) != 0);
    } else if (is_mc()) {
        ASSERT0(TY_mc_size(this) != 0);
    } else {
        //IR_mc_size may be not zero.
        //e.g: struct {int x;}s;
        //    int w = s.x;
        //    Here we get w IR_LD(s, offset=0, mc_size=4)
        //ASSERT0(IR_mc_size(this) == 0);
    }

    if (is_vector()) {
        ASSERT0(TY_vec_ety(this) != D_UNDEF);
        ASSERTN(IS_SIMPLEX(TY_vec_ety(this)) || IS_PTR(TY_vec_ety(this)),
                ("illegal vector elem type"));
        ASSERT0((TY_vec_size(this) >= tm->getDTypeByteSize(TY_vec_ety(this))) &&
                (TY_vec_size(this) %
                 tm->getDTypeByteSize(TY_vec_ety(this)) == 0));
    }
    return true;
}
//END Type


//
//START TypeMgr
//
TypeMgr::TypeMgr(RegionMgr * rm)
{
    ASSERT0(rm);
    m_rm = rm;
    m_type_tab.clean();
    m_pool = smpoolCreate(sizeof(Type) * 8, MEM_COMM);
    m_type_count = 1;
    ::memset((void*)m_simplex_type, 0, sizeof(m_simplex_type));
    m_b = getSimplexType(D_B);
    m_i8 = getSimplexType(D_I8);
    m_f4x2 = getSimplexType(D_F4X2);
    m_f8 = getSimplexType(D_F8);
    m_f8e4m3 = getSimplexType(D_F8E4M3);
    m_f8e5m2 = getSimplexType(D_F8E5M2);
    m_f8mix = getSimplexType(D_F8_MIX);
    m_i16 = getSimplexType(D_I16);
    m_i32 = getSimplexType(D_I32);
    m_i64 = getSimplexType(D_I64);
    m_i128 = getSimplexType(D_I128);
    m_u8 = getSimplexType(D_U8);
    m_u16 = getSimplexType(D_U16);
    m_u32 = getSimplexType(D_U32);
    m_u64 = getSimplexType(D_U64);
    m_u128 = getSimplexType(D_U128);
    m_bf16 = getSimplexType(D_BF16);
    m_f16 = getSimplexType(D_F16);
    m_f32 = getSimplexType(D_F32);
    m_tf32 = getSimplexType(D_TF32);
    m_f64 = getSimplexType(D_F64);
    m_f80 = getSimplexType(D_F80);
    m_f128 = getSimplexType(D_F128);
    m_str = getSimplexType(D_STR);
    m_any = getSimplexType(D_ANY);
    m_bool1 = getSimplexType(D_B1);
    m_bool2 = getSimplexType(D_B2);
    m_bool4 = getSimplexType(D_B4);
    m_bool8 = getSimplexType(D_B8);
    m_bool16 = getSimplexType(D_B16);
    m_bool32 = getSimplexType(D_B32);
    m_bool64 = getSimplexType(D_B64);
    m_u8m1 = getSimplexType(D_U8M1);
    m_u8m2 = getSimplexType(D_U8M2);
    m_u8m4 = getSimplexType(D_U8M4);
    m_u8m8 = getSimplexType(D_U8M8);
    m_u8mf2 = getSimplexType(D_U8MF2);
    m_u8mf4 = getSimplexType(D_U8MF4);
    m_u8mf8 = getSimplexType(D_U8MF8);
    m_i8m1 = getSimplexType(D_I8M1);
    m_i8m2 = getSimplexType(D_I8M2);
    m_i8m4 = getSimplexType(D_I8M4);
    m_i8m8 = getSimplexType(D_I8M8);
    m_i8mf2 = getSimplexType(D_I8MF2);
    m_i8mf4 = getSimplexType(D_I8MF4);
    m_i8mf8 = getSimplexType(D_I8MF8);
    m_u16m1 = getSimplexType(D_U16M1);
    m_u16m2 = getSimplexType(D_U16M2);
    m_u16m4 = getSimplexType(D_U16M4);
    m_u16m8 = getSimplexType(D_U16M8);
    m_u16mf2 = getSimplexType(D_U16MF2);
    m_u16mf4 = getSimplexType(D_U16MF4);
    m_i16m1 = getSimplexType(D_I16M1);
    m_i16m2 = getSimplexType(D_I16M2);
    m_i16m4 = getSimplexType(D_I16M4);
    m_i16m8 = getSimplexType(D_I16M8);
    m_i16mf2 = getSimplexType(D_I16MF2);
    m_i16mf4 = getSimplexType(D_I16MF4);
    m_u32m1 = getSimplexType(D_U32M1);
    m_u32m2 = getSimplexType(D_U32M2);
    m_u32m4 = getSimplexType(D_U32M4);
    m_u32m8 = getSimplexType(D_U32M8);
    m_u32mf2 = getSimplexType(D_U32MF2);
    m_i32m1 = getSimplexType(D_I32M1);
    m_i32m2 = getSimplexType(D_I32M2);
    m_i32m4 = getSimplexType(D_I32M4);
    m_i32m8 = getSimplexType(D_I32M8);
    m_i32mf2 = getSimplexType(D_I32MF2);
    m_u64m1 = getSimplexType(D_U64M1);
    m_u64m2 = getSimplexType(D_U64M2);
    m_u64m4 = getSimplexType(D_U64M4);
    m_u64m8 = getSimplexType(D_U64M8);
    m_i64m1 = getSimplexType(D_I64M1);
    m_i64m2 = getSimplexType(D_I64M2);
    m_i64m4 = getSimplexType(D_I64M4);
    m_i64m8 = getSimplexType(D_I64M8);
    m_bf16m1 = getSimplexType(D_BF16M1);
    m_bf16m2 = getSimplexType(D_BF16M2);
    m_bf16m4 = getSimplexType(D_BF16M4);
    m_bf16m8 = getSimplexType(D_BF16M8);
    m_bf16mf2 = getSimplexType(D_BF16MF2);
    m_bf16mf4 = getSimplexType(D_BF16MF4);
    m_f16m1 = getSimplexType(D_F16M1);
    m_f16m2 = getSimplexType(D_F16M2);
    m_f16m4 = getSimplexType(D_F16M4);
    m_f16m8 = getSimplexType(D_F16M8);
    m_f16mf2 = getSimplexType(D_F16MF2);
    m_f16mf4 = getSimplexType(D_F16MF4);
    m_f32m1 = getSimplexType(D_F32M1);
    m_f32m2 = getSimplexType(D_F32M2);
    m_f32m4 = getSimplexType(D_F32M4);
    m_f32m8 = getSimplexType(D_F32M8);
    m_f32mf2 = getSimplexType(D_F32MF2);
    m_f64m1 = getSimplexType(D_F64M1);
    m_f64m2 = getSimplexType(D_F64M2);
    m_f64m4 = getSimplexType(D_F64M4);
    m_f64m8 = getSimplexType(D_F64M8);
}


//Return existing type according to given DATA_TYPE.
Type const* TypeMgr::getSimplexTypeEx(DATA_TYPE dt) const
{
    switch (dt) {
    case D_B: return m_b;
    case D_I8: return m_i8;
    case D_I16: return m_i16;
    case D_I32: return m_i32;
    case D_I64: return m_i64;
    case D_I128: return m_i128;
    case D_U8: return m_u8;
    case D_U16: return m_u16;
    case D_U32: return m_u32;
    case D_U64: return m_u64;
    case D_U128: return m_u128;
    case D_F8E4M3: return m_f8e4m3;
    case D_F8E5M2: return m_f8e5m2;
    case D_F4X2: return m_f4x2;
    case D_BF16: return m_bf16;
    case D_F16: return m_f16;
    case D_F32: return m_f32;
    case D_F64: return m_f64;
    case D_F80: return m_f80;
    case D_F128: return m_f128;
    case D_B1: return m_bool1;
    case D_B2: return m_bool2;
    case D_B4: return m_bool4;
    case D_B8: return m_bool8;
    case D_B16: return m_bool16;
    case D_B32: return m_bool32;
    case D_B64: return m_bool64;
    case D_U8M1: return m_u8m1;
    case D_U8M2: return m_u8m2;
    case D_U8M4: return m_u8m4;
    case D_U8M8: return m_u8m8;
    case D_U8MF2: return m_u8mf2;
    case D_U8MF4: return m_u8mf4;
    case D_U8MF8: return m_u8mf8;
    case D_I8M1: return m_i8m1;
    case D_I8M2: return m_i8m2;
    case D_I8M4: return m_i8m4;
    case D_I8M8: return m_i8m8;
    case D_I8MF2: return m_i8mf2;
    case D_I8MF4: return m_i8mf4;
    case D_I8MF8: return m_i8mf8;
    case D_U16M1: return m_u16m1;
    case D_U16M2: return m_u16m2;
    case D_U16M4: return m_u16m4;
    case D_U16M8: return m_u16m8;
    case D_U16MF2: return m_u16mf2;
    case D_U16MF4: return m_u16mf4;
    case D_I16M1: return m_i16m1;
    case D_I16M2: return m_i16m2;
    case D_I16M4: return m_i16m4;
    case D_I16M8: return m_i16m8;
    case D_I16MF2: return m_i16mf2;
    case D_I16MF4: return m_i16mf4;
    case D_U32M1: return m_u32m1;
    case D_U32M2: return m_u32m2;
    case D_U32M4: return m_u32m4;
    case D_U32M8: return m_u32m8;
    case D_U32MF2: return m_u32mf2;
    case D_I32M1: return m_i32m1;
    case D_I32M2: return m_i32m2;
    case D_I32M4: return m_i32m4;
    case D_I32M8: return m_i32m8;
    case D_I32MF2: return m_i32mf2;
    case D_U64M1: return m_u64m1;
    case D_U64M2: return m_u64m2;
    case D_U64M4: return m_u64m4;
    case D_U64M8: return m_u64m8;
    case D_I64M1: return m_i64m1;
    case D_I64M2: return m_i64m2;
    case D_I64M4: return m_i64m4;
    case D_I64M8: return m_i64m8;
    case D_BF16M1: return m_bf16m1;
    case D_BF16M2: return m_bf16m2;
    case D_BF16M4: return m_bf16m4;
    case D_BF16M8: return m_bf16m8;
    case D_BF16MF2: return m_bf16mf2;
    case D_BF16MF4: return m_bf16mf4;
    case D_F16M1: return m_f16m1;
    case D_F16M2: return m_f16m2;
    case D_F16M4: return m_f16m4;
    case D_F16M8: return m_f16m8;
    case D_F16MF2: return m_f16mf2;
    case D_F16MF4: return m_f16mf4;
    case D_F32M1: return m_f32m1;
    case D_F32M2: return m_f32m2;
    case D_F32M4: return m_f32m4;
    case D_F32M8: return m_f32m8;
    case D_F32MF2: return m_f32mf2;
    case D_F64M1: return m_f64m1;
    case D_F64M2: return m_f64m2;
    case D_F64M4: return m_f64m4;
    case D_F64M8: return m_f64m8;
    case D_STR: return m_str;
    case D_ANY: return m_any;
    default: ASSERTN(0, ("not simplex type")); return 0;
    }
    return 0;
}


UINT TypeMgr::getVectorElemNumWithScalableElemType(DATA_TYPE vec_elem_ty)
{
    switch (vec_elem_ty) {
    case D_B1: return getMaxBitSizeOfVectorRegister() / 1;
    case D_B2: return getMaxBitSizeOfVectorRegister() / 2;
    case D_B4: return getMaxBitSizeOfVectorRegister() / 4;
    case D_B8: return getMaxBitSizeOfVectorRegister() / 8;
    case D_B16: return getMaxBitSizeOfVectorRegister() / 16;
    case D_B32: return getMaxBitSizeOfVectorRegister() / 32;
    case D_B64: return getMaxBitSizeOfVectorRegister() / 64;
    case D_U8M1: return getMaxBitSizeOfVectorRegister() / 8;
    case D_U8M2: return 2 * getMaxBitSizeOfVectorRegister() / 8;
    case D_U8M4: return 4 * getMaxBitSizeOfVectorRegister() / 8;
    case D_U8M8: return 8 * getMaxBitSizeOfVectorRegister() / 8;
    case D_U8MF2: return getMaxBitSizeOfVectorRegister() / 8 / 2;
    case D_U8MF4: return getMaxBitSizeOfVectorRegister() / 8 / 4;
    case D_U8MF8: return getMaxBitSizeOfVectorRegister() / 8 / 8;
    case D_I8M1: return getMaxBitSizeOfVectorRegister() / 8;
    case D_I8M2: return 2 * getMaxBitSizeOfVectorRegister() / 8;
    case D_I8M4: return 4 * getMaxBitSizeOfVectorRegister() / 8;
    case D_I8M8: return 8 * getMaxBitSizeOfVectorRegister() / 8;
    case D_I8MF2: return getMaxBitSizeOfVectorRegister() / 8 / 2;
    case D_I8MF4: return getMaxBitSizeOfVectorRegister() / 8 / 4;
    case D_I8MF8: return getMaxBitSizeOfVectorRegister() / 8 / 8;
    case D_U16M1: return getMaxBitSizeOfVectorRegister() / 16;
    case D_U16M2: return 2 * getMaxBitSizeOfVectorRegister() / 16;
    case D_U16M4: return 4 * getMaxBitSizeOfVectorRegister() / 16;
    case D_U16M8: return 8 * getMaxBitSizeOfVectorRegister() / 16;
    case D_U16MF2: return getMaxBitSizeOfVectorRegister() / 16 / 2;
    case D_U16MF4: return getMaxBitSizeOfVectorRegister() / 16 / 4;
    case D_I16M1: return getMaxBitSizeOfVectorRegister() / 16;
    case D_I16M2: return 2 * getMaxBitSizeOfVectorRegister() / 16;
    case D_I16M4: return 4 * getMaxBitSizeOfVectorRegister() / 16;
    case D_I16M8: return 8 * getMaxBitSizeOfVectorRegister() / 16;
    case D_I16MF2: return getMaxBitSizeOfVectorRegister() / 16 / 2;
    case D_I16MF4: return getMaxBitSizeOfVectorRegister() / 16 / 4;
    case D_U32M1: return getMaxBitSizeOfVectorRegister() / 32;
    case D_U32M2: return 2 * getMaxBitSizeOfVectorRegister() / 32;
    case D_U32M4: return 4 * getMaxBitSizeOfVectorRegister() / 32;
    case D_U32M8: return 8 * getMaxBitSizeOfVectorRegister() / 32;
    case D_U32MF2: return getMaxBitSizeOfVectorRegister() / 32 / 2;
    case D_I32M1: return getMaxBitSizeOfVectorRegister() / 32;
    case D_I32M2: return 2 * getMaxBitSizeOfVectorRegister() / 32;
    case D_I32M4: return 4 * getMaxBitSizeOfVectorRegister() / 32;
    case D_I32M8: return 8 * getMaxBitSizeOfVectorRegister() / 32;
    case D_I32MF2: return getMaxBitSizeOfVectorRegister() / 32 / 2;
    case D_U64M1: return getMaxBitSizeOfVectorRegister() / 64;
    case D_U64M2: return 2 * getMaxBitSizeOfVectorRegister() / 64;
    case D_U64M4: return 4 * getMaxBitSizeOfVectorRegister() / 64;
    case D_U64M8: return 8 * getMaxBitSizeOfVectorRegister() / 64;
    case D_I64M1: return getMaxBitSizeOfVectorRegister() / 64;
    case D_I64M2: return 2 * getMaxBitSizeOfVectorRegister() / 64;
    case D_I64M4: return 4 * getMaxBitSizeOfVectorRegister() / 64;
    case D_I64M8: return 8 * getMaxBitSizeOfVectorRegister() / 64;
    case D_BF16M1: return getMaxBitSizeOfVectorRegister() / 16;
    case D_BF16M2: return 2 * getMaxBitSizeOfVectorRegister() / 16;
    case D_BF16M4: return 4 * getMaxBitSizeOfVectorRegister() / 16;
    case D_BF16M8: return 8 * getMaxBitSizeOfVectorRegister() / 16;
    case D_BF16MF2: return getMaxBitSizeOfVectorRegister() / 16 / 2;
    case D_BF16MF4: return getMaxBitSizeOfVectorRegister() / 16 / 4;
    case D_F16M1: return getMaxBitSizeOfVectorRegister() / 16;
    case D_F16M2: return 2 * getMaxBitSizeOfVectorRegister() / 16;
    case D_F16M4: return 4 * getMaxBitSizeOfVectorRegister() / 16;
    case D_F16M8: return 8 * getMaxBitSizeOfVectorRegister() / 16;
    case D_F16MF2: return getMaxBitSizeOfVectorRegister() / 16 / 2;
    case D_F16MF4: return getMaxBitSizeOfVectorRegister() / 16 / 4;
    case D_F32M1: return getMaxBitSizeOfVectorRegister() / 32;
    case D_F32M2: return 2 * getMaxBitSizeOfVectorRegister() / 32;
    case D_F32M4: return 4 * getMaxBitSizeOfVectorRegister() / 32;
    case D_F32M8: return 8 * getMaxBitSizeOfVectorRegister() / 32;
    case D_F32MF2: return getMaxBitSizeOfVectorRegister() / 32 / 2;
    case D_F64M1: return getMaxBitSizeOfVectorRegister() / 64;
    case D_F64M2: return 2 * getMaxBitSizeOfVectorRegister() / 64;
    case D_F64M4: return 4 * getMaxBitSizeOfVectorRegister() / 64;
    case D_F64M8: return 8 * getMaxBitSizeOfVectorRegister() / 64;
    default: ASSERTN(0, ("Unsupported data type")); return 0;
    }
    return 0;
}


Type const* TypeMgr::getScalableBoolTypeWithNonBool(DATA_TYPE vec_elem_ty)
    const
{
    switch (vec_elem_ty) {
    case D_U8M1: return m_bool8;
    case D_U8M2: return m_bool4;
    case D_U8M8: return m_bool1;
    case D_U8M4: return m_bool2;
    case D_U8MF2: return m_bool16;
    case D_U8MF4: return m_bool32;
    case D_U8MF8: return m_bool64;
    case D_I8M1: return m_bool8;
    case D_I8M2: return m_bool4;
    case D_I8M4: return m_bool2;
    case D_I8M8: return m_bool1;
    case D_I8MF2: return m_bool16;
    case D_I8MF4: return m_bool32;
    case D_I8MF8: return m_bool64;
    case D_U16M1: return m_bool16;
    case D_U16M2: return m_bool8;
    case D_U16M4: return m_bool4;
    case D_U16M8: return m_bool2;
    case D_U16MF2: return m_bool32;
    case D_U16MF4: return m_bool64;
    case D_I16M1: return m_bool16;
    case D_I16M2: return m_bool8;
    case D_I16M4: return m_bool4;
    case D_I16M8: return m_bool2;
    case D_I16MF2: return m_bool32;
    case D_I16MF4: return m_bool64;
    case D_U32M1: return m_bool32;
    case D_U32M2: return m_bool16;
    case D_U32M4: return m_bool8;
    case D_U32M8: return m_bool4;
    case D_U32MF2: return m_bool64;
    case D_I32M1: return m_bool32;
    case D_I32M2: return m_bool16;
    case D_I32M4: return m_bool8;
    case D_I32M8: return m_bool4;
    case D_I32MF2: return m_bool64;
    case D_U64M1: return m_bool64;
    case D_U64M2: return m_bool32;
    case D_U64M4: return m_bool16;
    case D_U64M8: return m_bool8;
    case D_I64M1: return m_bool64;
    case D_I64M2: return m_bool32;
    case D_I64M4: return m_bool16;
    case D_I64M8: return m_bool8;
    case D_BF16M1: return m_bool16;
    case D_BF16M2: return m_bool8;
    case D_BF16M4: return m_bool4;
    case D_BF16M8: return m_bool2;
    case D_BF16MF2: return m_bool32;
    case D_BF16MF4: return m_bool64;
    case D_F16M1: return m_bool16;
    case D_F16M2: return m_bool8;
    case D_F16M4: return m_bool4;
    case D_F16M8: return m_bool2;
    case D_F16MF2: return m_bool32;
    case D_F16MF4: return m_bool64;
    case D_F32M1: return m_bool32;
    case D_F32M2: return m_bool16;
    case D_F32M4: return m_bool8;
    case D_F32M8: return m_bool4;
    case D_F32MF2: return m_bool64;
    case D_F64M1: return m_bool64;
    case D_F64M2: return m_bool32;
    case D_F64M4: return m_bool16;
    case D_F64M8: return m_bool8;
    case D_B1: return m_bool1;
    case D_B2: return m_bool2;
    case D_B4: return m_bool4;
    case D_B8: return m_bool8;
    case D_B16: return m_bool16;
    case D_B32: return m_bool32;
    case D_B64: return m_bool64;
    default: ASSERTN(0, ("Unsupported data type")); return nullptr;
    }
}


DATA_TYPE TypeMgr::getBasicDTypeWithDType(DATA_TYPE dtype)
{
    if (dtype >= D_B && dtype <= D_F128) { return dtype; }
    switch (dtype) {
    case D_B1:
    case D_B2:
    case D_B4:
    case D_B8: return D_U8;
    case D_B16: return D_U16;
    case D_B32: return D_U32;
    case D_B64: return D_U64;
    case D_U8M1:
    case D_U8M2:
    case D_U8M4:
    case D_U8M8:
    case D_U8MF2:
    case D_U8MF4:
    case D_U8MF8: return D_U8;
    case D_I8M1:
    case D_I8M2:
    case D_I8M4:
    case D_I8M8:
    case D_I8MF2:
    case D_I8MF4:
    case D_I8MF8: return D_I8;
    case D_I16M1:
    case D_I16M2:
    case D_I16M4:
    case D_I16M8:
    case D_I16MF2:
    case D_I16MF4: return D_I16;
    case D_U16M1:
    case D_U16M2:
    case D_U16M4:
    case D_U16M8:
    case D_U16MF2:
    case D_U16MF4: return D_U16;
    case D_U32M1:
    case D_U32M2:
    case D_U32M4:
    case D_U32M8:
    case D_U32MF2: return D_U32;
    case D_I32M1:
    case D_I32M2:
    case D_I32M4:
    case D_I32M8:
    case D_I32MF2: return D_I32;
    case D_U64M1:
    case D_U64M2:
    case D_U64M4:
    case D_U64M8: return D_U64;
    case D_I64M1:
    case D_I64M2:
    case D_I64M4:
    case D_I64M8: return D_I64;
    case D_BF16M1:
    case D_BF16M2:
    case D_BF16M4:
    case D_BF16M8:
    case D_BF16MF2:
    case D_BF16MF4: return D_BF16;
    case D_F16M1:
    case D_F16M2:
    case D_F16M4:
    case D_F16M8:
    case D_F16MF2:
    case D_F16MF4: return D_F16;
    case D_F32M1:
    case D_F32M2:
    case D_F32M4:
    case D_F32M8:
    case D_F32MF2: return D_F32;
    case D_F64M1:
    case D_F64M2:
    case D_F64M4:
    case D_F64M8: return D_F64;
    default: UNREACHABLE();
    }
    return D_UNDEF;
}


LMUL TypeMgr::getLmulWithDType(DATA_TYPE dtype)
{
    if (dtype >= D_B && dtype <= D_F128) { return LMUL_M1; }
    switch (dtype) {
    case D_B1: return LMUL_M8;
    case D_B2: return LMUL_M4;
    case D_B4: return LMUL_M2;
    case D_B8:
    case D_B16:
    case D_B32:
    case D_B64: return LMUL_M1;
    case D_U8M1:
    case D_I8M1:
    case D_U16M1:
    case D_I16M1:
    case D_BF16M1:
    case D_F16M1:
    case D_U32M1:
    case D_I32M1:
    case D_U64M1:
    case D_I64M1:
    case D_F32M1:
    case D_F64M1: return LMUL_M1;
    case D_U8M2:
    case D_I8M2:
    case D_U16M2:
    case D_I16M2:
    case D_BF16M2:
    case D_F16M2:
    case D_U32M2:
    case D_I32M2:
    case D_U64M2:
    case D_I64M2:
    case D_F32M2:
    case D_F64M2: return LMUL_M2;
    case D_U8M4:
    case D_I8M4:
    case D_U16M4:
    case D_I16M4:
    case D_BF16M4:
    case D_F16M4:
    case D_U32M4:
    case D_I32M4:
    case D_U64M4:
    case D_I64M4:
    case D_F32M4:
    case D_F64M4: return LMUL_M4;
    case D_U8M8:
    case D_I8M8:
    case D_U16M8:
    case D_I16M8:
    case D_BF16M8:
    case D_F16M8:
    case D_U32M8:
    case D_I32M8:
    case D_U64M8:
    case D_I64M8:
    case D_F32M8:
    case D_F64M8: return LMUL_M8;
    case D_U8MF2:
    case D_I8MF2:
    case D_U16MF2:
    case D_I16MF2:
    case D_BF16MF2:
    case D_F16MF2:
    case D_U32MF2:
    case D_I32MF2:
    case D_F32MF2: return LMUL_MF2;
    case D_U8MF4:
    case D_I8MF4:
    case D_U16MF4:
    case D_I16MF4:
    case D_BF16MF4:
    case D_F16MF4: return LMUL_MF4;
    case D_U8MF8:
    case D_I8MF8: return LMUL_MF8;
    default: UNREACHABLE();
    }
    return LMUL_UNDEF;
}


TypeMgr::~TypeMgr()
{
    smpoolDelete(m_pool);
    m_pool = nullptr;

    VectorElemTypeTabIter iter;
    VectorElemTypeTab * tab;
    for (Type const* d = m_vector_type_tab.get_first(iter, &tab);
         d != nullptr; d = m_vector_type_tab.get_next(iter, &tab)) {
        ASSERT0(tab);
        delete tab;
    }

    TensorElemTypeTabIter iter2;
    TensorElemTypeTab * tab2;
    for (Type const* d = m_tensor_type_tab.get_first(iter2, &tab2);
         d != nullptr; d = m_tensor_type_tab.get_next(iter2, &tab2)) {
        ASSERT0(tab2);
        delete tab2;
    }
}
//END TypeMgr

} //namespace xoc
