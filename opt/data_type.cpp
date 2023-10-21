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
    {D_UNDEF, "none",  0},
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

    {D_BF16, "bf16", 16}, //bfloat point 16 bits
    {D_F16, "f16", 16}, //float point 16 bits
    {D_F32, "f32", 32}, //float point 32 bits
    {D_F64, "f64", 64},
    {D_F80, "f80", 80},
    {D_F128, "f128", 128},

    {D_MC, "mc", 0}, //memory chunk, for structures, default bitsize is 0
    {D_STR, "str", BYTE_PER_POINTER * BIT_PER_BYTE}, //string is pointer
    {D_PTR, "*", BYTE_PER_POINTER * BIT_PER_BYTE}, //pointer
    {D_VEC, "vec", 0}, //vector, default bitsize is 0

    //void type, default bitsize is as long as pointer type.
    {D_ANY, "any", BYTE_PER_POINTER * BIT_PER_BYTE},
    {D_TENSOR, "tensor", 0}, //tensor type, default bitsize is 0
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


void TensorType::copy(TensorType const& src, TypeMgr * mgr)
{
    Type::copy(src);
    tensor_elem_type = src.tensor_elem_type;
    degree_of_dimension.copy(src.degree_of_dimension, mgr->get_pool());
}
//END TensorType


//The hoisting rules are:
//1. Return max bit size of DATA_TYPE between 'opnd0' and 'opnd1',
//2. else return SIGNED if one of them is signed;
//3. else return FLOAT if one of them is float,
//4. else return UNSIGNED.
//
//The C language rules are:
//1. If any operand is of a integral type smaller than int? Convert to int.
//2. Is any operand unsigned long? Convert the other to unsigned long.
//3. (else) Is any operand signed long? Convert the other to signed long.
//4. (else) Is any operand unsigned int? Convert the other to unsigned int.
//
//NOTE: The function does NOT hoist vector type.
Type const* TypeMgr::hoistDTypeForBinOp(IR const* opnd0, IR const* opnd1)
{
    Type const* d0 = opnd0->getType();
    Type const* d1 = opnd1->getType();
    ASSERT0(!d0->is_any() && !d1->is_any());
    ASSERTN(!d0->is_vector() && !d1->is_vector(),
            ("Can not hoist vector type."));
    ASSERTN(!d0->is_pointer() && !d1->is_pointer(),
            ("Can not hoist pointer type."));
    DATA_TYPE t0 = TY_dtype(d0);
    DATA_TYPE t1 = TY_dtype(d1);
    if (t0 == D_MC && t1 == D_MC) {
        ASSERT0(TY_mc_size(d0) == TY_mc_size(d1));
        return opnd0->getType();
    }
    if (t0 == D_MC) {
        ASSERT0(TY_mc_size(d0) != 0);
        UINT ty_size = MAX(TY_mc_size(d0), getByteSize(d1));
        if (ty_size == TY_mc_size(d0)) {
            return opnd0->getType();
        }
        return opnd1->getType();
    }
    if (t1 == D_MC) {
        ASSERT0(TY_mc_size(d1) != 0);
        UINT ty_size = MAX(TY_mc_size(d1), getByteSize(d0));
        if (ty_size == TY_mc_size(d1)) {
            return opnd1->getType();
        }
        return opnd0->getType();
    }

    //Always hoist to longest integer type.
    //t0 = hoistDtype(t0);
    //t1 = hoistDtype(t1);

    //Generic data type.
    INT bitsize = MAX(getDTypeBitSize(t0), getDTypeBitSize(t1));
    DATA_TYPE res;
    if (IS_FP(t0) || IS_FP(t1)) {
        res = getFPDType(bitsize, false);
    } else if (IS_SINT(t0) || IS_SINT(t1)) {
        res = getIntDType(bitsize, true);
    } else {
        res = getIntDType(bitsize, false);
    }
    ASSERT0(res != D_UNDEF);
    ASSERT0(IS_SIMPLEX(res));
    return getSimplexType(res);
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


UINT TypeMgr::getByteSize(Type const* type) const
{
    ASSERT0(type);
    DATA_TYPE dt = TY_dtype(type);
    switch (dt) {
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
    case D_PTR:
        return getPointerByteSize();
    case D_MC:
        return TY_mc_size(type);
    case D_VEC:
        return TY_vec_size(type);
    case D_TENSOR:
        return ((TensorType const*)type)->getByteSize(this);
    default: ASSERTN(0, ("unsupport"));
    }
    return 0;
}


CHAR const* TypeMgr::dump_type(Type const* type, OUT StrBuf & buf) const
{
    ASSERT0(type);
    DATA_TYPE dt = TY_dtype(type);
    switch (dt) {
    SWITCH_CASE_INT_DTYPE:
    SWITCH_CASE_FP_DTYPE:
    case D_B:
    case D_STR:
        buf.strcat("%s", DTNAME(dt));
        break;
    case D_MC:
        buf.strcat("%s<%d>", DTNAME(dt), getByteSize(type));
        break;
    case D_PTR:
        buf.strcat("%s<%d>", DTNAME(dt), TY_ptr_base_size(type));
        break;
    case D_VEC: {
        UINT elem_byte_size = getDTypeByteSize(TY_vec_ety(type));
        ASSERT0(elem_byte_size != 0);
        ASSERT0(getByteSize(type) % elem_byte_size == 0);
        UINT elemnum = getByteSize(type) / elem_byte_size;
        buf.strcat("%s<%s*%u>", DTNAME(dt), DTNAME(TY_vec_ety(type)), elemnum);
        break;
    }
    case D_TENSOR: {
        //Check byte size of element.
        ASSERT0(getDTypeByteSize(TY_tensor_ety(type)) != 0);
        ASSERT0(getByteSize(type) % getDTypeByteSize(TY_tensor_ety(type)) == 0);
        buf.strcat("%s:%s<", DTNAME(dt), DTNAME(TY_tensor_ety(type)));
        UINT dim = ((TensorType const*)type)->getDim();
        for (UINT i = 0; i < dim; i++) {
            if (i != 0) {
                buf.strcat("x");
            }
            buf.strcat("%d", ((TensorType const*)type)->getDegreeOfDim(i));
        }
        buf.strcat(">");
        break;
    }
    case D_ANY:
        buf.strcat("%s", DTNAME(dt));
        break;
    default: UNREACHABLE();
    }
    return buf.buf;
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


void TypeMgr::dump_type_tab() const
{
    StrBuf buf(64);
    if (!getRegionMgr()->isLogMgrInit()) { return; }
    note(getRegionMgr(), "\n==---- DUMP Type Table ----==\n");
    for (VecIdx i = 1; i <= m_type_tab.get_last_idx(); i++) {
        buf.clean();
        Type * d = m_type_tab.get(i);
        ASSERT0(d);
        prt(getRegionMgr(), "%s tyid:%d", dump_type(d, buf), i);
        note(getRegionMgr(), "\n");
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
    m_f64 = getSimplexType(D_F64);
    m_f80 = getSimplexType(D_F80);
    m_f128 = getSimplexType(D_F128);
    m_str = getSimplexType(D_STR);
    m_any = getSimplexType(D_ANY);
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
