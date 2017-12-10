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
    {D_B,     "bool",  8}, //BOOL
    {D_I8,    "i8",    8}, //signed integer 8 bits
    {D_I16,   "i16",   16},
    {D_I32,   "i32",   32},
    {D_I64,   "i64",   64},
    {D_I128,  "i128",  128},

    {D_U8,    "u8",    8},//unsigned integer 8 bits
    {D_U16,   "u16",   16},
    {D_U32,   "u32",   32},
    {D_U64,   "u64",   64},
    {D_U128,  "u128",  128},

    {D_F32,   "f32",   32}, //float point 32 bits
    {D_F64,   "f64",   64},
    {D_F80,   "f80",   80},
    {D_F128,  "f128",  128},

    {D_MC,    "mc",    0}, //memory chunk, for structures
    {D_STR,   "str",   BYTE_PER_POINTER * BIT_PER_BYTE}, //string is pointer
    {D_PTR,   "*",     BYTE_PER_POINTER * BIT_PER_BYTE}, //pointer
    {D_VEC,   "vec",   0}, //vector

    {D_VOID,  "void",  0}, //void type
};


#ifdef _DEBUG_
Type const* checkType(Type const* ty, DATA_TYPE dt)
{
    ASSERT(TY_dtype(ty) == dt, ("type is not '%s'", DTNAME(dt)));
    return ty;
}
#endif


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
Type const* TypeMgr::hoistDtypeForBinop(IR const* opnd0, IR const* opnd1)
{
    Type const* d0 = opnd0->get_type();
    Type const* d1 = opnd1->get_type();
    ASSERT0(!d0->is_void() && !d1->is_void());
    ASSERT(!d0->is_vector() && !d1->is_vector(),
           ("Can not hoist vector type."));
    ASSERT(!d0->is_pointer() && !d1->is_pointer(),
           ("Can not hoist pointer type."));

    DATA_TYPE t0 = TY_dtype(d0);
    DATA_TYPE t1 = TY_dtype(d1);
    if (t0 == D_MC && t1 == D_MC) {
        ASSERT0(TY_mc_size(d0) == TY_mc_size(d1));
        return opnd0->get_type();
    }

    if (t0 == D_MC) {
        ASSERT0(TY_mc_size(d0) != 0);
        UINT ty_size = MAX(TY_mc_size(d0), get_bytesize(d1));
        if (ty_size == TY_mc_size(d0)) {
            return opnd0->get_type();
        }
        return opnd1->get_type();
    }

    if (t1 == D_MC) {
        ASSERT0(TY_mc_size(d1) != 0);
        UINT ty_size = MAX(TY_mc_size(d1), get_bytesize(d0));
        if (ty_size == TY_mc_size(d1)) {
            return opnd1->get_type();
        }
        return opnd0->get_type();
    }

    //Always hoist to longest integer type.
    //t0 = hoistDtype(t0);
    //t1 = hoistDtype(t1);

    //Generic data type.
    INT bitsize = MAX(get_dtype_bitsize(t0), get_dtype_bitsize(t1));
    DATA_TYPE res;
    if (IS_FP(t0) || IS_FP(t1)) {
        res = get_fp_dtype(bitsize);
    } else if (IS_SINT(t0) || IS_SINT(t1)) {
        res = get_int_dtype(bitsize, true);
    } else {
        res = get_int_dtype(bitsize, false);
    }

    ASSERT0(res != D_UNDEF);
    ASSERT0(IS_SIMPLEX(res));
    return getSimplexType(res);
}


//Hoist DATA_TYPE up to upper bound of given bit length.
DATA_TYPE TypeMgr::hoistDtype(UINT data_size, OUT UINT * hoisted_data_size)
{
    DATA_TYPE dt = D_UNDEF;
    if (data_size > get_dtype_bitsize(D_I128)) {
        //Memory chunk
        dt = D_MC;
        *hoisted_data_size = data_size;
    } else {
        dt = hoistBSdtype(data_size, false);
        *hoisted_data_size = get_dtype_bytesize(dt);
    }
    return dt;
}


//Hoist DATA_TYPE up to upper bound of given type.
DATA_TYPE TypeMgr::hoistDtype(IN DATA_TYPE dt) const
{
    if (IS_INT(dt) &&
        get_dtype_bitsize(dt) < (BYTE_PER_INT * BIT_PER_BYTE)) {
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


//Return ty-idx in m_type_tab.
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
    if (entry != NULL) {
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
    ASSERT0(TY_vec_size(type) >= get_dtype_bytesize(TY_vec_ety(type)) &&
            TY_vec_size(type) % get_dtype_bytesize(TY_vec_ety(type)) == 0);

    ElemTypeTab * elemtab = m_vector_type_tab.get(type);
    if (elemtab != NULL) {
        TypeContainer const* entry = elemtab->get(type);
        if (entry != NULL) {
            return entry;
        }
    }

    //Add new element type into vector.
    //e.g:
    //    MC,size=100,vec_ty=D_UNDEF
    //    MC,size=200,vec_ty=D_UNDEF
    //        MC,size=200,vec_ty=D_I8
    //        MC,size=200,vec_ty=D_U8 //I8<U8
    //        MC,size=200,vec_ty=D_F32
    //    MC,size=300,vec_ty=D_UNDEF
    //        MC,size=300,vec_ty=D_F32
    //    ...
    TypeContainer * x = newTC();
    VectorType * ty = newVectorType();
    TC_type(x) = ty;
    ty->copy((VectorType const&)*type);
    if (elemtab == NULL) {
        //Add new vector into table.
        elemtab = new ElemTypeTab();
        m_vector_type_tab.set(ty, elemtab);
    }
    elemtab->set(ty, x);
    TC_typeid(x) = m_type_count++;
    m_type_tab.set(TC_typeid(x), ty);
    return x;
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

    TypeContainer const* entry = m_memorychunk_type_tab.get(type);
    if (entry != NULL) {
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


//Register simplex type, e.g:INT, UINT, FP, BOOL.
TypeContainer const* TypeMgr::registerSimplex(Type const* type)
{
    ASSERT0(type);
    TypeContainer ** head = &m_simplex_type[TY_dtype(type)];
    if (*head == NULL) {
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
        ASSERT0(TY_ptr_base_size(type) != 0);
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

    ASSERT(0, ("unsupport data type"));

    return NULL;
}


//READONLY
UINT TypeMgr::get_bytesize(Type const* type) const
{
    ASSERT0(type);
    DATA_TYPE dt = TY_dtype(type);
    switch (dt) {
    case D_B:
    case D_I8:
    case D_I16:
    case D_I32:
    case D_I64:
    case D_I128:
    case D_U8:
    case D_U16:
    case D_U32:
    case D_U64:
    case D_U128:
    case D_F32:
    case D_F64:
    case D_F80:
    case D_F128:
    case D_STR:
    case D_VOID:
        return get_dtype_bytesize(dt);
    case D_PTR:
        return get_pointer_bytesize();
    case D_MC:
        return TY_mc_size(type);
    case D_VEC:
        return TY_vec_size(type);
    default: ASSERT(0, ("unsupport"));
    }
    return 0;
}


CHAR const* TypeMgr::dump_type(Type const* type, OUT StrBuf & buf)
{
    ASSERT0(type);
    DATA_TYPE dt = TY_dtype(type);
    switch (dt) {
    case D_B:
    case D_I8:
    case D_I16:
    case D_I32:
    case D_I64:
    case D_I128:
    case D_U8:
    case D_U16:
    case D_U32:
    case D_U64:
    case D_U128:
    case D_F32:
    case D_F64:
    case D_F80:
    case D_F128:
    case D_STR:
        buf.strcat("%s", DTNAME(dt));
        break;
    case D_MC:
        buf.strcat("%s<%d>", DTNAME(dt), get_bytesize(type));
        break;
    case D_PTR:
        buf.strcat("%s<%d>", DTNAME(dt), TY_ptr_base_size(type));
        break;
    case D_VEC:
        {
            UINT elem_byte_size = get_dtype_bytesize(TY_vec_ety(type));
            ASSERT0(elem_byte_size != 0);
            ASSERT0(get_bytesize(type) % elem_byte_size == 0);
            UINT elemnum = get_bytesize(type) / elem_byte_size;
            buf.strcat("%s<%d*%s>", DTNAME(dt), elemnum, DTNAME(TY_vec_ety(type)));
        }
        break;
    case D_VOID:
        buf.strcat("%s", DTNAME(dt));
        break;
    default: UNREACH();
    }
    return buf.buf;
}


void TypeMgr::dump_type(UINT tyid)
{
    dump_type(get_type(tyid));
}


void TypeMgr::dump_type(Type const* type)
{
    if (g_tfile == NULL) return;
    StrBuf buf(64);
    fprintf(g_tfile, "%s", dump_type(type, buf));
    fflush(g_tfile);
}


void TypeMgr::dump_type_tab()
{
    StrBuf buf(64);
    if (g_tfile == NULL) return;
    fprintf(g_tfile, "\n==---- DUMP Type Table ----==\n");
    for (INT i = 1; i <= m_type_tab.get_last_idx(); i++) {
        buf.clean();
        Type * d = m_type_tab.get(i);
        ASSERT0(d);
        fprintf(g_tfile, "%s tyid:%d", dump_type(d, buf), i);
        fprintf(g_tfile, "\n");
        fflush(g_tfile);
    }
    fflush(g_tfile);
}

} //namespace xoc
