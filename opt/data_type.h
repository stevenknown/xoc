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
#ifndef __DATA_TYPE_H__
#define __DATA_TYPE_H__

namespace xoc {

class IR;
class Type;

#define UNDEF_TYID        0

#define IS_INT(t)               ((t) >= D_I8 && (t) <= D_U128)
#define IS_SINT(t)              ((t) >= D_I8 && (t) <= D_I128)
#define IS_FP(t)                ((t) >= D_F32 && (t) <= D_F128)
#define IS_BOOL(t)              ((t) == D_B)
#define IS_MC(t)                ((t) == D_MC)
#define IS_VEC(t)               ((t) == D_VEC)
#define IS_PTR(t)               ((t) == D_PTR)
#define IS_SIMPLEX(t)           (IS_INT(t) || IS_FP(t) || IS_BOOL(t) || \
                                 t == D_STR || t == D_VOID)

//Data Type, represented with bit length.
//
//Is unsigned type indispensible?
//
//Java, for example, gets rid of signed/unsigned mixup issues by eliminating
//unsigned types. This goes down a little hard for type convertion, although
//some argue that Java has demonstrated that unsigned types really aren't necessary.
//But unsigned types are useful for implementing memory operation for that span
//more than half of the memory address space, and useful for primitives
//implementing multiprecision arithmetic, etc.
typedef enum _DATA_TYPE {
    D_UNDEF = 0,

    //Note the type between D_B and D_U128 must be integer.
    D_B = 1,      //Boolean
    D_I8 = 2,     //Signed integer 8 bit
    D_I16 = 3,
    D_I32 = 4,
    D_I64 = 5,
    D_I128 = 6,
    D_U8 = 7,     //Unsigned integer 8 bit
    D_U16 = 8,
    D_U32 = 9,
    D_U64 = 10,
    D_U128 = 11,

    D_F32 = 12,   //Float point 32 bit
    D_F64 = 13,   //Float point 64 bit
    D_F80 = 14,   //Float point 96 bit
    D_F128 = 15,  //Float point 128 bit

    //Note all above types are scalar.

    D_MC = 16,    //MemoryChunk, used in structure/union/block type.
    D_STR = 17,   //String
    D_PTR = 18,   //Pointer
    D_VEC = 19,   //Vector
    D_VOID = 20,  //Void
    D_LAST = 21,
    //NOTE: Extend IR::rtype bit length if one extends type value large than 31.
} DATA_TYPE;

class TypeDesc {
public:
    DATA_TYPE dtype;
    CHAR const* name;
    UINT bitsize;
};
#define TYDES_dtype(d)             ((d)->dtype)
#define TYDES_name(d)              ((d)->name)
#define TYDES_bitsize(d)           ((d)->bitsize)


#ifdef _DEBUG_
Type const* checkType(Type const* ty, DATA_TYPE dt);

#define CK_TY(ty, dt)         (checkType(ty, dt))
#else
#define CK_TY(ty, dt)         (ty)
#endif

#define DTNAME(type)          (TYDES_name(&g_type_desc[type]))

//Define target bit size of WORD length.
#define WORD_BITSIZE    GENERAL_REGISTER_SIZE * BIT_PER_BYTE

//Data Type Descriptor.
#define TY_dtype(d)           ((d)->data_type)

//Indicate the pointer base size.
#define TY_ptr_base_size(d)   (((PointerType*)CK_TY(d, D_PTR))->pointer_base_size)

//Indicate the size of memory chunk.
#define TY_mc_size(d)         (((MCType*)CK_TY(d, D_MC))->mc_size)

//Indicate the total byte size of whole vector.
#define TY_vec_size(d)        (((VectorType*)CK_TY(d, D_VEC))->total_vector_size)

//Indicate the vector element size.
#define TY_vec_ety(d)         (((VectorType*)CK_TY(d, D_VEC))->vector_elem_type)

//Date Type Description.
class Type {
public:
    DATA_TYPE data_type;

public:
    Type() { data_type = D_UNDEF; }
    COPY_CONSTRUCTOR(Type);

    //Return true if data type is simplex type.
    bool is_simplex() const { return IS_SIMPLEX(TY_dtype(this)); }

    //Return true if data type is vector.
    bool is_vector() const { return TY_dtype(this) == D_VEC; }

    //Return true if data type is pointer.
    bool is_pointer() const { return TY_dtype(this) == D_PTR; }

    //Return true if data type is string.
    bool is_string() const { return TY_dtype(this) == D_STR; }

    //Return true if data type is memory chunk.
    bool is_mc() const { return TY_dtype(this) == D_MC; }

    //Return true if data type is void.
    bool is_void() const { return TY_dtype(this) == D_VOID; }

    //Return true if data type is boolean.
    bool is_bool() const { return TY_dtype(this) == D_B; }

    //Return true if data type is primitive.
    bool is_scalar() const
    { return TY_dtype(this) >= D_B && TY_dtype(this) <= D_F128; }

    //Return true if tyid is signed.
    inline bool is_signed() const
    {
        if ((TY_dtype(this) >= D_I8 && TY_dtype(this) <= D_I128) ||
            (TY_dtype(this) >= D_F32 && TY_dtype(this) <= D_F128)) {
            return true;
        }
        return false;
    }

    inline bool is_unsigned() const
    {
        if ((TY_dtype(this) >= D_U8 && TY_dtype(this) <= D_U128) ||
            TY_dtype(this) == D_STR ||
            TY_dtype(this) == D_PTR ||
            TY_dtype(this) == D_VEC) {
            return true;
        }
        return false;
    }

    //Return true if ir data type is signed integer.
    bool is_sint() const
    { return TY_dtype(this) >= D_I8 && TY_dtype(this) <= D_I128; }

    //Return true if ir data type is unsgined integer.
    bool is_uint() const
    { return TY_dtype(this) >= D_U8 && TY_dtype(this) <= D_U128; }

    //Return true if ir data type is integer.
    bool is_int() const
    { return TY_dtype(this) >= D_B && TY_dtype(this) <= D_U128; }

    //Return true if ir data type is float.
    bool is_fp() const
    { return TY_dtype(this) >= D_F32 && TY_dtype(this) <= D_F128; }

    //Return true if the type can be used to represent the
    //pointer's addend. e.g:The pointer arith, int * p; p = p + (type)value.
    bool is_ptr_addend() const
    { return !is_fp() && !is_mc() && !is_bool() && !is_pointer(); }

    void copy(Type const& src) { data_type = src.data_type; }
};


class PointerType : public Type {
public:
    //e.g  int * p, base size is 4,
    //long long * p, base size is 8
    //char * p, base size is 1.
    UINT pointer_base_size;

public:
    PointerType() { TY_dtype(this) = D_PTR; pointer_base_size = 0; }
    COPY_CONSTRUCTOR(PointerType);

    void copy(PointerType const& src)
    {
        Type::copy(src);
        pointer_base_size = src.pointer_base_size;
    }
};


class MCType : public Type {
public:
    //Record the BYTE size if 'rtype' is D_MC.
    //NOTE: If ir is pointer, its 'rtype' should NOT be D_MC.
    UINT mc_size;
public:
    MCType() { TY_dtype(this) = D_MC; mc_size = 0; }
    COPY_CONSTRUCTOR(MCType);

    void copy(MCType const& src)
    {
        Type::copy(src);
        mc_size = src.mc_size;
    }
};


class VectorType : public Type {
public:
    //Record the BYTE size of total vector.
    UINT total_vector_size;

    //Record the vector elem size if ir represent a vector.
    DATA_TYPE vector_elem_type;
public:
    VectorType()
    {
        TY_dtype(this) = D_VEC;
        total_vector_size = 0;
        vector_elem_type = D_UNDEF;
    }
    COPY_CONSTRUCTOR(VectorType);

    void copy(VectorType const& src)
    {
        Type::copy(src);
        total_vector_size = src.total_vector_size;
        vector_elem_type = src.vector_elem_type;
    }
};


//Container of Type.
#define TC_type(c)          ((c)->dtd)
#define TC_typeid(c)        ((c)->tyid)
class TypeContainer {
public:
    Type * dtd;
    UINT tyid;
};


class ComareTypeMC {
public:
    bool is_less(Type const* t1, Type const* t2) const
    { return TY_mc_size(t1) < TY_mc_size(t2); }

    bool is_equ(Type const* t1, Type const* t2) const
    { return TY_mc_size(t1) == TY_mc_size(t2); }

    Type const* createKey(Type const* t) { return t; }
};


class MCTab : public TMap<Type const*, TypeContainer const*, ComareTypeMC> {
public:
};


class ComareTypePointer {
public:
    bool is_less(Type const* t1, Type const* t2) const
    { return TY_ptr_base_size(t1) < TY_ptr_base_size(t2); }

    bool is_equ(Type const* t1, Type const* t2) const
    { return TY_ptr_base_size(t1) == TY_ptr_base_size(t2); }

    Type const* createKey(Type const* t) { return t; }
};


class PointerTab : public
    TMap<Type const*, TypeContainer const*, ComareTypePointer> {
public:
};


class ComareTypeVectoElemType {
public:
    bool is_less(Type const* t1, Type const* t2) const
    { return TY_vec_ety(t1) < TY_vec_ety(t2); }

    bool is_equ(Type const* t1, Type const* t2) const
    { return TY_vec_ety(t1) == TY_vec_ety(t2); }

    Type const* createKey(Type const* t) { return t; }
};


class ElemTypeTab : public
    TMap<Type const*, TypeContainer const*, ComareTypeVectoElemType> {
};

typedef TMapIter<Type const*, ElemTypeTab*> ElemTypeTabIter;


class ComareTypeVector {
public:
    bool is_less(Type const* t1, Type const* t2) const
    { return TY_vec_size(t1) < TY_vec_size(t2); }

    bool is_equ(Type const* t1, Type const* t2) const
    { return TY_vec_size(t1) == TY_vec_size(t2); }

    Type const* createKey(Type const* t) { return t; }
};


//MD hashed by MD_ofst.
class VectorTab : public TMap<Type const*, ElemTypeTab*, ComareTypeVector> {
public:
};


extern TypeDesc const g_type_desc[];
class TypeMgr {
    Vector<Type*> m_type_tab;
    SMemPool * m_pool;
    PointerTab m_pointer_type_tab;
    MCTab m_memorychunk_type_tab;
    VectorTab m_vector_type_tab;
    TypeContainer * m_simplex_type[D_LAST];
    UINT m_type_count;
    Type const* m_void;
    Type const* m_b;
    Type const* m_i8;
    Type const* m_i16;
    Type const* m_i32;
    Type const* m_i64;
    Type const* m_i128;
    Type const* m_u8;
    Type const* m_u16;
    Type const* m_u32;
    Type const* m_u64;
    Type const* m_u128;
    Type const* m_f32;
    Type const* m_f64;
    Type const* m_f80;
    Type const* m_f128;
    Type const* m_str;

    void * xmalloc(size_t size)
    {
        void * p = smpoolMalloc(size, m_pool);
        ASSERT0(p);
        ::memset(p, 0, size);
        return p;
    }

    Type * newType() { return (Type*)xmalloc(sizeof(Type)); }

    VectorType * newVectorType()
    { return (VectorType*)xmalloc(sizeof(VectorType)); }

    MCType * newMCType() { return (MCType*)xmalloc(sizeof(MCType)); }

    PointerType * newPointerType()
    { return (PointerType*)xmalloc(sizeof(PointerType)); }

    //Alloc TypeContainer.
    TypeContainer * newTC()
    { return (TypeContainer*)xmalloc(sizeof(TypeContainer)); }
public:
    TypeMgr()
    {
        m_type_tab.clean();
        m_pool = smpoolCreate(sizeof(Type) * 8, MEM_COMM);
        m_type_count = 1;
        ::memset(m_simplex_type, 0, sizeof(m_simplex_type));

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
        m_f32 = getSimplexType(D_F32);
        m_f64 = getSimplexType(D_F64);
        m_f80 = getSimplexType(D_F80);
        m_f128 = getSimplexType(D_F128);
        m_str = getSimplexType(D_STR);
        m_void = getSimplexType(D_VOID);
    }
    COPY_CONSTRUCTOR(TypeMgr);
    ~TypeMgr()
    {
        smpoolDelete(m_pool);
        m_pool = NULL;

        ElemTypeTabIter iter;
        ElemTypeTab * tab;
        for (Type const* d = m_vector_type_tab.get_first(iter, &tab);
             d != NULL; d = m_vector_type_tab.get_next(iter, &tab)) {
            ASSERT0(tab);
            delete tab;
        }
    }

    //Exported Functions.
    TypeContainer const* registerMC(Type const* ty);
    TypeContainer const* registerVector(Type const* ty);
    TypeContainer const* registerPointer(Type const* ty);
    TypeContainer const* registerSimplex(Type const* ty);
    Type * registerType(Type const* dtd);

    CHAR const* dump_type(Type const* dtd, OUT StrBuf & buf);
    void dump_type(Type const* dtd);
    void dump_type(UINT tyid);
    void dump_type_tab();

    DATA_TYPE hoistBSdtype(UINT bit_size, bool is_signed) const;
    DATA_TYPE hoistDtype(UINT bit_size, OUT UINT * hoisted_data_size);
    DATA_TYPE hoistDtype(DATA_TYPE stype) const;

    Type const* hoistDtypeForBinop(IR const* opnd0, IR const* opnd1);

    //Return DATA_TYPE which 'bitsize' corresponding to
    inline DATA_TYPE get_fp_dtype(INT bitsize) const
    {
        switch (bitsize) {
        case 32: return D_F32;
        case 64: return D_F64;
        case 128: return D_F128;
        default:;
        }
        return D_UNDEF;
    }

    //Return DATA-Type according to given byte size.
    DATA_TYPE get_uint_dtype(UINT bytesize) const
    { return get_int_dtype(bytesize * BIT_PER_BYTE, false); }

    //Return DATA-Type according to given bit size and sign.
    //If bitsize is not equal to 1, 8, 16, 32, 64, 128, this
    //function will return D_MC.
    DATA_TYPE get_int_dtype(UINT bitsize, bool is_signed) const
    {
        switch (bitsize) {
        case 1: return D_B;
        case 8: return is_signed ? D_I8 : D_U8;
        case 16: return is_signed ? D_I16 : D_U16;
        case 32: return is_signed ? D_I32 : D_U32;
        case 64: return is_signed ? D_I64 : D_U64;
        case 128: return is_signed ? D_I128 : D_U128;
        default: break;
        }
        return D_MC;
    }

    //Return DATA-Type according to given bit size and sign.
    //Note the bit size will be aligned to power of 2.
    DATA_TYPE get_dtype(UINT bit_size, bool is_signed) const
    { return hoistBSdtype(bit_size, is_signed); }

    //Return bits size of 'dtype' refers to.
    UINT get_dtype_bitsize(DATA_TYPE dtype) const
    {
        ASSERT(dtype != D_MC, ("this is memory chunk"));
        return TYDES_bitsize(&g_type_desc[dtype]);
    }

    //Return bits size of 'dtype' refers to.
    CHAR const* get_dtype_name(DATA_TYPE dtype) const
    {
        ASSERT0(dtype < D_LAST);
        return TYDES_name(&g_type_desc[dtype]);
    }

    //Return byte size of a pointer.
    //e.g: 32bit processor return 4, 64bit processor return 8.
    UINT get_pointer_bytesize() const { return BYTE_PER_POINTER; }

    //Return DATA-Type that has identical byte-size to pointer.
    //e.g: 32bit processor return U4, 64bit processor return U8.
    DATA_TYPE getPointerSizeDtype() const
    { return get_int_dtype(BYTE_PER_POINTER * BIT_PER_BYTE, false); }

    //Return the byte size of pointer's base.
    UINT getPointerBaseByteSize(Type const* type)
    {
        ASSERT0(type->is_pointer());
        return TY_ptr_base_size(type);
    }

    //Return bytes size of 'dtype' refer to.
    UINT get_dtype_bytesize(DATA_TYPE dtype) const
    {
        ASSERT0(dtype != D_UNDEF);
        UINT bitsize = get_dtype_bitsize(dtype);
        return bitsize < BIT_PER_BYTE ?
                (UINT)1 : (UINT)xceiling((INT)bitsize, BIT_PER_BYTE);
    }

    //Retrieve Type via 'type-index'.
    Type const* get_type(UINT tyid) const
    {
        ASSERT0(tyid != 0);
        ASSERT0(m_type_tab.get(tyid));
        return m_type_tab.get(tyid);
    }

    Type const* getBool() const { return m_b; }
    Type const* getI8() const { return m_i8; }
    Type const* getI16() const { return m_i16; }
    Type const* getI32() const { return m_i32; }
    Type const* getI64() const { return m_i64; }
    Type const* getI128() const { return m_i128; }
    Type const* getU8() const { return m_u8; }
    Type const* getU16() const { return m_u16; }
    Type const* getU32() const { return m_u32; }
    Type const* getU64() const { return m_u64; }
    Type const* getU128() const { return m_u128; }
    Type const* getF32() const { return m_f32; }
    Type const* getF64() const { return m_f64; }
    Type const* getF80() const { return m_f80; }
    Type const* getF128() const { return m_f128; }
    Type const* getString() const { return m_str; }
    Type const* getVoid() const { return m_void; }

    //Return tyid accroding to DATA_TYPE.
    Type const* getSimplexType(DATA_TYPE dt)
    {
        ASSERT0(IS_SIMPLEX(dt));
        Type d;
        TY_dtype(&d) = dt;
        return registerType(&d);
    }

    //Return tyid accroding to DATA_TYPE.
    inline Type const* getSimplexTypeEx(DATA_TYPE dt) const
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
        case D_F32: return m_f32;
        case D_F64: return m_f64;
        case D_F80: return m_f80;
        case D_F128: return m_f128;
        case D_STR: return m_str;
        case D_VOID: return m_void;
        default: ASSERT(0, ("not simplex type")); break;
        }
        return 0;
    }

    //Return vector type, and vector total size = <vec_elem_num x vec_elem_ty>.
    //e.g: int<16 x D_I32> means there are 16 elems in vector, each elem is
    //D_I32 type, and vector total size is 64 bytes.
    Type const* getVectorType(UINT vec_elem_num, DATA_TYPE vec_elem_ty)
    {
        ASSERT0(vec_elem_num != 0 && vec_elem_ty != D_UNDEF);
        VectorType d;
        TY_dtype(&d) = D_VEC;
        TY_vec_size(&d) = vec_elem_num * get_dtype_bytesize(vec_elem_ty);
        TY_vec_ety(&d) = vec_elem_ty;
        return TC_type(registerVector(&d));
    }

    //Register and return pointer type tyid accroding to pointer-base-size.
    Type const* getPointerType(UINT pt_base_sz)
    {
        //Pointer base size can be zero.
        //Note if pointer base size is 0, that means the pointer can not
        //do any arthimetic, because pointer arithmetic may use pointer
        //base size as addend.
        //ASSERT0(pt_base_sz != 0);
        PointerType d;
        TY_dtype(&d) = D_PTR;
        TY_ptr_base_size(&d) = pt_base_sz;
        return TC_type(registerPointer(&d));
    }

    //Return memory chunk tyid accroding to chunk size and vector element tyid.
    Type const* getMCType(UINT mc_sz)
    {
        MCType d;
        TY_dtype(&d) = D_MC;
        TY_mc_size(&d) = mc_sz;
        return TC_type(registerMC(&d));
    }

    //Return byte size according to given Type.
    UINT get_bytesize(Type const* dtd) const;

    //Return byte size according to given tyid.
    UINT get_bytesize(UINT tyid) const { return get_bytesize(get_type(tyid)); }

    bool is_scalar(UINT tyid) { return tyid >= D_B && tyid <= D_F128; }

    //Return true if tyid is signed.
    bool is_signed(UINT tyid) const { return get_type(tyid)->is_signed(); }

    //Return true if tyid is signed integer.
    bool is_sint(UINT tyid) const { return get_type(tyid)->is_sint(); }

    //Return true if tyid is unsigned integer.
    bool is_uint(UINT tyid) const { return get_type(tyid)->is_uint(); }

    //Return true if tyid is Float point.
    bool is_fp(UINT tyid) const { return get_type(tyid)->is_fp(); }

    //Return true if tyid is Boolean.
    bool is_bool(UINT tyid) const { return m_b == get_type(tyid); }

    //Return true if tyid is String.
    bool is_str(UINT tyid) const { return m_str == get_type(tyid); }

    //Return true if tyid is Memory chunk.
    bool is_mc(UINT tyid) const { return get_type(tyid)->is_mc(); }

    //Return true if tyid is Pointer.
    bool is_ptr(UINT tyid) const { return get_type(tyid)->is_pointer(); }

    //Return true if tyid is Void.
    bool is_void(UINT tyid) const { return get_type(tyid)->is_void(); }

    //Return true if data type is Vector.
    bool is_vec(UINT tyid) const { return get_type(tyid)->is_vector(); }
};

} //namespace xoc
#endif
