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
#ifndef _IR_GVN_H_
#define _IR_GVN_H_

namespace xoc {
class VN;
typedef Vector<VN*> VEC1;
typedef Vector<VEC1*> VEC2;
typedef Vector<VEC2*> VEC3;
typedef Vector<VEC3*> VEC4;

typedef enum _VN_TYPE {
    VN_UNKNOWN = 0,
    VN_OP,
    VN_VAR,
    VN_INT,
    VN_FP,
    VN_STR,
    VN_MC_INT,
} VN_TYPE;


#define VN_id(v)        ((v)->id)
#define VN_type(v)      ((v)->vn_type)
#define VN_int_val(v)   ((v)->u1.iv)
#define VN_fp_val(v)    ((v)->u1.dv)
#define VN_str_val(v)   ((v)->u1.str)
#define VN_op(v)        ((v)->u1.op)
#define VN_is_cst(v)    (VN_type(v) == VN_INT || \
                         VN_type(v) == VN_MC_INT || \
                         VN_type(v) == VN_FP || \
                         VN_type(v) == VN_STR)
class VN {
public:
    UINT id;
    VN_TYPE vn_type; //value type
    union {
        LONGLONG iv;
        double dv;
        SYM const* str;
        IR_TYPE op; //operation
    } u1;

public:
    VN() { clean();    }
    COPY_CONSTRUCTOR(VN);

    void clean()
    {
        id = 0;
        vn_type = VN_UNKNOWN;
        u1.iv = 0;
    }
};


class DOUBLE_HF : public HashFuncBase<double> {
public:
    DOUBLE_HF() {}
    COPY_CONSTRUCTOR(DOUBLE_HF);
    UINT get_hash_value(double d, UINT bucket_size) const
    {
        double * p = &d;
        LONGLONG * pl = (LONGLONG*)p;
        return *pl % bucket_size;
    }
};

typedef HMap<double, VN*, DOUBLE_HF> FP2VN_MAP;
typedef HMap<LONGLONG, VN*> LONGLONG2VN_MAP;
typedef HMap<LONGLONG, VN*> LONGLONGMC2VN_MAP;
typedef HMap<MD const*, VN*> MD2VN_MAP;


//Note: SymbolHashFunc request bucket size must be the power of 2.
class SYM2VN_MAP : public HMap<SYM const*, VN*, SymbolHashFunc> {
public:
    SYM2VN_MAP() : HMap<SYM const*, VN*, SymbolHashFunc>(0) {}
    COPY_CONSTRUCTOR(SYM2VN_MAP);
};


class IR2VN : public Vector<VN*> {
public:
    IR2VN() {}
    COPY_CONSTRUCTOR(IR2VN);
    void set(INT i, VN * elem) { Vector<VN*>::set(i, elem); }
};


//VN Expression of Scalar
class VNE_SC {
public:
    UINT mdid;
    UINT ofst;
    UINT sz;

public:
    VNE_SC(UINT id, UINT o, UINT s)
    {
        mdid = id;
        ofst = o;
        sz = s;
    }
    //COPY_CONSTRUCTOR(VNE_SC);

    bool is_equ(VNE_SC & ve)
    {
        return mdid == ve.mdid &&
               ofst == ve.ofst &&
               sz == ve.sz;
    }

    void copy(VNE_SC & ve) { *this = ve; }
    void clean()
    {
        mdid = 0;
        ofst = 0;
        sz = 0;
    }
};


//VN Expression of ILD
class VNE_ILD {
public:
    UINT base_vn_id;
    UINT ofst;
    UINT sz;

public:
    VNE_ILD(UINT vnid, UINT o, UINT s)
    {
        base_vn_id = vnid;
        ofst = o;
        sz = s;
    }
    //COPY_CONSTRUCTOR(VNE_ILD);

    bool is_equ(VNE_ILD & ve)
    {
        return base_vn_id == ve.base_vn_id &&
               ofst == ve.ofst &&
               sz == ve.sz;
    }

    void copy(VNE_ILD & ve) { *this = ve; }
    void clean()
    {
        base_vn_id = 0;
        ofst = 0;
        sz = 0;
    }
};


//VN Expression of ARRAY
class VNE_ARR : public VNE_ILD {
public:
    UINT ofst_vn_id;

public:
    VNE_ARR(UINT bvnid, UINT ovnid, UINT o, UINT s) : VNE_ILD(bvnid, o, s)
    { ofst_vn_id = ovnid; }
    //COPY_CONSTRUCTOR(VNE_ARR);

    bool is_equ(VNE_ARR & ve)
    { return VNE_ILD::is_equ(ve) && ofst_vn_id == ve.ofst_vn_id; }

    void copy(VNE_ARR & ve) { *this = ve; }
    void clean()
    {
        VNE_ILD::clean();
        ofst_vn_id = 0;
    }
};


class VNE_SC_HF {
public:
    VNE_SC_HF() {}
    COPY_CONSTRUCTOR(VNE_SC_HF);

    UINT get_hash_value(VNE_SC * x, UINT bucket_size) const
    {
        UINT n = (x->mdid << 20) | (x->ofst << 10) | x->sz;
        ASSERT0(isPowerOf2(bucket_size));
        return hash32bit(n) & (bucket_size - 1);
    }

    UINT get_hash_value(OBJTY v, UINT bucket_size) const
    { return get_hash_value((VNE_SC*)v, bucket_size); }

    bool compare(VNE_SC * x1, OBJTY x2) const
    { return x1->is_equ(*(VNE_SC*)x2); }

    bool compare(VNE_SC * x1, VNE_SC * x2) const
    { return x1->is_equ(*x2); }
};


class SCVNE2VN : public HMap<VNE_SC*, VN*, VNE_SC_HF> {
protected:
    SMemPool * m_pool;
    List<VNE_SC*> m_free_lst;

public:
    SCVNE2VN(SMemPool * pool, UINT bsize) :
        HMap<VNE_SC*, VN*, VNE_SC_HF>(bsize)
    {
        ASSERT0(pool);
        m_pool = pool;
    }
    COPY_CONSTRUCTOR(SCVNE2VN);
    virtual ~SCVNE2VN() {}

    virtual VNE_SC * create(OBJTY v)
    {
        VNE_SC * ve = m_free_lst.remove_head();
        if (ve == NULL) {
            ve = (VNE_SC*)smpoolMalloc(sizeof(VNE_SC), m_pool);
        }
        ve->copy(*(VNE_SC*)v);
        return ve;
    }

    virtual void clean()
    {
        INT c;
        for (VNE_SC * ve = get_first(c); ve != NULL; ve = get_next(c)) {
            ve->clean();
            m_free_lst.append_head(ve);
        }
        HMap<VNE_SC*, VN*, VNE_SC_HF>::clean();
    }
};


class VNE_ILD_HF {
public:
    VNE_ILD_HF() {}
    COPY_CONSTRUCTOR(VNE_ILD_HF);

    UINT get_hash_value(VNE_ILD * x, UINT bucket_size) const
    {
        UINT n = (x->base_vn_id << 20) | (x->ofst << 10) | x->sz;
        ASSERT0(isPowerOf2(bucket_size));
        return hash32bit(n) & (bucket_size - 1);
    }

    UINT get_hash_value(OBJTY v, UINT bucket_size) const
    { return get_hash_value((VNE_ILD*)v, bucket_size); }

    bool compare(VNE_ILD * x1, OBJTY x2) const
    { return x1->is_equ(*(VNE_ILD*)x2); }

    bool compare(VNE_ILD * x1, VNE_ILD * x2) const
    { return x1->is_equ(*x2); }
};


class ILD_VNE2VN : public HMap<VNE_ILD*, VN*, VNE_ILD_HF> {
protected:
    SMemPool * m_pool;
    List<VNE_ILD*> m_free_lst;

public:
    ILD_VNE2VN(SMemPool * pool, UINT bsize) :
        HMap<VNE_ILD*, VN*, VNE_ILD_HF>(bsize)
    {
        ASSERT0(pool);
        m_pool = pool;
    }
    COPY_CONSTRUCTOR(ILD_VNE2VN);
    virtual ~ILD_VNE2VN() {}

    virtual VNE_ILD * create(OBJTY v)
    {
        VNE_ILD * ve = m_free_lst.remove_head();
        if (ve == NULL) {
            ve = (VNE_ILD*)smpoolMalloc(sizeof(VNE_ILD), m_pool);
        }
        ve->copy(*(VNE_ILD*)v);
        return ve;
    }

    virtual void clean()
    {
        INT c;
        for (VNE_ILD * ve = get_first(c); ve != NULL; ve = get_next(c)) {
            ve->clean();
            m_free_lst.append_head(ve);
        }
        HMap<VNE_ILD*, VN*, VNE_ILD_HF>::clean();
    }
};


class IR2SCVNE : public TMap<IR const*, SCVNE2VN*> {
public:
    IR2SCVNE() {}
    COPY_CONSTRUCTOR(IR2SCVNE);
    ~IR2SCVNE()
    {
        TMapIter<IR const*, SCVNE2VN*> ii;
        SCVNE2VN * v;
        for (get_first(ii, &v); v != NULL; get_next(ii, &v)) {
            delete v;
        }
    }

    void clean()
    {
        TMapIter<IR const*, SCVNE2VN*> ii;
        SCVNE2VN * v;
        for (get_first(ii, &v); v != NULL; get_next(ii, &v)) {
            v->clean();
        }
    }
};


class IR2ILDVNE : public TMap<IR const*, ILD_VNE2VN*> {
public:
    IR2ILDVNE() {}
    COPY_CONSTRUCTOR(IR2ILDVNE);
    ~IR2ILDVNE()
    {
        TMapIter<IR const*, ILD_VNE2VN*> ii;
        ILD_VNE2VN * vne2vn;
        for (get_first(ii, &vne2vn); vne2vn != NULL; get_next(ii, &vne2vn)) {
            delete vne2vn;
        }
    }

    void clean()
    {
        TMapIter<IR const*, ILD_VNE2VN*> ii;
        ILD_VNE2VN * vne2vn;
        for (get_first(ii, &vne2vn); vne2vn != NULL; get_next(ii, &vne2vn)) {
            vne2vn->clean();
        }
    }
};


class VNE_ARR_HF {
public:
    VNE_ARR_HF() {}
    COPY_CONSTRUCTOR(VNE_ARR_HF);

    UINT get_hash_value(VNE_ARR * x, UINT bucket_size) const
    {
        UINT n = (x->base_vn_id << 24) | (x->ofst_vn_id << 16) |
                 (x->ofst << 8) | x->sz;
        ASSERT0(isPowerOf2(bucket_size));
        return hash32bit(n) & (bucket_size - 1);
    }

    UINT get_hash_value(OBJTY v, UINT bucket_size) const
    { return get_hash_value((VNE_ARR*)v, bucket_size); }

    bool compare(VNE_ARR * x1, OBJTY x2) const
    { return x1->is_equ(*(VNE_ARR*)x2); }

    bool compare(VNE_ARR * x1, VNE_ARR * x2) const
    { return x1->is_equ(*(VNE_ARR*)x2); }
};


class ARR_VNE2VN : public HMap<VNE_ARR*, VN*, VNE_ARR_HF> {
protected:
    SMemPool * m_pool;
    List<VNE_ARR*> m_free_lst;

public:
    ARR_VNE2VN(SMemPool * pool, UINT bsize) :
        HMap<VNE_ARR*, VN*, VNE_ARR_HF>(bsize)
    {
        ASSERT0(pool);
        m_pool = pool;
    }
    COPY_CONSTRUCTOR(ARR_VNE2VN);
    virtual ~ARR_VNE2VN() {}

    VNE_ARR * create(OBJTY v)
    {
        VNE_ARR * ve = m_free_lst.remove_head();
        if (ve == NULL) {
            ve = (VNE_ARR*)smpoolMalloc(sizeof(VNE_ARR), m_pool);
        }
        ve->copy(*(VNE_ARR*)v);
        return ve;
    }

    void clean()
    {
        INT c;
        for (VNE_ARR * ve = get_first(c); ve != NULL; ve = get_next(c)) {
            ve->clean();
            m_free_lst.append_head(ve);
        }
        HMap<VNE_ARR*, VN*, VNE_ARR_HF>::clean();
    }
};


class IR2ARRVNE : public TMap<IR const*, ARR_VNE2VN*> {
public:
    IR2ARRVNE() {}
    COPY_CONSTRUCTOR(IR2ARRVNE);
    virtual ~IR2ARRVNE()
    {
        TMapIter<IR const*, ARR_VNE2VN*> ii;
        ARR_VNE2VN * v;
        for (get_first(ii, &v); v != NULL; get_next(ii, &v)) {
            delete v;
        }
    }

    void clean()
    {
        TMapIter<IR const*, ARR_VNE2VN*> ii;
        ARR_VNE2VN * v;
        for (get_first(ii, &v); v != NULL; get_next(ii, &v)) {
            delete v;
        }
    }
};


class IR2IR : public HMap<IR const*, IR const*,
                          HashFuncBase2<IR const*> > {
public:
    IR2IR() : HMap<IR const*, IR const*, HashFuncBase2<IR const*> >(0) {}
    COPY_CONSTRUCTOR(IR2IR);
};


//Perform Global Value Numbering.
class IR_GVN : public Pass {
protected:
    Region * m_rg;
    IR_DU_MGR * m_du;
    TypeMgr * m_tm;
    MDSystem * m_md_sys;
    IR_CFG * m_cfg;
    VN * m_zero_vn;
    VN * m_mc_zero_vn;
    UINT m_vn_count;
    IR2VN m_ir2vn;
    VEC3 m_irt_vec;
    SMemPool * m_pool;
    LONGLONG2VN_MAP m_ll2vn;
    LONGLONGMC2VN_MAP m_llmc2vn;
    FP2VN_MAP m_fp2vn;
    SYM2VN_MAP m_str2vn;
    List<IR const*> m_tmp;
    List<VN*> m_free_lst;
    List<Vector<VN*>*> m_vec_lst;
    List<Vector<VN*>*> m_vnvec_lst;
    MD2VN_MAP m_md2vn;
    IR2ILDVNE m_def2ildtab;
    IR2ARRVNE m_def2arrtab;
    IR2SCVNE m_def2sctab;
    IR2IR m_stmt2domdef;
    BYTE m_is_vn_fp:1; //true if compute VN for float type.
    BYTE m_is_valid:1;
    BYTE m_is_comp_ild_vn_by_du:1;

    //Set true if we want to compute the vn for LDA(ID(str)).
    //And it's default value is false.
    //NOTE if you are going to enable it, you should ensure
    //RegionMgr::m_is_regard_str_as_same_md is false, because this
    //flag regard all strings as the same one to reduce the MD which
    //generated by different VAR.
    //
    //e.g: LDA(ID("xxx")),  and LDA(ID("yyy")), if
    //RegionMgr::m_is_regard_str_as_same_md is true, ID("xxx") and
    //ID("yyy") will refer to same MD, and the MD is inexact.
    BYTE m_is_comp_lda_string:1;

protected:
    VN * allocLiveinVN(IR const* exp, MD const* emd, bool & change);

    void clean();
    VN * computeScalarByAnonDomDef(IR const* ild,
                                   IR const* domdef,
                                   bool & change);
    VN * computeILoadByAnonDomDef(IR const* ild,
                                  VN const* mlvn,
                                  IR const* domdef,
                                  bool & change);
    VN * computeArrayByAnonDomDef(IR const* arr,
                                  VN const* basevn,
                                  VN const* ofstvn,
                                  IR const* domdef,
                                  bool & change);
    IR const* computeDomDef(IR const* exp, IR const* exp_stmt, SList<IR*> * defs);
    void computeArrayAddrRef(IR const* ir, bool & change);
    VN * computeArray(IR const* exp, bool & change);
    VN * computeScalar(IR const* exp, bool & change);
    VN * computeILoad(IR const* exp, bool & change);
    VN * computeExactMemory(IR const* exp, bool & change);
    VN * computePR(IR const* exp, bool & change);
    VN * computeVN(IR const* exp, bool & change);

    void * xmalloc(UINT size)
    {
        void * p = smpoolMalloc(size, m_pool);
        ASSERT0(p);
        ::memset(p, 0, size);
        return p;
    }

    VN * registerQuadVN(IR_TYPE irt,
                        VN const* v0,
                        VN const* v1,
                        VN const* v2,
                        VN const* v3);
    VN * registerTripleVN(IR_TYPE irt,
                          VN const* v0,
                          VN const* v1,
                          VN const* v2);
    VN * registerBinVN(IR_TYPE irt, VN const* v0, VN const* v1);
    VN * registerUnaVN(IR_TYPE irt, VN const* v0);
    VN * registerVNviaMD(MD const* md);
    VN * registerVNviaMC(LONGLONG v);
    VN * registerVNviaINT(LONGLONG v);
    VN * registerVNviaFP(double v);
    VN * registerVNviaSTR(SYM const* v);

    inline VN * newVN()
    {
        VN * vn = m_free_lst.remove_head();
        if (vn == NULL) {
            vn = (VN*)xmalloc(sizeof(VN));
        } else {
            vn->clean();
        }
        VN_id(vn) = m_vn_count++;
        return vn;
    }

    void dump_h1(IR const* k, VN * x);

    void processBB(IRBB * bb, bool & change);
    void processCall(IR const* ir, bool & change);
    void processRegion(IR const* ir, bool & change);
    void processPhi(IR const* ir, bool & change);

public:
    explicit IR_GVN(Region * rg);
    COPY_CONSTRUCTOR(IR_GVN);
    virtual ~IR_GVN();

    bool calcCondMustVal(IR const* ir, bool & must_true, bool & must_false);
    void dump();
    void dumpBB(UINT bbid);
    void dumpIR2VN();

    virtual CHAR const* getPassName() const
    { return "Global Value Numbering"; }

    PASS_TYPE getPassType() const { return PASS_GVN; }

    bool is_triple(IR_TYPE i) const { return i == IR_ILD; }
    bool is_quad(IR_TYPE i) const { return i == IR_ARRAY; }
    bool is_valid() const { return m_is_valid; }

    VN * mapIR2VN(IR const* ir) { return m_ir2vn.get(ir->id()); }

    void setMapIR2VN(IR const* ir, VN * vn) { m_ir2vn.set(ir->id(), vn); }
    void setComputeVNForFP(bool doit) { m_is_vn_fp = doit; }
    void set_valid(bool valid) { m_is_valid = valid; }
    void setComputeILoadVNviaDU(bool doit) { m_is_comp_ild_vn_by_du = doit; }

    bool reperform(OptCtx & oc);
    bool verify();
    virtual bool perform(OptCtx & oc);
};

} //namespace xoc
#endif
