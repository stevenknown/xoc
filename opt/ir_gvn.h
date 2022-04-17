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

typedef xcom::TMapIter<UINT, VN*> Tab1Iter;

class Tab1 : public xcom::TMap<UINT, VN*> {
};

typedef xcom::TMapIter<UINT, Tab1*> Tab2Iter;

class Tab2 : public xcom::TMap<UINT, Tab1*> {
};

typedef xcom::TMapIter<UINT, Tab2*> Tab3Iter;

class Tab3 : public xcom::TMap<UINT, Tab2*> {
};

typedef xcom::TMapIter<UINT, Tab3*> Tab4Iter;

class Tab4 : public xcom::TMap<UINT, Tab3*> {
};

typedef enum _VN_TYPE {
    VN_UNKNOWN = 0,
    VN_OP,
    VN_VAR,
    VN_INT,
    VN_FP,
    VN_STR,
    VN_MC_INT,
    VN_NUM,
} VN_TYPE;


#define VN_id(v) ((v)->m_id)
#define VN_type(v) ((v)->m_vn_type)
#define VN_int_val(v) ((v)->u1.iv)
#define VN_fp_val(v) ((v)->u1.dv)
#define VN_str_val(v) ((v)->u1.str)
#define VN_op(v) ((v)->u1.op)
#define VN_is_cst(v) (VN_type(v) == VN_INT || VN_type(v) == VN_MC_INT || \
                      VN_type(v) == VN_FP || VN_type(v) == VN_STR)
class VN {
    COPY_CONSTRUCTOR(VN);
public:
    UINT m_id;
    VN_TYPE m_vn_type; //value type
    union {
        LONGLONG iv;
        double dv;
        Sym const* str;
        IR_TYPE op; //operation
    } u1;

public:
    VN() { clean(); }

    void clean()
    {
        m_id = 0;
        m_vn_type = VN_UNKNOWN;
        u1.iv = 0;
    }

    UINT id() const { return VN_id(this); }

    VN_TYPE getType() const { return VN_type(this); }
};


class DoubleHashFunc : public HashFuncBase<double> {
    COPY_CONSTRUCTOR(DoubleHashFunc);
public:
    DoubleHashFunc() {}
    UINT get_hash_value(double d, UINT bucket_size) const
    {
        double * p = &d;
        LONGLONG * pl = (LONGLONG*)p;
        return *pl % bucket_size;
    }
};

typedef HMap<double, VN*, DoubleHashFunc> FP2VN_MAP;
typedef HMap<LONGLONG, VN*> LONGLONG2VN_MAP;
typedef HMap<LONGLONG, VN*> LONGLONGMC2VN_MAP;
typedef HMap<MD const*, VN*> MD2VN_MAP;

//Note: SymbolHashFunc request bucket size must be the power of 2.
class SYM2VN_MAP : public HMap<Sym const*, VN*, SymbolHashFunc> {
    COPY_CONSTRUCTOR(SYM2VN_MAP);
public:
    SYM2VN_MAP() : HMap<Sym const*, VN*, SymbolHashFunc>(0) {}
};


typedef TMapIter<UINT, VN*> IR2VNIter;

class IR2VN : public TMap<UINT, VN*> {
    COPY_CONSTRUCTOR(IR2VN);
public:
    IR2VN() {}
};


//VN Expression of Scalar
class VNE_SC {
    //COPY_CONSTRUCTOR(VNE_SC);
public:
    UINT mdid;
    TMWORD ofst;
    UINT sz;

public:
    VNE_SC(UINT id, TMWORD o, UINT s)
    {
        mdid = id;
        ofst = o;
        sz = s;
    }

    bool is_equ(VNE_SC const& ve) const
    { return mdid == ve.mdid && ofst == ve.ofst && sz == ve.sz; }

    void copy(VNE_SC const& ve) { *this = ve; }
    void clean() { mdid = 0; ofst = 0; sz = 0; }
};


//VN Expression of ILD
class VNE_ILD {
public:
    UINT base_vn_id;
    TMWORD ofst;
    UINT sz;

public:
    VNE_ILD(UINT vnid, TMWORD o, UINT s)
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
    VNE_ARR(UINT bvnid, UINT ovnid, TMWORD o, UINT s) : VNE_ILD(bvnid, o, s)
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


//VN Expression of Hash Function
class VNE_SC_HF {
    COPY_CONSTRUCTOR(VNE_SC_HF);
public:
    VNE_SC_HF() {}

    UINT get_hash_value(VNE_SC * x, UINT bucket_size) const
    {
        HOST_UINT n = (x->mdid << 20) | (x->ofst << 10) | x->sz;
        ASSERT0(xcom::isPowerOf2(bucket_size));
        return xcom::hash32bit((UINT32)n) & (bucket_size - 1);
    }

    UINT get_hash_value(OBJTY v, UINT bucket_size) const
    { return get_hash_value((VNE_SC*)v, bucket_size); }

    bool compare(VNE_SC * x1, OBJTY x2) const
    { return x1->is_equ(*(VNE_SC*)x2); }

    bool compare(VNE_SC * x1, VNE_SC * x2) const
    { return x1->is_equ(*x2); }
};


class SCVNE2VN : public HMap<VNE_SC*, VN*, VNE_SC_HF> {
    COPY_CONSTRUCTOR(SCVNE2VN);
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
    virtual ~SCVNE2VN() {}

    virtual VNE_SC * create(OBJTY v)
    {
        VNE_SC * ve = m_free_lst.remove_head();
        if (ve == nullptr) {
            ve = (VNE_SC*)smpoolMalloc(sizeof(VNE_SC), m_pool);
        }
        ve->copy(*(VNE_SC*)v);
        return ve;
    }

    virtual void clean()
    {
        VecIdx c;
        for (VNE_SC * ve = get_first(c); ve != nullptr; ve = get_next(c)) {
            ve->clean();
            m_free_lst.append_head(ve);
        }
        HMap<VNE_SC*, VN*, VNE_SC_HF>::clean();
    }
};


//VN Expression of Hash Function
class VNE_ILD_HF {
    COPY_CONSTRUCTOR(VNE_ILD_HF);
public:
    VNE_ILD_HF() {}

    UINT get_hash_value(VNE_ILD * x, UINT bucket_size) const
    {
        HOST_UINT n = (x->base_vn_id << 20) | (x->ofst << 10) | x->sz;
        ASSERT0(xcom::isPowerOf2(bucket_size));
        return xcom::hash32bit((UINT32)n) & (bucket_size - 1);
    }

    UINT get_hash_value(OBJTY v, UINT bucket_size) const
    { return get_hash_value((VNE_ILD*)v, bucket_size); }

    bool compare(VNE_ILD * x1, OBJTY x2) const
    { return x1->is_equ(*(VNE_ILD*)x2); }

    bool compare(VNE_ILD * x1, VNE_ILD * x2) const
    { return x1->is_equ(*x2); }
};


class ILD_VNE2VN : public HMap<VNE_ILD*, VN*, VNE_ILD_HF> {
    COPY_CONSTRUCTOR(ILD_VNE2VN);
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
    virtual ~ILD_VNE2VN()
    {
        //All elements are allocated in external mempool. Nothing to do.
    }

    virtual VNE_ILD * create(OBJTY v)
    {
        VNE_ILD * ve = m_free_lst.remove_head();
        if (ve == nullptr) {
            ve = (VNE_ILD*)smpoolMalloc(sizeof(VNE_ILD), m_pool);
        }
        ve->copy(*(VNE_ILD*)v);
        return ve;
    }

    virtual void clean()
    {
        VecIdx c;
        for (VNE_ILD * ve = get_first(c); ve != nullptr; ve = get_next(c)) {
            ve->clean();
            m_free_lst.append_head(ve);
        }
        HMap<VNE_ILD*, VN*, VNE_ILD_HF>::clean();
    }
};


class IR2SCVNE : public TMap<IR const*, SCVNE2VN*> {
    COPY_CONSTRUCTOR(IR2SCVNE);
public:
    IR2SCVNE() {}
    ~IR2SCVNE() { destroy(); }

    void destroy()
    {
        TMapIter<IR const*, SCVNE2VN*> ii;
        SCVNE2VN * v;
        for (get_first(ii, &v); v != nullptr; get_next(ii, &v)) {
            delete v;
        }
        TMap<IR const*, SCVNE2VN*>::destroy();
    }

    void clean()
    {
        //TMapIter<IR const*, SCVNE2VN*> ii;
        //SCVNE2VN * v;
        //for (get_first(ii, &v); v != nullptr; get_next(ii, &v)) {
        //    v->clean();
        //}
        //In order to free memory as soon as possible, replace clean with
        //destroy.
        destroy();
        TMap<IR const*, SCVNE2VN*>::init();
    }
};


class IR2ILDVNE : public TMap<IR const*, ILD_VNE2VN*> {
    COPY_CONSTRUCTOR(IR2ILDVNE);
public:
    IR2ILDVNE() {}
    ~IR2ILDVNE() { destroy(); }

    void destroy()
    {
        TMapIter<IR const*, ILD_VNE2VN*> ii;
        ILD_VNE2VN * vne2vn;
        for (get_first(ii, &vne2vn); vne2vn != nullptr; get_next(ii, &vne2vn)) {
            delete vne2vn;
        }
        TMap<IR const*, ILD_VNE2VN*>::destroy();
    }

    void clean()
    {
        //TMapIter<IR const*, ILD_VNE2VN*> ii;
        //ILD_VNE2VN * vne2vn;
        //for (get_first(ii, &vne2vn); vne2vn != nullptr;
        //     get_next(ii, &vne2vn)) {
        //    vne2vn->clean();
        //}

        //In order to free memory as soon as possible, replace clean with
        //destroy.
        destroy();
        TMap<IR const*, ILD_VNE2VN*>::init();
    }
};


class VNE_ARR_HF {
    COPY_CONSTRUCTOR(VNE_ARR_HF);
public:
    VNE_ARR_HF() {}

    UINT get_hash_value(VNE_ARR * x, UINT bucket_size) const
    {
        HOST_UINT n = (x->base_vn_id << 24) | (x->ofst_vn_id << 16) |
                      (x->ofst << 8) | x->sz;
        ASSERT0(xcom::isPowerOf2(bucket_size));
        return xcom::hash32bit((UINT32)n) & (bucket_size - 1);
    }

    UINT get_hash_value(OBJTY v, UINT bucket_size) const
    { return get_hash_value((VNE_ARR*)v, bucket_size); }

    bool compare(VNE_ARR * x1, OBJTY x2) const
    { return x1->is_equ(*(VNE_ARR*)x2); }

    bool compare(VNE_ARR * x1, VNE_ARR * x2) const
    { return x1->is_equ(*(VNE_ARR*)x2); }
};


class ARR_VNE2VN : public HMap<VNE_ARR*, VN*, VNE_ARR_HF> {
    COPY_CONSTRUCTOR(ARR_VNE2VN);
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
    virtual ~ARR_VNE2VN() {}

    VNE_ARR * create(OBJTY v)
    {
        VNE_ARR * ve = m_free_lst.remove_head();
        if (ve == nullptr) {
            ve = (VNE_ARR*)smpoolMalloc(sizeof(VNE_ARR), m_pool);
        }
        ve->copy(*(VNE_ARR*)v);
        return ve;
    }

    void clean()
    {
        VecIdx c;
        for (VNE_ARR * ve = get_first(c); ve != nullptr; ve = get_next(c)) {
            ve->clean();
            m_free_lst.append_head(ve);
        }
        HMap<VNE_ARR*, VN*, VNE_ARR_HF>::clean();
    }
};


class IR2ARRVNE : public TMap<IR const*, ARR_VNE2VN*> {
    COPY_CONSTRUCTOR(IR2ARRVNE);
public:
    IR2ARRVNE() {}
    virtual ~IR2ARRVNE() { clean(); }

    void clean()
    {
        TMapIter<IR const*, ARR_VNE2VN*> ii;
        ARR_VNE2VN * v;
        for (get_first(ii, &v); v != nullptr; get_next(ii, &v)) {
            delete v;
        }
        TMap<IR const*, ARR_VNE2VN*>::destroy();
        TMap<IR const*, ARR_VNE2VN*>::init();
    }
};


class IR2IR : public HMap<IR const*, IR const*, HashFuncBase2<IR const*> > {
    COPY_CONSTRUCTOR(IR2IR);
public:
    IR2IR() : HMap<IR const*, IR const*, HashFuncBase2<IR const*> >(0) {}
};


//Perform Global Value Numbering.
class GVN : public Pass {
    COPY_CONSTRUCTOR(GVN);
protected:
    BYTE m_is_vn_fp:1; //true if compute VN for float type.

    //Set true if we want to compute the vn for LDA(ID(str)).
    //And it's default value is false.
    //NOTE if you are going to enable it, you should ensure
    //RegionMgr::m_is_regard_str_as_same_md is false, because this
    //flag regard all strings as the same one to reduce the MD which
    //generated by different Var.
    //
    //e.g: LDA(ID("xxx")),  and LDA(ID("yyy")), if
    //RegionMgr::m_is_regard_str_as_same_md is true, ID("xxx") and
    //ID("yyy") will refer to same MD, and the MD is inexact.
    BYTE m_is_comp_lda_string:1;

    //Set true if we intend to allocate VN for livein MD, the livein MD may be
    //global variable or parameter. Note each livein MD will be assigned
    //different VN, that may misjudge the equivalence of two MemRef
    //e.g: given ld a, ld b, which VN is VN1, VN2 respectively. We could say
    //the value of a is not equal to b if VN1 != VN2. However, they may equal
    //if a and b are parameter.
    BYTE m_is_alloc_livein_vn:1;

    DUMgr * m_du;
    TypeMgr * m_tm;
    MDSystem * m_md_sys;
    PRSSAMgr * m_prssamgr;
    MDSSAMgr * m_mdssamgr;
    IRCFG * m_cfg;
    VN * m_zero_vn;
    VN * m_mc_zero_vn;
    SMemPool * m_pool;
    UINT m_vn_count;
    IR2VN m_ir2vn;
    Vector<void*> m_irt_vec;
    LONGLONG2VN_MAP m_ll2vn;
    LONGLONGMC2VN_MAP m_llmc2vn;
    FP2VN_MAP m_fp2vn;
    SYM2VN_MAP m_str2vn;
    List<IR const*> m_tmp;
    List<VN*> m_free_lst;
    List<Tab1*> m_tab_lst;
    MD2VN_MAP m_md2vn;
    IR2ILDVNE m_def2ildtab;
    IR2ARRVNE m_def2arrtab;
    IR2SCVNE m_def2sctab;
    IR2IR m_stmt2domdef;
protected:
    VN * allocLiveinVN(IR const* exp, MD const* emd, bool & change);

    void cleanIR2VN();
    void clean();
    VN * computeSelect(IR const* exp, bool & change);
    VN * computeBin(IR const* exp, bool & change);
    VN * computeConst(IR const* exp, bool & change);
    VN * computeLda(IR const* exp, bool & change);
    VN * computeUna(IR const* exp, bool & change);
    VN * computeScalarByAnonDomDef(IR const* ild, IR const* domdef,
                                   bool & change);
    VN * computeILoadByAnonDomDef(IR const* ild, VN const* mlvn,
                                  IR const* domdef, bool & change);
    VN * computeArrayByAnonDomDef(IR const* arr, VN const* basevn,
                                  VN const* ofstvn, IR const* domdef,
                                  bool & change);
    IR const* computeDomDef(IR const* exp, IR const* exp_stmt,
                            SList<IR*> * defs);
    void computeArrayAddrRef(IR const* ir, bool & change);
    VN * computeArray(IR const* exp, bool & change);
    VN * computeScalar(IR const* exp, bool & change);
    VN * computeILoad(IR const* exp, bool & change);
    VN * computeExactMemory(IR const* exp, bool & change);
    VN * computePR(IR const* exp, bool & change);    

    void dumpBB(UINT bbid) const;
    void dumpIR2VN() const;
    void dumpVNHash() const;
    void dump_h1(IR const* k, VN const* x) const;
    void destroyLocalUsed();

    virtual bool isUnary(IR_TYPE irt) const;
    virtual bool isBinary(IR_TYPE irt) const;
    virtual bool isTriple(IR_TYPE irt) const;
    virtual bool isQuad(IR_TYPE irt) const;

    void * xmalloc(UINT size)
    {
        void * p = smpoolMalloc(size, m_pool);
        ASSERT0(p);
        ::memset(p, 0, size);
        return p;
    }

    VN * registerQuadVN(IR_TYPE irt, VN const* v0, VN const* v1,
                        VN const* v2, VN const* v3);
    VN * registerTripleVN(IR_TYPE irt, VN const* v0, VN const* v1,
                          VN const* v2);
    VN * registerBinVN(IR_TYPE irt, VN const* v0, VN const* v1);
    VN * registerUnaVN(IR_TYPE irt, VN const* v0);
    VN * registerVNviaMD(MD const* md);
    VN * registerVNviaMC(LONGLONG v);
    VN * registerVNviaINT(LONGLONG v);
    VN * registerVNviaFP(double v);
    VN * registerVNviaSTR(Sym const* v);

    inline VN * newVN()
    {
        VN * vn = m_free_lst.remove_head();
        if (vn == nullptr) {
            vn = (VN*)xmalloc(sizeof(VN));
        } else {
            vn->clean();
        }
        VN_id(vn) = m_vn_count++;
        return vn;
    }

    void processGETELEM(IR * ir, bool & change);
    void processSETELEM(IR * ir, bool & change);
    void processBB(IRBB * bb, bool & change);
    void processCall(IR const* ir, bool & change);
    void processRegion(IR const* ir, bool & change);
    void processPhi(IR const* ir, bool & change);
    void processIST(IR * ir, bool & change);
    void processSTARRAY(IR * ir, bool & change);
    void processSTandSTPR(IR * ir, bool & change);

    bool useMDSSADU() const
    { return m_mdssamgr != nullptr && m_mdssamgr->is_valid(); }
    bool usePRSSADU() const
    { return m_prssamgr != nullptr && m_prssamgr->is_valid(); }
public:
    explicit GVN(Region * rg);
    virtual ~GVN();

    void init();
    void destroy();

    //Return true if GVN is able to determine the result of 'ir', otherwise
    //return false that GVN know nothing about ir.
    bool calcCondMustVal(IR const* ir, bool & must_true,
                         bool & must_false) const;
    void cleanIRTreeVN(IR const* ir);

    //Compute VN to given exp.
    //change: set to true if new VN generated.
    VN * computeVN(IR const* exp, bool & change);

    //The function dump pass relative information before performing the pass.
    //The dump information is always used to detect what the pass did.
    //Return true if dump successed, otherwise false.
    virtual bool dumpBeforePass() const { return Pass::dumpBeforePass(); }

    //The function dump pass relative information.
    //The dump information is always used to detect what the pass did.
    //Return true if dump successed, otherwise false.
    virtual bool dump() const;

    virtual CHAR const* getPassName() const { return "Global Value Numbering"; }
    PASS_TYPE getPassType() const { return PASS_GVN; }

    //Return true if ir1 and ir2 represent identical memory location.
    //Note this function does NOT consider data type
    //that ir1 or ir2 referrenced.
    bool isSameMemLoc(IR const* ir1, IR const* ir2) const;
    //Return true if ir1 and ir2 represent different memory location, otherwise
    //return false to tell caller we do not know more about these object.
    //Note this function will consider data type that ir1 or ir2 referrenced.
    bool isDiffMemLoc(IR const* ir1, IR const* ir2) const
    { return ir1->isDiffMemLoc(ir2); }

    //Return true if GVN will alloc different VN for livein variable.
    bool isAllocLiveinVN() const { return m_is_alloc_livein_vn; }

    //Get VN of ir.
    VN * getVN(IR const* ir) const { return m_ir2vn.get(ir->id()); }

    //Get VN of ir. Readonly function.
    VN const* getConstVN(IR const* ir) const { return m_ir2vn.get(ir->id()); }

    //Set true if you intend to alloc different VN for livein variable.
    void setAllocLiveinVN(bool doit) { m_is_alloc_livein_vn = doit; }

    //Set VN to ir.
    void setVN(IR const* ir, VN * vn) { m_ir2vn.setAlways(ir->id(), vn); }

    //Update VN to ir if ir has been mapped.
    void setVNIfFind(IR const* ir, VN * vn) { m_ir2vn.setIfFind(ir->id(), vn); }

    //Ask GVN to compute VN for IR with FP type.
    void setComputeVNForFP(bool doit) { m_is_vn_fp = doit; }

    virtual bool perform(OptCtx & oc);
};

} //namespace xoc
#endif
