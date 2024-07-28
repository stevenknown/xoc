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

//Note the key-value pair type of Tab1, Tab2, ... TabN must be <UINT, pointer>.
typedef xcom::TMapIter<UINT, VN*> Tab1Iter;
class Tab1 : public xcom::TMap<UINT, VN*> {
};

//Note the key-value pair type of Tab1, Tab2, ... TabN must be <UINT, pointer>.
typedef xcom::TMapIter<UINT, Tab1*> Tab2Iter;
class Tab2 : public xcom::TMap<UINT, Tab1*> {
};

//Note the key-value pair type of Tab1, Tab2, ... TabN must be <UINT, pointer>.
typedef xcom::TMapIter<UINT, Tab2*> Tab3Iter;
class Tab3 : public xcom::TMap<UINT, Tab2*> {
};

//Note the key-value pair type of Tab1, Tab2, ... TabN must be <UINT, pointer>.
typedef xcom::TMapIter<UINT, Tab3*> Tab4Iter;
class Tab4 : public xcom::TMap<UINT, Tab3*> {
};

typedef enum _VN_TYPE {
    VN_UNKNOWN = 0,
    VN_OP, //The numbered value reasoned from IR stmt or expression.
    VN_MDDEF, //The numbered value reasoned from MDDef.
    VN_VMD, //The numbered value reasoned from VMD.
    VN_VAR, //The numbered value reasoned from Var or MD.
    VN_INT, //The numbered value reasoned from Integer.
    VN_FP, //The numbered value reasoned from Float-Point.
    VN_STR, //The numbered value reasoned from String.
    VN_MC_INT, //The numbered value reasoned from MC typed value.

    //The numbered value reasoned from IR, Var, MD or constant which is unique.
    //When we compare two VNs which both are CONST, they are absolutely not
    //equal.
    VN_CONST,
    VN_NUM,
} VN_TYPE;


#define VN_id(v) ((v)->m_id)
#define VN_type(v) ((v)->m_vn_type)
#define VN_int_val(v) ((v)->u1.iv)
#define VN_fp_val(v) ((v)->u1.dv)
#define VN_str_val(v) ((v)->u1.str)
#define VN_op(v) ((v)->u1.op)
#define VN_mddef(v) ((v)->u1.mddef)
#define VN_vmd(v) ((v)->u1.vmd)
#define VN_is_cst(v) (VN_type(v) == VN_INT || VN_type(v) == VN_MC_INT || \
                      VN_type(v) == VN_FP || VN_type(v) == VN_STR || \
                      VN_type(v) == VN_CONST)
class VN {
    COPY_CONSTRUCTOR(VN);
public:
    UINT m_id;
    VN_TYPE m_vn_type; //value type
    union {
        HOST_INT iv;
        HOST_FP dv;
        Sym const* str;
        MDDef const* mddef;
        VMD const* vmd;
        IR_CODE op; //operation
    } u1;
public:
    VN() { clean(); }

    void clean()
    {
        m_id = 0;
        m_vn_type = VN_UNKNOWN;
        u1.iv = 0;
    }

    void dump(Region const* rg) const;

    UINT id() const { return VN_id(this); }

    //Return true if VN reasoned from constant and unique value.
    bool is_const() const { return VN_is_cst(this); }

    //Return true if VN reasoned from integer value.
    bool is_int() const
    { return VN_type(this) == VN_INT || VN_type(this) == VN_MC_INT; }

    //Return true if VN reasoned from float-point value.
    bool is_fp() const { return VN_type(this) == VN_FP; }

    //Return true if VN reasoned from string value.
    bool is_str() const { return VN_type(this) == VN_STR; }

    //Return true if VN reasoned from MDDef.
    bool is_mddef() const { return VN_type(this) == VN_MDDEF; }

    //Return true if VN reasoned from VMD.
    bool is_vmd() const { return VN_type(this) == VN_VMD; }

    //Return true if VN is undetermined.
    bool is_unknown() const { return VN_type(this) == VN_UNKNOWN; }

    //Get the value-number type.
    VN_TYPE getType() const { return VN_type(this); }

    //Get the value-number type.
    HOST_INT getVNIntVal() const { return VN_int_val(this); }

    //Get the value of VN of misc type.
    HOST_FP getVNFPVal() const { return VN_fp_val(this); }
    Sym const* getVNStrVal() const { return VN_str_val(this); }
    MDDef const* getVNMDDef() const { return VN_mddef(this); }
    VMD const* getVNVMD() const { return VN_vmd(this); }
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

typedef xcom::VecIdx FP2VNIter;
typedef xcom::HMap<double, VN*, DoubleHashFunc> FP2VN;

typedef VecIdx Longlong2VNIter;
typedef xcom::HMap<LONGLONG, VN*> Longlong2VN;

typedef VecIdx LonglongMC2VNIter;
typedef xcom::HMap<LONGLONG, VN*> LonglongMC2VN;

typedef VecIdx MD2VNIter;
typedef xcom::HMap<MD const*, VN*> MD2VN;

//Note: SymbolHashFunc request bucket size must be the power of 2.
typedef VecIdx Sym2VNIter;
class Sym2VN : public xcom::HMap<Sym const*, VN*, SymbolHashFunc> {
    COPY_CONSTRUCTOR(Sym2VN);
public:
    Sym2VN() : HMap<Sym const*, VN*, SymbolHashFunc>(0) {}
};

typedef TMapIter<UINT, VN const*> IR2VNIter;
class IR2VN : public xcom::TMap<UINT, VN const*> {
    COPY_CONSTRUCTOR(IR2VN);
public:
    IR2VN() {}
};

typedef TMapIter<UINT, VN const*> MDPhi2VNIter;
class MDPhi2VN : public xcom::TMap<UINT, VN const*> {
    COPY_CONSTRUCTOR(MDPhi2VN);
public:
    MDPhi2VN() {}
};

typedef TMapIter<UINT, VN const*> VMD2VNIter;
class VMD2VN : public xcom::TMap<UINT, VN const*> {
    COPY_CONSTRUCTOR(VMD2VN);
public:
    VMD2VN() {}
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


class IR2ILDVNE : public xcom::TMap<IR const*, ILD_VNE2VN*> {
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

    bool compare(VNE_ARR * x1, OBJTY x2) const
    { return x1->is_equ(*(VNE_ARR*)x2); }

    bool compare(VNE_ARR * x1, VNE_ARR * x2) const
    { return x1->is_equ(*(VNE_ARR*)x2); }

    UINT get_hash_value(VNE_ARR * x, UINT bucket_size) const
    {
        HOST_UINT n = (x->base_vn_id << 24) | (x->ofst_vn_id << 16) |
                      (x->ofst << 8) | x->sz;
        ASSERT0(xcom::isPowerOf2(bucket_size));
        return xcom::hash32bit((UINT32)n) & (bucket_size - 1);
    }

    UINT get_hash_value(OBJTY v, UINT bucket_size) const
    { return get_hash_value((VNE_ARR*)v, bucket_size); }
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


template <typename IntType>
class IntSet2VN : public IntSetMap<IntType, VN*> {
public:
    //The target dependent code to dump the content of user defined
    //MappedObject.
    virtual void dumpMappedObj(FILE * h, UINT indent, VN * const& mapped) const
    {
        if (mapped == nullptr) { return; }
        xcom::log(h, 0, ":VN%u", mapped->id());
    }
};


class VNHashTab {
public:
    typedef HOST_UINT IntType;
    typedef xcom::List<IntType> IntList;
    typedef xcom::List<IntType>::Iter IntListIter;
    typedef xcom::List<VN const*> VNList;
    typedef xcom::List<VN const*>::Iter VNListIter;
protected:
    GVN * m_gvn;
    IntSet2VN<IntType> m_intset2vn;
    IntList m_tmp_ilst; //just used for temporary purpose.
protected:
    VN * registerVN(IntList const& ilst);
public:
    VNHashTab(GVN * gvn) : m_gvn(gvn) {}
    ~VNHashTab();

    void dump(Region const* rg, UINT indent) const;

    VN const* registerVN(IR_CODE irt, UINT vnnum, ...);
    VN const* registerVN(IR_CODE irt, VNList const& ilst);
    VN const* registerIntList(IR_CODE irt, UINT num, ...);
};

typedef VNHashTab::IntType VNHashInt;

class InferCtx {
protected:
    xcom::TTab<UINT> m_mdphi_tab;
public:
    InferCtx() {}

    void clean() { m_mdphi_tab.clean(); }

    bool isVisited(MDPhi const* phi) const
    { return m_mdphi_tab.find(phi->id()); }

    void setVisitMDPhi(MDPhi const* phi) { m_mdphi_tab.append(phi->id()); }
};


//The class inferences Equivalent-VN via walk through DefUse chain.
//Equivalent-VN describes a kind of VN comparasion strategy when determining
//whether two given IR exps's value are equal.
//The passes can reason out that two IR expressions have same runtime value
//if their Equivalent-VN are the same one.
//ONE KEY NOTE: the Equivalent-VN is different to the normal VN that GVN
//computed in usually.
//If given two IR expressions's Equivalent-VN is different, we can NOT
//exactly conclude the IR expressions have different runtime value.
//On the contrary, GVN's VN comparison strategy could exactly say the IR
//expressions have different runtime value.
//e.g: given two IR expressions ld x and $y, if InferEVN reasoned out that
//ld x has EVN1, and $y has EVN2, we just tell user that we have no knowledge
//about ld x and $y's value.
//NOTE: the EVN's inference may utilize GVN's VN if exist to infer EVN.
class InferEVN {
    COPY_CONSTRUCTOR(InferEVN);
protected:
    GVN * m_gvn;
    Region * m_rg;
    MDSSAMgr * m_mdssamgr;
    PRSSAMgr * m_prssamgr;
    IR2VN m_irid2vn;
    VMD2VN m_vmd2vn;
    MDPhi2VN m_mdphi2vn;
    VNHashTab m_vnhashtab;
protected:
    //Return true if the inference will try to infer Phi's EVN via inferring
    //each operands of Phi respectively.
    virtual bool allowCrossPhi() { return true; }

    //The function allocates a VN given stmt.
    VN const* allocVNForStmt(IR const* ir, InferCtx & ctx);
    VN const* allocVNForMDDef(MDDef const* mddef, InferCtx & ctx);
    VN const* allocVNForVMD(VMD const* vmd, InferCtx & ctx);

    //The function allocates a VN given extended stmt.
    virtual VN const* allocVNForExtStmt(IR const* ir, InferCtx & ctx);

    VN const* getVN(IR const* ir) { return m_irid2vn.get(ir->id()); }
    VN const* getVN(MDDef const* mddef)
    { return m_mdphi2vn.get(mddef->id()); }
    VN const* getVN(VMD const* vmd) { return m_vmd2vn.get(vmd->id()); }
    Region * getRegion() const { return m_rg; }

    virtual VN const* inferExtStmt(IR const* ir, InferCtx & ctx);
    VN const* inferIndirectStmt(IR const* ir, InferCtx & ctx);
    VN const* inferLiveinPR(IR const* ir, InferCtx & ctx);
    VN const* inferLiveinVMDForDirectExp(IR const* ir, InferCtx & ctx);
    VN const* inferVNByIterKid(IR const* ir, InferCtx & ctx);
    VN const* inferArrayKidOp(IR const* ir, InferCtx & ctx);
    VN const* inferWriteArray(IR const* ir, InferCtx & ctx);
    VN const* inferStmt(IR const* ir, InferCtx & ctx);
    VN const* inferMDPhi(MDPhi const* phi, InferCtx & ctx);
    VN const* inferVNViaBase(IR const* ir, InferCtx & ctx);
    VN const* inferDirectExp(IR const* ir, InferCtx & ctx);
    VN const* inferArray(IR const* ir, InferCtx & ctx);
    VN const* inferIndirectMemExp(IR const* ir, InferCtx & ctx);
    VN const* inferDirectExpViaMDSSA(IR const* ir, InferCtx & ctx);
    VN const* inferDirectExpViaPRSSA(IR const* ir, InferCtx & ctx);
    VN const* inferDirectExpViaSSA(IR const* ir, InferCtx & ctx);

    //The function maps given mddef information into an unique IR_CODE.
    //The mapped ir-code is used to conform the hashing-rules when registers
    //a MDDef and a set of integers.
    IR_CODE mapMDDef2IRCode(MDDef const* mddef) const;

    void setVN(IR const* ir, VN const* vn)
    {
        ASSERT0(!vn->is_unknown());
        //NOTE: each IR can be set only once, user has to check VN before
        //inoke the function.
        m_irid2vn.set(ir->id(), vn);
    }
    void setVN(MDDef const* mddef, VN const* vn)
    { m_mdphi2vn.set(mddef->id(), vn); }
    void setVN(VMD const* vmd, VN const* vn)
    { m_vmd2vn.set(vmd->id(), vn); }

    bool useMDSSADU() const
    { return m_mdssamgr != nullptr && m_mdssamgr->is_valid(); }
    bool usePRSSADU() const
    { return m_prssamgr != nullptr && m_prssamgr->is_valid(); }
public:
    InferEVN(GVN * gvn);
    virtual ~InferEVN() {}
    void dump() const;
    VN const* inferExp(IR const* ir, InferCtx & ctx);
};


//Perform Global Value Numbering.
class GVN : public Pass {
    friend class InferEVN;
    COPY_CONSTRUCTOR(GVN);
protected:
    BYTE m_is_vn_fp:1; //true if compute VN for float type.

    //Set true if we want to compute the VN for LDA(ID(str)).
    //And it's default value is false.
    //NOTE if you are going to enable it, you should ensure
    //RegionMgr::m_is_regard_str_as_same_md is false, because this
    //flag regards all strings as a same string to reduce the MD which
    //generated by different string type Vars.
    //e.g: LDA(ID("xxx")) and LDA(ID("yyy")) if
    //RegionMgr::m_is_regard_str_as_same_md is true, ID("xxx") and
    //ID("yyy") will refer to the same MD, and the MD is unbound.
    BYTE m_is_comp_lda_string:1;

    //Set true if we intend to allocate VN for livein MD, the livein MD may be
    //global variable or parameter. Note each livein MD will be assigned
    //different VN, that may misjudge the equivalence of two MemRef
    //e.g: given ld a, ld b, which VN is VN1, VN2 respectively. We could say
    //the value of a is not equal to b if VN1 != VN2. However, they may equal
    //if a and b are parameter. The default value is FALSE for conservative
    //purpose. GVN also try to set livein MD with different VN if the MD is
    //'restrict'.
    BYTE m_is_alloc_livein_vn:1;
    DUMgr * m_du;
    TypeMgr * m_tm;
    MDSystem * m_md_sys;
    PRSSAMgr * m_prssamgr;
    MDSSAMgr * m_mdssamgr;
    IRCFG * m_cfg;
    IRMgr * m_irmgr;
    Refine * m_refine;
    VN * m_zero_vn;
    VN * m_mc_zero_vn;
    xcom::SMemPool * m_pool;
    xcom::Vector<VN const*> * m_vn_vec; //optional, usually used to dump.
    OptCtx * m_oc;
    UINT m_vn_count;
    IR2VN m_ir2vn;
    MDPhi2VN m_mdphi2vn;
    Longlong2VN m_ll2vn;
    LonglongMC2VN m_llmc2vn;
    FP2VN m_fp2vn;
    Sym2VN m_str2vn;
    MD2VN m_md2vn;
    xcom::List<VN*> m_free_lst;
    xcom::List<Tab1*> m_tab_lst;
    xcom::Vector<void*> m_irc_vec;
    IR2ILDVNE m_def2ildtab;
    IR2ARRVNE m_def2arrtab;
    IR2SCVNE m_def2sctab;
    IR2IR m_stmt2domdef;
protected:
    void assignRHSVN();
    VN * allocLiveinVN(IR const* exp, MD const* emd, bool & change);

    bool calcCondMustValEQ(IR const* ir, bool & must_true,
                           bool & must_false) const;
    bool calcCondMustValNE(IR const* ir, bool & must_true,
                           bool & must_false) const;
    bool calcCondMustValLAndLOr(IR const* ir, bool & must_true,
                                bool & must_false) const;
    bool calcCondMustValLEGE(IR const* ir, bool & must_true,
                             bool & must_false) const;
    bool calcCondMustValLTGT(IR const* ir, bool & must_true,
                             bool & must_false) const;
    bool calcCondMustValBin(IR const* ir, bool & must_true,
                            bool & must_false) const;
    void cleanIR2VN();
    void clean();
    VN const* computeIntConst(HOST_INT val);
    VN const* computeSelect(IR const* exp, bool & change);
    VN const* computeBin(IR const* exp, bool & change);
    VN const* computeConst(IR const* exp, bool & change);
    VN const* computeLda(IR const* exp, bool & change);
    VN const* computeCvt(IR const* cvt, bool & change);
    VN const* computeUna(IR const* exp, bool & change);
    VN const* computeInexactScalarByClassicDU(IR const* exp, bool & change);
    VN const* computeScalarByAnonDomDef(IR const* ild, IR const* domdef,
                                        bool & change);
    VN const* computeILoadByAnonDomDef(IR const* ild, VN const* mlvn,
                                       IR const* domdef, bool & change);
    VN const* computeArrayByAnonDomDef(IR const* arr, VN const* basevn,
                                       VN const* ofstvn, IR const* domdef,
                                       bool & change);
    IR const* computeDomDef(IR const* exp, IR const* exp_stmt,
                            SList<IR*> * defs);
    void computeArrayAddrRef(IR const* ir, bool & change);
    VN const* computeArray(IR const* exp, bool & change);
    VN const* computeScalar(IR const* exp, bool & change);
    VN const* computeILoad(IR const* exp, bool & change);
    VN const* computeExactMemory(IR const* exp, bool & change);
    VN const* computePR(IR const* exp, bool & change);
    VN const* computeMDPhiMemOpnd(IR const* exp, bool & change);
    VN const* computeMDPhiOpnd(IR const* exp, bool & change);
    VN const* computePhiPROpnd(IR const* exp, bool & change);
    VN const* computePhiOpnd(IR const* exp, bool & change);

    void dumpBB(UINT bbid) const;
    void dumpIR2VN() const;
    void destroyLocalUsed();

    OptCtx const* getOptCtx() const { return m_oc; }
    IRMgr * getIRMgr() const { return m_irmgr; }

    //Return true if the value of ir1 and ir2 are definitely same, otherwise
    //return false to indicate unknown.
    bool hasSameValueByPRSSA(IR const* ir1, IR const* ir2) const;

    //Return true if the value of ir1 and ir2 are definitely same, otherwise
    //return false to indicate unknown.
    bool hasSameValueByMDSSA(IR const* ir1, IR const* ir2) const;
    bool hasOverlappedDef(IR const* exp) const;

    //Infer the VN of 'exp' via killing def.
    //exp: expression.
    //kdef: killing-def.
    VN const* inferVNViaKillingDef(IR const* exp, IR const* kdef,
                                   OUT bool & change);

    //Infer the VN of 'exp' via dominated-killing-def.
    //exp: expression.
    //kdef: killing-def.
    VN const* inferVNViaDomKillingDef(IR const* exp, IR const* kdef,
                                      OUT bool & change);
    VN const* inferVNViaMDPhi(IR const* exp, MDPhi const* phi, bool & change);
    VN const* inferVNThroughCFG(IR const* exp, bool & change);
    VN const* inferVNViaPhi(IR const* exp, IR const* phi, bool & change);
    VN const* inferVNViaHint(IR const* exp, MD const* md, bool & change);
    VN const* inferPRVNViaHint(IR const* exp, bool & change);
    VN const* inferNonPRVNViaHint(IR const* exp, bool & change);
    bool isSameMemLocForArrayOp(IR const* ir1, IR const* ir2) const;
    bool isSameMemLocForIndirectOp(IR const* ir1, IR const* ir2) const;
    virtual bool isUnary(IR_CODE irt) const;
    virtual bool isBinary(IR_CODE irt) const;
    virtual bool isTriple(IR_CODE irt) const;
    virtual bool isQuad(IR_CODE irt) const;

    void * xmalloc(UINT size)
    {
        void * p = smpoolMalloc(size, m_pool);
        ASSERT0(p);
        ::memset((void*)p, 0, size);
        return p;
    }

    VN * registerQuadVN(IR_CODE irt, VN const* v0, VN const* v1,
                        VN const* v2, VN const* v3);
    VN * registerTripleVN(IR_CODE irt, VN const* v0, VN const* v1,
                          VN const* v2);
    VN * registerBinVN(IR_CODE irt, VN const* v0, VN const* v1);
    VN * registerUnaVN(IR_CODE irt, VN const* v0);
    VN * registerVNviaMD(MD const* md);
    VN * registerVNviaMC(LONGLONG v);
    VN * registerVNviaINT(LONGLONG v);
    VN * registerVNviaFP(double v);
    VN * registerVNviaSTR(Sym const* v);

    void processMDPhi(IRBB * bb, bool & change);
    void processMDPhi(MDPhi const* phi, bool & change);
    void processGETELEM(IR * ir, bool & change);
    void processSETELEM(IR * ir, bool & change);
    void processBB(IRBB * bb, bool & change);
    void processBBListInRPO();
    void processCall(IR const* ir, bool & change);
    void processRegion(IR const* ir, bool & change);
    void processPhi(IR const* ir, bool & change);
    void processIndirectMemOp(IR * ir, bool & change);
    void processDirectMemOp(IR * ir, bool & change);
    void processSTARRAY(IR * ir, bool & change);
    virtual void processStmt(IR * ir, bool & change);
    virtual void processExtStmt(IR * ir, bool & change);

    bool useClassicDU() const
    { return useClassicPRDU() || useClassicNonPRDU(); }
    bool useClassicPRDU() const { return getOptCtx()->is_pr_du_chain_valid(); }
    bool useClassicNonPRDU() const
    { return getOptCtx()->is_nonpr_du_chain_valid(); }
    bool useMDSSADU() const
    { return m_mdssamgr != nullptr && m_mdssamgr->is_valid(); }
    bool usePRSSADU() const
    { return m_prssamgr != nullptr && m_prssamgr->is_valid(); }
public:
    explicit GVN(Region * rg);
    virtual ~GVN();

    VN * allocVN();

    //Return true if GVN is able to determine the result of 'ir', otherwise
    //return false that GVN know nothing about ir.
    bool calcCondMustVal(IR const* ir, bool & must_true,
                         bool & must_false) const;
    void cleanIRTreeVN(IR const* ir);
    void copyVN(IR const* from, IR const* to);

    //Compute VN to given exp.
    //change: set to true if new VN generated.
    VN const* computeVN(IR const* exp, bool & change);

    //Compute VN to extend IR expression.
    virtual VN const* computeExtExp(IR const* exp, bool & change)
    {
        //Target Dependent Code.
        return nullptr;
    }

    //The function dump pass relative information before performing the pass.
    //The dump information is always used to detect what the pass did.
    //Return true if dump successed, otherwise false.
    virtual bool dumpBeforePass() const { return Pass::dumpBeforePass(); }

    //The function dump pass relative information.
    //The dump information is always used to detect what the pass did.
    //Return true if dump successed, otherwise false.
    virtual bool dump() const;
    void dumpAllVN() const;
    void dumpMiscMap() const;
    void destroy();

    virtual CHAR const* getPassName() const { return "Global Value Numbering"; }
    PASS_TYPE getPassType() const { return PASS_GVN; }

    //Return true if the value of ir1 and ir2 are definitely same, otherwise
    //return false to indicate unknown.
    //The function will retrieve dependence through SSA, thus MDSSA and PRSSA
    //have to be avaiable.
    bool hasSameValueBySSA(IR const* ir1, IR const* ir2) const;

    //Return true if vn1 is exactly equal to vn2.
    //e.g:if vn1 of ir1 reasoned from a ILD, and vn2 of ir2 reasoned from
    //a ARRAY, we can conclude that ir1's value is equal to ir2 if vn1 == vn2.
    //Note both vn1 and vn2 can not be NULL.
    bool hasSameValue(VN const* vn1, VN const* vn2) const
    {
        ASSERT0(vn1 && vn2);
        return vn1 == vn2;
    }

    //Return true if ir has unique and constant VN.
    bool hasConstVN(IR const* ir) const;

    //Return true if vn1 is exactly not equal to vn2.
    //e.g:if vn1 of ir1 reasoned from a ILD, and vn2 of ir2 reasoned from
    //a ARRAY, we can not simply conclude that ir1's value is exactly NOT
    //equal to ir2 even if vn1 != vn2.
    bool hasDifferentValue(VN const* vn1, IR const* ir1,
                           VN const* vn2, IR const* ir2) const;
    bool hasDifferentValue(VN const* vn1, Type const* vn1type,
                           VN const* vn2, Type const* vn2type) const;
    bool hasDifferentValue(VN const* vn1, VN const* vn2) const;

    void init();

    //Return true if ir has unique and constant VN.
    //ir: the stmt or exp that has the VN 'irvn'.
    bool isConstVN(VN const* irvn, IR const* ir) const;

    //Return true if ir1 and ir2 represent identical memory location.
    //Note this function does NOT consider data type
    //that ir1 or ir2 referrenced.
    bool isSameMemLoc(IR const* ir1, IR const* ir2) const;

    //Return true if GVN will alloc different VN for livein variable.
    bool isAllocLiveinVN() const { return m_is_alloc_livein_vn; }

    //Get VN of ir.
    VN const* getVN(IR const* ir) const { return m_ir2vn.get(ir->id()); }
    VN const* getVN(MDPhi const* phi) const
    { return m_mdphi2vn.get(phi->id()); }

    //Set true if you intend to alloc different VN for livein variable.
    void setAllocLiveinVN(bool doit) { m_is_alloc_livein_vn = doit; }

    //Set VN to ir.
    void setVN(IR const* ir, VN const* vn) { m_ir2vn.setAlways(ir->id(), vn); }

    //Set VN to MDPhi.
    void setVN(MDPhi const* phi, VN const* vn)
    { m_mdphi2vn.setAlways(phi->id(), vn); }

    //Set VN to ir only once.
    void setVNOnce(IR const* ir, VN const* vn) { m_ir2vn.set(ir->id(), vn); }

    //Update VN to ir if ir has been mapped.
    void setVNIfFind(IR const* ir, VN const* vn)
    { m_ir2vn.setIfFind(ir->id(), vn); }

    //Ask GVN to compute VN for IR with FP type.
    void setComputeVNForFP(bool doit) { m_is_vn_fp = doit; }

    virtual bool perform(OptCtx & oc);
};

} //namespace xoc
#endif
