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
DISCLAIMED. IN NO EVENT SHALL THE CONTRIBUTORS BE LIABLE FOR ANY
DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
(INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND
ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
(INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
@*/
#ifndef _LOOP_DEP_ANA_H_
#define _LOOP_DEP_ANA_H_

namespace xoc {

class LDACtx;
class LICM;

typedef enum tagLOOP_DEP_KIND {
    LOOP_DEP_UNDEF = 0,
    LOOP_DEP_CARRIED, //indicates loop-carried dependency.
    LOOP_DEP_INDEPENDENT, //indicates loop-independent dependency.
    LOOP_DEP_REDUCE, //indicates loop-reduce dependency.
    LOOP_DEP_NUM,
} LOOP_DEP_KIND;


#define LDI_kind(l) ((l)->m_kind)
#define LDI_src(l) ((l)->m_src)
#define LDI_tgt_ir(l) ((l)->u1.m_tgt_ir)
#define LDI_tgt_mddef(l) ((l)->u1.m_tgt_mddef)
#define LDI_tgt_is_ir(l) ((l)->m_tgt_is_ir)
class LoopDepInfo {
public:
    bool m_tgt_is_ir;
    LOOP_DEP_KIND m_kind;
    IR const* m_src;
    union {
        IR const* m_tgt_ir;
        MDDef const* m_tgt_mddef;
    } u1;
public:
    LoopDepInfo()
    {
        LDI_tgt_is_ir(this) = true;
        LDI_kind(this) = LOOP_DEP_UNDEF;
        LDI_src(this) = nullptr;
        LDI_tgt_ir(this) = nullptr;
    }

    void copy(LoopDepInfo const& src) { *this = src; }

    CHAR const* dumpTgt(OUT xcom::StrBuf & buf) const;

    CHAR const* getDepName() const;
    IR const* getSrc() const { return LDI_src(this); }
    IR const* getTgtIR() const
    {
        ASSERT0(isTgtIR());
        return LDI_tgt_ir(this);
    }
    MDDef const* getTgtMDDef() const
    {
       ASSERT0(isTgtMDDef());
       return LDI_tgt_mddef(this);
    }

    bool isTgtIR() const { return LDI_tgt_is_ir(this); }
    bool isTgtMDDef() const { return !LDI_tgt_is_ir(this); }
    bool isLoopIndep() const { return LDI_kind(this) == LOOP_DEP_INDEPENDENT; }
    bool isLoopCarr() const { return LDI_kind(this) == LOOP_DEP_CARRIED; }
    bool isLoopRed() const { return LDI_kind(this) == LOOP_DEP_REDUCE; }

    void setTgtMDDef(MDDef const* mddef)
    {
        ASSERT0(mddef && LDI_tgt_mddef(this) == nullptr);
        LDI_tgt_is_ir(this) = false;
        LDI_tgt_mddef(this) = mddef;
    }
    void setTgtIR(IR const* ir)
    {
        ASSERT0(ir && LDI_tgt_ir(this) == nullptr);
        LDI_tgt_is_ir(this) = true;
        LDI_tgt_ir(this) = ir;
    }

    bool verify() const
    {
        ASSERT0(LDI_kind(this) != LOOP_DEP_UNDEF &&
                LDI_kind(this) < LOOP_DEP_NUM);
        if (isTgtIR()) { ASSERT0(LDI_tgt_ir(this)); }
        if (isTgtMDDef()) { ASSERT0(LDI_tgt_mddef(this)); }
        return true;
    }
};


class LoopDepInfoDesc {
public:
    LOOP_DEP_KIND kind;
    CHAR const* name;
public:
    static CHAR const* getDepName(LOOP_DEP_KIND k);
};


class LDAActMgr : public ActMgr {
    COPY_CONSTRUCTOR(LDAActMgr);
public:
    LDAActMgr(Region const* rg) : ActMgr(rg) {}

    //Dump misc action that related to given ir.
    //format: the reason.
    void dumpAct(CHAR const* format, ...) const;
    void dumpAct(IR const* ir, CHAR const* format, ...) const;
    void dumpLinRepAct(IVLinearRep const& linrep,
                       CHAR const* format, ...) const;
};


//The class represents loop dependence analysis context.
class LDACtx {
    COPY_CONSTRUCTOR(LDACtx);
protected:
    ConstIRTab m_analyzed_irs; //record all IRs that has analyzed.
public:
    LI<IRBB> const* m_li;
public:
    LDACtx(LI<IRBB> const* li)
    {
        ASSERT0(li);
        m_li = li;
    }
    ~LDACtx() {}

    //Record 'ir' as the IR that has participated the analysis.
    void add(IR const* ir) { m_analyzed_irs.append(ir); }

    LI<IRBB> const* getLI() const { return m_li; }

    //Return true 'ir' has analyzed.
    bool is_contain(IR const* ir) const { return m_analyzed_irs.find(ir); }
};


class LDAInfoSet : public xcom::List<LoopDepInfo*> {
public:
    LDAInfoSet() {}
    void dump(Region const* rg) const;
};

typedef xcom::List<LoopDepInfo*>::Iter LDAInfoSetIter;

//This class represents loop dependence analysis.
class LoopDepAna : public Pass {
    COPY_CONSTRUCTOR(LoopDepAna);
    SMemPool * m_pool;
    IVR * m_ivr;
    GVN * m_gvn;
    LICM * m_licm;
    IRCFG * m_cfg;
    PRSSAMgr * m_prssamgr;
    MDSSAMgr * m_mdssamgr;
    DUMgr * m_dumgr;
    OptCtx * m_oc;
    InferEVN * m_infer_evn;
    LDAActMgr m_act_mgr;
protected:
    LoopDepInfo * allocLoopDepInfo();
    void analyzeLinearDep(
        IR const* ir, xcom::List<IR*> const& lst, OUT LDAInfoSet & set,
        MOD LDACtx & ctx);
    void analyzeRedDep(
        IR const* ir, xcom::List<IR*> const& lst, OUT LDAInfoSet & set,
        MOD LDACtx & ctx);

    void destroy();

    OptCtx * getOptCtx() const { return m_oc; }

    void init(GVN * gvn);

    //Return true if given two IRs in 'info' are indicates same memory
    //location.
    //The funtion uses Equal VN to determine whether these two IRs reference
    //same memory base address.
    //e.g: ist x VS ild y, return true if x's EVN is equal to y's EVN.
    //Return true if these two IRs are reference identical memory location,
    //otherwise tell caller 'I KNOW NOTHING ABOUT THAT' by returning false.
    bool isSameMemLocViaEVN(LoopDepInfo const& info);
    bool isSameMemLocArrayOp(LoopDepInfo const& info);
    bool isSameMemLocIndirectOp(LoopDepInfo const& info);

    void reset();

    bool transLoopCarrToLoopIndep(IR const* ir, LDAInfoSet const& set);

    bool useLICM() const;
    bool useMDSSADU() const
    { return m_mdssamgr != nullptr && m_mdssamgr->is_valid(); }
    bool usePRSSADU() const
    { return m_prssamgr != nullptr && m_prssamgr->is_valid(); }
    bool useGVN() const
    { return m_gvn != nullptr && m_gvn->is_valid(); }
public:
    explicit LoopDepAna(Region * rg, GVN * gvn) : Pass(rg), m_act_mgr(rg)
    {
        m_pool = nullptr;
        init(gvn);
    }
    virtual ~LoopDepAna() { destroy(); }

    void analyzeDepForIRTree(
        IR const* ir, xcom::List<IR*> const& lst, OUT LDAInfoSet & set,
        MOD LDACtx & ctx);
    void analyzeDep(IR const* ir, xcom::List<IR*> const& lst,
                    OUT LDAInfoSet & set, MOD LDACtx & ctx);

    bool containLoopRedDep(LDAInfoSet const& set);
    bool containLoopCarrDep(LDAInfoSet const& set);

    virtual bool dump() const;
    void dumpInferEVN() const;

    virtual CHAR const* getPassName() const
    { return "Loop Dependence Analysis"; }
    PASS_TYPE getPassType() const { return PASS_LOOP_DEP_ANA; }
    LDAActMgr & getActMgr() { return m_act_mgr; }
    InferEVN & getInferEVN() const { return *m_infer_evn; }

    virtual bool perform(OptCtx & oc);
};

} //namespace xoc
#endif
