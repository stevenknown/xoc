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
#ifndef _ALGE_REASSOCIATE_
#define _ALGE_REASSOCIATE_

namespace xoc {

#define RANK_UNDEF 0

class ReassCtx;
class AlgeReassociate;
class VarRefMgr;

typedef UINT RANK;

class VRTGenMapped {
    COPY_CONSTRUCTOR(VRTGenMapped);
protected:
    VarRefMgr * m_vrmgr;
public:
    VRTGenMapped() { m_vrmgr = nullptr; }
    ConstVarTab * createMapped(IR const*);
    void setVarRefMgr(VarRefMgr * vrm) { m_vrmgr = vrm; }
};

//Map Var to VarRef Tab.
typedef xcom::TMapIter<IR const*, ConstVarTab*> IR2VRTabIter;
class IR2VRTab : public xcom::TMap<
    IR const*, ConstVarTab*, CompareKeyBase<IR const*>, VRTGenMapped> {
    COPY_CONSTRUCTOR(IR2VRTab);
public:
    IR2VRTab() {}
};

class VarRefMgr {
    friend class VRTGenMapped;
    COPY_CONSTRUCTOR(VarRefMgr);
protected:
    xcom::List<ConstVarTab*> m_vrt_list;
    IR2VRTab m_ir2vrt;
protected:
    ConstVarTab * allocConstVarTab();
public:
    VarRefMgr();
    ~VarRefMgr();
    void dump(Region const* rg) const;
    bool find(IR const* ir, Var const* ref);
    ConstVarTab * getAndGenVRT(IR const* ir);
    ConstVarTab const* getVRT(IR const* ir) const;
    void set(IR const* ir, Var const* ref);
};

#define REASSEXP_code(e) ((e)->m_code)
#define REASSEXP_opnd_buf(e) ((e)->m_opnd_buf)
#define REASSEXP_opnd(e, idx) (REASSEXP_opnd_buf(e)[e->checkIdx(idx)])
#define REASSEXP_opnd_num(e) ((e)->m_opnd_num)
class ReassExp {
public:
    IR_CODE m_code;
    UINT m_opnd_num;
    IR const** m_opnd_buf;
public:
    UINT checkIdx(UINT idx) const
    {
        ASSERT0(idx < REASSEXP_opnd_num(this));
        return idx;
    }
    void dump(Region const* rg) const;
    IR const* getOpnd(UINT idx) const { return REASSEXP_opnd(this, idx); }
    IR_CODE getCode() const { return REASSEXP_code(this); }
    UINT getOpndNum() const { return m_opnd_num; }
};

class ReassExpMgr {
    COPY_CONSTRUCTOR(ReassExpMgr);
protected:
    SMemPool * m_pool;
protected:
    ReassExp * allocReassExp(UINT opndnum);
    void * xmalloc(size_t size)
    {
        void * p = xcom::smpoolMalloc(size, m_pool);
        ASSERT0(p);
        ::memset((void*)p, 0, size);
        return p;
    }
public:
    ReassExpMgr();
    ~ReassExpMgr();

    //Generate reass-exp by user input information.
    ReassExp * genConstReassExp(IR const* op);
    ReassExp * genDirectReassExp(IR const* op);

    //Generate reass-exp by user input information.
    ReassExp * genBinReassExp(IR_CODE code, IR const* op0, IR const* op1);

    //Generate reass-exp by extracting information form given 'ir'.
    ReassExp * genBinReassExp(IR const* ir);
};

class LinOpVec : public xcom::Vector<IR const*> {
    IR_CODE m_code;
    xcom::TTab<IR const*> m_exist;
protected:
    bool find(IR const* ir) { return m_exist.find(ir); }
public:
    LinOpVec() { m_code = IR_UNDEF; }

    void append(IR const* ir)
    {
        ASSERT0(ir && !ir->is_undef());
        if (find(ir)) { return; }
        m_exist.append(ir);
        xcom::Vector<IR const*>::append(ir);
    }

    void clean();
    void cleanLastFrom(VecIdx idx);

    void dump(ReassCtx const& ctx) const;

    IR_CODE getCode() const { return m_code; }

    bool is_unique(IR const* ir) const;

    void setCode(IR_CODE c)
    {
        ASSERT0(m_code == IR_UNDEF || c == m_code);
        m_code = c;
    }

    bool verify() const;
};


class ReassActMgr : public ActMgr {
    COPY_CONSTRUCTOR(ReassActMgr);
public:
    ReassActMgr(Region const* rg) : ActMgr(rg) {}

    //Dump misc action that related to given ir.
    //format: the reason.
    void dumpAct(CHAR const* format, ...);
};


class IR2Var {
public:
    IR const* key;
    Var const* value;
public:
    IR2Var() {}
    IR2Var(UINT) {}
    IR2Var(IR const* k, Var const* v) : key(k), value(v) {}
};

typedef xcom::List<IR2Var>::Iter IR2VLstIter;
typedef xcom::List<IR2Var> IR2VLst;

class ReassCtx : public PassCtx {
    //THE CLASS ALLOWS COPY-CONSTRUCTOR.
    friend class AlgeIntlImpl;
protected:
    //Progagate information bottom up to user.
    //True to tell user that GVN changed and should be recomputed.
    bool m_need_recomp_gvn;
    UINT m_ir_rank_range_bitsize;
    RANK m_cur_rank;
    AlgeReassociate * m_alge_reass;
    IRMgr * m_irmgr;
    ReassExpMgr m_reass_exp_mgr;
    LinOpVec m_lin_opvec; //record the linearized operations.
    IRTab m_gened_ir;
    xcom::TMap<UINT, RANK> m_ir2rank;
    xcom::TMap<UINT, RANK> m_bb2rank;
    IR2VLst m_ir2var_lst;
protected:
    void dumpBBListWithRankImpl(MOD IRDumpCtx<> & dumpctx) const;
    void dumpBBListWithRank() const;
    void recordGenedIRTree(IR * ir);
public:
    ReassCtx(OptCtx & oc, AlgeReassociate * algereass);
    ~ReassCtx();

    //Clean the information that propagate to user bottom-up.
    void cleanBottomUp();

    CHAR const* dumpRank(RANK rank, OUT DefFixedStrBuf & buf) const;
    void dump() const;

    AlgeReassociate * getAlgeReass() const { return m_alge_reass; }
    IRMgr * getIRMgr() const { return m_irmgr; }
    LinOpVec & getLinOpVec() { return m_lin_opvec; }
    ReassExpMgr & getReassExpMgr() { return m_reass_exp_mgr; }
    RANK getCurRank() const { return m_cur_rank; }
    RANK getRank(IR const* ir) const { return m_ir2rank.get(ir->id()); }
    RANK getRank(IRBB const* bb) const { return m_bb2rank.get(bb->id()); }
    UINT getIRRankRangeBitSize() const { return m_ir_rank_range_bitsize; }
    UINT getBBRankRangeBitSize() const;
    IR2VLst const& getIR2VLst() const { return m_ir2var_lst; }

    bool needRecompGVN() const { return m_need_recomp_gvn; }

    void recordIR2Var(IR const* key, Var const* value);

    void setIRRankRangeBitSize(UINT bs) { m_ir_rank_range_bitsize = bs; }
    void setCurRank(RANK r) { m_cur_rank = r; }
    void setRank(IR const* ir, RANK rank) { m_ir2rank.set(ir->id(), rank); }
    void setRank(IRBB const* bb, RANK rank) { m_bb2rank.set(bb->id(), rank); }
    void setRecompGVN(bool recomp) { m_need_recomp_gvn = recomp; }

    bool verify() const;
};


//This class represents algebraic reasscociation.
class AlgeReassociate : public Pass {
    COPY_CONSTRUCTOR(AlgeReassociate);
    friend class AlgeIntlImpl;
protected:
    bool m_is_aggressive;
    IRCFG * m_cfg;
    TypeMgr * m_tm;
    PRSSAMgr * m_prssamgr;
    MDSSAMgr * m_mdssamgr;
    IRMgr * m_irmgr;
    Refine * m_refine;
    IRSimp * m_simp;
    ReassActMgr m_am;

    //The table is used to prevent endless-loop compilation that casued by
    //associating same MemRef repeatedly when the pass performed
    //multiple times.
    VarRefMgr m_vr_mgr;
protected:
    //The function also generates MD reference for new IRs.
    void buildDUChainForReassExp(
        IRVec const& reassopvec, ReassCtx const& ctx) const;

    bool canBeReass(IR const* ir) const;
    bool canBeReass(IR_CODE c) const;
    bool canBeCandStmt(IR const* ir) const;
    bool canBeCandBinOp(IR const* ir) const;
    bool canBeCandConstOp(IR const* ir) const
    {
        ASSERT0(ir->isConstExp());
        return true; //always regard const-exp as candidate.
    }

    //Return true if there are some expressions are linearized.
    bool computeRankAndLinearExp(MOD IR * ir, MOD ReassCtx & ctx) const;
    bool computeRankAndReassForBB(IRBB const* bb, MOD ReassCtx & ctx) const;
    RANK computeRankOfNonAlgeExp(IR const* ir) const;

    //The rank of operand can be used to determine the layout order of operand.
    //The operand with a higher rank will be processed preferentially.
    bool doReass(MOD ReassCtx & ctx);

    bool foldConstLastTwoOp(MOD ReassCtx & ctx) const;
    bool foldConstLast(MOD ReassCtx & ctx) const;

    RANK getLowestRank() const { return RANK(RANK_UNDEF + 1); }

    bool hasSideEffect(IR const* ir) const;

    bool initDepPass(MOD OptCtx & oc);
    bool isTypeSafeToCommutate(IR const* ir) const
    {
        //Target Dependent Code.
        if (!g_do_opt_float && ir->isFP()) { return false; }
        return true;
    }

    bool isSafeToFoldConstBinOpInCase1(
        IR_CODE opc, IR const* op0, IR const* op1) const;
    bool isSafeToFoldConstBinOpInCase2(
        IR_CODE opc, IR const* op0, IR const* op1) const;
    bool isSafeToFoldConstBinOp(
        IR_CODE opc, IR const* op0, IR const* op1) const;
    bool isOpCodeConsistent(
        ReassExp const* reassexp, ReassCtx const& ctx) const;
    bool isOpCodeConsistent(
        IR_CODE reasscode, ReassCtx const& ctx) const;

    //The function linearizes and computes the rank for extended operations.
    virtual bool linearExtOp(IR const*, MOD ReassCtx &) const;

    //The function computes the rank for extended operations according to
    //ReassExp information.
    virtual bool linearExtOpForReassExp(
        ReassExp const* reassexp, MOD ReassCtx &) const;
    bool linearDirectMemExp(IR const* ir, MOD ReassCtx & ctx) const;
    bool linearBinOp(IR const* ir, MOD ReassCtx & ctx) const;
    bool linearConstOpForReassExp(
        ReassExp const* reassexp, MOD ReassCtx & ctx) const;
    bool linearBinOpForReassExp(
        ReassExp const* reassexp, MOD ReassCtx & ctx) const;
    bool linearDirectOpForReassExp(
        ReassExp const* reassexp, MOD ReassCtx & ctx) const;
    bool linearReassExp(ReassExp const* reassexp, MOD ReassCtx & ctx) const;
    bool linearExpViaStoreStmt(MOD IR * ir, MOD ReassCtx & ctx) const;
    bool linearExpViaPhi(MOD IR * ir, MOD ReassCtx & ctx) const;
    bool linearConst(IR const* ir, MOD ReassCtx & ctx) const;
    bool linearConstExpTree(IR const* ir, MOD ReassCtx & ctx) const;

    //The function linearizes and computes the rank for expression.
    bool linearExp(IR const* ir, MOD ReassCtx & ctx) const;

    //Perform optimization to operations that recorded in the opvec.
    //Return true if operations in vector have been optimized.
    bool reassociate(MOD IR * ir, MOD ReassCtx & ctx) const;

    //Return true if given 'ir' has been rewrote.
    bool replaceRHSWithReassExp(MOD IR * ir, MOD ReassCtx & ctx) const;

    //Record the res-var to ref-var into pass-object to prevent handling
    //the same pattern over and over again.
    void recordHandledReassInfo(ReassCtx const& ctx);
    void reset();

    //It is not worth to reassociate expressions.
    IR * reassociatedExp(OUT IRVec & reassopvec, ReassCtx const& ctx) const;

    void setAtLeastLowestRank(IR const* ir, MOD ReassCtx & ctx) const;
    void setAtLeastCombineRank(IR const* ir, MOD ReassCtx & ctx) const;
    void setRankByOther(
        IR const* ir, IR const* ref, MOD ReassCtx & ctx) const;

    bool useMDSSADU() const
    { return m_mdssamgr != nullptr && m_mdssamgr->is_valid(); }
    bool usePRSSADU() const
    { return m_prssamgr != nullptr && m_prssamgr->is_valid(); }

    bool verifyRank(ReassCtx const& ctx) const;
public:
    explicit AlgeReassociate(Region * rg) : Pass(rg), m_am(rg)
    {
        ASSERT0(rg != nullptr);
        m_cfg = rg->getCFG();
        m_tm = rg->getTypeMgr();
        m_irmgr = nullptr;
        m_prssamgr = nullptr;
        m_mdssamgr = nullptr;
        m_refine = nullptr;
        m_simp = nullptr;
        m_is_aggressive = false;
    }
    virtual ~AlgeReassociate() {}

    void dumpAllAct() const { m_am.dump(); }
    virtual bool dump() const;

    virtual CHAR const* getPassName() const { return "Alge Reasscociation"; }
    ReassActMgr & getActMgr() { return m_am; }
    VarRefMgr & getVarRefMgr() { return m_vr_mgr; }
    PASS_TYPE getPassType() const { return PASS_ALGE_REASSOCIATE; }
    IRSimp * getIRSimp() const { return m_simp; }

    //Return true if user ask to perform aggressive optimization that without
    //consideration of compilation time and memory.
    bool is_aggressive() const { return m_is_aggressive; }

    virtual bool perform(OptCtx & oc);

    //Set to true if user ask to perform aggressive optimization that without
    //consideration of compilation time and memory.
    void setAggressive(bool doit) { m_is_aggressive = doit; }
};

} //namespace xoc

#endif
