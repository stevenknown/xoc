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
#ifndef _ALGE_REASSCOCIATE_
#define _ALGE_REASSCOCIATE_

namespace xoc {

#define RANK_UNDEF 0

class ReassCtx;

typedef UINT RANK;

class LinOpVec : public xcom::Vector<IR const*> {
    IR_CODE m_code;
public:
    LinOpVec() { m_code = IR_UNDEF; }

    void append(IR const* ir)
    {
        ASSERT0(ir && !ir->is_undef());
        ASSERT0(is_unique(ir));
        xcom::Vector<IR const*>::append(ir);
    }

    void clean() { m_code = IR_UNDEF; xcom::Vector<IR const*>::clean(); }

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


class ReassCtx : public PassCtx {
    //THE CLASS ALLOWS COPY-CONSTRUCTOR.
protected:
    bool m_need_recomp_gvn; //True to indicate that GVN should be recomputed.
    UINT m_ir_rank_range_bitsize;
    RANK m_cur_rank;
    LinOpVec m_lin_opvec;
    xcom::TMap<UINT, RANK> m_ir2rank;
    xcom::TMap<UINT, RANK> m_bb2rank;
protected:
    void dumpBBListWithRankImpl(MOD IRDumpCtx<> & dumpctx) const;
    void dumpBBListWithRank() const;
public:
    ReassCtx(OptCtx & oc, Region const* rg);

    void cleanBottomUp();

    CHAR const* dumpRank(RANK rank, OUT DefFixedStrBuf & buf) const;
    void dump() const;

    LinOpVec & getLinOpVec() { return m_lin_opvec; }
    RANK getCurRank() const { return m_cur_rank; }
    RANK getRank(IR const* ir) const { return m_ir2rank.get(ir->id()); }
    RANK getRank(IRBB const* bb) const { return m_bb2rank.get(bb->id()); }
    UINT getIRRankRangeBitSize() const { return m_ir_rank_range_bitsize; }
    UINT getBBRankRangeBitSize() const;

    bool needRecompGVN() const { return m_need_recomp_gvn; }

    void setIRRankRangeBitSize(UINT bs) { m_ir_rank_range_bitsize = bs; }
    void setCurRank(RANK r) { m_cur_rank = r; }
    void setRank(IR const* ir, RANK rank) { m_ir2rank.set(ir->id(), rank); }
    void setRank(IRBB const* bb, RANK rank) { m_bb2rank.set(bb->id(), rank); }
    void setRecompGVN(bool recomp) { m_need_recomp_gvn = recomp; }
};


//This class represents algebraic reasscociation.
class AlgeReasscociate : public Pass {
    COPY_CONSTRUCTOR(AlgeReasscociate);
    bool m_is_aggressive;
    IRCFG * m_cfg;
    TypeMgr * m_tm;
    PRSSAMgr * m_prssamgr;
    MDSSAMgr * m_mdssamgr;
    IRMgr * m_irmgr;
    Refine * m_refine;
    IRSimp * m_simp;
    ReassActMgr m_am;
protected:
    //The function also generates MD reference for new IRs.
    void buildDUChainForReassExp(
        IRVec const& reassopvec, ReassCtx const& ctx) const;

    bool canBeCandStmt(IR const* ir) const;
    virtual bool canBeCandBinOp(IR const* ir) const;
    virtual RANK computeRankForExtOp(IR const*, MOD ReassCtx &) const
    { ASSERTN(0, ("Target Dependent Code")); return RANK_UNDEF; }
    RANK computeConst(IR const* ir, MOD ReassCtx & ctx) const;
    RANK computeBinOp(MOD IR * ir, MOD ReassCtx & ctx) const;
    RANK computeDirectMemExp(IR const* ir, MOD ReassCtx & ctx) const;
    RANK computeStoreStmt(MOD IR * ir, MOD ReassCtx & ctx) const;
    RANK computePhi(MOD IR * ir, MOD ReassCtx & ctx) const;
    RANK computeRankForExp(MOD IR * ir, MOD ReassCtx & ctx) const;
    RANK computeRankForStmt(MOD IR * ir, MOD ReassCtx & ctx) const;
    bool computeRankAndReassForBB(IRBB const* bb, MOD ReassCtx & ctx) const;

    //The rank of operand can be used to determine the layout order of operand.
    //The operand with a higher rank will be processed preferentially.
    bool doReass(MOD ReassCtx & ctx);

    bool foldConstLastTwoOp(MOD ReassCtx & ctx) const;

    RANK getLowestRank() const { return RANK(RANK_UNDEF + 1); }

    bool initDepPass(MOD OptCtx & oc);
    bool isTypeSafeToCommutate(IR const* ir) const
    {
        //Target Dependent Code.
        if (!g_do_opt_float && ir->isFP()) { return false; }
        return true;
    }
    bool isOpCodeConsistent(IR const* ir, LinOpVec const& opvec) const;

    //Perform optimization to operations that recorded in the opvec.
    //Return true if operations in vector have been optimized.
    bool optimizeLinOpVec(MOD IR * ir, MOD ReassCtx & ctx) const;

    //Return true if given 'ir' has been rewrote.
    bool replaceRHSWithReassExp(MOD IR * ir, MOD ReassCtx & ctx) const;
    void reset();

    //It is not worth to reasscociate expressions.
    IR * reasscociatedExp(OUT IRVec & reassopvec, ReassCtx const& ctx) const;

    bool useMDSSADU() const
    { return m_mdssamgr != nullptr && m_mdssamgr->is_valid(); }
    bool usePRSSADU() const
    { return m_prssamgr != nullptr && m_prssamgr->is_valid(); }

    bool verifyRank(ReassCtx const& ctx) const;
public:
    explicit AlgeReasscociate(Region * rg) : Pass(rg), m_am(rg)
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
    virtual ~AlgeReasscociate() {}

    void dumpAllAct() const { m_am.dump(); }
    virtual bool dump() const;

    virtual CHAR const* getPassName() const { return "Alge Reasscociation"; }
    ReassActMgr & getActMgr() { return m_am; }
    PASS_TYPE getPassType() const { return PASS_ALGE_REASSCOCIATE; }
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
