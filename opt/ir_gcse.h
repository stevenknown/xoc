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
// This file is distributed under the BSD License. See LICENSE.TXT for details.

#ifndef _IR_GCSE_H_
#define _IR_GCSE_H_

namespace xoc {

class GCSE;

class TG : public xcom::DGraph {
    Region * m_rg;
protected:
    virtual void * cloneEdgeInfo(xcom::Edge *)
    { return nullptr; }
    virtual void * cloneVertexInfo(xcom::Vertex *)
    { return nullptr; }
public:
    explicit TG(Region * rg) { m_rg = rg; }
    COPY_CONSTRUCTOR(TG);

    //Pick out exception-handling-vertex and related edge.
    void pickEH()
    {
        List<IRBB*> * bbs = m_rg->getBBList();
        for (IRBB * bb = bbs->get_head(); bb != nullptr; bb = bbs->get_next()) {
            if (!bb->isExceptionHandler()) { continue; }
            removeVertex(bb->id());
        }
    }

    inline void computeDomAndIdom()
    {
        if (!computeDom()) { UNREACHABLE(); }
        if (!computeIdom()) { UNREACHABLE(); }
    }

    inline void computePdomAndIpdom(xcom::Vertex * root)
    {
        if (!computePdomByRPO(root, nullptr)) { UNREACHABLE(); }
        if (!computeIpdom()) { UNREACHABLE(); }
        revisePdomByIpdom();
    }
};


//The class map a VN to its IR expression that generated the VN.
typedef xcom::TMap<VN const*, IR*> VN2IRTabIter;
class VN2IRTab : public xcom::TMap<VN const*, IR*> {
public:
    void clean(VN const* vn) { setAlways(vn, nullptr); }
    void clean() { xcom::TMap<VN const*, IR*>::clean(); }
    void set(VN const* vn, IR * ir) { setAlways(vn, ir); }
};


//The class map a CSE to its delegate PR.
class CSE2DeleTab : public xcom::TMap<IR const*, IR*> {
};

class GCSECtx : public PassCtx {
    COPY_CONSTRUCTOR(GCSECtx);
protected:
    IRCFG * m_cfg;
    TG * m_tg;
    GCSE * m_gcse;
protected:
    TG * allocTG(Region * rg);
public:
    GCSECtx(OptCtx & oc, xcom::DomTree const& domtree, ActMgr * am,
            GCSE * gcse);
    ~GCSECtx();
    IRCFG * getCFG() const { return m_cfg; }
    TG * getTG() const { return m_tg; }

    //The function try to judge if given 'ir' may reference IV, GVN etc.
    //If it is true, the function will invalidate related passes to avoid
    //inconsistent access of these informations.
    void tryInvalidInfoBeforeFreeIR(IR const* ir) const;
};

//The class performs global common subexpression recognization and replacement.
//Note the CSE recognization and replacement only apply to the entire RHS of
//stmt. If user is going to handle sub-IR-tree operation, the corresponding
//simplification should have been done.
//e.g: ild:i32 ld:*<4> 'p1' is a CSE candidiate. However, the CSE in 'falserbr'
//determinator expression could not be recognized because the RHS of it
//starts from 'gt' operation, not 'ild'.
//  stpr $3:i32 =
//    ild:i32 ld:*<4> 'p1';
//  falsebr _$L1
//    gt:bool
//      ild:i32 ld:*<4> 'p1', 10;
//In order to trigger CSE optimization, user has to simplify 'falsebr' at
//least to:
//  stpr $4:i32 =
//    ild:i32 ld:*<4> 'p1';
//  falsebr _$L1
//    gt:bool $4, 10

class GCSE : public Pass {
    COPY_CONSTRUCTOR(GCSE);
    friend class PropVNVisitFunc;
    friend class PropExpVisitFunc;
private:
    bool m_enable_filter; //filter determines which expression can be CSE.
    bool m_is_in_ssa_form; //Set to true if PR is in SSA form.
    IRCFG * m_cfg;
    DUMgr * m_dumgr;
    AliasAnalysis * m_aa;
    PRSSAMgr * m_prssamgr;
    MDSSAMgr * m_mdssamgr;
    ExprTab * m_expr_tab;
    TypeMgr * m_tm;
    GVN * m_gvn;
    TG * m_tg;
    InferEVN * m_infer_evn;
    DefMiscBitSetMgr m_misc_bs_mgr;
    CSE2DeleTab m_exp2pr;
    VN2IRTab m_vn2exp;
    VN2IRTab m_evn2exp;
    List<IR*> m_newst_lst;
    ActMgr m_am;
protected:
    void copyVN(IR const* newir, IR const* oldir);
    bool canElimCVT(IR const* exp, IR const* gen) const;
    bool canElimRelationOp(IR const* exp, IR const* gen) const;

    bool doPropVNDirectStmt(IR * ir, GCSECtx const& ctx);
    bool doPropVNIndirectStmt(IR * ir, GCSECtx const& ctx);
    bool doPropVNCallStmt(IR * ir, GCSECtx const& ctx);
    bool doPropVNBrStmt(IR * ir, GCSECtx const& ctx);
    bool doPropVNRetStmt(IR * ir, GCSECtx const& ctx);
    bool doPropVNStmt(IR * ir, GCSECtx const& ctx);
    bool doPropBranch(IR * ir, MOD List<IR*> & livexp, GCSECtx const& ctx);
    bool doPropCall(IR * ir, MOD List<IR*> & livexp, GCSECtx const& ctx);
    bool doPropAssign(IR * ir, MOD List<IR*> & livexp, GCSECtx const& ctx);
    bool doPropReturn(IR * ir, MOD List<IR*> & livexp, GCSECtx const& ctx);
    bool doPropStmt(IR * ir, List<IR*> & livexp, GCSECtx const& ctx);
    bool doPropExp(IRBB * bb, List<IR*> & livexp, GCSECtx const& ctx);
    bool doPropVN(IRBB * bb, GCSECtx const& ctx);
    bool doPropVNInDomTreeOrder(
        xcom::DomTree const& domtree, GCSECtx const& ctx);
    bool doPropExpInDomTreeOrder(
        xcom::DomTree const& domtree, GCSECtx const& ctx);

    bool elim(IR * use, IR * use_stmt, IR * gen,
              IR * gen_stmt, GCSECtx const& ctx);

    // If find 'exp' is CSE, replace it with related PR.
    //NOTE: exp should be freed.
    bool findAndElim(IR * exp, IR * gen, GCSECtx const& ctx);

    //Generate delegate-PR of CSE at generation point.
    IR * genDelegatePR(IR const* gen, IR const* gen_stmt);

    bool handleCandidate(IR * exp, IRBB * bb, GCSECtx const& ctx);
    bool handleCandidateByExprRep(IR * exp, GCSECtx const& ctx);
    bool hasSideEffect(IR const* ir) const;

    virtual bool isCseCandidate(IR const* ir) const;
    bool initDepPass(MOD OptCtx & oc);

    bool isRelationOpCSEConcerned(IR const* exp) const
    { return exp->is_lt() || exp->is_le() || exp->is_gt() || exp->is_ge(); }

    //Replace 'use' CSE with PR that related to 'gen' CSE.
    //e.g: ...=a+b <--generate CSE
    //     ...
    //     ...=a+b <--use CSE
    //This function do replacement via gvn info.
    //use: the referrence of CSE.
    //use_stmt: the stmt contains use.
    //gen: the referrence of CSE.
    //NOTE: 'use' should be freed.
    //      'use' must be rhs of 'use_stmt'.
    //Return true if 'use' is replaced.
    bool elimCSE(IR * use, IR * use_stmt, IR const* gen, GCSECtx const& ctx);

    //Return true if 'use' is replaced.
    bool elimCseOfBranch(
        IR * use, IR * use_stmt, IR * gen, GCSECtx const& ctx);

    //Return true if 'use' is replaced.
    bool elimCseOfAssignment(
        IR * use, IR * use_stmt, IR * gen, GCSECtx const& ctx);

    //Reset local used data.
    void reset();

    //Process the expression in CSE generation.
    //This function do replacement via gvn info.
    //e.g: ...=a+b <--generate CSE
    //     ...
    //     ...=a+b <--use CSE
    //gen: generated CSE.
    void processCseGen(MOD IR * gen, MOD IR * gen_stmt, bool & change,
                       GCSECtx const& ctx);

    //If find 'exp' is CSE, replace it with related pr.
    //NOTE: exp should be freed.
    bool processCSE(MOD IR * ir, List<IR*> & livexp, GCSECtx const& ctx);

    virtual bool shouldBeCSE(IR const* det) const;

    void removeMayKill(IR * ir, MOD List<IR*> & livexp);

    bool useMDSSADU() const
    { return m_mdssamgr != nullptr && m_mdssamgr->is_valid(); }
    bool usePRSSADU() const
    { return m_prssamgr != nullptr && m_prssamgr->is_valid(); }
public:
    GCSE(Region * rg, GVN * gvn);
    virtual ~GCSE();

    //The function dump pass relative information before performing the pass.
    //The dump information is always used to detect what the pass did.
    //Return true if dump successed, otherwise false.
    virtual bool dumpBeforePass() const { return Pass::dumpBeforePass(); }

    //The function dump pass relative information.
    //The dump information is always used to detect what the pass did.
    //Return true if dump successed, otherwise false.
    virtual bool dump() const;
    void dumpEVN() const;

    virtual CHAR const* getPassName() const
    { return "Global Common Subexpression Elimination"; }
    PASS_TYPE getPassType() const { return PASS_GCSE; }
    ActMgr const& getActMgr() const { return m_am; }
    InferEVN * getInferEVN() const { return m_infer_evn; }

    bool perform(OptCtx & oc);
};

} //namespace xoc
#endif
