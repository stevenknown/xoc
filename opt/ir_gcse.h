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

    void pick_eh()
    {
        List<IRBB*> * bbs = m_rg->getBBList();
        for (IRBB * bb = bbs->get_head(); bb != nullptr; bb = bbs->get_next()) {
            if (bb->isExceptionHandler()) {
                removeVertex(bb->id());
            }
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


class GCSE : public Pass {
private:
    bool m_enable_filter; //filter determines which expression can be CSE.
    bool m_is_in_ssa_form; //Set to true if PR is in SSA form.
    IRCFG * m_cfg;
    DUMgr * m_dumgr;
    AliasAnalysis * m_aa;
    PRSSAMgr * m_ssamgr;
    MDSSAMgr * m_mdssamgr;
    ExprTab * m_expr_tab;
    TypeMgr * m_tm;
    GVN * m_gvn;
    TG * m_tg;
    OptCtx const* m_oc;
    DefMiscBitSetMgr m_misc_bs_mgr;
    TMap<IR*, IR*> m_exp2pr;
    TMap<VN const*, IR*> m_vn2exp;
    List<IR*> m_newst_lst;

    //ONLY USED FOR DEBUG PURPOSE
    UINT m_num_of_elim;
    Vector<UINT> m_elimed;
protected:
    virtual bool doPropStmt(IR * ir, List<IR*> & livexp);
    bool doProp(IRBB * bb, List<IR*> & livexp);
    bool doPropVN(IRBB * bb, UINT entry_id);
    bool doPropVNInDomTreeOrder(xcom::Graph const* domtree);
    bool doPropInDomTreeOrder(xcom::Graph const* domtree);

    bool elim(IR * use, IR * use_stmt, IR * gen, IR * gen_stmt);

    bool findAndElim(IR * exp, IR * gen);

    OptCtx const* getOptCtx() const { return m_oc; }

    void handleCandidate(IR * exp, IRBB * bb, UINT entry_id, bool & change);

    bool isCseCandidate(IR * ir);

    void elimCseAtDirectMemOp(IR * use, IR * use_stmt, IR * gen);
    void elimCseAtCall(IR * use, IR * use_stmt, IR * gen);
    void elimCseAtReturn(IR * use, IR * use_stmt, IR * gen);
    void elimCseAtBranch(IR * use, IR * use_stmt, IR * gen);

    void processCseGen(IR * cse, IR * cse_stmt, bool & change);
    bool processCse(IR * ir, List<IR*> & livexp);

    bool shouldBeCse(IR * det);
public:
    GCSE(Region * rg, GVN * gvn) : Pass(rg)
    {
        ASSERT0(rg);
        m_cfg = rg->getCFG();
        m_dumgr = rg->getDUMgr();
        m_aa = rg->getAA();
        ASSERT0(m_dumgr && m_aa && m_cfg);
        m_expr_tab = nullptr;
        m_tm = rg->getTypeMgr();
        m_gvn = gvn;
        m_tg = nullptr;
        m_oc = nullptr;
        m_is_in_ssa_form = false;
        m_ssamgr = nullptr;
        m_mdssamgr = nullptr;
        m_num_of_elim = 0;
    }
    COPY_CONSTRUCTOR(GCSE);
    virtual ~GCSE() {}

    //The function dump pass relative information before performing the pass.
    //The dump information is always used to detect what the pass did.
    //Return true if dump successed, otherwise false.
    virtual bool dumpBeforePass() const { return Pass::dumpBeforePass(); }

    //The function dump pass relative information.
    //The dump information is always used to detect what the pass did.
    //Return true if dump successed, otherwise false.
    virtual bool dump() const;

    virtual CHAR const* getPassName() const
    { return "Global Command Subexpression Elimination"; }
    PASS_TYPE getPassType() const { return PASS_GCSE; }

    bool perform(OptCtx & oc);
};

} //namespace xoc
#endif
