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

class TG : public DGraph {
protected:
    Region * m_ru;

protected:
    virtual void * cloneEdgeInfo(Edge *)
    { return NULL; }

    virtual void * cloneVertexInfo(Vertex *)
    { return NULL; }

public:
    explicit TG(Region * ru) { m_ru = ru; }
    COPY_CONSTRUCTOR(TG);

    void pick_eh()
    {
        List<IRBB*> * bbs = m_ru->get_bb_list();
        for (IRBB * bb = bbs->get_head(); bb != NULL; bb = bbs->get_next()) {
            if (bb->is_exp_handling()) {
                removeVertex(BB_id(bb));
            }
        }
    }

    inline void computeDomAndIdom()
    {
        if (!computeDom()) { ASSERT0(0); }
        if (!computeIdom()) { ASSERT0(0); }
    }

    inline void computePdomAndIpdom(Vertex * root)
    {
        if (!computePdomByRpo(root, NULL)) { ASSERT0(0); }
        if (!computeIpdom()) { ASSERT0(0); }
    }
};


class IR_GCSE : public Pass {
protected:
    bool m_enable_filter; //filter determines which expression can be CSE.
    bool m_is_in_ssa_form; //Set to true if PR is in SSA form.
    Region * m_ru;
    IR_CFG * m_cfg;
    IR_DU_MGR * m_du;
    IR_AA * m_aa;
    IR_SSA_MGR * m_ssamgr;
    IR_EXPR_TAB * m_expr_tab;
    TypeMgr * m_dm;
    IR_GVN * m_gvn;
    TG * m_tg;
    DefMiscBitSetMgr * m_misc_bs_mgr;
    TMap<IR*, IR*> m_exp2pr;
    TMap<VN const*, IR*> m_vn2exp;
    List<IR*> m_newst_lst;

    bool doProp(IRBB * bb, List<IR*> & livexp);
    bool doPropVN(IRBB * bb, UINT entry_id);
    bool elim(IR * use, IR * use_stmt, IR * gen, IR * gen_stmt);
    bool findAndElim(IR * exp, IR * gen);
    void handleCandidate(IR * exp, IRBB * bb, UINT entry_id, bool & change);
    bool isCseCandidate(IR * ir);
    void elimCseAtStore(IR * use, IR * use_stmt, IR * gen);
    void elimCseAtCall(IR * use, IR * use_stmt, IR * gen);
    void elimCseAtReturn(IR * use, IR * use_stmt, IR * gen);
    void elimCseAtBranch(IR * use, IR * use_stmt, IR * gen);
    void prcessCseGen(IR * cse, IR * cse_stmt, bool & change);
    bool prcessCse(IR * ir, List<IR*> & livexp);
    bool shouldBeCse(IR * det);
public:
    IR_GCSE(Region * ru, IR_GVN * gvn)
    {
        ASSERT0(ru);
        m_ru = ru;
        m_cfg = ru->get_cfg();
        m_du = ru->get_du_mgr();
        m_aa = ru->get_aa();
        ASSERT0(m_du && m_aa);
        m_expr_tab = NULL;
        m_dm = ru->get_type_mgr();
        m_gvn = gvn;
        m_misc_bs_mgr = ru->getMiscBitSetMgr();
        m_tg = NULL;
        m_is_in_ssa_form = false;
        m_ssamgr = NULL;
    }
    virtual ~IR_GCSE() {}
    virtual CHAR const* get_pass_name() const
    { return "Global Command Subscript Elimination"; }

    PASS_TYPE get_pass_type() const { return PASS_GCSE; }

    bool perform(OptCTX & oc);
};

} //namespace xoc
#endif
