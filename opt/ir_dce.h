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
#ifndef _IR_DCE_H_
#define _IR_DCE_H_

namespace xoc {

class EFFECT_STMT : public BitSet {
public:
    void bunion(INT elem)
    {
        BitSet::bunion(elem);
    }
};

//Perform dead code and redundant control flow elimination.
class IR_DCE : public Pass {
protected:
    MDSystem * m_md_sys;
    TypeMgr * m_tm;
    Region * m_ru;
    IR_CFG * m_cfg;
    CDG * m_cdg;
    IR_DU_MGR * m_du;
    ConstIRIter m_citer;
    bool m_is_elim_cfs; //Eliminate control flow structure if necessary.

    //Whether utilize MD du chain to find effect stmt.
    //If the value is false, all memory operations are considered used
    //except the operations which operate on PR.
    bool m_is_use_md_du;

    void fix_control_flow(List<IRBB*> & bblst, List<C<IRBB*>*> & ctlst);
    bool find_effect_kid(IN IRBB * bb, IN IR * ir,
                         IN EFFECT_STMT & is_stmt_effect);
    bool preserve_cd(IN OUT BitSet & is_bb_effect,
                     IN OUT EFFECT_STMT & is_stmt_effect,
                     IN OUT List<IR const*> & act_ir_lst);
    void mark_effect_ir(IN OUT EFFECT_STMT & is_stmt_effect,
                        IN OUT BitSet & is_bb_effect,
                        IN OUT List<IR const*> & work_list);

    bool is_effect_write(VAR * v) const
    { return VAR_is_global(v) || VAR_is_volatile(v); }

    bool is_effect_read(VAR * v) const { return VAR_is_volatile(v); }

    bool is_cfs(IR const* ir) const
    {
        switch (ir->get_code()) {
        case IR_TRUEBR:
        case IR_FALSEBR:
        case IR_GOTO:
        case IR_IGOTO:
            return true;
        default: ASSERT0(ir->isStmtInBB());
        }
        return false;
    }

    void iter_collect(IN OUT EFFECT_STMT & is_stmt_effect,
                      IN OUT BitSet & is_bb_effect,
                      IN OUT List<IR const*> & work_list);
    void record_all_ir(IN OUT Vector<Vector<IR*>*> & all_ir);
    void revise_successor(IRBB * bb, C<IRBB*> * bbct, BBList * bbl);

    bool check_stmt(IR const* ir);
    bool check_call(IR const* ir);
public:
    explicit IR_DCE(Region * rg)
    {
        ASSERT0(rg != NULL);
        m_ru = rg;
        m_tm = rg->getTypeMgr();
        m_cfg = rg->getCFG();
        m_du = rg->getDUMgr();
        m_md_sys = rg->getMDSystem();
        ASSERT0(m_cfg && m_du && m_md_sys && m_tm);
        m_is_elim_cfs = false;
        m_is_use_md_du = true;
        m_cdg = NULL;
    }
    virtual ~IR_DCE() {}

    void dump(EFFECT_STMT const& is_stmt_effect,
              BitSet const& is_bb_effect,
              IN Vector<Vector<IR*>*> & all_ir);

    virtual CHAR const* getPassName() const
    { return "Dead Code Eliminiation"; }
    virtual PASS_TYPE getPassType() const { return PASS_DCE; }

    void set_elim_cfs(bool doit) { m_is_elim_cfs = doit; }
    void set_use_md_du(bool use_md_du) { m_is_use_md_du = use_md_du; }

    virtual bool perform(OptCtx & oc);
};

} //namespace xoc
#endif
