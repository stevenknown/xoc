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
#ifndef _IR_LICM_H_
#define _IR_LICM_H_

namespace xoc {

//Loop Invariant code Motion.
class IR_LICM : public Pass {
protected:
    Region * m_ru;
    IR_AA * m_aa;
    IR_DU_MGR * m_du;
    IR_CFG * m_cfg;
    ConstIRIter m_iriter;
    ConstMDIter m_mditer;
    TypeMgr * m_tm;
    MDSystem * m_md_sys;
    SMemPool * m_pool;
    List<IR*> m_analysable_stmt_list;
    TMap<MD const*, UINT*> m_md2num;

    //Indicate whether current IR is tranformed to ssa form.
    PRSSAMgr * m_ssamgr;

protected:
    bool doLoopTree(LI<IRBB> * li,
                    OUT bool & du_set_info_changed,
                    OUT bool & insert_bb,
                    TTab<IR*> & invariant_stmt,
                    TTab<IR*> & invariant_exp);

    bool checkDefStmt(
            IR * def,
            TTab<IR*> & invariant_stmt,
            IN IRBB * prehead,
            IN LI<IRBB> * li);

    inline bool is_stmt_dom_its_use(
                    IR const* stmt,
                    IR const* use,
                    LI<IRBB> const* li,
                    IRBB const* stmtbb) const;
    bool is_dom_all_use_in_loop(IR const* ir, LI<IRBB> * li);

    bool hoistInvariantStmt(
            TTab<IR*> & invariant_stmt,
            IR * stmt,
            IRBB * prehead,
            IN LI<IRBB> * li);
    bool hoistCand(TTab<IR*> & invariant_exp,
                   TTab<IR*> & invariant_stmt,
                   IN IRBB * prehead,
                   IN LI<IRBB> * li);

    bool markExpAndStmt(IR * ir, TTab<IR*> & invariant_exp);

    bool scanOpnd(IN LI<IRBB> * li,
                  OUT TTab<IR*> & invariant_exp,
                  TTab<IR*> & invariant_stmt,
                  bool * isLegal, bool first_scan);
    bool scanResult(OUT TTab<IR*> & invariant_stmt);
    bool isStmtCanBeHoisted(IR * stmt, IRBB * backedge_bb);
    bool isUniqueDef(MD const* md);
    void updateMD2Num(IR * ir);

    void * xmalloc(UINT size)
    {
        ASSERT0(m_pool != NULL);
        void * p = smpoolMallocConstSize(sizeof(UINT), m_pool);
        ASSERT0(p != NULL);
        ::memset(p, 0, size);
        return p;
    }
public:
    explicit IR_LICM(Region * rg)
    {
        ASSERT0(rg != NULL);
        m_ru = rg;
        m_aa = rg->getAA();
        m_du = rg->getDUMgr();
        m_cfg = rg->getCFG();
        m_tm = rg->getTypeMgr();
        m_md_sys = rg->getMDSystem();
        ASSERT0(m_cfg && m_du && m_md_sys && m_tm);
        m_pool = smpoolCreate(4 * sizeof(UINT), MEM_CONST_SIZE);
        m_ssamgr = NULL;
    }
    COPY_CONSTRUCTOR(IR_LICM);
    virtual ~IR_LICM() { smpoolDelete(m_pool); }

    bool analysis(IN LI<IRBB> * li,
                  OUT TTab<IR*> & invariant_stmt,
                  OUT TTab<IR*> & invariant_exp);

    //Given loop info li, dump the invariant stmt and invariant expression.
    void dumpInvariantExpStmt(
            TTab<IR*> const& invariant_stmt,
            TTab<IR*> const& invariant_exp);

    //Consider whether exp is worth hoisting.
    bool isWorthHoist(IR * exp)
    {
        CHECK_DUMMYUSE(exp);
        ASSERT0(exp->is_exp());
        //If IR_has_sideeffect(ir) is true, that means exp can not be removed,
        //but still can be moved.
        return !IR_no_move(exp);
    }

    virtual CHAR const* getPassName() const
    { return "Loop Invariant Code Motion"; }

    PASS_TYPE getPassType() const { return PASS_LICM; }

    virtual bool perform(OptCtx & oc);
};

} //namespace xoc
#endif
