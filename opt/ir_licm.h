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
class LICM : public Pass {
    COPY_CONSTRUCTOR(LICM);
    Region * m_rg;
    DUMgr * m_du;
    IRCFG * m_cfg;
    RCE * m_rce;
    MDSSAMgr * m_mdssamgr;
    PRSSAMgr * m_prssamgr;
    ConstIRIter m_iriter;
    TypeMgr * m_tm;
    MDSystem * m_md_sys;
    SMemPool * m_pool;
    xcom::List<IR*> m_analysable_stmt_list;
    xcom::TMap<MD const*, UINT*> m_md2num;    
    //Record if the result of stmt is loop invariant
    //xcom::TTab<IR*> m_invariant_stmt;
    //Record if the result of stmt is loop invariant
    InvStmtList m_invariant_stmt;
    xcom::TTab<IR*> m_invariant_exp;
    xcom::TTab<LI<IRBB> const*> m_insert_guard_bb;

    void addInvariantStmt(IR * stmt);
    void addInvariantExp(IR * exp);

    //Regard entire RHS of ir as invariant expression, find valuable expression
    //and add it into invariant expression list.
    //Record the stmt in work list to next round analysis.
    //Return true if we add new invariant expression into list.
    //Note caller has to guarantee that whole RHS expressions of ir
    //are invariant.
    bool chooseExpAndStmt(IR * ir);

    bool doLoopTree(LI<IRBB> * li,
                    OUT bool & du_set_info_changed,
                    OUT bool & insert_bb,
                    OptCtx & oc);

    //Return true if gurard BB of LOOP 'li' has been inserted.
    bool hasInsertedGuardBB(LI<IRBB> const* li) const;

    //Return true if any stmt that is related to invariant stmt
    //is moved outside from loop, return false if there is stmt that
    //prevents 'exp' from being hoisted from the loop.
    bool handleDefByDUChain(IR const* exp,
                            OUT IRBB * prehead,
                            OUT LI<IRBB> * li);
    //Return true if any stmt is moved outside from loop.
    bool hoistInvariantStmt(OUT IR * stmt,
                            OUT IRBB * prehead,
                            OUT LI<IRBB> * li);
    //Hoist candidate IR to preheader BB.
    bool hoistCand(OUT IRBB * prehead, OUT LI<IRBB> * li, OUT bool & insert_bb);
    //This function will maintain RPO of generated guard BB.
    //Return true if BB or STMT changed.
    bool hoistCandHelper(IRBB const* backedge_bb,
                         OUT bool & insert_guard_bb,
                         OUT IR * cand_exp,
                         OUT IRBB * prehead,
                         OUT LI<IRBB> * li);

    //Return true if ir dominate all USE which in loop.
    bool is_dom_all_use_in_loop(IR const* ir, LI<IRBB> * li);
    bool isInvariantStmt(IR const* ir) const;
    bool isStmtCanBeHoisted(IR * stmt, IRBB * backedge_bb);
    
    //Return true if md modified in loop only once.
    bool isUniqueDef(MD const* md) const;

    //Insert guard controlling BB to predominate the execution of 'prehead'.
    //This function will maintain RPO of generated guard BB.
    //prehead: preheader BB of loop.
    //loophead: loopheader BB of loop.
    //e.g:given BB_prehead, insert BB_guard
    //  BB_prehead
    //  |
    //  v
    //  BB_loophead
    //after insertion:
    //  BB_guard
    //  |  |
    //  |  v
    //  |  BB_prehead
    //  | / 
    //  |/
    //  v
    //  BB_loophead
    IRBB * insertGuardBB(IRBB * prehead, IRBB * loophead);

    //Return true if loop body is executed conditionally which is in charged of
    //the judgement stmt in loophead BB.
    //e.g:Return true for while-do loop, and false for do-while loop.
    bool isLoopExecConditional(LI<IRBB> const* li) const;

    //Try to move and check that each definitions of candidate has been
    //already hoisted from loop.
    bool tryMoveAllDefStmtOutFromLoop(IR const* c,
                                     IRBB * prehead,
                                     OUT LI<IRBB> * li);

    //Return true if any stmt is moved outside from loop.
    bool tryHoistDefStmt(IR * def,
                         IN IRBB * prehead,
                         IN LI<IRBB> * li);
    //Try to evaluate the value of loop execution condition.
    //Returnt true if this function evaluated successfully,
    //otherwise return false.
    bool tryEvalLoopExecCondition(LI<IRBB> const* li,  
                                  bool & must_true,
                                  bool & must_false) const;

    //Scan operand to find invariant candidate.
    //'isLegal': set to true if loop is legal to perform invariant motion.
    //           otherwise set to false to prohibit code motion.
    //Return true if find loop invariant expression.
    bool scanOpnd(IN LI<IRBB> * li, bool * isLegal, bool first_scan);

    //Propagate invariant property to result.
    //This operation will generate more invariant.
    //This function will modify m_invariant_stmt, record if the result of
    //stmt is loop invariant.
    //Note this function assumes whole RHS tree of stmt in
    //analysable_stmt_list are loop invariant expressions.
    bool scanResult();

    //hoistCand may append stmt into BB which has down-boundary stmt.
    //That makes BB invalid. Split such invalid BB into two or more BBs.
    bool splitBBIfNeeded(IRBB * bb);

    void updateMD2Num(IR * ir);
    bool useMDSSADU() const
    { return m_mdssamgr != NULL && m_mdssamgr->is_valid(); }
    bool usePRSSADU() const
    { return m_prssamgr != NULL && m_prssamgr->is_valid(); }

    void * xmalloc(UINT size)
    {
        ASSERT0(m_pool != NULL);
        void * p = smpoolMallocConstSize(sizeof(UINT), m_pool);
        ASSERT0(p != NULL);
        ::memset(p, 0, size);
        return p;
    }
public:
    explicit LICM(Region * rg)
    {
        ASSERT0(rg != NULL);
        m_rg = rg;
        m_du = rg->getDUMgr();
        m_cfg = rg->getCFG();
        m_tm = rg->getTypeMgr();
        m_md_sys = rg->getMDSystem();
        ASSERT0(m_cfg && m_du && m_md_sys && m_tm);
        m_pool = smpoolCreate(4 * sizeof(UINT), MEM_CONST_SIZE);
        m_mdssamgr = NULL;
        m_prssamgr = NULL;
        m_rce = NULL;
    }
    virtual ~LICM() { smpoolDelete(m_pool); }

    bool analysis(IN LI<IRBB> * li);

    //Given loop info li, dump the invariant stmt and invariant expression.
    void dumpInvariantExpStmt(LI<IRBB> const* li) const;
    virtual bool dump() const;

    //Consider whether exp is worth hoisting.
    bool isWorthHoist(IR * exp)
    {
        CHECK_DUMMYUSE(exp);
        ASSERT0(exp->is_exp());
        //If IR_has_sideeffect(ir) is true, that means exp can not be removed,
        //but still can be moved.
        return !exp->isNoMove();
    }

    Region * getRegion() const { return m_rg; }
    virtual CHAR const* getPassName() const
    { return "Loop Invariant Code Motion"; }
    PASS_TYPE getPassType() const { return PASS_LICM; }

    virtual bool perform(OptCtx & oc);
};

} //namespace xoc
#endif
