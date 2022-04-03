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
//Note in order to reduce the complexity of LICM, the pass only handle the
//scenario that whole RHS of stmt is loop-invariant. For cases that
//anticipating to scan and hoist kid IR tree in RHS, will be handled in
//Register Promotion.
class LICM : public Pass {
    class IRListMgr {
        List<IRList*> m_lst;
        List<IRList*> m_free_lst;
    public:
        ~IRListMgr() { clean(); }
        IRList * alloc()
        {
            IRList * freed = m_free_lst.remove_head();
            if (freed != nullptr) {
                return freed;
            }
            freed = new IRList();
            m_lst.append_tail(freed);
            return freed;
        }
        void clean()
        {
            for (IRList * l = m_lst.get_head(); l != nullptr;
                 l = m_lst.get_next()) {
                delete l;
            }
            m_lst.clean();
            m_free_lst.clean();
        }
        void free(IRList * lst)
        {
            lst->clean();
            m_free_lst.append_head(lst);
        }
    };

    class CompareFuncOfIR {
    public:
        bool is_less(IR * t1, IR * t2) const { return t1->id() < t2->id(); }
        bool is_equ(IR * t1, IR * t2) const { return t1 == t2; }
        IR * createKey(IR * t) { return t; }
    };

    class HoistCtx {
    public:
        //Top-down propagate information.
        DomTree * domtree;

        //Top-down and botom-up propagate information.
        OptCtx * oc;

        //Bottom-up propagate information.
        //Record whether guard-bb of preheader has been inserted.
        //The flag is used only for one LoopInfo.
        bool inserted_guard_bb;

        //Bottom-up collect information.
        //Record whether the CFG changed that always because inserting BB.
        //The flag is used for entire dom-tree.
        bool cfg_changed;

        //Bottom-up collect information.
        //Record whether classic DUSet changed.
        //The flag is used for entire dom-tree.
        bool duset_changed;
        
        //Bottom-up propagate information.
        //Record cand-stmt that has been hoisted out of loop.
        List<IR*> hoisted_stmt;
    public:
        HoistCtx(OptCtx * t) :
            domtree(nullptr), oc(t), inserted_guard_bb(false),
            cfg_changed(false), duset_changed(false) {}
        ~HoistCtx()
        {
            if (domtree != nullptr) {
                delete domtree;
                domtree = nullptr;
            }
        }
        void buildDomTree(IRCFG * cfg)
        {
            ASSERT0(oc->is_dom_valid());
            if (domtree == nullptr) {
                domtree = new DomTree();
            } else {
                domtree->erase();
            }
            cfg->genDomTree(*domtree);
        }
        void cleanAfterLoop()
        {
            hoisted_stmt.clean();
            inserted_guard_bb = false;
        }
    };
    COPY_CONSTRUCTOR(LICM);
    DUMgr * m_du;
    IRCFG * m_cfg;
    RCE * m_rce;
    MDSSAMgr * m_mdssamgr;
    PRSSAMgr * m_prssamgr;
    TypeMgr * m_tm;
    MDSystem * m_md_sys;
    SMemPool * m_pool;
    xcom::TTab<LI<IRBB> const*> m_insert_guard_bb;
    IRListMgr m_irs_mgr;
    //The list records stmts that are possibly invariant stmt.
    //And these stmts will be analyzed in scanResult().
    xcom::List<IR*> m_analysable_stmt_list;
    xcom::TMap<MD const*, UINT*> m_md2num;
    //Record if the result of stmt is loop invariant
    InvStmtList m_invariant_stmt;
    //Record if the expression is loop invariant
    xcom::TTab<IR*, CompareFuncOfIR> m_invariant_exp;
    //Record if the expression is hoist candidate
    //Note hoist-candidate is not always same as invariant-exp.
    xcom::TTab<IR*, CompareFuncOfIR> m_hoist_cand;
private:
    //Post-process hoisted-stmt.
    void addSSADUChainForHoistedStmt(HoistCtx & ctx);
    void addInvariantStmt(IR * stmt);
    void addInvariantExp(IR * exp);
    void addInvariantExp(IRList const& list);
    void addHoistCand(IRList const& list);
    void addSSADUChainForExp(IR * exp, HoistCtx const& ctx);

    //Scan whole IR tree to find loop invariant expression
    //and add it into invariant expression list.
    //Return true if at least one invariant expression added into list.
    //ir: the root IR.
    //all_exp_invariant: true if all IR expressions start at 'ir' are
    //                   loop invariant.
    bool chooseExp(LI<IRBB> * li, IR * ir, IRIter & irit,
                   OUT bool * all_rhs_exp_invariant, OUT IRList * invlist);
    bool chooseStmt(LI<IRBB> * li, IR * ir, IRIter & irit);
    bool chooseSTandSTPR(LI<IRBB> * li, IR * ir, IRIter & irit);
    bool chooseIST(LI<IRBB> * li, IR * ir, IRIter & irit);
    bool chooseSTARRAY(LI<IRBB> * li, IR * ir, IRIter & irit);
    bool chooseCallStmt(LI<IRBB> * li, IR * ir, IRIter & irit);
    bool chooseBranch(LI<IRBB> * li, IR * ir,IRIter & irit);
    bool chooseSwitch(LI<IRBB> * li, IR * ir, IRIter & irit);
    void cleanBeforeLoop();
    void checkAndInsertGuardBB(LI<IRBB> const* li, IRBB * prehead,
                               HoistCtx & ctx);

    bool doLoopTree(LI<IRBB> * li, OUT HoistCtx & ctx);

    void hoistStmt(MOD IR * def, MOD IRBB * prehead, OUT HoistCtx & ctx);

    //Return true if gurard BB of LOOP 'li' has been inserted.
    bool hasInsertedGuardBB(LI<IRBB> const* li) const;

    //Return true if any stmt that is related to invariant stmt
    //is moved outside from loop, return false if there is stmt that
    //prevents 'exp' from being hoisted from the loop.
    bool hoistDefByDUChain(IR const* exp, OUT IRBB * prehead,
                           OUT LI<IRBB> * li, MOD HoistCtx & ctx);

    //Hoist candidate IR to preheader BB.
    bool hoistCand(OUT IRBB * prehead, OUT LI<IRBB> * li, OUT HoistCtx & ctx);

    //Return true if BB or STMT changed.
    bool hoistCandHelper(OUT IR * cand_exp, OUT IRBB * prehead,
                         OUT LI<IRBB> * li, OUT HoistCtx & ctx);

    //Return true if stmt is marked and collected into invariant-stmt set.
    bool markedAsInvStmt(IR const* ir) const;
    //Return true if exp is marked and collected into invariant-stmt set.
    bool markedAsInvExp(IR const* ir) const;

    //Return true if md modified in loop only once.
    bool isUniqueDef(MD const* md) const;

    //Return true if loop body is executed conditionally which is in charged of
    //the judgement stmt in loophead BB.
    //e.g:Return true for while-do loop, and false for do-while loop.
    bool isLoopExecConditional(LI<IRBB> const* li) const;

    //Try to move and check that each definitions of candidate has been
    //already hoisted from loop.
    bool tryMoveAllDefStmtOutFromLoop(IR const* c, IRBB * prehead,
                                     OUT LI<IRBB> * li, MOD HoistCtx & ctx);

    //Return true if any stmt is moved outside from loop.
    bool tryHoistDefStmt(MOD IR * def, MOD IRBB * prehead, MOD LI<IRBB> * li,
                         MOD HoistCtx & ctx);

    //Try to evaluate the value of loop execution condition.
    //Returnt true if this function evaluated successfully,
    //otherwise return false.
    bool tryEvalLoopExecCondition(LI<IRBB> const* li, OUT bool & must_true,
                                  OUT bool & must_false) const;

    //Scan expression to find invariant candidate.
    //islegal: set to true if loop is legal to perform invariant motion.
    //         otherwise set to false to prohibit code motion.
    //Return true if find loop invariant expression.
    bool scanLoopBody(IN LI<IRBB> * li, bool * islegal, bool first_scan);

    //Return true if find loop invariant expression.
    //Note the function try to recognize the loop invariant expression and stmt.
    //So far, the function only regard whole RHS IR tree as loop invariant ONLY
    //if all kid IR trees in RHS are loop invariant.
    //TODO: recognize the partial IR tree that is loop invariant.
    bool scanBB(IRBB * bb, IN LI<IRBB> * li, bool * islegal, bool first_scan);

    //Propagate invariant property to result.
    //This operation will generate more invariant.
    //This function will modify m_invariant_stmt, record if the result of
    //stmt is loop invariant.
    //Note this function assumes whole RHS tree of stmt in
    //analysable_stmt_list are loop invariant expressions.
    bool scanResult();

    //hoistCand may append stmt into BB which has down-boundary stmt.
    //That makes BB invalid. Split such invalid BB into two or more BBs.
    bool splitBBIfNeeded(IRBB * bb, OptCtx & oc);

    //Return true if any stmt is moved outside from loop.
    bool tryHoistStmt(IR * stmt, OUT IRBB * prehead, OUT LI<IRBB> * li,
                      OUT HoistCtx & ctx);

    void updateMD2Num(IR * ir);
    bool useMDSSADU() const
    { return m_mdssamgr != nullptr && m_mdssamgr->is_valid(); }
    bool usePRSSADU() const
    { return m_prssamgr != nullptr && m_prssamgr->is_valid(); }

    void * xmalloc(UINT size)
    {
        ASSERT0(m_pool != nullptr);
        void * p = smpoolMallocConstSize(sizeof(UINT), m_pool);
        ASSERT0(p != nullptr);
        ::memset(p, 0, size);
        return p;
    }
public:
    explicit LICM(Region * rg) : Pass(rg)
    {
        ASSERT0(rg != nullptr);
        m_du = rg->getDUMgr();
        m_cfg = rg->getCFG();
        m_tm = rg->getTypeMgr();
        m_md_sys = rg->getMDSystem();
        ASSERT0(m_cfg && m_du && m_md_sys && m_tm);
        m_pool = smpoolCreate(4 * sizeof(UINT), MEM_CONST_SIZE);
        m_mdssamgr = nullptr;
        m_prssamgr = nullptr;
        m_rce = nullptr;
    }
    virtual ~LICM() { smpoolDelete(m_pool); }

    bool analysis(IN LI<IRBB> * li);

    //Given loop info li, dump the invariant stmt and invariant expression.
    void dumpInvariantExpStmt(LI<IRBB> const* li) const;
    virtual bool dump() const;

    //Consider whether exp is worth hoisting.
    bool isWorthHoist(IR * exp)
    {
        CHECK0_DUMMYUSE(exp);
        ASSERT0(exp->is_exp());
        //If IR_has_sideeffect(ir) is true, that means exp can not be removed,
        //but still can be moved.
        return !exp->isNoMove(true);
    }

    virtual CHAR const* getPassName() const
    { return "Loop Invariant Code Motion"; }
    PASS_TYPE getPassType() const { return PASS_LICM; }

    virtual bool perform(OptCtx & oc);
};

} //namespace xoc
#endif
