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

class HoistCtx {
public:
    //Top-down propagate information.
    //All inherited context shares the same DomTree object.
    DomTree * domtree;

    //Top-down and bottom-up propagate information.
    OptCtx * oc;

    //Top-down and bottom-up propagate information.
    IRCFG const* cfg;

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

    //Bottom-up collect information.
    //Record whether stmt has been hoisted or changed in-place.
    bool stmt_changed;
public:
    HoistCtx(OptCtx * t, DomTree * dt, IRCFG * g) :
        domtree(dt), oc(t), cfg(g), inserted_guard_bb(false),
        cfg_changed(false), duset_changed(false), stmt_changed(false)
    {}
    HoistCtx(HoistCtx const& src) :
        inserted_guard_bb(false), cfg_changed(false), duset_changed(false),
        stmt_changed(false)
    { copyTopDownInfo(src); }
    ~HoistCtx() {}

    void buildDomTree(IRCFG * c)
    {
        ASSERT0(oc->is_dom_valid());
        ASSERT0(domtree);
        domtree->erase();
        c->genDomTree(*domtree);
    }

    void cleanAfterLoop() { inserted_guard_bb = false; }
    void copyTopDownInfo(HoistCtx const& src)
    {
        domtree = src.domtree;
        oc = src.oc;
        cfg = src.cfg;
    }

    void unionBottomUpInfo(HoistCtx const& src)
    {
        inserted_guard_bb |= src.inserted_guard_bb;
        cfg_changed |= src.cfg_changed;
        duset_changed |= src.duset_changed;
        stmt_changed |= src.stmt_changed;
    }

    bool verifyDomTree() const
    {
        ASSERT0(domtree);
        return domtree->verify(*cfg);
    }
};


class LICMAnaCtx {
    COPY_CONSTRUCTOR(LICMAnaCtx);
protected:
    //Record if the stmt is loop invariant.
    InvStmtList m_invariant_stmt;

    //Record if the expression is loop invariant
    IRTab m_invariant_exp;

    //The list records stmts that are possibly invariant stmt.
    //And these stmts will be analyzed in scanResult().
    //It is used for temporary purpose.
    IRList m_analysable_stmt_list;

    //Record if the expression is hoist-candidate.
    //Note hoist-candidate is not always can be hoisted legally at last. It
    //depends on other conditions, such like the inside-loop DEF stmt has to
    //be hoisted first. Whether a exp/stmt can be hoisted will be determined
    //at hoistCand() finally.
    IRTab m_hoist_cand;
    ComputeMD2DefCnt m_md2defcnt;
public:
    LICMAnaCtx(Region const* rg, LI<IRBB> const* li) :
        m_md2defcnt(rg, li) {}

    void addInvStmt(IR * stmt)
    {
        ASSERT0(!m_invariant_stmt.find(stmt));
        m_invariant_stmt.append_tail(stmt);
    }
    void addInvExp(IR * exp)
    {
        ASSERT0(!m_invariant_exp.find(exp));
        m_invariant_exp.append(exp);
    }
    void addHoistCand(IRList const& list);
    void addHoistCand(IR * ir);

    void dump(Region const* rg) const;

    InvStmtList & getInvStmtList() { return m_invariant_stmt; }
    InvStmtList const& getInvStmtList() const { return m_invariant_stmt; }
    IRTab & getInvExpTab() { return m_invariant_exp; }
    IRTab const& getInvExpTab() const { return m_invariant_exp; }
    IRTab & getCandTab() { return m_hoist_cand; }
    IRTab const& getConstCandTab() const { return m_hoist_cand; }
    IRList & getAnaStmtList() { return m_analysable_stmt_list; }
    ComputeMD2DefCnt & getMD2DefCnt() { return m_md2defcnt; }

    //Return true if stmt is marked and collected into invariant-stmt set.
    bool isInvStmt(IR const* stmt) const
    {
        ASSERT0(stmt->is_stmt());
        return m_invariant_stmt.find(const_cast<IR*>(stmt));
    }

    //Return true if exp is marked and collected into invariant-stmt set.
    bool isInvExp(IR const* exp) const
    {
        ASSERT0(exp->is_exp());
        return m_invariant_exp.find(const_cast<IR*>(exp));
    }

    //Return true if ir is the unqiue DEF stmt of MustRef in the loop.
    bool isUniqueDef(IR const* ir) const { return m_md2defcnt.isUniqueDef(ir); }

    //Return true if all kids of 'ir' are inv-exp.
    bool verifyInvStmt(IR const* ir) const;
};


//Loop Invariant code Motion.
//Note in order to reduce the complexity of LICM, the pass only handle the
//scenario that whole RHS of stmt is loop-invariant. For cases that
//anticipating to scan and hoist kid IR tree in RHS, will be handled in
//Register Promotion.
class LICM : public Pass {
    friend class InsertPreheaderMgr;
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
protected:
    COPY_CONSTRUCTOR(LICM);
    BYTE m_is_hoist_stmt:1;
    BYTE m_is_aggressive:1; //true to apply LICM along with RCE
    DUMgr * m_dumgr;
    IRCFG * m_cfg;
    //LICM use RCE to determine whether a branch must-execute.
    RCE * m_rce;
    MDSSAMgr * m_mdssamgr;
    PRSSAMgr * m_prssamgr;
    TypeMgr * m_tm;
    MDSystem * m_md_sys;
    SMemPool * m_pool;
    xcom::TTab<LI<IRBB> const*> m_insert_guard_bb;
    IRListMgr m_irs_mgr;
    ActMgr m_act_mgr;
protected:
    void clean();
    bool chooseConst(IR * ir, OUT bool * all_exp_invariant,
                     OUT LICMAnaCtx & anactx);
    bool chooseBin(LI<IRBB> const* li, IR * ir, IRIter & irit,
                   OUT bool * all_exp_invariant, OUT IRList * invlist,
                   OUT LICMAnaCtx & anactx);
    bool chooseUna(LI<IRBB> const* li, IR * ir, IRIter & irit,
                   OUT bool * all_exp_invariant, OUT IRList * invlist,
                   OUT LICMAnaCtx & anactx);
    bool chooseArray(LI<IRBB> const* li, IR * ir, IRIter & irit,
                     OUT bool * all_exp_invariant, OUT IRList * invlist,
                     OUT LICMAnaCtx & anactx);
    bool chooseILD(LI<IRBB> const* li, IR * ir, IRIter & irit,
                   OUT bool * all_exp_invariant, OUT IRList * invlist,
                   OUT LICMAnaCtx & anactx);
    bool choosePR(LI<IRBB> const* li, IR * ir, IRIter & irit,
                  OUT bool * all_exp_invariant, OUT IRList * invlist,
                  OUT LICMAnaCtx & anactx);
    bool chooseLD(LI<IRBB> const* li, IR * ir, IRIter & irit,
                  OUT bool * all_exp_invariant, OUT IRList * invlist,
                  OUT LICMAnaCtx & anactx);
    bool chooseSELECT(LI<IRBB> const* li, IR * ir, IRIter & irit,
                      OUT bool * all_exp_invariant, OUT IRList * invlist,
                      OUT LICMAnaCtx & anactx);
    //Scan whole IR tree to find loop invariant expression
    //and add it into invariant expression list.
    //Return true if at least one invariant expression added into list.
    //ir: the root IR.
    //all_exp_invariant: true if all IR expressions start at 'ir' are
    //                   loop invariant.
    //invlist: record the invariant exp|stmt that found.
    bool chooseExp(LI<IRBB> const* li, IR * ir, IRIter & irit,
                   OUT bool * all_rhs_exp_invariant, OUT IRList * invlist,
                   OUT LICMAnaCtx & anactx);
    bool chooseExpList(LI<IRBB> const* li, IR * ir,
                       OUT bool & all_exp_invariant,
                       IRIter & irit, OUT LICMAnaCtx & anactx);
    //ir: may be exp or stmt.
    bool chooseKid(LI<IRBB> const* li, IR * ir, OUT bool & all_kid_invariant,
                   IRIter & irit, OUT LICMAnaCtx & anactx);
    bool chooseStmt(LI<IRBB> const* li, IR * ir, IRIter & irit,
                    OUT LICMAnaCtx & anactx);
    bool chooseCallStmt(LI<IRBB> const* li, IR * ir, IRIter & irit,
                        OUT LICMAnaCtx & anactx);
    bool chooseBranch(LI<IRBB> const* li, IR * ir,IRIter & irit,
                      OUT LICMAnaCtx & anactx);
    bool chooseSwitch(LI<IRBB> const* li, IR * ir, IRIter & irit,
                      OUT LICMAnaCtx & anactx);
    bool canBeRegardAsInvExp(IR const* ir, LICMAnaCtx const& anactx) const;
    bool canBeRegardAsInvExpList(IR const* ir, LICMAnaCtx const& anactx) const;

    //Given loop info li, dump the invariant stmt and invariant expression.
    void dumpInvariantExpStmt(LI<IRBB> const* li,
                              LICMAnaCtx const& anactx) const;
    void dumpHoistedIR(IR const* ir) const;
    //Return true if code motion happened.
    //The funtion will maintain LoopInfo.
    bool doLoopTree(LI<IRBB> * li, OUT HoistCtx & ctx);

    //Return true if BB or STMT changed.
    bool hoistStmtCand(MOD LICMAnaCtx & anactx, OUT IRBB * prehead,
                       OUT LI<IRBB> * li, OUT HoistCtx & ctx);
    bool hoistExpCand(MOD LICMAnaCtx & anactx, OUT IRBB * prehead,
                      OUT LI<IRBB> * li, OUT HoistCtx & ctx);

    //Return true if gurard BB of LOOP 'li' has been inserted.
    bool hasInsertedGuardBB(LI<IRBB> const* li) const;

    //Return true if all DEF stmt/phi that is related to invariant stmt
    //are moved outside from loop, return false if there is stmt that
    //prevents 'exp' from being hoisted from the loop.
    //Note some DEF that has been hoisted by this function is recorded in 'ctx'
    //even not all of DEF hoisted totally.
    bool hoistDefByMDSSA(LICMAnaCtx const& anactx, IR const* exp,
                         OUT IRBB * prehead, OUT LI<IRBB> * li,
                         MOD HoistCtx & ctx) const;
    bool hoistDefByClassicDU(LICMAnaCtx const& anactx, IR const* exp,
                             OUT IRBB * prehead, OUT LI<IRBB> * li,
                             MOD HoistCtx & ctx) const;
    bool hoistDefByPRSSA(LICMAnaCtx const& anactx, IR const* exp,
                         OUT IRBB * prehead, OUT LI<IRBB> * li,
                         MOD HoistCtx & ctx) const;
    bool hoistDefByDUChain(LICMAnaCtx const& anactcx, IR const* exp,
                           OUT IRBB * prehead, OUT LI<IRBB> * li,
                           MOD HoistCtx & ctx) const;

    //Hoist candidate IR to preheader BB.
    bool hoistCand(MOD LICMAnaCtx & anactcx, OUT IRBB * prehead,
                   OUT LI<IRBB> * li, OUT HoistCtx & ctx);

    //Return true if BB or STMT changed.
    bool hoistCandHelper(LICMAnaCtx const& anactx, OUT IR * cand_exp,
                         OUT IRBB * prehead, OUT LI<IRBB> * li,
                         OUT HoistCtx & ctx);

    //Return true if the pass will try to hoist stmt out of loop.
    bool isHoistStmt() const { return m_is_hoist_stmt; }

    //Consider whether ir is worth hoisting.
    bool isWorthHoist(IR const* ir) const
    {
        //If IR_has_sideeffect(ir) is true, that means exp can not be removed,
        //but still can be moved.
        return !ir->isNoMove(true) && !ir->is_volatile();
    }

    //Consider whether variable is worth hoisting.
    bool isWorthHoist(Var const* var) const { return !var->is_volatile(); }

    //Return true if LICM will perform aggressive strategy with RCE.
    //The aggressive strategy will take longer compilation time.
    bool is_aggressive() const { return m_is_aggressive; }

    //Return true if judgement operation 'ir' can be hoisted.
    bool isJudgeHoistCand(IR const* ir) const;

    void moveStmtToPreheader(
        MOD IR * stmt, MOD IRBB * tgtbb, OUT HoistCtx & ctx) const;

    //Process a loop.
    //Return true if code motion happened.
    //The funtion will maintain LoopInfo.
    bool processLoop(LI<IRBB> * li, HoistCtx & ctx);
    void postProcessIfChanged(HoistCtx const& hoistctx, OptCtx & oc);
    void postProcess(HoistCtx const& hoistctx, bool change, OptCtx & oc);

    //Return true if some stmts are marked as invariant-stmt.
    bool scanDirectStmt(IR * stmt, LI<IRBB> const* li, LICMAnaCtx & anactx);

    //Return true if some stmts are marked as invariant-stmt.
    bool scanInDirectStmt(IR * stmt, LI<IRBB> const* li, LICMAnaCtx & anactx);

    //Return true if some stmts are marked as invariant-stmt.
    bool scanArrayStmt(IR * stmt, LI<IRBB> const* li, LICMAnaCtx & anactx);

    //Return true if some stmts are marked as invariant-stmt.
    bool scanCallStmt(IR * stmt, LICMAnaCtx & anactx);

    //Scan expression to find invariant candidate.
    //islegal: set to true if loop is legal to perform invariant motion.
    //         otherwise set to false to prohibit code motion.
    //Return true if find loop invariant expression.
    bool scanLoopBody(LI<IRBB> const* li, bool * islegal,
                      OUT LICMAnaCtx & anactx);

    //Return true if find loop invariant expression.
    //Note that finding loop invariant does not mean finding hoist candidate.
    //Note the function try to recognize the loop invariant expression and stmt.
    //So far, the function only regard whole RHS IR tree as loop invariant ONLY
    //if all kid IR trees in RHS are loop invariant.
    //TODO: recognize the partial IR tree that is loop invariant.
    bool scanBB(IRBB * bb, LI<IRBB> const* li, bool * islegal,
                OUT LICMAnaCtx & anactx);

    //Propagate invariant property to result.
    //Return true if some stmts are marked as invariant-stmt.
    //The function aim is to generate as more as invariants.
    //The function will modify m_invariant_stmt, record if the result of
    //stmt become loop invariant.
    //Note the function assumes whole RHS tree of stmt in m_analysable_stmt_list
    //are loop invariant-exp.
    bool scanResult(LI<IRBB> const* li, OUT LICMAnaCtx & anactx);

    //The funtion record the LoopInfo status until it get to the entire LICM
    //object is destroyed.
    //that in order to avoid re-insert Guard BB over and over again.
    //Return true if gurard BB of LOOP 'li' has been inserted.
    void setLoopHasBeenGuarded(LI<IRBB> const* li);

    //The function try to pick-up invariant expression for kid of 'c'.
    //Note c is hoist-candidate, but for some reason, it can not be hoist.
    //The function is going to append its kid, also invariant exp, to be new
    //hoist-candidate.
    void tryPickKidInvExp(LICMAnaCtx const& anactx, IR * c,
                          OUT xcom::Vector<IR*> & tryagain) const;

    //Try to move and check that each definitions of candidate has been
    //already hoisted from loop.
    //Return true if all DEF stmt of 'c' has been hoisted.
    bool tryHoistAllDefStmt(LICMAnaCtx const& anactx, IR const* c,
                            IRBB * prehead, OUT LI<IRBB> * li,
                            MOD HoistCtx & ctx);

    //Return true if any stmt is moved outside from loop.
    bool tryHoistDefStmt(LICMAnaCtx const& anactx, MOD IR * def,
                         MOD IRBB * prehead, MOD LI<IRBB> * li,
                         MOD HoistCtx & ctx) const;

    //Try hoisting the dependent stmt to 'stmt' firstly.
    //Return true if all dependent stmts have been hoisted outside of loop.
    bool tryHoistDependentStmt(LICMAnaCtx const& anactx, MOD IR * stmt,
                               MOD IRBB * prehead, MOD LI<IRBB> * li,
                               OUT HoistCtx & ctx) const;

    //Return true if any stmt is moved outside from loop.
    bool tryHoistStmt(LICMAnaCtx const& anactx, IR * stmt,
                      OUT IRBB * prehead, OUT LI<IRBB> * li,
                      OUT HoistCtx & ctx) const;

    //The funtion should be invoked after stmt hoisted.
    void updateMDSSADUForStmtInLoopBody(MOD IR * stmt,
                                        HoistCtx const& ctx) const;

    bool useMDSSADU() const
    { return m_mdssamgr != nullptr && m_mdssamgr->is_valid(); }
    bool usePRSSADU() const
    { return m_prssamgr != nullptr && m_prssamgr->is_valid(); }

    void * xmalloc(UINT size)
    {
        ASSERT0(m_pool != nullptr);
        void * p = smpoolMallocConstSize(sizeof(UINT), m_pool);
        ASSERT0(p != nullptr);
        ::memset((void*)p, 0, size);
        return p;
    }
public:
    explicit LICM(Region * rg) : Pass(rg), m_act_mgr(rg)
    {
        ASSERT0(rg != nullptr);
        m_dumgr = rg->getDUMgr();
        m_cfg = rg->getCFG();
        m_tm = rg->getTypeMgr();
        m_md_sys = rg->getMDSystem();
        ASSERT0(m_cfg && m_dumgr && m_md_sys && m_tm);
        m_pool = smpoolCreate(4 * sizeof(UINT), MEM_CONST_SIZE);
        m_mdssamgr = nullptr;
        m_prssamgr = nullptr;
        m_rce = nullptr;
        m_is_hoist_stmt = true;
        m_is_aggressive = true;
    }
    virtual ~LICM() { smpoolDelete(m_pool); }

    //Analysis invariant expression and stmt for given loop.
    //Return true if find them, otherwise return false.
    //Note that finding loop invariant does not mean finding hoist candidate.
    //Collect and analyse information of invariant-exp and invariant-stmt.
    //Whether a exp/stmt can be hoisted will be determined at
    //hoistCand() finally.
    bool analysisInvariantOp(LI<IRBB> const* li, OUT LICMAnaCtx & anactx);

    virtual bool dump() const;

    ActMgr & getActMgr() { return m_act_mgr; }
    virtual CHAR const* getPassName() const
    { return "Loop Invariant Code Motion"; }
    PASS_TYPE getPassType() const { return PASS_LICM; }

    virtual bool perform(OptCtx & oc);
};

} //namespace xoc
#endif
