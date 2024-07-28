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
#ifndef _INSERT_GUARD_HELPER_H_
#define _INSERT_GUARD_HELPER_H_

namespace xoc {

class InsertGuardHelper;

class MDID2PhiMap : public TMap<MDIdx, MDPhi*> {
public:
    MDID2PhiMap(IRBB const* bb, InsertGuardHelper const& helper);
};


class InsertGuardHelper {
    COPY_CONSTRUCTOR(InsertGuardHelper);
    bool m_usemdssa;
    bool m_useprssa;
    MDSSAMgr * m_mdssa;
    PRSSAMgr * m_prssa;
    DUMgr * m_du;
    Region * m_rg;
    IRCFG * m_cfg;
    OptCtx * m_oc;
    IRMgr * m_irmgr;
    IRBB * m_guard_start;
    IRBB * m_guard_end;
    IRBB * m_guarded_bb;
private:
    LabelInfo const* addJumpEdge(IRBB * guard_start, IRBB * guard_end);

    void chooseTargetBBOfGuard(LI<IRBB> const* li, IRCFG * cfg, IRBB * guard,
                               OUT IRBB ** target, OUT LabelInfo const** lab);

    //Return true if all USE expression of 'ir' have been move to guard-BB.
    bool haveAllUseMoveToGuardBBInPRSSA(IR const* ir) const;
    bool haveAllUseMoveToGuardBBInMDSSA(IR const* ir) const;

    //Return true if the determinate-expression of loop and related DU chain
    //are too complicated to analysz and recompute.
    bool hasComplicatedDefForPR(LI<IRBB> const* li, IR const* ir) const;
    bool hasComplicatedDefForNonPR(LI<IRBB> const* li, IR const* ir) const;
    bool hasComplicatedDef(LI<IRBB> const* li, IR const* ir) const;

    IR * insertGuardIR(IRBB * guard_start, IRBB * next_to_guarded_bb,
                       LabelInfo const* guard_end_lab);
    IRBB * insertGuardStart(IRBB * guarded_bb);
    IRBB * insertGuardEnd(IRBB * guarded_bb, IRBB * next_to_guarded_bb);

    //Note DOM info must be valid.
    void insertMDSSAPhiForGuardedStmt(IR * ir, DomTree const& domtree);

    //e.g:given guarded stmt in loop body is $15=...
    //  after moving to guarded BB, the layout will be:
    //       GuardBranchCondition
    //      /       |
    //  #GuardBB:   |
    //  $15=...     |
    //      \       |
    //       $45 phi=...
    IR * insertPRSSAPhiForGuardedStmt(IR * ir);

    //Replace the USE of original stmt to PHI.
    //e.g:given guarded stmt in loop body is $15=...
    //  after moving to guarded BB, the layout will be:
    //       GuardBranchCondition
    //      /       |
    //  #GuardBB:   |
    //  $15=...     |
    //      \       |
    //       $45 phi=...
    //  Replace the USE of $15 to $45.
    void replaceUseOfGuardedStmt(IR * guarded, IR * phi) const;
    void reviseGuardDetPRSSA(LI<IRBB> const* li, IR * guard_br,
                             IRBB * guard_end);

    //Maintaining the DU chain of generated IR.
    void setSSALiveInDef(IR * exp, OptCtx const& oc)
    {
        ASSERT0(usePRSSADU() || useMDSSADU());
        ASSERTN(oc.is_dom_valid(), ("DOM info must be available"));
        ASSERT0(exp->is_exp());
        IR * init_stmt = exp->getStmt();
        ASSERT0(init_stmt);
        IRBB * bb = init_stmt->getBB();
        xoc::findAndSetLiveInDef(exp, bb->getPrevIR(init_stmt), bb, m_rg, oc);
    }

    void updateGuardDUChain(LI<IRBB> const* li, IR * guard_br,
                            IRBB * guard_end, IR * loophead_br);
    bool useMDSSADU() const { return m_usemdssa; }
    bool usePRSSADU() const { return m_useprssa; }
public:
    InsertGuardHelper(Region * rg, OptCtx * oc) : m_rg(rg), m_oc(oc)
    {
        m_cfg = m_rg->getCFG();
        m_irmgr = m_rg->getIRMgr();
        m_mdssa = m_rg->getMDSSAMgr();
        m_prssa = m_rg->getPRSSAMgr();
        m_du = m_rg->getDUMgr();
        m_usemdssa = m_mdssa != nullptr && m_mdssa->is_valid();
        m_useprssa = m_prssa != nullptr && m_prssa->is_valid();
        m_guard_start = nullptr;
        m_guard_end = nullptr;
        m_guarded_bb = nullptr;
    }

    IRMgr * getIRMgr() const { return m_irmgr; }
    MDSSAMgr * getMDSSAMgr() const { return m_mdssa; }
    OptCtx * getOptCtx() const { return m_oc; }
    IRBB * getGuardEnd() const { return m_guard_end; }
    IRBB * getGuardStart() const { return m_guard_start; }
    IRBB * getGuardedBB() const { return m_guarded_bb; }

    //Return true if guard helper has inserted guard BB.
    bool hasInsertedGuard() const { return m_guarded_bb != nullptr; }

    //Insert guard BB to control the execution of 'prehead'.
    //Return the guard BB.
    //The function will maintain RPO of generated guard BB.
    //prehead: preheader BB of loop.
    //li: LoopInfo.
    //Return the new guard controlling BB.
     IRBB * insertGuard(LI<IRBB> const* li, IRBB * prehead);

    //Insert PHI to guard_end BB for stmts in guarded BB to keep legality of
    //SSA information.
    //Note DOM info must be valid.
    //e.g:given guarded stmt in loop body is $15=...
    //  after moving to guarded BB, the layout will be:
    //       GuardBranchCondition
    //      /       |
    //  #GuardBB:   |
    //  $15=...     |
    //      \       |
    //       $45 phi=...
    void insertPhiForGuardedBB(DomTree const& domtree);

    //Return true if the determinate expression in guard will be too
    //complicated.
    bool needComplicatedGuard(LI<IRBB> const* li) const;

    //The function remove guard_start, guarded_bb, and guard_end and stmt in
    //them, whereas fix corresponding DU chain.
    void removeGuardRegion(MOD DomTree & domtree);
};

} //namespace xoc
#endif
