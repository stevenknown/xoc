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
#include "cominc.h"
#include "comopt.h"

namespace xoc {

//
//START LICM
//

void LICM::addInvariantStmt(IR * stmt)
{
    m_invariant_stmt.append_tail(stmt);
}


void LICM::addInvariantExp(IR * exp)
{
    m_invariant_exp.append(exp);
}


//Scan operand to find invariant candidate.
//'isLegal': set to true if loop is legal to perform invariant motion.
//           otherwise set to false to prohibit code motion.
//Return true if find loop invariant expression.
bool LICM::scanOpnd(IN LI<IRBB> * li, bool * isLegal, bool first_scan)
{
    bool change = false;
    IRBB * head = li->getLoopHead();
    UINT headid = head->id();
    for (INT i = li->getBodyBBSet()->get_first();
         i != -1; i = li->getBodyBBSet()->get_next(i)) {
        if (i != headid && !m_cfg->is_dom(headid, i)) {
            //Loop head should anticipate into analysis as well.
            //The candidate BB must dominate all other loop body BBs.
            continue;
        }

        IRBB * bb = m_cfg->getBB(i);
        ASSERT0(bb && m_cfg->getVertex(i));
        for (IR * ir = BB_first_ir(bb); ir != NULL; ir = BB_next_ir(bb)) {
            if (!ir->isContainMemRef() || ir->isNoMove()) { continue; }
            if ((ir->isCallStmt() && !ir->isReadOnly()) ||
                ir->is_region() || ir->is_phi()) {
                //TODO: support call.
                *isLegal = false;
                return false;
            }
            if (first_scan) { updateMD2Num(ir); }

            //Check if rhs is loop invariant.
            bool is_cand = true;
            m_iriter.clean();
            for (IR const* x = iterRhsInitC(ir, m_iriter);
                 x != NULL; x = iterRhsNextC(m_iriter)) {                
                if (!x->isMemoryOpnd() ||
                    x->isReadOnly() ||
                    m_invariant_exp.find(const_cast<IR*>(x))) {
                    continue;
                }
                if (!isLoopInvariant(x, li, m_rg, &m_invariant_stmt, true)) {
                    is_cand = false;
                    break;
                }
            }

            if (!is_cand) { continue; }
            
            //ir is loop invariant.
            change |= chooseExpAndStmt(ir);
        }
    }
    return change;
}


//Regard entire RHS of ir as invariant expression, find valuable expression
//and add it into invariant expression list.
//Record the stmt in work list to next round analysis.
//Return true if we add new invariant expression into list.
//Note caller has to guarantee that whole RHS expressions of ir are invariant.
bool LICM::chooseExpAndStmt(IR * ir)
{
    switch (ir->getCode()) {
    case IR_ST:
    case IR_STPR: {
        bool add_invariant = false;
        IR * e = ir->getRHS();
        //CASE: assign const expression by ST/STPR
        //      a = 0x10;
        //Regard this stmt as loop invariant if a is not volatile.
        if (!e->is_pr()) {
            //e.g: st x = P2; regard P2 as licm candidate.
            //e.g: stpr $x = P2; regard P2 as licm candidate.
            if (!m_invariant_exp.find(e)) {
                addInvariantExp(e);
                add_invariant = true;
            }
        }

        if (add_invariant) {
            if (ir->is_stpr() ||
                (ir->is_st() && !ST_idinfo(ir)->is_volatile())) {
                ASSERT0(!m_analysable_stmt_list.find(ir));
                m_analysable_stmt_list.append_tail(ir);
            }
        }
        return add_invariant;
    }
    case IR_IST: {
        bool rhs_is_invariant = false;
        bool base_is_invariant = false;
        bool add_rhs = false;
        bool add_base = false;

        IR * e = IST_rhs(ir);
        //CASE: assign const expression by ST/STPR
        //      a = 0x10;
        //Regard this stmt as loop invariant if a is not volatile.
        if (!e->is_pr()) {
            //e.g: ist = a+b, P2; regard P2 as licm candidate.
            if (!m_invariant_exp.find(e)) {
                addInvariantExp(e);
                rhs_is_invariant = true;
                add_rhs = true;
            }
        }

        e = IST_base(ir);
        if (e->is_pr()) {
            base_is_invariant = true;
        } else {
            //e.g: ist = a+b, P2; regard a+b as licm candidate.
            if (!m_invariant_exp.find(e)) {
                addInvariantExp(e);
                base_is_invariant = true;
                add_base = true;
            }
        }

        if (rhs_is_invariant && base_is_invariant) {
            //Both base and rhs have to be invariant.
            ASSERT0(!m_analysable_stmt_list.find(ir));
            m_analysable_stmt_list.append_tail(ir);
        }
        return add_rhs || add_base;
    }
    case IR_CALL:
    case IR_ICALL: {
        bool add_param_invariant = false;
        bool has_variant = false;
        //Hoisting CALL out of loop should generate a guard as well to
        //guarantee CALL will not be exectued if the loop
        //will never execute.
        for (IR * p = CALL_param_list(ir); p != NULL; p = p->get_next()) {
            if (p->isConstExp() || p->is_pr()) { continue; }
            if (!m_invariant_exp.find(p)) {
                addInvariantExp(p);
                add_param_invariant = true;
            }
        }

        //The result of call may not be loop invariant.
        if ((add_param_invariant || CALL_param_list(ir) == NULL) &&
            ir->isReadOnly()) {
            ASSERT0(!m_analysable_stmt_list.find(ir));
            m_analysable_stmt_list.append_tail(ir);
            return true;
        }
        return false;
    }
    case IR_TRUEBR:
    case IR_FALSEBR: {
        IR * e = BR_det(ir);
        if (!BIN_opnd0(e)->is_leaf() || !BIN_opnd1(e)->is_leaf()) {
            if (!m_invariant_exp.find(e)) {
                addInvariantExp(e);
                return true;
            }
        }
        return false;
    }
    case IR_SWITCH: {
        IR * e = SWITCH_vexp(ir);        
        //CASE: assign const expression by ST/STPR
        //      a = 0x10;
        //Regard this stmt as loop invariant if a is not volatile.
        if (!e->is_pr()) {
            //e.g: switch (P2); regard a+b as licm candidate.
            if (!m_invariant_exp.find(e)) {
                addInvariantExp(e);
                return true;
            }
        }
        return false;
    }
    default:;
    }
    return false;
}


//Return true if md modified in loop only once.
bool LICM::isUniqueDef(MD const* md) const
{
    ASSERT0(md);
    UINT * n = m_md2num.get(md);
    ASSERTN(n, ("should call updateMD2Num() first"));
    if (*n > 1) { return false; }

    MDTab * mdt = m_md_sys->getMDTab(MD_base(md));
    if (mdt == NULL) { return true; }

    MD const* x = mdt->get_effect_md();
    if (x != NULL && x != md && x->is_overlap(md)) {
        UINT * n2 = m_md2num.get(x);
        if (n2 != NULL && *n2 > 1) { return false; }
    }

    OffsetTab * ofstab = mdt->get_ofst_tab();
    if (ofstab == NULL) { return true; }
    if (ofstab->get_elem_count() == 0) { return true; }

    ConstMDIter mditer;
    for (MD const* x2 = ofstab->get_first(mditer, NULL);
         x2 != NULL; x2 = ofstab->get_next(mditer, NULL)) {
        if (x2 != md && x2->is_overlap(md)) {
            UINT * n2 = m_md2num.get(x2);
            if (n2 != NULL && *n2 > 1) { return false; }
        }
    }
    return true;
}


//Propagate invariant property to result.
//This operation will generate more invariant.
//This function will modify m_invariant_stmt, record if the result of
//stmt is loop invariant.
//Note this function assumes whole RHS tree of stmt in
//analysable_stmt_list are loop invariant expressions.
bool LICM::scanResult()
{
    bool change = false;
    for (IR * stmt = m_analysable_stmt_list.remove_head(); stmt != NULL;
         stmt = m_analysable_stmt_list.remove_head()) {
        switch (stmt->getCode()) {
        case IR_ST:
        case IR_STPR: {
            MD const* must = stmt->getRefMD();
            ASSERT0(must);
            if (isUniqueDef(must) &&
                !isInvariantStmt(stmt)) {
                addInvariantStmt(stmt);
                change = true;
            }
            break;
        }
        case IR_IST: {
            MD const* must = stmt->getRefMD();
            if (must != NULL &&
                must->is_effect() &&
                isUniqueDef(must) &&
                !isInvariantStmt(stmt)) {
                addInvariantStmt(stmt);
                change = true;
            }
            break;
        }
        case IR_CALL:
        case IR_ICALL: {            
            MD const* must = stmt->getRefMD();
            if ((!stmt->hasReturnValue() || isUniqueDef(must)) &&
                stmt->isReadOnly() &&
                !isInvariantStmt(stmt)) {
                addInvariantStmt(stmt);
                change = true;
            }
            break;
        }
        default: UNREACHABLE(); //TODO: support more operations.
        }
    }
    return change;
}


void LICM::updateMD2Num(IR * ir)
{
    switch (ir->getCode()) {
    case IR_ST:
    case IR_STPR:
    case IR_STARRAY:
    case IR_IST: {
        MD const* md = ir->getRefMD();
        if (md != NULL) {
            UINT * n = m_md2num.get(const_cast<MD*>(md));
            if (n == NULL) {
                n = (UINT*)xmalloc(sizeof(UINT));
                m_md2num.set(md, n);
            }
            (*n)++;
        }
        MDSet const* mds = ir->getRefMDSet();
        if (mds != NULL) {
            MDSetIter iter;
            for (INT i = mds->get_first(&iter);
                 i >= 0; i = mds->get_next(i, &iter)) {
                MD * md2 = m_md_sys->getMD(i);
                UINT * n = m_md2num.get(md2);
                if (n == NULL) {
                    n = (UINT*)xmalloc(sizeof(UINT));
                    m_md2num.set(md2, n);
                }
                (*n)++;
            }
        }
        break;
    }    
    case IR_CALL:
    case IR_ICALL: {
        ASSERT0(ir->isReadOnly());
        MD const* md = ir->getRefMD();
        if (md != NULL) {
            UINT * n = m_md2num.get(const_cast<MD*>(md));
            if (n == NULL) {
                n = (UINT*)xmalloc(sizeof(UINT));
                m_md2num.set(md, n);
            }
            (*n)++;
        }
        MDSet const* mds = ir->getRefMDSet();
        if (mds != NULL) {
            MDSetIter iter;
            for (INT i = mds->get_first(&iter);
                 i >= 0; i = mds->get_next(i, &iter)) {
                MD * md2 = m_md_sys->getMD(i);
                UINT * n = m_md2num.get(md2);
                if (n == NULL) {
                    n = (UINT*)xmalloc(sizeof(UINT));
                    m_md2num.set(md2, n);
                }
                (*n)++;
            }
        }
        break;
    }
    case IR_GOTO:
    case IR_IGOTO:
    case IR_SWITCH:
    case IR_TRUEBR:
    case IR_FALSEBR:
    case IR_RETURN:
        break;
    default: UNREACHABLE(); //Unsupport.
    }
}


//Given loop info li, dump the invariant stmt and invariant expression.
void LICM::dumpInvariantExpStmt() const
{
    if (g_tfile == NULL) { return; }
    note("\n==---- DUMP LICM Analysis Result '%s' ----==\n",
         m_rg->getRegionName());
    if (m_invariant_exp.get_elem_count() > 0) {
        TabIter<IR*> ti;
        prt("-- Invariant Expression (num=%d) -- :",
            m_invariant_exp.get_elem_count());
        g_indent += 3;
        for (IR * c = m_invariant_exp.get_first(ti);
             c != NULL; c = m_invariant_exp.get_next(ti)) {
             dumpIR(c, m_rg);
        }
        g_indent -= 3;

    }
    note("\n");
    if (m_invariant_stmt.get_elem_count() > 0) {
        prt("-- Invariant Statement (num=%d) -- :",
            m_invariant_stmt.get_elem_count());
        g_indent += 3;
        C<IR*> * it;
        for (IR * c = m_invariant_stmt.get_head(&it);
             c != NULL; c = m_invariant_stmt.get_next(&it)) {
             dumpIR(c, m_rg);
        }
        g_indent -= 3;
    }
}


//Analysis loop invariant expression and stmt.
//Return true if find them, otherwise return false.
bool LICM::analysis(IN LI<IRBB> * li)
{
    m_invariant_stmt.clean();
    m_invariant_exp.clean();
    m_analysable_stmt_list.clean();
    m_md2num.clean();

    //Record if the result of stmt is invariant.
    bool change = true;
    bool find = false;
    bool first_scan = true;
    while (change) {
        bool isLegal = true;
        change = scanOpnd(li, &isLegal, first_scan);

        if (!isLegal) {
            m_invariant_exp.clean();
            return false;
        }

        if (change) {
            find = true;
            if (m_analysable_stmt_list.get_elem_count() > 0) {
                change |= scanResult();
            }
            ASSERT0(m_analysable_stmt_list.get_elem_count() == 0);
        } else {
            //Before next round analysis, we must make sure all
            //stmts in this list is invariant or not.
            m_analysable_stmt_list.clean();
        }
        first_scan = false;
    }
    return find;
}


static bool is_stmt_dom_its_use(IR const* stmt,
                                IR const* use,
                                LI<IRBB> const* li,
                                IRBB const* stmtbb,
                                IRCFG * cfg)
{
    IR const* ustmt = use->getStmt();
    UINT ubbid = ustmt->getBB()->id();
    if (!li->isInsideLoop(ubbid)) { return true; }

    UINT stmtbbid = stmtbb->id();
    if ((stmtbbid != ubbid && cfg->is_dom(stmtbbid, ubbid)) ||
        (stmtbbid == ubbid && stmtbb->is_dom(stmt, ustmt, true))) {
        return true;
    }

    return false;
}


static bool verifyPRSSADomUse(IR const* ir,
                              LI<IRBB> const* li,
                              SSAInfo const* info,
                              Region const* rg,
                              IRCFG * cfg)
{
    ASSERT0(info->getDef() == ir);
    IRBB const* irbb = ir->getBB();
    ASSERT0(irbb);
    SSAUseIter iter;
    for (INT i = info->getUses().get_first(&iter);
         iter != NULL; i = info->getUses().get_next(i, &iter)) {
        IR const* use = const_cast<Region*>(rg)->getIR(i);
        if (!use->is_pr()) {
            ASSERT0(!use->isReadPR());
            continue;
        }
        ASSERTN(PR_no(use) == ir->getPrno(), ("prno is unmatch"));
        ASSERT0(PR_ssainfo(use) == info);
        ASSERT0(is_stmt_dom_its_use(ir, use, li, irbb, cfg));
    }
    return true;
}


static bool verifyMDSSADomUse(IR const* ir,
                              LI<IRBB> const* li,
                              MDSSAInfo const* info,
                              Region const* rg,
                              IRCFG * cfg,
                              MDSSAMgr * mgr)
{
    IRBB const* irbb = ir->getBB();
    ASSERT0(irbb);
    ConstMDSSAUSEIRIter ii;
    for (IR const* use = mgr->iterUseInitC(ir, ii);
         use != NULL; use = mgr->iterUseNextC(ii)) {
        ASSERT0(is_stmt_dom_its_use(ir, use, li, irbb, cfg));
    }
    return true;
}


bool LICM::isInvariantStmt(IR const* stmt) const
{
    ASSERT0(stmt->is_stmt());
    return m_invariant_stmt.find(const_cast<IR*>(stmt));
}


//Return true if ir dominate all USE which in loop.
bool LICM::is_dom_all_use_in_loop(IR const* ir, LI<IRBB> * li)
{
    ASSERT0(ir->is_stmt());
    IRBB * irbb = ir->getBB();
    PRSSAMgr * prssamgr = m_rg->getPRSSAMgr();
    if (prssamgr != NULL && prssamgr->is_valid() &&
        (ir->isWritePR() || ir->isCallHasRetVal())) {
        SSAInfo * info = ir->getSSAInfo();
        ASSERTN(info, ("miss PRSSAInfo"));
        ASSERT0(verifyPRSSADomUse(ir, li, info, m_rg, m_cfg));
        return true;
    }

    MDSSAMgr * mdssamgr = m_rg->getMDSSAMgr();
    if (mdssamgr != NULL &&
        mdssamgr->is_valid() &&
        ir->isMemoryRefNotOperatePR()) {
        MDSSAInfo * info = mdssamgr->getMDSSAInfoIfAny(ir);
        ASSERTN(info, ("miss MDSSAInfo"));
        ASSERT0(verifyMDSSADomUse(ir, li, info, m_rg, m_cfg, mdssamgr));
        return true;
    }

    DUSet const* useset = ir->readDUSet();
    if (useset != NULL) {
        DUIter di = NULL;
        for (INT i = useset->get_first(&di);
             i >= 0; i = useset->get_next(i, &di)) {
            IR const* u = m_rg->getIR(i);
            ASSERT0(u->is_exp() && u->getStmt());
            if (!is_stmt_dom_its_use(ir, u, li, irbb, m_cfg)) {
                return false;
            }
        }
    }
    return true;
}


bool LICM::isStmtCanBeHoisted(IR * stmt, IRBB * backedge_bb)
{
    if (backedge_bb == NULL) {
        //Loop has multiple exits.
        return false;
    }
    if (stmt->isNoMove()) {
        return false;
    }
    if (stmt->getBB() != backedge_bb &&
        !m_cfg->is_dom(stmt->getBB()->id(), backedge_bb->id())) {
        //Stmt is at the dominate path in loop.
        return false;
    }
    return true;
}


//Return true if any stmt that is related to invariant stmt
//is moved outside from loop, return false if there is stmt that
//prevents 'exp' from being hoisted from the loop.
bool LICM::handleDefByDUChain(IR const* exp,
                              OUT IRBB * prehead,
                              OUT LI<IRBB> * li)
{
    ASSERT0(exp->is_exp());
    PRSSAMgr * prssamgr = m_rg->getPRSSAMgr();
    if (prssamgr != NULL && prssamgr->is_valid() && exp->isReadPR()) {
        SSAInfo * info = exp->getSSAInfo();
        ASSERTN(info, ("miss PRSSAInfo"));
        //Check if SSA def is loop invariant.
        IR * def = SSA_def(info);
        if (def != NULL && !tryHoistDefStmt(def, prehead, li)) {
            return false;
        }
        return true;
    }

    MDSSAMgr * mdssamgr = m_rg->getMDSSAMgr();
    if (mdssamgr != NULL && mdssamgr->is_valid() && exp->isMemoryOpnd()) {
        MDSSAInfo * info = mdssamgr->getMDSSAInfoIfAny(exp);
        ASSERTN(info, ("def stmt even not in MDSSA system"));
        //mdssamgr->addMDSSAOcc(use, info);
        return true;
    }

    DUMgr * dumgr = m_rg->getDUMgr();
    DUSet const* defset = exp->readDUSet();
    if (dumgr != NULL && defset != NULL) {
        DUIter di = NULL;
        for (INT i = defset->get_first(&di);
             i >= 0; i = defset->get_next(i, &di)) {
            IR * def = m_rg->getIR(i);
            ASSERT0(def);
            if (!tryHoistDefStmt(def, prehead, li)) {
                return false;
            }
        }
    }
    return true;
}


//Return true if any stmt is moved outside from loop.
bool LICM::hoistInvariantStmt(IR * stmt, OUT IRBB * prehead, OUT LI<IRBB> * li)
{
    ASSERT0(stmt->getBB());
    ConstIRIter iriter;
    for (IR const* x = iterRhsInitC(stmt, iriter);
         x != NULL; x = iterRhsNextC(iriter)) {
        if (!handleDefByDUChain(x, prehead, li)) {
            //stmt can not be hoisted.
            return false;
        }
        //Go ahead and check next expression.
    }

    //OK, stmt can be hoisted to preheader.
    BB_irlist(stmt->getBB()).remove(stmt);
    BB_irlist(prehead).append_tail(stmt);
    return true;
}


//Return true if any stmt is moved outside from loop.
bool LICM::tryHoistDefStmt(IR * def, OUT IRBB * prehead, OUT LI<IRBB> * li)
{
    ASSERT0(def->is_stmt());
    IRBB * dbb = def->getBB();
    ASSERT0(dbb);
    if (isInvariantStmt(def)) {
        if (!li->isInsideLoop(dbb->id()) ||
            hoistInvariantStmt(def, prehead, li)) {
            return true;
        }
        return false;
    }

    if (li->isInsideLoop(dbb->id())) {
        return false;
    }

    //def stmt has been moved to prehead.
    return true;
}


//Insert guard controlling BB to predominate the execution of 'prehead'.
//This function will maintain RPO of generated guard BB.
//prehead: preheader BB of loop.
//loophead: loopheader BB of loop.
//e.g:given BB_prehead, insert BB_guard
//  BB_prehead
//  |
//  V
//  BB_loophead
//after insertion:
//  BB_guard
//  | \
//  |  V
//  |  BB_prehead
//  | / 
//  |/
//  V
//  BB_loophead
IRBB * LICM::insertGuardBB(IRBB * prehead, IRBB * loophead)
{
    IRBB * guard = m_rg->allocBB();
    //1.BB_prehead
    //  |
    //  V
    //  BB_loophead <--
    //after insertion guard:
    //  BB_guard
    //  |
    //  V
    //  BB_prehead
    //  |
    //  V
    //  BB_loophead <--
    m_cfg->addVertex(guard->id()); //Add vertex to graph before updating RPO.
    m_cfg->tryUpdateRPO(guard, prehead, true);
    m_cfg->insertBBbefore(prehead, guard);

    //Try update RPO incrementally to avoid recompute whole BB list.
    //m_cfg->tryUpdateRPO(prehead, guard, true);

    //2.BB_guard
    //  |
    //  V
    //  BB_prehead
    //  |
    //  V
    //  BB_loophead <--
    //after insertion edge:
    //  BB_guard
    //  | \
    //  |  V
    //  |  BB_prehead
    //  |   /
    //  |  /
    //  V V
    //  BB_loophead <--
    m_cfg->addEdge(guard, loophead); 

    IR * br = loophead->getLastIR();
    ASSERT0(br && br->isConditionalBr());
    IR * det = m_rg->dupIRTree(BR_det(br));
    if (br->is_truebr()) {
        //Make sure the guard BR is FALSEBR because FALSEBR uses
        //fewer instructions than TRUEBR.
        Refine::invertCondition(&det, m_rg);
    }
    ASSERT0(br->is_single());
    
    //The taken target is loophead.
    LabelInfo const* li = loophead->getLabelList().get_head();
    ASSERT0(li);
    IR * guard_br = m_rg->buildBranch(false, det, li);

    //Assign MD for generated new IR.
    m_rg->assignMDForIRList(guard_br, true, true);
    ASSERT0(guard->getNumOfIR() == 0);
    guard->getIRList()->append_tail(guard_br);
    return guard;
}


//Try to evaluate the value of loop execution condition.
//Returnt true if this function evaluated successfully, otherwise return false.
bool LICM::tryEvalLoopExecCondition(LI<IRBB> const* li,  
                                    bool & must_true,
                                    bool & must_false) const
{
    if (m_rce == NULL) { return false; }
    IRBB const* head = li->getLoopHead();
    ASSERT0(head);
    IR const* last = const_cast<IRBB*>(head)->getLastIR();
    ASSERT0(last && last->isConditionalBr());
  
    //Try to evaluate the value of judgement operation. 
    return m_rce->calcCondMustVal(BR_det(last), must_true, must_false);
}


//Return true if loop body is executed conditionally which is in charged of
//the judgement stmt in loophead BB.
//e.g:Return true for while-do loop, and false for do-while loop.
bool LICM::isLoopExecConditional(LI<IRBB> const* li) const
{
    IRBB const* head = li->getLoopHead();
    ASSERT0(head);
    IR const* last = const_cast<IRBB*>(head)->getLastIR();
    return last != NULL && last->isConditionalBr();
}


//Return true if gurard BB of LOOP 'li' has been inserted.
bool LICM::hasInsertedGuardBB(LI<IRBB> const* li) const
{
    return m_insert_guard_bb.get(li) != NULL;
}


//This function will maintain RPO of generated guard BB.
//Return true if BB or STMT changed.
bool LICM::hoistCandHelper(IRBB const* backedge_bb,
                           OUT IR * cand_exp,
                           OUT IRBB * prehead,
                           OUT LI<IRBB> * li)
{
    ASSERT0(cand_exp->is_exp());
    IR * cand_stmt = cand_exp->getStmt();
    if ((cand_stmt->is_st() || cand_stmt->is_stpr()) &&
        cand_exp == cand_stmt->getRHS() &&
        isInvariantStmt(cand_stmt) &&
        backedge_bb != NULL && //loop only have one exit.
        is_dom_all_use_in_loop(cand_stmt, li)) {
        //isStmtCanBeHoisted(cand_stmt, backedge_bb)

        //cand is store-value and the result memory object is ID|PR.

        //NOTE: If we hoist entire stmt out from loop,
        //we should make sure the stmt will be execute at least once
        //or never. Conditional branch should be generated and encapsulate
        //the hoisted stmt to ensure that.
        //    while (a > 0) {
        //        a = 10;
        //        foo();
        //    }
        //    =>
        //    if (a > 0)  {
        //        a = 10;
        //    }
        //    while (a > 0) {
        //        foo();
        //    }
        ASSERT0(cand_stmt->getBB());
        if (cand_stmt->getBB() != li->getLoopHead() &&
            isLoopExecConditional(li) &&
            !hasInsertedGuardBB(li)) {
            bool must_true, must_false;
            if (tryEvalLoopExecCondition(li, must_true, must_false) &&
                must_true) {
                ; //guard BB is unnecessary
            } else {
                insertGuardBB(prehead, li->getLoopHead());
                m_insert_guard_bb.append(li);
            }
        }
        cand_stmt->getBB()->getIRList()->remove(cand_stmt);        
        prehead->getIRList()->append_tail_ex(cand_stmt);

        //The code motion do not modify DU chain info of 'exp' and
        //'cand_stmt'. So it is no need to revise DU chain.
        //But the live-expr, reach-def, avail-reach-def set
        //info of each BB changed.
        return true;
    }

    if (cand_exp->is_const()) {
        //CASE1: given
        //    n = 0x10; //S1
        //No need to build STPR in preheader.
        return false;
    }

    //CASE2: given
    //    n = cand_exp; //S1
    //
    //Generate new stmt S2, change S1 to S3:
    //    p1 = cand_exp; //S2
    //    n = p1; //S3
    //move S2 into prehead BB.
    IR * t = m_rg->buildPR(cand_exp->getType());
    bool f = cand_stmt->replaceKid(cand_exp, t, false);
    CHECK_DUMMYUSE(f);

    IR * stpr = m_rg->buildStorePR(PR_no(t), t->getType(), cand_exp);

    //Revise MD info.
    MD const* tmd = m_rg->genMDforPR(t);
    t->setRefMD(tmd, m_rg);
    stpr->setRefMD(tmd, m_rg);

    addDUChain(stpr, t, m_rg);
    prehead->getIRList()->append_tail(stpr);
    return true;
}


//Hoist candidate IRs to preheader BB.
//This function will maintain RPO if new BB inserted.
//Return true if BB or STMT changed.
bool LICM::hoistCand(OUT IRBB * prehead, OUT LI<IRBB> * li)
{
    bool du_set_info_changed = false;
    Vector<IR*> removed;
    TabIter<IR*> ti;
    IRBB * backedge_bb = findSingleBackedgeStartBB(li, m_cfg);
    while (m_invariant_exp.get_elem_count() > 0) {
        UINT removednum = 0;
        for (IR * c = m_invariant_exp.get_first(ti);
             c != NULL; c = m_invariant_exp.get_next(ti)) {
            ASSERT0(c->is_exp());
            if (!isWorthHoist(c)) { continue; }

            //Check that each definitions of candidate have been
            //already hoisted from loop.
            bool do_hoist_now = true;
            m_iriter.clean();
            for (IR const* x = iterInitC(c, m_iriter);
                 x != NULL; x = iterNextC(m_iriter)) {
                if (!handleDefByDUChain(x, prehead, li)) {
                    //x's DEF can not be hoisted.
                    do_hoist_now = false;
                    break;
                }
            }
            if (!do_hoist_now) { continue; }

            removed.set(removednum, c);
            removednum++;
            if (hoistCandHelper(backedge_bb, c, prehead, li)) {
                du_set_info_changed = true;
            }
        }
        ASSERTN(removednum > 0, ("not find any hoistable exp?"));

        for (UINT i = 0; i < removednum; i++) {
            IR * c = removed.get(i);
            ASSERT0(c);
            m_invariant_exp.remove(c);
        }
    }
    return du_set_info_changed;
}


//hoistCand may append stmt into BB which has down-boundary stmt.
//That makes BB invalid. Split such invalid BB into two or more BBs.
bool LICM::splitBBIfNeeded(IRBB * bb)
{    
    IRListIter it;
    for (bb->getIRList()->get_head(&it); it != NULL;) {
        IRListIter cur = it;
        bb->getIRList()->get_next(&it);
        if (IRBB::isDownBoundary(cur->val()) && it != NULL) {
            m_cfg->splitBB(bb, cur);
            return true;
        }
    }
    return false;
}


//Return true if code motion happened.
//This funtion maintain Loop Info.
bool LICM::doLoopTree(LI<IRBB> * li,
                      OUT bool & du_set_info_changed,
                      OUT bool & insert_bb,
                      OptCtx & oc)
{
    if (li == NULL) { return false; }
    bool changed = false;
    for (LI<IRBB> * tli = li; tli != NULL; tli = LI_next(tli)) {
        changed |= doLoopTree(LI_inner_list(tli), du_set_info_changed,
                              insert_bb, oc);
        analysis(tli);

        if (g_is_dump_after_pass && g_dump_opt.isDumpLICM()) {
            //Dump invariant info here because they will be replaced soon.
            dumpInvariantExpStmt();
        }

        if (m_invariant_exp.get_elem_count() == 0) {
            continue;
        }

        bool inserted = false;
        IRBB * prehead = findAndInsertPreheader(tli, m_rg, inserted, false);
        if (prehead == NULL || m_cfg->isRegionEntry(prehead)) {
            prehead = findAndInsertPreheader(tli, m_rg, inserted, true);
            changed = true;
        }
        ASSERT0(prehead);
        insert_bb |= inserted;
        if (inserted && !tli->isOuterMost()) {
            //Update loop body BB set, add preheader to outer loop body.
            tli->getOuter()->getBodyBBSet()->bunion(prehead->id());
        }

        if (inserted) {
            //Recompute DOM related info.
            OC_is_dom_valid(oc) = false;
            OC_is_rpo_valid(oc) = false;
            m_cfg->computeDomAndIdom(oc);
        }

        du_set_info_changed |= hoistCand(prehead, tli);
        changed |= du_set_info_changed;

        bool inserted2 = splitBBIfNeeded(prehead);
        insert_bb |= inserted2;
        if (inserted2) {
            //Recompute DOM related info.
            OC_is_dom_valid(oc) = false;
            OC_is_rpo_valid(oc) = false;
            m_cfg->computeDomAndIdom(oc);
        }
    }
    return changed;
}


bool LICM::dump() const
{
    note("\n==---- DUMP %s '%s' ----==", getPassName(), m_rg->getRegionName());
    m_rg->dumpBBList();
    return true;
}


bool LICM::perform(OptCtx & oc)
{
    if (m_rg->getBBList() == NULL || m_rg->getBBList()->get_elem_count() == 0) {
        return false;
    }

    if (!OC_is_ref_valid(oc)) { return false; }
    m_mdssamgr = (MDSSAMgr*)m_rg->getPassMgr()->queryPass(PASS_MD_SSA_MGR);
    m_prssamgr = (PRSSAMgr*)m_rg->getPassMgr()->queryPass(PASS_PR_SSA_MGR);
    if (!OC_is_pr_du_chain_valid(oc) && !usePRSSADU()) {
        //DCE use either classic PR DU chain or PRSSA.
        //At least one kind of DU chain should be avaiable.
        return false;
    }
    if (!OC_is_nonpr_du_chain_valid(oc) && !useMDSSADU()) {
        //DCE use either classic MD DU chain or MDSSA.
        //At least one kind of DU chain should be avaiable.
        return false;
    }
 
    START_TIMER(t, getPassName());
    m_rg->checkValidAndRecompute(&oc, PASS_DOM, PASS_LOOP_INFO, PASS_UNDEF);
    m_rce = (RCE*)m_rg->getPassMgr()->registerPass(PASS_RCE);
    ASSERT0(m_rce);
    if (m_rce->is_use_gvn() && !m_rce->getGVN()->is_valid()) {
        m_rce->getGVN()->reperform(oc);
    }

    bool du_set_info_changed = false;
    bool insert_bb = false;
    bool change = doLoopTree(m_cfg->getLoopInfo(), du_set_info_changed,
                             insert_bb, oc);
    if (change) {
        if (insert_bb) {
            OC_is_rpo_valid(oc) = m_cfg->isRPOValid();
            //For conservative purpose, we hope to recompute RPO BB list
            //when it is needed.
            m_cfg->cleanRPOBBList();

            //Loop info is maintained, but DOM/PDOM and CDG are not.
            OC_is_dom_valid(oc) = false;
            OC_is_pdom_valid(oc) = false;
            OC_is_cdg_valid(oc) = false;
        }
        OC_is_expr_tab_valid(oc) = false;
        if (du_set_info_changed) {
            OC_is_live_expr_valid(oc) = false;
            OC_is_avail_reach_def_valid(oc) = false;
            OC_is_reach_def_valid(oc) = false;
            if (m_rce != NULL && m_rce->is_use_gvn()) {
                m_rce->getGVN()->set_valid(false);
            }
        }
        m_cfg->performMiscOpt(oc);

        //DU chain and du ref is maintained.
        ASSERT0(m_rg->verifyMDRef());
        ASSERT0(m_du->verifyMDDUChain(DUOPT_COMPUTE_PR_DU|
                                      DUOPT_COMPUTE_NONPR_DU));
        if (g_is_dump_after_pass && g_dump_opt.isDumpLICM()) {
            dump();
        }
    }
    END_TIMER(t, getPassName());
    return change;
}
//END LICM

} //namespace xoc
