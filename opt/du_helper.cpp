/*@
Copyright (c) 2013-2014, Su Zhenyu steven.known@gmail.com

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
#include "cominc.h"
#include "comopt.h"

namespace xoc {

void changeDef(IR * olddef, IR * newdef, Region * rg)
{
    ASSERT0(olddef->is_stmt() && newdef->is_stmt());
    MDSSAMgr * mdssamgr = rg->getMDSSAMgr();
    if (mdssamgr != NULL) {
        mdssamgr->changeDef(olddef, newdef);
    }

    DUMgr * dumgr = rg->getDUMgr();
    if (dumgr != NULL) {
        dumgr->changeDef(newdef, olddef, dumgr->getSBSMgr());
    }
}
 

void changeUse(IR * olduse, IR * newuse, Region * rg)
{
    ASSERT0(olduse->is_exp() && newuse->is_exp());
    MDSSAMgr * mdssamgr = rg->getMDSSAMgr();
    if (mdssamgr != NULL) {
        mdssamgr->changeUse(olduse, newuse);
    }

    DUMgr * dumgr = rg->getDUMgr();
    if (dumgr != NULL) {
        dumgr->changeUse(newuse, olduse, dumgr->getSBSMgr());
    }
}


//Build DU chain : def->use.
void addDUChain(IR * def, IR * use, Region * rg)
{
    ASSERT0(def->is_stmt() && use->is_exp());
    DUMgr * dumgr = rg->getDUMgr();
    if (dumgr != NULL) {
        dumgr->buildDUChain(def, use);
    }

    if  (def->isMemoryRefNotOperatePR()) {
        MDSSAMgr * mdssamgr = rg->getMDSSAMgr(); 
        if (mdssamgr != NULL && mdssamgr->is_valid()) {
            MDSSAInfo * info = mdssamgr->getMDSSAInfoIfAny(def);
            ASSERTN(info, ("def stmt even not in MDSSA system"));
            mdssamgr->addMDSSAOcc(use, info);
        }
    }

    if (def->isWritePR() || def->isCallStmt()) {
        PRSSAMgr * prssamgr = rg->getPRSSAMgr();
        if (prssamgr != NULL && prssamgr->is_valid()) {
            prssamgr->buildDUChain(def, use);
        }
    }
}


//Check each USE of stmt, remove the expired one which is not reference
//the memory any more that stmt defined.
bool removeExpiredDUForStmt(IR * stmt, Region * rg)
{
    ASSERT0(stmt->is_stmt());
    bool change = PRSSAMgr::removeExpiredDUForStmt(stmt, rg);

    DUMgr * dumgr = rg->getDUMgr();
    if (dumgr != NULL) {
        change |= dumgr->removeExpiredDUForStmt(stmt);
    }

    MDSSAMgr * mdssamgr = rg->getMDSSAMgr(); 
    if (mdssamgr != NULL) {
        MDSSAInfo * info = mdssamgr->getMDSSAInfoIfAny(stmt);
        ASSERTN(info, ("def stmt even not in MDSSA system"));
        change |= mdssamgr->removeExpiredDUForStmt(stmt);
    }
    return change;
}


//Coalesce DU chain of 'from' to 'to'.
//This function replace definition of USE of 'from' to defintion of 'to'.
//Just like copy-propagation.
//e.g: to_def =...
//     from = to
//     ...= from_use
//=> after coalescing
//     to_def = ...
//     ------ //removed
//     ... = to
void coalesceDUChain(IR * from, IR * to, Region * rg)
{
    ASSERT0(from && to);
    ASSERT0(from->is_stmt() && to->is_exp() && to->getStmt() == from);
    DUMgr * dumgr = rg->getDUMgr();
    if (dumgr != NULL) {
        dumgr->coalesceDUChain(from, to);
    }

    if (from->isMemoryRefNotOperatePR()) {
        ASSERT0(to->isMemoryRefNotOperatePR());
        MDSSAMgr * mdssamgr = rg->getMDSSAMgr(); 
        if (mdssamgr != NULL) {
            mdssamgr->coalesceDUChain(from, to);
        }
    }
}


//Remove Use-Def chain.
//e.g: ir = ...
//    = ir //S1
//If S1 will be deleted, ir should be removed from its useset in MDSSAInfo.
//NOTE: If ir is a IR tree, e.g: ild(x, ld(y)), remove ild(x) means
//ld(y) will be removed as well. And ld(y)'s MDSSAInfo will be
//updated as well.
void removeIRTreeUse(IR * exp, Region * rg)
{
    ASSERT0(exp && exp->is_exp());
    PRSSAMgr::removePRSSAUse(exp);

    DUMgr * dumgr = rg->getDUMgr();
    if (dumgr != NULL) {
        dumgr->removeUseFromDefset(exp);
    }

    if (exp->isMemoryRefNotOperatePR()) {
        MDSSAMgr * mdssamgr = rg->getMDSSAMgr(); 
        if (mdssamgr != NULL) {
            mdssamgr->removeMDSSAUse(exp);
        }
    }
}


//Remove all DU info of 'stmt'.
void removeStmt(IR * stmt, Region * rg)
{
    ASSERT0(stmt->is_stmt());
    DUMgr * dumgr = rg->getDUMgr();
    if (dumgr != NULL) {
        dumgr->removeIRFromDUMgr(stmt);
    }

    PRSSAMgr::removePRSSAUse(stmt);

    if (stmt->isMemoryRefNotOperatePR()) {
        MDSSAMgr * mdssamgr = rg->getMDSSAMgr(); 
        if (mdssamgr != NULL) {
            mdssamgr->removeStmtFromMDSSAMgr(stmt);
        }
    }
}


static void addUseInPRSSAMode(IR * to_ir, IR const* from_ir)
{
    SSAInfo * ssainfo = from_ir->getSSAInfo();
    if (ssainfo != NULL) {
        if (from_ir->isWritePR() || from_ir->isCallHasRetVal()) {
            ASSERTN(0, ("SSA only has one def"));
        }

        ASSERT0(to_ir->isReadPR());
        PR_ssainfo(to_ir) = ssainfo;
        ssainfo->addUse(to_ir);
    }
}


static void addUseInDUMode(IR * to_ir,
                           IR const* from_ir,
                           DUMgr * dumgr,
                           Region * rg)
{
    DUSet const* from_du = from_ir->readDUSet();
    if (from_du == NULL || from_du->is_empty()) { return; }

    DUSet * to_du = dumgr->getAndAllocDUSet(to_ir);
    to_du->copy(*from_du, *dumgr->getSBSMgr());

    //Add DU chain between DEF and USE.
    DUIter di = NULL;
    for (UINT i = from_du->get_first(&di);
         di != NULL; i = from_du->get_next(i, &di)) {
        //x is stmt if from_ir is expression.
        //x is expression if from_ir is stmt.
        IR const* x = rg->getIR(i);
        DUSet * x_duset = x->getDUSet();
        if (x_duset == NULL) { continue; }
        x_duset->add(IR_id(to_ir), *dumgr->getSBSMgr());
    }
}


static void addUseInMDSSAMode(IR * to_ir,
                              IR const* from_ir,
                              MDSSAMgr * mdssamgr,
                              Region * rg)
{
    ASSERT0(0);
}


//DU chain and Memory Object reference operation.
//This function copy MustUse and MayUse mds from tree 'from' to tree 'to'
//and build new DU chain for 'to'.
//add_duchain: if true to add DU chain from tree 'from' to tree 'to'.
//    this operation will establish new DU chain between the DEF of 'from' and
//    'to'.
//'to': root expression of target tree.
//'from': root expression of source tree.
//NOTE: IR tree 'to' and 'from' must be identical structure.
//'to' and 'from' must be expression.
void copyRefAndAddDUChain(IR * to,
                          IR const* from,
                          Region * rg,
                          bool add_duchain)
{
    if (to == from) { return; }
    ASSERT0(to->is_exp() && from->is_exp());
    ASSERT0(to->isIREqual(from, true));

    MDSSAMgr * mdssamgr = rg->getMDSSAMgr();
    bool use_mdssa = mdssamgr != NULL && mdssamgr->is_valid();
    PRSSAMgr * prssamgr = rg->getPRSSAMgr();
    bool use_prssa = prssamgr != NULL && prssamgr->is_valid();
    DUMgr * dumgr = rg->getDUMgr();
    ConstIRIter cit;
    IRIter it;
    IR const* from_ir = iterInitC(from, cit);
    for (IR * to_ir = iterInit(to, it);
         to_ir != NULL; to_ir = iterNext(it), from_ir = iterNextC(cit)) {
        ASSERT0(to_ir->isIREqual(from_ir, true));
        if (!to_ir->isMemoryRef() && !to_ir->is_id()) {
            //Copy MD for IR_ID, some Passes need it, e.g. GVN.
            continue;
        }
        to_ir->copyRef(from_ir, rg);
        if (!add_duchain) { continue; }

        if (use_prssa && from_ir->isReadPR()) {
            addUseInPRSSAMode(to_ir, from_ir);
        }
        if (dumgr != NULL) {
            addUseInDUMode(to_ir, from_ir, dumgr, rg);
        }
        if (use_mdssa && from_ir->isMemoryRefNotOperatePR()) {
            addUseInMDSSAMode(to_ir, from_ir, mdssamgr, rg);
        }
    }
    ASSERT0(cit.get_elem_count() == 0 && it.get_elem_count() == 0);
}


//Return true if def is killing-def of use.
//Note this functin does not check if there is DU chain between def and use.
bool isKillingDef(IR const* def, IR const* use)
{
    ASSERT0(def && use);
    MD const* mustusemd = use->getRefMD();
    if (mustusemd == NULL) { return false; }
    return isKillingDef(def, mustusemd);
}


//Return true if def is killing-def of usemd.
//Note this functin does not check if there is DU chain between def and usemd.
bool isKillingDef(IR const* def, MD const* usemd)
{
    ASSERT0(def && usemd);
    MD const* mustdefmd = def->getRefMD();
    if (mustdefmd == NULL) { return false; }
    return isKillingDef(mustdefmd, usemd);
}


//Return true if defmd is killing-def MD of usemd.
//Note this functin does not check if there is DU chain between defmd and usemd.
bool isKillingDef(MD const* defmd, MD const* usemd)
{
    ASSERT0(defmd && usemd);
    if (defmd != NULL &&
        usemd != NULL &&
        defmd->is_exact() &&
        (defmd == usemd || defmd->is_exact_cover(usemd))) {
        //def is killing must-def.
        return true;
    }
    return false;
}


//Move IR_PHI and MDPhi from 'from' to 'to'.
//This function often used in updating PHI when adding new dominater BB to 'to'.
void movePhi(IRBB * from, IRBB * to, Region * rg)
{
    ASSERT0(from && to);
    PRSSAMgr::movePhi(from, to);

    MDSSAMgr * mdssamgr = rg->getMDSSAMgr();
    if (mdssamgr != NULL) {
        mdssamgr->movePhi(from, to);
    }
}

} //namespace xoc
