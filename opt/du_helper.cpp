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
#include "cominc.h"
#include "comopt.h"

namespace xoc {

static bool checkChange(Region const* rg, bool mdssa_changed,
                        bool prssa_changed, bool classic_du_changed)
{
    if (g_opt_level == OPT_LEVEL0) { return true; }
    if (rg->getMDSSAMgr() != nullptr || rg->getPRSSAMgr() != nullptr ||
        rg->getDUMgr() != nullptr) {
        //At least one kind of DU exist.
        ASSERTN(mdssa_changed || prssa_changed || classic_du_changed,
                ("DU Chain is not available"));
    }
    return true;
}


void changeDefForPartialUseSet(IR * olddef, IR * newdef,
                               IRSet const& partial_useset, Region * rg)
{
    ASSERT0(olddef->is_stmt() && newdef->is_stmt());
    ASSERTN(olddef->isMemRef() && newdef->isMemRef(),
            ("should not query its DU"));
    bool mdssa_changed = false;
    bool prssa_changed = false;
    bool classic_du_changed = false;
    MDSSAMgr * mdssamgr = rg->getMDSSAMgr();
    if (mdssamgr != nullptr && mdssamgr->is_valid() &&
        olddef->isMemRefNonPR()) {
        //mdssamgr->changeDefForPartialUseSet(olddef, newdef, partial_useset);
        ASSERTN(0, ("TODO"));
        mdssa_changed = true;
    }

    PRSSAMgr * prssamgr = rg->getPRSSAMgr();
    if (prssamgr != nullptr && prssamgr->is_valid() &&
        olddef->isPROp()) {
        ASSERTN(newdef->isPROp(), ("unsupport"));
        prssamgr->changeDefForPartialUseSet(olddef, newdef, partial_useset);
        prssa_changed = true;
    }

    DUMgr * dumgr = rg->getDUMgr();
    if (dumgr != nullptr) {
        DUSet * oldduse = olddef->getDUSet();
        if (oldduse != nullptr) {
            //dumgr->changeDefForPartialUseSet(newdef, olddef, partial_useset);
            ASSERTN(0, ("TODO"));
        }
        classic_du_changed = true;
    }

    ASSERT0_DUMMYUSE(checkChange(rg, mdssa_changed, prssa_changed,
                     classic_du_changed));
}


void changeDef(IR * olddef, IR * newdef, Region * rg)
{
    ASSERT0(olddef->is_stmt() && newdef->is_stmt());
    ASSERTN(olddef->isMemRef() && newdef->isMemRef(),
            ("should not query its DU"));
    bool mdssa_changed = false;
    bool prssa_changed = false;
    bool classic_du_changed = false;
    MDSSAMgr * mdssamgr = rg->getMDSSAMgr();
    if (mdssamgr != nullptr && mdssamgr->is_valid() &&
        olddef->isMemRefNonPR()) {
        mdssamgr->changeDef(olddef, newdef);
        mdssa_changed = true;
    }

    PRSSAMgr * prssamgr = rg->getPRSSAMgr();
    if (prssamgr != nullptr && prssamgr->is_valid() &&
        olddef->isPROp()) {
        ASSERTN(newdef->isPROp(), ("unsupport"));
        prssamgr->changeDef(olddef, newdef);
        prssa_changed = true;
    }

    DUMgr * dumgr = rg->getDUMgr();
    if (dumgr != nullptr) {
        DUSet * oldduse = olddef->getDUSet();
        if (oldduse != nullptr) {
            dumgr->changeDef(newdef, olddef);
        } else {
            //There is no USE if olddef's DUSet is nullptr or empty.
        }
        classic_du_changed = true;
    }

    ASSERT0_DUMMYUSE(checkChange(rg, mdssa_changed, prssa_changed,
                     classic_du_changed));
}


static IR * getUniqueDef(IRSet const* defset, Region * rg)
{
    //In the situation, the function just utilizes the Definition
    //Informations to build DU chain between 'newuse' and these
    //definitions.
    ASSERT0(defset);
    IR * unique_def = nullptr;
    IRSetIter it;
    for (BSIdx i = defset->get_first(&it); i != BS_UNDEF;
         i = defset->get_next(i, &it)) {
        IR * def = rg->getIR(i);
        ASSERT0(def && def->is_stmt());
        if (unique_def == nullptr) {
            unique_def = def;
        } else {
            //Not unique
            return nullptr;
        }
    }
    return unique_def;
}


//DU chain operation.
//The function changes USE of some DU chain from 'olduse' to 'newuse'.
//newuse: indicates the target expression which is changed to.
//olduse: indicates the source expression which will be changed.
//defset: if the function meets CASE3, the defst will supply the Definition
//        Stmt Set that will be 'newuse'. The defset may NOT come
//        from 'olduse'.
//e.g: given DU chain DEF->'olduse', the result is DEF->'newuse'.
//Note the function allows NonPR->PR or PR->NonPR.
//The function is an extended version of 'changeUse'. The function will handle
//the combination of different category of Memory Reference, e.g: PR<->NonPR.
void changeUseEx(IR * olduse, IR * newuse, IRSet const* defset, Region * rg,
                 OptCtx const& oc)
{
    ASSERT0(olduse->is_exp() && newuse->is_exp());
    if (newuse->isMemOpnd()) {
        ASSERT0(olduse->isMemOpnd());
        if (olduse->isMemRefNonPR() && newuse->isMemRefNonPR()) {
            //CASE1:ld x -> ld y
            xoc::changeUse(olduse, newuse, rg);
            return;
        }

        if (olduse->isReadPR() && newuse->isReadPR()) {
            //CASE1:$x -> $y
            if (olduse->getSSAInfo() != nullptr &&
                newuse->getSSAInfo() == nullptr) {
                //CASE:olduse and newuse are both in PRSSA mode, but newuse
                //lacked PRSSAInfo for unknown reason.
                //The method that attempt to find PRSSAInfo for
                //newuse is under the prerequisite that the DEF of newuse is
                //also in PRSSA mode, namely an unique DEF, then regard the
                //PRSSAInfo of the unique DEF as it were of newuse.
                IR * unique_def = getUniqueDef(defset, rg);
                ASSERT0(unique_def && unique_def->getSSAInfo());
                xoc::copySSAInfo(newuse, unique_def);
            }
            xoc::changeUse(olduse, newuse, rg);
            return;
        }

        if ((olduse->isMemRefNonPR() && newuse->isReadPR()) ||
            (newuse->isMemRefNonPR() && olduse->isReadPR())) {
            //CASE2:ld x -> $y
            //      $x -> ld y
            //In the situation, the function just utilizes the Definition
            //Informations to build DU chain between 'newuse' and these
            //definitions.
            ASSERT0(defset);
            xoc::removeUseForTree(olduse, rg, oc);

            IRSetIter it;
            for (BSIdx i = defset->get_first(&it); i != BS_UNDEF;
                 i = defset->get_next(i, &it)) {
                IR * def = rg->getIR(i);
                ASSERT0(def && def->is_stmt());
                xoc::buildDUChain(def, newuse, rg, oc);
            }
            return;
        }

        UNREACHABLE();
    }

    ASSERT0(newuse->is_const() || newuse->is_lda() ||
            (newuse->is_cvt() &&
             (((CCvt*)newuse)->getLeafExp()->is_const() ||
              ((CCvt*)newuse)->getLeafExp()->is_lda())));
    //CASE3:ld x -> const|lda
    //      $x -> const|lda
    //      id -> const|lda
    xoc::removeUseForTree(olduse, rg, oc);
}


//DU chain operation.
//The function changes USE of some DU chain from 'olduse' to 'newuse'.
//newuse: indicates the target expression which is changed to.
//olduse: indicates the source expression which will be changed.
//e.g: given DU chain DEF->'olduse', the result is DEF->'newuse'.
//Note the function does NOT allow NonPR->PR or PR->NonPR.
void changeUse(IR * olduse, IR * newuse, Region * rg)
{
    ASSERT0(olduse->is_exp() && newuse->is_exp());
    ASSERTN(olduse->isMemRef() && newuse->isMemRef(),
            ("should not query its DU"));
    bool mdssa_changed = false;
    bool prssa_changed = false;
    bool classic_du_changed = false;
    MDSSAMgr * mdssamgr = rg->getMDSSAMgr();
    if (mdssamgr != nullptr && mdssamgr->is_valid() &&
        olduse->isMemRefNonPR()) {
        ASSERT0(!(mdssamgr->hasMDSSAInfo(olduse) ^
                  mdssamgr->hasMDSSAInfo(newuse)));
        if (!mdssamgr->hasMDSSAInfo(olduse)) {
            ASSERT0(!mdssamgr->hasMDSSAInfo(newuse));
            //nothing to do
        } else {
            mdssamgr->changeUse(olduse, newuse);
        }
        mdssa_changed = true;
    }
    DUMgr * dumgr = rg->getDUMgr();
    if (dumgr != nullptr) {
        if (olduse->getDUSet() != nullptr) {
            dumgr->changeUse(newuse, olduse);
        } else {
            //There is no DEF if olduse's DUSet is nullptr or empty.
        }
        classic_du_changed = true;
    }
    if (olduse->isPROp() || newuse->isPROp()) {
        PRSSAMgr const* prssamgr = rg->getPRSSAMgr();
        if (prssamgr != nullptr && prssamgr->is_valid()) {
            SSAInfo * oldssainfo = olduse->getSSAInfo();
            SSAInfo * newssainfo = newuse->getSSAInfo();
            ASSERT0(oldssainfo && newssainfo);
            oldssainfo->removeUse(olduse);
            newssainfo->addUse(newuse);
            prssa_changed = true;
        }
    }
    ASSERT0_DUMMYUSE(checkChange(rg, mdssa_changed, prssa_changed,
                                 classic_du_changed));
}


//Build DU chain from stmt 'def' to expression 'use'.
//The function establishs DU chain from 'def' to 'use'.
//def: stmt
//use: expression
void buildDUChain(IR * def, IR * use, Region * rg, OptCtx const& oc)
{
    ASSERT0(def && use && def->is_stmt() && use->is_exp());
    bool mdssa_changed = false;
    bool prssa_changed = false;
    bool classic_du_changed = false;
    if (def->isMemRefNonPR()) {
        MDSSAMgr * mdssamgr = rg->getMDSSAMgr();
        if (mdssamgr != nullptr && mdssamgr->is_valid()) {
            ASSERT0(use->isMemRefNonPR());
            MDSSAInfo * info = mdssamgr->getMDSSAInfoIfAny(def);
            ASSERTN(info, ("def stmt even not in MDSSA system"));
            mdssamgr->copyAndAddMDSSAOcc(use, info);
            mdssa_changed = true;
        }
    }
    if (def->isPROp()) {
        ASSERT0(use->isPROp());
        PRSSAMgr * prssamgr = rg->getPRSSAMgr();
        if (prssamgr != nullptr && prssamgr->is_valid()) {
            prssamgr->buildDUChain(def, use);
            prssa_changed = true;
        }
    }
    DUMgr * dumgr = rg->getDUMgr();
    if (dumgr != nullptr) {
        classic_du_changed = dumgr->buildDUChain(def, use, oc);
    }
    ASSERT0_DUMMYUSE(checkChange(rg, mdssa_changed, prssa_changed,
                                 classic_du_changed));
}


//The function checks each DEF|USE occurrence of ir, remove the expired
//expression which is not reference the memory any more that ir referenced.
//Return true if DU changed.
//e.g: stpr1 = ... //S1
//     ..... = pr2 //S2
//  If there is DU between S1 and S2, cutoff the DU chain.
//stmt: PR write operation.
//exp: PR read operation.
bool removeExpiredDU(IR const* ir, Region * rg)
{
    ASSERT0(ir);
    bool mdssa_changed = false;
    bool prssa_changed = false;
    bool classic_du_changed = false;
    bool change = false;
    if (ir->isPROp()) {
        PRSSAMgr const* prssamgr = rg->getPRSSAMgr();
        if (prssamgr != nullptr && prssamgr->is_valid()) {
            change |= PRSSAMgr::removeExpiredDU(ir, rg);
            prssa_changed = true;
        }
    }
    DUMgr * dumgr = rg->getDUMgr();
    if (dumgr != nullptr) {
        change |= dumgr->removeExpiredDU(ir);
        classic_du_changed = true;
    }
    MDSSAMgr * mdssamgr = rg->getMDSSAMgr();
    if (mdssamgr != nullptr && mdssamgr->is_valid() &&
        ir->isMemRefNonPR()) {
        ASSERTN(mdssamgr->getMDSSAInfoIfAny(ir),
                ("def stmt even not in MDSSA system"));
        change |= mdssamgr->removeExpiredDU(ir);
        mdssa_changed = true;
    }
    ASSERT0_DUMMYUSE(checkChange(rg, mdssa_changed, prssa_changed,
                     classic_du_changed));
    return change;
}


//Coalesce DU chain of 'from' to 'to'.
//'from' and 'to' refered the same md.
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
    bool mdssa_changed = false;
    bool prssa_changed = false;
    bool classic_du_changed = false;
    DUMgr * dumgr = rg->getDUMgr();
    if (dumgr != nullptr) {
        dumgr->coalesceDUChain(from, to);
        classic_du_changed = true;
    }
    if (from->isMemRefNonPR()) {
        ASSERT0(to->isMemRefNonPR());
        MDSSAMgr * mdssamgr = rg->getMDSSAMgr();
        if (mdssamgr != nullptr && mdssamgr->is_valid()) {
            mdssamgr->coalesceDUChain(from, to);
            mdssa_changed = true;
        }
    }
    ASSERT0_DUMMYUSE(checkChange(rg, mdssa_changed, prssa_changed,
                                 classic_du_changed));
}


void removeUse(IR const* exp, Region * rg)
{
    ASSERT0(exp && exp->is_exp()); //exp is the root of IR tree.
    bool mdssa_changed = false;
    bool prssa_changed = false;
    bool classic_du_changed = false;
    PRSSAMgr const* prssamgr = rg->getPRSSAMgr();
    if (prssamgr != nullptr && prssamgr->is_valid()) {
        PRSSAMgr::removeUse(exp);
        prssa_changed = true;
    }
    DUMgr * dumgr = rg->getDUMgr();
    if (dumgr != nullptr) {
        dumgr->removeUse(exp);
        classic_du_changed = true;
    }
    MDSSAMgr * mdssamgr = rg->getMDSSAMgr();
    if (mdssamgr != nullptr && mdssamgr->is_valid()) {
        mdssamgr->removeUse(exp);
        mdssa_changed = true;
    }
    ASSERT0_DUMMYUSE(checkChange(rg, mdssa_changed, prssa_changed,
                                 classic_du_changed));
}


//Remove Use-Def chain.
//exp: it is the root of IR tree that to be removed.
//e.g: ir = ...
//    = ir //S1
//If S1 will be deleted, ir should be removed from its useset in MDSSAInfo.
//NOTE: If ir is a IR tree, e.g: ild(x, ld(y)), remove ild(x) means
//ld(y) will be removed as well. And ld(y)'s MDSSAInfo will be
//updated as well.
void removeUseForTree(IR const* exp, Region * rg, OptCtx const& oc)
{
    ASSERT0(exp && exp->is_exp()); //exp is the root of IR tree.
    bool mdssa_changed = false;
    bool prssa_changed = false;
    bool classic_du_changed = false;
    PRSSAMgr const* prssamgr = rg->getPRSSAMgr();
    if (prssamgr != nullptr && prssamgr->is_valid()) {
        //Access whole IR tree root at 'exp'.
        PRSSAMgr::removePRSSAOcc(exp);
        prssa_changed = true;
    }
    DUMgr * dumgr = rg->getDUMgr();
    if (dumgr != nullptr) {
        dumgr->removeUseFromDefset(exp);
        classic_du_changed = true;
    }
    MDSSAMgr * mdssamgr = rg->getMDSSAMgr();
    if (mdssamgr != nullptr && mdssamgr->is_valid()) {
        //Access whole IR tree root at 'exp'.
        MDSSAUpdateCtx ctx(oc);
        mdssamgr->removeMDSSAOccForTree(exp, ctx);
        mdssa_changed = true;
    }
    ASSERT0_DUMMYUSE(checkChange(rg, mdssa_changed, prssa_changed,
                                 classic_du_changed));
}


void removeStmt(IR * stmt, Region * rg, OptCtx const& oc)
{
    ASSERT0(stmt->is_stmt());
    bool mdssa_changed = false;
    bool prssa_changed = false;
    bool classic_du_changed = false;
    DUMgr * dumgr = rg->getDUMgr();
    if (dumgr != nullptr &&
        (oc.is_pr_du_chain_valid() || oc.is_nonpr_du_chain_valid())) {
        dumgr->removeIRFromDUMgr(stmt);
        classic_du_changed = true;
    }
    PRSSAMgr const* prssamgr = rg->getPRSSAMgr();
    if (prssamgr != nullptr && prssamgr->is_valid()) {
        PRSSAMgr::removePRSSAOcc(stmt);
        prssa_changed = true;
    }
    MDSSAMgr * mdssamgr = rg->getMDSSAMgr();
    if (mdssamgr != nullptr && mdssamgr->is_valid()) {
        //Remove stmt and its RHS.
        MDSSAUpdateCtx ctx(oc);
        if (!oc.is_dom_valid()) {
            MDSSAUPDATECTX_update_duchain(&ctx) = false;
        }
        mdssamgr->removeMDSSAOccForTree(stmt, ctx);
        mdssa_changed = true;
    }
    ASSERT0_DUMMYUSE(checkChange(rg, mdssa_changed, prssa_changed,
                                 classic_du_changed));
}


static void addUseInPRSSAMode(IR * to_ir, IR const* from_ir)
{
    SSAInfo * ssainfo = from_ir->getSSAInfo();
    if (ssainfo == nullptr) { return; }
    if (from_ir->isWritePR() || from_ir->isCallHasRetVal()) {
        ASSERTN(0, ("PRSSA only has one def"));
    }
    ASSERT0(to_ir->isReadPR());
    PR_ssainfo(to_ir) = ssainfo;
    ssainfo->addUse(to_ir);
}


//The function only process single IR rather than IR tree.
static void addUseInMDSSAMode(IR * to_ir, IR const* from_ir,
                              MDSSAMgr * mdssamgr, Region * rg)
{
    ASSERT0(to_ir->isMemRefNonPR() && from_ir->isMemRefNonPR());
    MDSSAInfo * info = mdssamgr->getMDSSAInfoIfAny(from_ir);
    ASSERT0(info);
    mdssamgr->copyAndAddMDSSAOcc(to_ir, info);
    return;
}


//The function manipulates DU chain. It adds DU chain from 'from' to
//'to',  and the function will establish new DU chain between DEF of
//'from' and expression 'to'.
//to: target IR expression.
//from: source IR expression.
//Both 'to' and 'from' must be expression.
void addUse(IR * to, IR const* from, Region * rg)
{
    if (to == from) { return; }
    ASSERT0(to->is_exp() && from->is_exp());
    if (!to->isMemRef() && !to->is_id()) {
        //Copy MD for IR_ID, some Passes require it, e.g. GVN.
        return;
    }

    bool mdssa_changed = false;
    bool prssa_changed = false;
    bool classic_du_changed = false;

    PRSSAMgr * prssamgr = rg->getPRSSAMgr();
    if (prssamgr != nullptr && prssamgr->is_valid() && from->isReadPR()) {
        addUseInPRSSAMode(to, from);
        prssa_changed = true;
    }

    MDSSAMgr * mdssamgr = rg->getMDSSAMgr();
    if (mdssamgr != nullptr && mdssamgr->is_valid() &&
        from->isMemRefNonPR()) {
        addUseInMDSSAMode(to, from, mdssamgr, rg);
        mdssa_changed = true;
    }

    DUMgr * dumgr = rg->getDUMgr();
    if (dumgr != nullptr) {
        dumgr->addUseForTree(to, from);
        classic_du_changed = true;
    }

    ASSERT0_DUMMYUSE(checkChange(rg, mdssa_changed, prssa_changed,
                                 classic_du_changed));
}


void addUseForTree(IR * to, IR const* from, Region * rg)
{
    if (to == from) { return; }

    ASSERT0(to->is_exp() && from->is_exp());
    ASSERT0(to->isIREqual(from, rg->getIRMgr(), true));

    //Priority of DU chain: PRSSA > MDSSA > Classic DU.
    MDSSAMgr * mdssamgr = rg->getMDSSAMgr();
    bool use_mdssa = mdssamgr != nullptr && mdssamgr->is_valid();
    PRSSAMgr * prssamgr = rg->getPRSSAMgr();
    bool use_prssa = prssamgr != nullptr && prssamgr->is_valid();
    DUMgr * dumgr = rg->getDUMgr();
    ConstIRIter cit;
    IRIter it;
    //Only iterate the IR tree that root are from and to.
    IR const* from_ir = xoc::iterInitC(from, cit, false);
    IR * to_ir = xoc::iterInit(to, it, false);
    ASSERT0(cit.get_elem_count() == it.get_elem_count());
    for (; to_ir != nullptr;
         to_ir = xoc::iterNext(it, true), from_ir = xoc::iterNextC(cit, true)) {
        ASSERT0(to_ir->isIREqual(from_ir, rg->getIRMgr(), true));
        if (!to_ir->isMemRef() && !to_ir->is_id()) {
            //Copy MD for IR_ID, some Passes require it, e.g. GVN.
            continue;
        }
        if (use_prssa && from_ir->isReadPR()) {
            addUseInPRSSAMode(to_ir, from_ir);
        }
        if (use_mdssa && from_ir->isMemRefNonPR()) {
            addUseInMDSSAMode(to_ir, from_ir, mdssamgr, rg);
        }
        if (dumgr != nullptr) {
            dumgr->addUseForTree(to_ir, from_ir);
        }
    }
    ASSERT0(cit.get_elem_count() == 0 && it.get_elem_count() == 0);
}


//This function try to require VN of base of ir.
//Return the VN if found, and the indirect operation level.
//e.g: given ILD(ILD(p)), return p and indirect_level is 2.
//e.g2: given IST(ILD(q)), return q and indirect_level is 2.
VN const* getVNOfIndirectOp(IR const* ir, OUT UINT * indirect_level,
                            GVN const* gvn)
{
    ASSERT0(ir && ir->isIndirectMemOp());
    ASSERT0(gvn && gvn->is_valid());
    IR const* base = ir;
    *indirect_level = 0;
    for (; base != nullptr && base->isIndirectMemOp();
         base = base->getBase()) {
        (*indirect_level)++;
    }
    ASSERT0(base);

    //Get the VN of base expression.
    return const_cast<GVN*>(gvn)->getVN(base);
}


bool hasSameRegionLiveIn(IR const* ir1, IR const* ir2, Region const* rg)
{
    ASSERT0(ir1 && ir2 && ir1->is_exp() && ir2->is_exp());
    ASSERT0(ir1->isMemOpnd() && ir2->isMemOpnd());
    //Prefer PRSSA and MDSSA DU.
    if (ir1->isReadPR()) {
        ASSERT0(ir2->isReadPR());
        PRSSAMgr const* prssamgr = rg->getPRSSAMgr();
        if (prssamgr != nullptr && prssamgr->is_valid()) {
            ASSERT0(ir1->getSSAInfo() && ir2->getSSAInfo());
            return ir1->getSSAInfo() == ir2->getSSAInfo();
        }
        //Try classic DU.
        goto CLASSIC_DU;
    }
    if (ir1->isMemRefNonPR()) {
        ASSERT0(ir2->isMemRefNonPR());
        MDSSAMgr const* mdssamgr = rg->getMDSSAMgr();
        if (mdssamgr != nullptr && mdssamgr->is_valid()) {
            MDSSAInfo * si1 = mdssamgr->getMDSSAInfoIfAny(ir1);
            MDSSAInfo * si2 = mdssamgr->getMDSSAInfoIfAny(ir2);
            ASSERT0(si1 && si2);
            return si1->getVOpndSet()->is_equal(*si2->getVOpndSet());
        }
        //Try classic DU.
        goto CLASSIC_DU;
    }
CLASSIC_DU:
    //TODO:Determine livein via classic du chain.
    return false;
}


bool hasSameUniqueMustDefForTree(IR const* ir1, IR const* ir2,
                                 Region const* rg)
{
    ASSERT0(ir1 && ir2 && ir1->is_exp() && ir2->is_exp());
    if (ir1->isMemOpnd() && ir2->isMemOpnd()) {
        if (hasSameUniqueMustDef(ir1, ir2, rg)) {
            //CASE:given kid1 is ILD, kid2 is ARRAY, if DEF of them are same
            //stmt, there is no need to analyze the kid of ILD and ARRAY.
            return true;
        }
        return false;
    }
    if (!ir1->isIREqual(ir2, rg->getIRMgr(), false)) { return false; }
    if (IR_MAX_KID_NUM(ir1) != IR_MAX_KID_NUM(ir2)) { return false; }
    for (UINT i = 0; i < IR_MAX_KID_NUM(ir1); i++) {
        IR const* kid1 = ir1->getKid(i);
        IR const* kid2 = ir2->getKid(i);
        if (!hasSameUniqueMustDefForTree(kid1, kid2, rg)) {
            return false;
        }
    }
    return true;
}


bool hasSameUniqueMustDef(IR const* ir1, IR const* ir2, Region const* rg)
{
    ASSERT0(ir1 && ir2 && ir1->is_exp() && ir2->is_exp());
    ASSERT0(ir1->isMemOpnd() && ir2->isMemOpnd());
    //For convservative purpose, some exp, such as livein variable,
    //there is no VN about it. Try infer the equality through DU chain.
    IR const* def1 = xoc::findUniqueMustDef(ir1, rg);
    IR const* def2 = xoc::findUniqueMustDef(ir2, rg);
    if (def1 == def2 && def1 != nullptr) { return true; }
    if ((ir1->isPROp() && ir2->isPROp()) ||
        (ir1->isMemRefNonPR() && ir2->isMemRefNonPR())) {
        return hasSameRegionLiveIn(ir1, ir2, rg);
    }
    return false;
}


static bool hasSameValueArrayOp(IR const* ir1, IR const* ir2, GVN const* gvn)
{
    ASSERT0(gvn && gvn->is_valid());
    ASSERT0(ir1->isArrayOp() && ir2->isArrayOp());
    IR const* sub1 = ARR_sub_list(ir1);
    IR const* sub2 = ARR_sub_list(ir2);
    for (; sub1 != nullptr && sub2 != nullptr;
         sub1 = sub1->get_next(), sub2 = sub2->get_next()) {
        VN const* v1 = gvn->getVN(sub1);
        VN const* v2 = gvn->getVN(sub2);
        if (v1 != nullptr && v1 == v2) { continue; }
        if (!hasSameUniqueMustDefForTree(sub1, sub2, gvn->getRegion())) {
            return false;
        }
    }
    if (sub1 != nullptr || sub2 != nullptr) {
        //Dimension is different.
        return false;
    }
    IR const* base1 = ARR_base(ir1);
    IR const* base2 = ARR_base(ir2);
    VN const* v1 = gvn->getVN(base1);
    VN const* v2 = gvn->getVN(base2);
    if ((v1 != v2 || v1 == nullptr) &&
        !hasSameUniqueMustDefForTree(base1, base2, gvn->getRegion())) {
        return false;
    }
    return ir1->isCover(ir2, gvn->getRegion());
}


static bool hasSameValueIndirectOp(IR const* ir1, IR const* ir2, GVN const* gvn)
{
    ASSERT0(ir1->isIndirectMemOp() && ir2->isIndirectMemOp());
    //Find the base expression that is not IndirectOp.
    //e.g: given ILD(ILD(ILD(p))), the function will reason out 'p'.

    //The number of indirect operation.
    //e.g: given ILD(ILD(ILD(p))), indirect_num is 3.
    UINT indirect_num1 = 0;
    VN const* basevn1 = getVNOfIndirectOp(ir1, &indirect_num1, gvn);
    UINT indirect_num2 = 0;
    VN const* basevn2 = getVNOfIndirectOp(ir2, &indirect_num2, gvn);

    //VN is not match or indirect level is not match
    if (basevn1 == basevn2 && basevn1 != nullptr &&
        indirect_num1 == indirect_num2) {
        return true;
    }
    IR const* base1 = ir1->getBase();
    IR const* base2 = ir2->getBase();
    if (!hasSameUniqueMustDefForTree(base1, base2, gvn->getRegion())) {
        return false;
    }
    return ir1->isCover(ir2, gvn->getRegion());
}


bool hasDef(IR const* exp, Region const* rg)
{
    ASSERT0(exp->is_exp());
    ASSERTN(exp->isMemOpnd(), ("should not query its DU"));
    //Prefer PRSSA and MDSSA DU.
    if (exp->isReadPR()) {
        PRSSAMgr const* prssamgr = rg->getPRSSAMgr();
        if (prssamgr != nullptr && prssamgr->is_valid()) {
            return prssamgr->hasDef(exp);
        }
        //Try classic DU.
        goto CLASSIC_DU;
    }
    if (exp->isMemRefNonPR()) {
        MDSSAMgr * mdssamgr = rg->getMDSSAMgr();
        if (mdssamgr != nullptr && mdssamgr->is_valid()) {
            ASSERTN(mdssamgr->getMDSSAInfoIfAny(exp),
                    ("exp does not have MDSSAInfo"));
            return mdssamgr->hasDef(exp);
        }
        //Try classic DU.
        goto CLASSIC_DU;
    }
CLASSIC_DU:
    ASSERTN(rg->getDUMgr(), ("DU Chain is not available"));
    return rg->getDUMgr()->hasDef(exp);
}


IR * findUniqueMustDef(IR const* exp, Region const* rg)
{
    ASSERT0(exp->is_exp());
    ASSERTN(exp->isMemOpnd(), ("should not query its DU"));
    //Prefer PRSSA and MDSSA DU.
    if (exp->isReadPR()) {
        PRSSAMgr const* prssamgr = rg->getPRSSAMgr();
        if (prssamgr != nullptr && prssamgr->is_valid()) {
            IR * d = PR_ssainfo(exp)->getDef();
            if (d != nullptr) {
                ASSERT0(d->getMustRef());
                return d;
            }
            return nullptr;
        }
        //Try classic DU.
        goto CLASSIC_DU;
    }
    if (exp->isMemRefNonPR()) {
        MDSSAMgr * mdssamgr = rg->getMDSSAMgr();
        if (mdssamgr != nullptr && mdssamgr->is_valid()) {
            ASSERTN(mdssamgr->getMDSSAInfoIfAny(exp),
                    ("exp does not have MDSSAInfo"));
            MDDef const* mddef = mdssamgr->findMustMDDef(exp);
            if (mddef == nullptr || mddef->is_phi()) { return nullptr; }
            return mddef->getOcc();
        }
        //Try classic DU.
        goto CLASSIC_DU;
    }
CLASSIC_DU:
    ASSERTN(rg->getDUMgr(), ("DU Chain is not available"));
    return rg->getDUMgr()->findNearestDomDef(exp);
}


IR * findUniqueDefInLoopForMustRef(IR const* exp, LI<IRBB> const* li,
                                   Region const* rg, OUT IRSet * set)
{
    ASSERT0(li && exp && exp->is_exp());
    ASSERTN(exp->isMemRef(), ("not memref operation"));
    MD const* mustref = exp->getMustRef();
    if (mustref == nullptr) { return nullptr; }

    //Prefer PRSSA and MDSSA DU.
    if (exp->isReadPR()) {
        PRSSAMgr const* prssamgr = rg->getPRSSAMgr();
        if (prssamgr != nullptr && prssamgr->is_valid()) {
            return prssamgr->findUniqueDefInLoopForMustRef(exp, li, rg, set);
        }
        //Try classic DU.
        goto CLASSIC_DU;
    }
    if (exp->isMemRefNonPR()) {
        MDSSAMgr * mdssamgr = rg->getMDSSAMgr();
        if (mdssamgr != nullptr && mdssamgr->is_valid()) {
            return mdssamgr->findUniqueDefInLoopForMustRef(exp, li, rg, set);
        }
        //Try classic DU.
        goto CLASSIC_DU;
    }
CLASSIC_DU:
    if (rg->getDUMgr() != nullptr) {
        return DUMgr::findUniqueDefInLoopForMustRef(exp, li, rg, set);
    }
    ASSERTN(0, ("DU Chain is not available"));
    return nullptr;
}


IR * findKillingDef(IR const* exp, Region const* rg)
{
    ASSERT0(exp->is_exp());
    ASSERTN(exp->isMemOpnd(), ("should not query its DU"));

    //Prefer PRSSA and MDSSA DU.
    if (exp->isReadPR()) {
        PRSSAMgr const* prssamgr = rg->getPRSSAMgr();
        if (prssamgr != nullptr && prssamgr->is_valid()) {
            ASSERT0(exp->getSSAInfo());
            return prssamgr->findKillingDefStmt(exp);
        }
        //Try classic DU.
        goto CLASSIC_DU;
    }
    if (exp->isMemRefNonPR()) {
        MDSSAMgr * mdssamgr = rg->getMDSSAMgr();
        if (mdssamgr != nullptr && mdssamgr->is_valid()) {
            ASSERTN(mdssamgr->getMDSSAInfoIfAny(exp),
                    ("exp does not have MDSSAInfo"));
            return mdssamgr->findKillingDefStmt(exp);
        }
        //Try classic DU.
        goto CLASSIC_DU;
    }
CLASSIC_DU:
    ASSERTN(rg->getDUMgr(), ("DU Chain is not available"));
    GVN * gvn = (GVN*)rg->getPassMgr()->queryPass(PASS_GVN);
    IR * domdef = rg->getDUMgr()->findNearestDomDef(exp);
    if (domdef != nullptr && isKillingDef(domdef, exp, gvn)) {
        return domdef;
    }
    return nullptr;
}


bool isUniqueDefInLoopForMustRef(
    IR const* ir, LI<IRBB> const* li, Region const* rg,
    MOD DefMiscBitSetMgr & sbsmgr)
{
    ASSERT0(ir && ir->is_stmt());
    ASSERTN(ir->isMemRef(), ("not memref operation"));
    MD const* mustref = ir->getMustRef();
    if (mustref == nullptr) { return false; }
    IRSet set(sbsmgr.getSegMgr());
    xoc::collectUseSetInLoop(ir, rg, li, &set);
    IRSetIter it = nullptr;
    BSIdx i = set.get_first(&it);
    if (i == BS_UNDEF) { return false; }
    bool is_unique = true;
    //Each USE of 'ir' has to have an unique DEF.
    for (; i != BS_UNDEF; i = set.get_next(i, &it)) {
        IR * exp = rg->getIR(i);
        ASSERT0(exp && exp->is_exp());
        IR const* def = xoc::findUniqueDefInLoopForMustRef(exp, li,
                                                           rg, nullptr);
        if (def == nullptr) {
            //Note exp should have at least one DEF, that is 'ir'.
            is_unique = false;
            break;
        }
    }
    return is_unique;
}


//Return true if ir1 is exact-cover of ir2 MD reference.
//Note this function does not check if there is DU chain between ir1 and ir2.
//gvn:if it is not NULL, the function will attempt to reason out the
//    relation between 'ir1' and 'ir2' through GVN info.
static bool isCoverExcludeCallStmt(IR const* ir1, IR const* ir2, GVN const* gvn)
{
    MD const* mustir2md = ir2->getMustRef();
    if (mustir2md != nullptr && isCover(ir1, mustir2md)) {
        return true;
    }
    if (gvn == nullptr || !gvn->is_valid()) { return false; }
    if (ir1->isIndirectMemOp() && ir2->isIndirectMemOp()) {
        return hasSameValueIndirectOp(ir1, ir2, gvn);
    }
    if (ir1->isArrayOp() && ir2->isArrayOp()) {
        return hasSameValueArrayOp(ir1, ir2, gvn);
    }
    return false;
}


//Return true if def is killing-def of use.
//Note this functin does not check if there is DU chain between def and use.
//gvn: if it is not NULL, the function will attempt to reason out the
//     relation between 'def' and 'use' through gvn info.
bool isKillingDef(IR const* def, IR const* use, GVN const* gvn)
{
    ASSERT0(def && use);
    ASSERT0(def->is_stmt() && use->is_exp());
    ASSERTN(def->isMemRef() && use->isMemRef(), ("should not query its DU"));
    if (def->isCallStmt() && def->getMayRef() != nullptr &&
        !def->getMayRef()->is_empty()) {
        //Can not determine whether call-stmt must def 'use'.
        return false;
    }
    return isCoverExcludeCallStmt(def, use, gvn);
}


//Return true if ir1's MD reference exactly cover ir2's MD reference.
//Note the function does not check if there is DU chain between ir1 and ir2.
//gvn:if it is not NULL, the function will attempt to reason out the
//    relation between 'ir1' and 'ir2' through gvn info.
bool isCover(IR const* ir1, IR const* ir2, GVN const* gvn)
{
    if (ir1 == ir2) { return true; }
    if (ir1->is_stmt() && ir2->is_exp()) {
        return isKillingDef(ir1, ir2, gvn);
    }
    if (ir2->is_stmt() && ir1->is_exp()) {
        return isKillingDef(ir2, ir1, gvn);
    }
    ASSERTN((ir1->is_stmt() && ir2->is_stmt()) ||
            (ir1->is_exp() && ir2->is_exp()),
            ("ir1 and ir2 must both be stmt or exp"));
    if (ir1->isCallStmt() && const_cast<IR*>(ir1)->getResultPR() == nullptr) {
        //Only return-value of call-stmt may generate covering info.
        return false;
    }
    if (ir2->isCallStmt() && const_cast<IR*>(ir2)->getResultPR() == nullptr) {
        //Only return-value of call-stmt may generate covering info.
        return false;
    }
    return isCoverExcludeCallStmt(ir1, ir2, gvn);
}


//Return true if ir1's MD reference exactly cover md2.
//Note the functin does not check if there is DU chain between ir1 and md2.
bool isCover(IR const* ir1, MD const* md2)
{
    ASSERT0(ir1 && md2);
    MD const* mustir1md = ir1->getMustRef();
    if (mustir1md == nullptr) { return false; }
    return isCover(mustir1md, md2);
}


bool isCover(MD const* md1, MD const* md2)
{
    ASSERT0(md1 && md2);
    if (md1 != nullptr && md2 != nullptr && md1->is_exact() &&
        (md1 == md2 || md1->is_exact_cover(md2))) {
        return true;
    }
    return false;
}


//Move IR_PHI and MDPhi from 'from' to 'to'.
//This function often used in updating PHI when adding new dominater BB to 'to'.
void movePhi(IRBB * from, IRBB * to, Region * rg)
{
    ASSERT0(from && to);
    PRSSAMgr const* prssamgr = rg->getPRSSAMgr();
    if (prssamgr != nullptr && prssamgr->is_valid()) {
        PRSSAMgr::movePhi(from, to);
    }
    MDSSAMgr * mdssamgr = rg->getMDSSAMgr();
    if (mdssamgr != nullptr && mdssamgr->is_valid()) {
        mdssamgr->movePhi(from, to);
    }
}


//Collect all USE expressions that inside loop of 'def' into 'useset'.
//This function give priority to PRSSA and MDSSA DU chain and then classic
//DU chain in doing collection.
void collectUseSetInLoop(IR const* def, Region const* rg, LI<IRBB> const* li,
                         OUT IRSet * useset)
{
    ASSERT0(def->is_stmt());
    ASSERTN(def->isMemRef(), ("should not query its DU"));
    useset->clean();
    SSAInfo const* prssainfo = def->isPROp() ? def->getSSAInfo() : nullptr;
    if (prssainfo != nullptr) {
        ASSERT0(rg->getPRSSAMgr() && rg->getPRSSAMgr()->is_valid());
        SSAUseIter sc;
        for (BSIdx u = SSA_uses(prssainfo).get_first(&sc);
             u != BS_UNDEF; u = SSA_uses(prssainfo).get_next(u, &sc)) {
            ASSERT0_DUMMYUSE(rg->getIR(u));
            IR * stmt = rg->getIR(u)->getStmt();
            if (li->isInsideLoop(stmt->getBB()->id())) {
                useset->bunion(u);
            }
        }
        return;
    }
    MDSSAMgr * mdssamgr = rg->getMDSSAMgr();
    if (mdssamgr != nullptr && mdssamgr->is_valid()) {
        MDSSAInfo const* mdssainfo = mdssamgr->getMDSSAInfoIfAny(def);
        if (mdssainfo != nullptr) {
            CollectCtx ctx(COLLECT_CROSS_PHI|COLLECT_INSIDE_LOOP);
            ctx.setLI(li);
            CollectUse cu(mdssamgr, mdssainfo, ctx, useset);
            return;
        }
    }
    if (rg->getDUMgr() != nullptr) {
        DUMgr::findUseInLoop(def, li, rg, useset);
        return;
    }
    ASSERTN(0, ("DU Chain is not available"));
}


//Collect all USE expressions of 'def' into 'useset'.
//This function give priority to PRSSA and MDSSA DU chain and then classic
//DU chain in doing collection.
void collectUseSet(IR const* def, Region const* rg, OUT IRSet * useset)
{
    ASSERT0(def->is_stmt());
    ASSERTN(def->isMemRef(), ("should not query its DU"));
    useset->clean();
    SSAInfo const* prssainfo = def->isPROp() ? def->getSSAInfo() : nullptr;
    if (prssainfo != nullptr) {
        ASSERT0(rg->getPRSSAMgr() && rg->getPRSSAMgr()->is_valid());
        SSAUseIter sc;
        for (BSIdx u = SSA_uses(prssainfo).get_first(&sc);
             u != BS_UNDEF; u = SSA_uses(prssainfo).get_next(u, &sc)) {
            ASSERT0_DUMMYUSE(rg->getIR(u));
            useset->bunion(u);
        }
        return;
    }
    MDSSAMgr * mdssamgr = rg->getMDSSAMgr();
    if (mdssamgr != nullptr && mdssamgr->is_valid()) {
        MDSSAInfo const* mdssainfo = mdssamgr->getMDSSAInfoIfAny(def);
        if (mdssainfo != nullptr) {
            CollectCtx ctx(COLLECT_CROSS_PHI);
            CollectUse cu(mdssamgr, mdssainfo, ctx, useset);
            return;
        }
    }
    if (def->readDUSet() != nullptr) {
        useset->copy((DefSBitSetCore&)*def->readDUSet());
        return;
    }
    ASSERTN(0, ("DU Chain is not available"));
}


//Collect all DEF expressions of 'use' into 'defset'.
//This function give priority to PRSSA and MDSSA DU chain and then classic
//DU chain in doing collection.
//The function will keep iterating DEF of PHI operand.
void collectDefSet(IR const* use, Region const* rg, OUT IRSet * defset)
{
    ASSERT0(defset && use->is_exp());
    ASSERTN(use->isMemRef(), ("should not query its DU"));
    defset->clean();

    SSAInfo const* prssainfo = use->isPROp() ? use->getSSAInfo() : nullptr;
    if (prssainfo != nullptr &&
        prssainfo->getDef()) { //PR may not have a DEF, e.g: parameter PR.
        ASSERT0(rg->getPRSSAMgr() && rg->getPRSSAMgr()->is_valid());
        defset->bunion(prssainfo->getDef()->id());
        return;
    }

    MDSSAMgr * mdssamgr = rg->getMDSSAMgr();
    MDSSAInfo const* mdssainfo = nullptr;
    if (mdssamgr != nullptr && mdssamgr->is_valid()) {
        mdssainfo = mdssamgr->getMDSSAInfoIfAny(use);
    }
    if (mdssainfo != nullptr) {
        CollectCtx ctx(COLLECT_CROSS_PHI);
        CollectDef cd(mdssamgr, mdssainfo, ctx, use->getMustRef(), defset);
        return;
    }

    if (use->readDUSet() != nullptr) {
        defset->copy((DefSBitSetCore&)*use->readDUSet());
        return;
    }

    ASSERTN(0, ("DU Chain is not available"));
}


IR * findNearestDomDef(IR const* exp, Region const* rg)
{
    ASSERT0(exp && rg);
    //Prefer PRSSA and MDSSA DU.
    if (exp->isReadPR()) {
        PRSSAMgr const* prssamgr = rg->getPRSSAMgr();
        if (prssamgr != nullptr && prssamgr->is_valid()) {
            ASSERT0(exp->getSSAInfo());
            return exp->getSSAInfo()->getDef();
        }
        //Try classic DU.
        goto CLASSIC_DU;
    }
    if (exp->isMemRefNonPR()) {
        ASSERT0(exp->isMemRefNonPR());
        MDSSAMgr const* mdssamgr = rg->getMDSSAMgr();
        if (mdssamgr != nullptr && mdssamgr->is_valid()) {
            MDDef const* mddef = mdssamgr->findNearestDef(exp);
            ASSERT0(mddef);
            return mddef->is_phi() ? nullptr : mddef->getOcc();
        }
        //Try classic DU.
        goto CLASSIC_DU;
    }
CLASSIC_DU:
    ASSERTN(rg->getDUMgr(), ("DU Chain is not available"));
    return rg->getDUMgr()->findNearestDomDef(exp);
}


//Find the nearest dominated DEF stmt of 'exp'.
//Note RPO of BB must be available.
//exp: given expression.
//defset: DEF stmt set of 'exp'.
IR * findNearestDomDef(IR const* exp, IRSet const& defset, Region const* rg)
{
    ASSERTN(rg->getDUMgr(), ("DUMgr is not available"));
    return rg->getDUMgr()->findNearestDomDef(exp, &defset);
}


//The function copy MDSSAInfo from 'src' to 'tgt'. Then add 'tgt' as an USE of
//the new MDSSAInfo.
void copyAndAddMDSSAOcc(IR * tgt, IR const* src, Region * rg)
{
    MDSSAMgr * mdssamgr = rg->getMDSSAMgr();
    if (mdssamgr != nullptr && mdssamgr->is_valid()) {
        MDSSAInfo const* src_mdssainfo = MDSSAMgr::getMDSSAInfoIfAny(src);
        if (src_mdssainfo != nullptr) {
            mdssamgr->copyAndAddMDSSAOcc(tgt, src_mdssainfo);
        }
    }
}


bool hasSameUniqueMustDefForIsomoKidTree(IR const* ir1, IR const* ir2,
                                         Region const* rg)
{
    if (ir1 == ir2) { return true; }
    ASSERTN(!ir1->is_leaf() && !ir2->is_leaf(),
            ("the function only handle IR with kid"));

    //No need to ask memory ref have to be same name, the function will
    //determine their equality by retrieving its value.
    //e.g: ild(x) is non-isomo to ist(y), but they may describe the same
    //memory if x's value is equal to y.
    ASSERT0(ir1->isIsomoTo(ir2, rg->getIRMgr(), true, IsomoFlag(ISOMO_UNDEF)));
    switch (ir1->getCode()) {
    SWITCH_CASE_INDIRECT_MEM_OP:
        ASSERT0(ir2->isIndirectMemOp());
        return hasSameUniqueMustDefForTree(ir1->getBase(), ir2->getBase(), rg);
    SWITCH_CASE_ARRAY_OP: {
        ASSERT0(ir2->isArrayOp());
        if (!xoc::hasSameUniqueMustDefForTree(ir1->getBase(), ir2->getBase(),
                                              rg)) {
            return false;
        }
        IR const* sub1 = ARR_sub_list(ir1);
        IR const* sub2 = ARR_sub_list(ir2);
        for (; sub1 != nullptr && sub2 != nullptr;
             sub1 = sub1->get_next(), sub2 = sub2->get_next()) {
            if (!hasSameUniqueMustDefForTree(sub1, sub2, rg)) {
                return false;
            }
        }
        ASSERTN(sub1 == nullptr && sub2 == nullptr, ("not isomo"));
        return true;
    }
    default:
        ASSERTN(IR_MAX_KID_NUM(ir1) == IR_MAX_KID_NUM(ir2),
                ("should be handled"));
        for (UINT i = 0; i < IR_MAX_KID_NUM(ir1); i++) {
            IR const* kid1 = ir1->getKid(i);
            IR const* kid2 = ir2->getKid(i);
            if (!hasSameUniqueMustDefForTree(kid1, kid2, rg)) {
                return false;
            }
        }
    }
    return false;
}


bool isLoopCarried(IR const* ir, Region const* rg, bool is_aggressive,
                   bool include_itselfstmt, LI<IRBB> const* li, GVN const* gvn,
                   OUT LoopDepInfo & info)
{
    ASSERT0(li);
    xcom::List<IR*> lst;
    for (BSIdx i = li->getBodyBBSet()->get_first();
         i != BS_UNDEF; i = li->getBodyBBSet()->get_next(i)) {
        IRBB * bb = rg->getBB(i);
        ASSERT0(bb);
        for (IR * stmt = BB_first_ir(bb);
             stmt != nullptr; stmt = BB_next_ir(bb)) {
            lst.append_tail(stmt);
        }
    }
    return isLoopCarried(ir, rg, is_aggressive, include_itselfstmt,
                         lst, li, gvn, info);
}


bool isLoopCarried(IR const* ir1, IR const* ir2, bool costly_analysis,
                   LI<IRBB> const* li, Region const* rg, GVN const* gvn,
                   OUT LoopDepInfo & info)
{
    if (!xoc::isDependent(ir1, ir2, costly_analysis, rg)) {
        return false;
    }
    if (!xoc::isLoopIndependent(ir1, ir2, costly_analysis, li, rg, gvn)) {
        LDI_kind(&info) = LOOP_DEP_CARRIED;
        LDI_src(&info) = ir1;
        info.setTgtIR(ir2);
        return true;
    }
    return false;
}


static bool hasLoopReduceDepInPRSSA(
    IR const* ir, Region const* rg, LI<IRBB> const* li,
    PRSSAMgr const* prssamgr, OUT LoopDepInfo & info)
{
    ASSERT0(ir->is_exp() && ir->isPROp());
    ASSERT0(ir->getSSAInfo());
    IR const* def = ir->getSSAInfo()->getDef();
    if (def == nullptr) { return false; }
    ASSERT0(def->getBB());
    if (!li->isInsideLoop(def->getBB()->id())) {  return false; }
    if (def->is_phi()) {
        //TODO:phi may not be in loopheader. If phi is place in loop body,
        //it may not be loop-carried.
        LDI_kind(&info) = LOOP_DEP_REDUCE;
        LDI_src(&info) = ir;
        info.setTgtIR(def);
        return true;
    }
    MD const* defmd = def->getMustRef();
    ASSERT0(defmd);
    MD const* irmd = ir->getMustRef();
    ASSERT0(irmd);
    if (isCover(defmd, irmd)) { return false; }

    //It may exist loop carried dependence if there is a MayDef between
    //ir and its Def in the DefDef Chain.
    LDI_kind(&info) = LOOP_DEP_REDUCE;
    LDI_src(&info) = ir;
    info.setTgtIR(def);
    return true;
}


static bool hasLoopReduceDepInMDSSA(
    IR const* ir, Region const* rg, LI<IRBB> const* li,
    MDSSAMgr const* mdssamgr, OUT LoopDepInfo & info)
{
    ASSERT0(ir->is_exp() && ir->isMemRef());
    ASSERT0(mdssamgr->hasMDSSAInfo(ir));
    MDDef * def = mdssamgr->findMustMDDef(ir);
    if (def == nullptr) { return false; }
    ASSERT0(def->getBB());
    if (!li->isInsideLoop(def->getBB()->id())) {  return false; }
    if (def->is_phi()) {
        //TODO:Handle the case that phi is not in loopheader, because that
        //if phi is placed in loop body, it may not be loop-carried.
        //e.g:there may exist loop carried dependence if there is a MayDef
        //between ir and its Def in the DefDef Chain.
        LDI_kind(&info) = LOOP_DEP_REDUCE;
        LDI_src(&info) = ir;
        info.setTgtMDDef(def);
        return true;
    }
    ASSERT0(def->getOcc());
    MD const* defmd = def->getOcc()->getMustRef();
    ASSERT0(defmd);
    MD const* irmd = ir->getMustRef();
    ASSERT0(irmd);
    if (isCover(defmd, irmd)) { return false; }

    //It may exist loop carried dependence if there is a MayDef between
    //ir and its Def in the DefDef Chain.
    LDI_kind(&info) = LOOP_DEP_REDUCE;
    LDI_src(&info) = ir;
    info.setTgtIR(def->getOcc());
    return true;
}


bool hasLoopReduceDep(
    IR const* ir, Region const* rg, LI<IRBB> const* li, OUT LoopDepInfo & info)
{
    ASSERT0(ir->is_exp() && ir->isMemRef());
    if (ir->isMemRefNonPR()) {
        MDSSAMgr * mdssamgr = rg->getMDSSAMgr();
        if (mdssamgr != nullptr && mdssamgr->is_valid()) {
            ASSERT0(mdssamgr->hasMDSSAInfo(ir));
            return hasLoopReduceDepInMDSSA(ir, rg, li, mdssamgr, info);
        }
    } else if (ir->isPROp()) {
        PRSSAMgr const* prssamgr = rg->getPRSSAMgr();
        if (prssamgr != nullptr && prssamgr->is_valid()) {
            ASSERT0(ir->getSSAInfo());
            return hasLoopReduceDepInPRSSA(ir, rg, li, prssamgr, info);
        }
    }
    DUMgr * dumgr = rg->getDUMgr();
    if (dumgr != nullptr) {
        ASSERT0(ir->getDUSet());
        UNREACHABLE(); //TODO:
    }
    return true;
}


bool hasLoopReduceDepForIRTree(IR const* ir, Region const* rg,
                               LI<IRBB> const* li)
{
    ASSERT0(ir->is_exp());
    ConstIRIter it;
    LoopDepInfo tmp;
    for (IR const* x = xoc::iterInitC(ir, it, false);
         x != nullptr; x = xoc::iterNextC(it, true)) {
        if (!x->isMemRef()) { continue; }
        if (hasLoopReduceDep(x, rg, li, tmp)) {
            //TODO:Handle overlapped stmt by applying loop peeling or
            //loop fission.
            return true;
        }
    }
    return false;
}


static bool hasMoreThanOneDefInBBForMustRef(
    IR const* ir, IRBB const* bb, Region const* rg, OUT UINT & defcnt)
{
    ASSERT0(defcnt <= 1);
    IRIter irit;
    IRBB * pbb = const_cast<IRBB*>(bb);
    for (IR const* t = BB_first_ir(pbb); t != nullptr; t = BB_next_ir(pbb)) {
        if (t == ir || xoc::isDependent(t, ir, true, rg)) {
            defcnt++;
            if (defcnt > 1) { return true; }
        }
    }
    return true;
}


bool hasMoreThanOneDefInLoopForMustRef(
    IR const* ir, Region const* rg, LI<IRBB> const* li, OUT UINT & defcnt)
{
    ASSERT0(ir->isMemRef());
    defcnt = 0;
    for (BSIdx i = li->getBodyBBSet()->get_first();
         i != BS_UNDEF; i = li->getBodyBBSet()->get_next(i)) {
        IRBB * bb = rg->getBB(i);
        ASSERT0(bb);
        if (hasMoreThanOneDefInBBForMustRef(ir, bb, rg, defcnt)) {
            return true;
        }
    }
    return false;
}


bool hasUniqueDefInLoopForMustRef(
    IR const* ir, Region const* rg, LI<IRBB> const* li)
{
    ASSERT0(ir && ir->isMemRef() && ir->getMustRef());
    if (ir->is_stmt()) {
        DefMiscBitSetMgr sbsmgr;
        return xoc::isUniqueDefInLoopForMustRef(ir, li, rg, sbsmgr);
    }
    ASSERT0(ir->is_exp());
    IR const* def = xoc::findUniqueDefInLoopForMustRef(ir, li, rg, nullptr);
    return def != nullptr;
}


bool isLoopCarriedForIRTree(
    IR const* ir, Region const* rg, bool is_aggressive, bool include_itselfstmt,
    xcom::List<IR*> const& lst, LI<IRBB> const* li, GVN const* gvn,
    OUT LoopDepInfo & info)
{
    ConstIRIter it;
    for (IR const* x = xoc::iterInitC(ir, it, false);
         x != nullptr; x = xoc::iterNextC(it, true)) {
        if (!x->isMemRefNonPR()) { continue; }
        if (isLoopCarried(x, rg, is_aggressive, include_itselfstmt,
                          lst, li, gvn, info)) {
            //TODO:Handle overlapped stmt by applying loop peeling or
            //loop fission.
            return true;
        }
    }
    return false;
}


bool isLoopCarried(
    IR const* ir, Region const* rg, bool is_aggressive, bool include_itselfstmt,
    xcom::List<IR*> const& lst, LI<IRBB> const* li, GVN const* gvn,
    OUT LoopDepInfo & info)
{
    ASSERT0(ir);
    IR const* irstmt = ir->is_stmt() ? ir : ir->getStmt();
    ASSERT0(irstmt);
    xcom::List<IR*>::Iter it;
    for (IR * cand = lst.get_head(&it);
         cand != nullptr; cand = lst.get_next(&it)) {
        if (cand == ir) {
            //If 'cand' is equal to 'ir', this is loop-independent dependence.
            continue;
        }
        if (cand == irstmt && !include_itselfstmt) { continue; }
        if (xoc::isLoopCarried(ir, cand, is_aggressive, li, rg, gvn, info)) {
            return true;
        }
    }
    return false;
}


bool isLoopIndependent(IR const* ir1, IR const* ir2, bool costly_analysis,
                       LI<IRBB> const* li, Region const* rg, GVN const* gvn)
{
    if (ir1 == ir2) { return true; }
    if (!ir1->isMemRef() || !ir2->isMemRef()) { return false; }
    ASSERTN(xoc::isDependent(ir1, ir2, costly_analysis, rg),
            ("user should rule out cases that ir1 and ir2 are independent"));
    IRBB const* ir1bb = ir1->is_stmt() ? ir1->getBB() : ir1->getStmt()->getBB();
    IRBB const* ir2bb = ir2->is_stmt() ? ir2->getBB() : ir2->getStmt()->getBB();
    ASSERT0(ir1bb && ir2bb);
    if (!li->isInsideLoop(ir1bb->id()) || !li->isInsideLoop(ir2bb->id())) {
        return true;
    }
    MD const* ir1ref = ir1->getExactRef();
    MD const* ir2ref = ir2->getExactRef();
    if (ir1ref && ir2ref && ir1ref == ir2ref) { return true; }
    if (ir1->isIsomoTo(ir2, rg->getIRMgr(), true, IsomoFlag(ISOMO_CK_ALL))) {
        //CASE:All kind of isomophism checking need to perform.
        //e.g: given two references of 'x',
        //  st:u32 'x' = ...
        //  ...        = ld:any 'x'
        //The two references of x are NOT isomophic because their type are
        //differents.
        return true;
    }
    if (gvn != nullptr && gvn->is_valid() && gvn->isSameMemLoc(ir1, ir2)) {
        //GVN tell us that ir1 and ir2 are reference the same memory address
        //except the memory size.
        return true;
    }
    if (ir1->is_leaf() || ir2->is_leaf()) {
        //There is no method to determine direct-memory-op dependence.
        return false;
    }
    //For now, we attempt to judge ir1 and ir2 value equivalence via killing
    //DEF. And do isomophic-check before quering MustDef comparison.
    if (!ir1->isIsomoTo(ir2, rg->getIRMgr(), true, IsomoFlag(ISOMO_UNDEF))) {
        return false;
    }
    //TODO:handle the comparison between ILD/IST and ARRAY.
    return hasSameUniqueMustDefForIsomoKidTree(ir1, ir2, rg);
}


//Return true if ir1 is may overlap to phi.
bool isDependent(IR const* ir, MDPhi const* phi)
{
    ASSERT0(ir && phi);
    MD const* irmust = ir->getMustRef();
    MDSet const* irmay = ir->getMayRef();
    MDIdx phimdid = phi->getResult()->mdid();
    ASSERT0(phimdid != MD_UNDEF);
    if ((irmust != nullptr && irmust->id() == phimdid) ||
        (irmay != nullptr && irmay->is_contain_pure(phimdid))) {
        return true;
    }
    return false;
}


//CALL, ICALL may have sideeffect, we are not only check
//the MustRef, but also consider the overlapping between MayRef.
static bool isDependentForCallStmt(IR const* ir1, IR const* ir2,
                                   bool costly_analysis, Region const* rg)
{
    ASSERT0(ir1->isCallStmt() && !ir2->is_undef());
    MD const* mustdef = ir1->getMustRef();
    MDSet const* maydef = ir1->getMayRef();
    MD const* mustuse = ir2->getMustRef();
    MDSet const* mayuse = ir2->getMayRef();
    //Compare MustRef firstly.
    if (mustdef != nullptr) {
        if (mustuse != nullptr) {
            if (mustdef == mustuse || mustdef->is_overlap(mustuse)) {
                return true;
            }
            if (maydef != nullptr && maydef->is_overlap(mustuse, rg)) {
                return true;
            }
            if (maydef != nullptr && mayuse != nullptr &&
                mayuse->is_intersect(*maydef)) {
                return true;
            }
            return false;
        }
        if (mayuse != nullptr) {
            if (costly_analysis) {
                //The function will iterate elements in MDSet, which is costly.
                return mayuse->is_overlap_ex(mustdef, rg, rg->getMDSystem()) ||
                       maydef == mayuse ||
                       (maydef != nullptr && mayuse->is_intersect(*maydef));
            }
            return mayuse->is_contain(mustdef, rg) || maydef == mayuse ||
                   (maydef != nullptr && mayuse->is_intersect(*maydef));
        }
        UNREACHABLE();
        return false;
    }
    if (maydef != nullptr) {
        if (mustuse != nullptr) {
            if (costly_analysis) {
                //The function will iterate elements in MDSet, which is costly.
                return maydef->is_overlap_ex(mustuse, rg, rg->getMDSystem());
            }
            return maydef->is_contain(mustuse, rg);
        }
        if (mayuse != nullptr) {
            return mayuse == maydef || mayuse->is_intersect(*maydef);
        }
        UNREACHABLE();
        return false;
    }
    UNREACHABLE();
    return false;
}


bool isDependent(IR const* ir1, IR const* ir2, bool costly_analysis,
                 Region const* rg)
{
    ASSERT0(!ir1->is_undef() && !ir2->is_undef());
    if (ir1 == ir2) { return true; }
    if (ir1->isCallStmt()) {
       return isDependentForCallStmt(ir1, ir2, costly_analysis, rg);
    }
    MD const* must1 = ir1->getMustRef();
    MDSet const* may1 = ir1->getMayRef();
    MD const* must2 = ir2->getMustRef();
    MDSet const* may2 = ir2->getMayRef();
    if (must1 != nullptr) {
        if (must2 != nullptr && !must1->is_may() && !must2->is_may()) {
            //CASE: the MustRef of ID may be GLOBAL_MEM or IMPORT_MEM.
            return must1 == must2 || must1->is_overlap(must2);
        }
        if (may2 != nullptr) {
            if (costly_analysis) {
                //The function will iterate all elements in MDSet,
                //which is costly.
                return may2->is_overlap_ex(must1, rg, rg->getMDSystem());
            }
            return may2->is_contain(must1, rg);
        }
        return false;
    }
    if (may1 != nullptr) {
        if (must2 != nullptr) {
            if (costly_analysis) {
                //The function will iterate all elements in MDSet,
                //which is costly.
                return may1->is_overlap_ex(must2, rg, rg->getMDSystem());
            }
            return may1->is_contain(must2, rg);
        }
        if (may2 != nullptr) {
            return may2 == may1 || may2->is_intersect(*may1);
        }
        return false;
    }
    return false;
}


bool isDependentForTree(IR const* ir1, IR const* ir2, bool costly_analysis,
                        Region const* rg)
{
    if (ir1 == ir2) { return true; }
    ConstIRIter it;
    //Only iterate the IR tree that root is ir2.
    for (IR const* x = iterInitC(ir2, it, false);
         x != nullptr; x = iterNextC(it, true)) {
        if (!x->isMemOpnd()) { continue; }
        if (x == ir1) { return true; }
        if (isDependent(ir1, x, costly_analysis, rg)) { return true; }
    }
    return false;
}


void findAndSetLiveInDef(IR * root, IR * startir, IRBB * startbb, Region * rg,
                         OptCtx const& oc)
{
    ASSERT0(root->is_exp());
    PRSSAMgr * prssamgr = rg->getPRSSAMgr();
    MDSSAMgr * mdssamgr = rg->getMDSSAMgr();
    bool use_prssa = prssamgr != nullptr && prssamgr->is_valid();
    bool use_mdssa = mdssamgr != nullptr && mdssamgr->is_valid();
    if (!use_prssa && !use_mdssa) { return; }
    IRIter it;
    for (IR * x = iterInit(root, it, true);
         x != nullptr; x = iterNext(it, true)) {
        if (!x->isMemRef()) { continue; }
        if (use_mdssa && x->isMemRefNonPR()) {
            mdssamgr->findAndSetLiveInDef(x, startir, startbb, oc);
            continue;
        }
        if (use_prssa && x->isReadPR()) {
            prssamgr->findAndSetLiveinDef(x);
            continue;
        }
        //NOTE classic DU chain does not need to find the live-in DEF.
    }
}


static void removeUseOfNonPR(IR const* call, Region const* rg,
                             MOD DUMgr * dumgr)
{
    ASSERT0(call->isCallStmt());
    DUSet * useset = call->getDUSet();
    if (useset == nullptr) { return; }
    DUSetIter di = nullptr;
    BSIdx nexti;
    for (BSIdx i = useset->get_first(&di); i != BS_UNDEF; i = nexti) {
        nexti = useset->get_next(i, &di);
        IR const* use = rg->getIR(i);
        if (use->isMemRefNonPR()) {
            useset->remove(i, *dumgr->getSBSMgr());
        }
    }
}


static void removeUseOfPR(IR const* call, Region const* rg, MOD DUMgr * dumgr)
{
    ASSERT0(call->isCallStmt());
    DUSet * useset = call->getDUSet();
    if (useset == nullptr) { return; }
    DUSetIter di = nullptr;
    BSIdx nexti;
    for (BSIdx i = useset->get_first(&di); i != BS_UNDEF; i = nexti) {
        nexti = useset->get_next(i, &di);
        IR const* use = rg->getIR(i);
        if (use->isReadPR()) {
            useset->remove(i, *dumgr->getSBSMgr());
        }
    }
}


static inline void removeClassicDUChainForIR(
    IR * ir, Region * rg, DUMgr * dumgr, bool rmprdu, bool rmnonprdu)
{
    ASSERT0(rmprdu ^ rmnonprdu);
    if (ir->isCallStmt()) {
        if (rmprdu) {
            //If classic NonPR DU chain is retained, then CallStmt's DUSet
            //can not be freed, because it DUSet also contain NonPR MD, and
            //it still be DEF to some memory-ref operation.
            removeUseOfPR(ir, rg, dumgr);
            return;
        }
        //If classic PR DU chain is retained, then CallStmt's DUSet
        //can not be freed, because it DUSet also contain PR MD, and
        //it still be DEF to some PR operation.
        ASSERT0(ir->hasDU());
        removeUseOfNonPR(ir, rg, dumgr);
        return;
    }
    if (rmprdu && ir->isPROp()) {
        ASSERT0(ir->hasDU());
        ir->freeDUset(dumgr);
    }
    if (rmnonprdu && ir->isMemRefNonPR()) {
        ASSERT0(ir->hasDU());
        ir->freeDUset(dumgr);
    }
}


void removeClassicDUChain(Region * rg, bool rmprdu, bool rmnonprdu)
{
    if (!rmprdu && !rmnonprdu) { return; }
    if (rmprdu && rmnonprdu) {
        //Remove all DUSets at once.
        if (rg->getDUMgr() != nullptr) {
            rg->getDUMgr()->removeAllDUChain();
        }
        return;
    }
    DUMgr * dumgr = rg->getDUMgr();
    if (dumgr == nullptr) { return; }
    if (rg->getIRList() != nullptr) {
        IRIter it;
        for (IR * ir = iterInit(rg->getIRList(), it, true);
             ir != nullptr; ir = iterNext(it, true)) {
            removeClassicDUChainForIR(ir, rg, dumgr, rmprdu, rmnonprdu);
        }
        return;
    }
    BBList * bblst = rg->getBBList();
    if (bblst == nullptr) { return; }
    IRIter it;
    for (IRBB const* bb = bblst->get_head(); bb != nullptr;
         bb = bblst->get_next()) {
        for (IR * ir = const_cast<IRBB*>(bb)->getIRList().get_head();
             ir != nullptr;
             ir = const_cast<IRBB*>(bb)->getIRList().get_next()) {
            it.clean();
            for (IR * t = iterInit(ir, it, true);
                 t != nullptr; t = iterNext(it, true)) {
                removeClassicDUChainForIR(t, rg, dumgr, rmprdu,
                                          rmnonprdu);
            }
        }
    }
}


void destructInvalidClassicDUChain(Region * rg, OptCtx const& oc)
{
    removeClassicDUChain(rg, !oc.is_pr_du_chain_valid(),
                         !oc.is_nonpr_du_chain_valid());
}


//
//START CopmuteMD2DefCnt
//
void ComputeMD2DefCnt::dump() const
{
    if (!m_rg->isLogMgrInit()) { return; }
    MD2UINTPtrIter it;
    UINT * cnt;
    note(m_rg, "\n==-- MD2DefCnt --==");
    m_rg->getLogMgr()->incIndent(2);
    for (MD const* md = m_md2defcnt.get_first(it, &cnt);
         !it.end(); md = m_md2defcnt.get_next(it, &cnt)) {
        ASSERT0(md && cnt);
        note(m_rg, "\nMD%u,'%s',DEFCNT:%u", md->id(),
             md->get_base()->get_name()->getStr(), *cnt);
    }
    m_rg->getLogMgr()->decIndent(2);
}


void ComputeMD2DefCnt::computeBB(IRBB const* bb, OUT ConstIRList & only_maydef)
{
    IRBB * pbb = const_cast<IRBB*>(bb);
    BBIRListIter it;
    for (IR * ir = pbb->getIRList().get_head(&it);
         ir != nullptr; ir = pbb->getIRList().get_next(&it)) {
        updateMD2DefCnt(ir, only_maydef);
    }
}


void ComputeMD2DefCnt::applyMayDefEffect(ConstIRList const& only_maydef)
{
    ConstIRListIter irit;
    for (IR const* ir = only_maydef.get_head(&irit);
         ir != nullptr; ir = only_maydef.get_next(&irit)) {
        ASSERT0(ir->getMustRef() == nullptr || ir->isCallStmt());
        MDSet const* mds = ir->getMayRef();
        if (mds == nullptr) { continue; }
        MD2UINTPtrIter it;
        UINT * cnt = nullptr;
        for (MD const* md = m_md2defcnt.get_first(it, &cnt);
             !it.end(); md = m_md2defcnt.get_next(it, &cnt)) {
            ASSERT0(md && cnt);
            if (mds->is_contain(md, m_rg)) {
                (*cnt)++;
            }
        }
    }
    for (IR const* ir = only_maydef.get_head(&irit);
         ir != nullptr; ir = only_maydef.get_next(&irit)) {
        MDSet const* mds = ir->getMayRef();
        if (mds == nullptr) { continue; }
        updateMDSet2DefCnt(mds, nullptr);
    }
}


void ComputeMD2DefCnt::updateMDSet2DefCnt(MDSet const* may, MD const* must)
{
    ASSERT0(may);
    MDSetIter iter;
    for (BSIdx i = may->get_first(&iter);
         i != BS_UNDEF; i = may->get_next(i, &iter)) {
        MD * t = m_md_sys->getMD(i);
        if (t == must) {
            //MayDef set allows containing must-ref.
            continue;
        }
        UINT * n = m_md2defcnt.get(t);
        if (n == nullptr) {
            n = (UINT*)xmalloc(sizeof(UINT));
            m_md2defcnt.set(t, n);
        }
        (*n)++;
    }
}


void ComputeMD2DefCnt::updateMD2DefCnt(
    IR const* ir, OUT ConstIRList & only_maydef)
{
    if (!ir->hasResult()) {
        ASSERTN(!ir->isMemRef(), ("TODO"));
        return;
    }
    MD const* must = ir->getMustRef();
    if (must != nullptr) {
        UINT * n = m_md2defcnt.get(const_cast<MD*>(must));
        if (n == nullptr) {
            n = (UINT*)xmalloc(sizeof(UINT));
            m_md2defcnt.set(must, n);
        }
        (*n)++;
    } else {
        only_maydef.append_tail(ir);
        return; //Leave the MayDef counting to applyMayDefEffect().
    }
    if (ir->isCallStmt() && !ir->isReadOnly()) {
        //Leave the CALL's MayDef counting to applyMayDefEffect().
        only_maydef.append_tail(ir);
    }
    MDSet const* mds = ir->getMayRef();
    if (mds == nullptr) { return; }
    updateMDSet2DefCnt(mds, must);
}


bool ComputeMD2DefCnt::isUniqueDef(IR const* ir) const
{
    ASSERT0(ir->getMustRef());
    UINT cnt = getMustRefDefCnt(ir);
    return cnt == 1;
}


bool ComputeMD2DefCnt::isUniqueDefStrict(IR const* ir) const
{
    MD const* md = ir->getMustRef();
    if (md == nullptr) {
        //Can not determine the count of MayDef MDs.
        return false;
    }
    UINT * n = m_md2defcnt.get(md);
    ASSERTN(n, ("should call updateMD2Num() first"));
    if (*n > 1) { return false; }

    MDTab * mdt = const_cast<MDSystem*>(m_md_sys)->getMDTab(MD_base(md));
    if (mdt == nullptr) { return true; }

    MD const* x = mdt->get_effect_md();
    if (x != nullptr && x != md && x->is_overlap(md)) {
        UINT * n2 = m_md2defcnt.get(x);
        if (n2 != nullptr && *n2 > 1) { return false; }
    }

    OffsetTab * ofstab = mdt->get_ofst_tab();
    if (ofstab == nullptr) { return true; }
    if (ofstab->get_elem_count() == 0) { return true; }

    ConstMDIter mditer;
    for (MD const* x2 = ofstab->get_first(mditer, nullptr);
         x2 != nullptr; x2 = ofstab->get_next(mditer, nullptr)) {
        if (x2 != md && x2->is_overlap(md)) {
            UINT * n2 = m_md2defcnt.get(x2);
            if (n2 != nullptr && *n2 > 1) { return false; }
        }
    }
    return true;
}


UINT ComputeMD2DefCnt::getMDDefCnt(MD const* md) const
{
    ASSERT0(md);
    UINT * np = m_md2defcnt.get(md);
    return np != nullptr ? *np : 0;
}


UINT ComputeMD2DefCnt::getMustRefDefCnt(IR const* ir) const
{
    MD const* md = ir->getMustRef();
    ASSERTN(md, ("can not determine the count of MayDef MDs."));
    return getMDDefCnt(md);
}


void ComputeMD2DefCnt::compute()
{
    IRBB * head = m_li->getLoopHead();
    ASSERTN(head, ("loopinfo is invalid"));
    UINT headid = head->id();
    ConstIRList only_maydef; //record the stmt that only have MayDef.
    for (BSIdx i = m_li->getBodyBBSet()->get_first();
         i != BS_UNDEF; i = m_li->getBodyBBSet()->get_next(i)) {
        if (i != (BSIdx)headid && !m_cfg->is_dom(headid, i)) {
            //The BB that does not affect loop body will be skipped.
            //Note loop head will take particapate in the computation.
            //The candidate BB must dominate all other loop body BBs.
            continue;
        }
        IRBB * bb = m_cfg->getBB(i);
        ASSERT0(bb && bb->getVex());
        computeBB(bb, only_maydef);
    }
    applyMayDefEffect(only_maydef);
}
//END ComputeMD2DefCnt

} //namespace xoc
