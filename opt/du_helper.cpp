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
        DUSet * oldduset = olddef->getDUSet();
        if (oldduset != nullptr) {
            dumgr->changeDef(newdef, olddef);
            classic_du_changed = true;
        }
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
    if (dumgr != nullptr && olduse->getDUSet() != nullptr) {
        dumgr->changeUse(newuse, olduse);
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


//Remove Use-Def chain.
//exp: the expression to be removed.
//e.g: ir = ...
//    = ir //S1
//If S1 will be deleted, ir should be removed from its useset in MDSSAInfo.
//NOTE: the function only process exp itself.
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


//Remove all DU info of 'stmt'.
//Note do NOT remove stmt from BBIRList before call the function.
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
    ASSERT0(to->isIREqual(from, true));

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
        ASSERT0(to_ir->isIREqual(from_ir, true));
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
VN const* getVNOfIndirectOp(IR const* ir, UINT * indirect_level,
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
    if (ir1->getCode() != ir2->getCode()) { return false; }
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
    if (def1 == def2) {
        if (def1 != nullptr) { return true; }
        if ((ir1->isPROp() && ir2->isPROp()) ||
            (ir1->isMemRefNonPR() && ir2->isMemRefNonPR())) {
            return hasSameRegionLiveIn(ir1, ir2, rg);
        }
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
            MDDef const* mddef = mdssamgr->findMustDef(exp);
            if (mddef == nullptr || mddef->is_phi()) { return nullptr; }
            return mddef->getOcc();
        }
        //Try classic DU.
        goto CLASSIC_DU;
    }
CLASSIC_DU:
    if (rg->getDUMgr() != nullptr) {
        DUSet const* du = exp->readDUSet();
        if (du != nullptr) {
            IR const* exp_stmt = const_cast<IR*>(exp)->getStmt();
            return rg->getDUMgr()->findNearestDomDef(exp, exp_stmt, du);
        }
        return nullptr;
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
            IR * d = PR_ssainfo(exp)->getDef();
            if (d != nullptr && d->getExactRef() != nullptr) {
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
            return mdssamgr->findKillingDefStmt(exp);
        }
        //Try classic DU.
        goto CLASSIC_DU;
    }

CLASSIC_DU:
    if (rg->getDUMgr() != nullptr) {
        DUSet const* du = exp->readDUSet();
        if (du != nullptr) {
            IR const* exp_stmt = const_cast<IR*>(exp)->getStmt();
            return rg->getDUMgr()->findNearestDomDef(exp, exp_stmt, du);
        }
        return nullptr;
    }

    ASSERTN(0, ("DU Chain is not available"));
    return nullptr;
}


//Return true if def is killing-def of use.
//Note this functin does not check if there is DU chain between def and use.
//gvn: if it is not NULL, the function will attempt to reason out the
//     relation between 'def' and 'use' through gvn info.
bool isKillingDef(IR const* def, IR const* use, GVN const* gvn)
{
    ASSERT0(def && use);
    ASSERTN(def->isMemRef() && use->isMemRef(), ("should not query its DU"));
    MD const* mustusemd = use->getMustRef();
    if (mustusemd != nullptr && isKillingDef(def, mustusemd)) {
        return true;
    }
    if (gvn == nullptr || !gvn->is_valid()) { return false; }
    if (def->isIndirectMemOp() && use->isIndirectMemOp()) {
        return hasSameValueIndirectOp(def, use, gvn);
    }
    if (def->isArrayOp() && use->isArrayOp()) {
        return hasSameValueArrayOp(def, use, gvn);
    }
    return false;
}


//Return true if def is killing-def of usemd.
//Note this functin does not check if there is DU chain between def and usemd.
bool isKillingDef(IR const* def, MD const* usemd)
{
    ASSERT0(def && usemd);
    MD const* mustdefmd = def->getMustRef();
    if (mustdefmd == nullptr) { return false; }
    return isKillingDef(mustdefmd, usemd);
}


//Return true if defmd is killing-def MD of usemd.
//Note this functin does not check if there is DU chain between defmd and usemd.
bool isKillingDef(MD const* defmd, MD const* usemd)
{
    ASSERT0(defmd && usemd);
    if (defmd != nullptr &&
        usemd != nullptr &&
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
    PRSSAMgr const* prssamgr = rg->getPRSSAMgr();
    if (prssamgr != nullptr && prssamgr->is_valid()) {
        PRSSAMgr::movePhi(from, to);
    }

    MDSSAMgr * mdssamgr = rg->getMDSSAMgr();
    if (mdssamgr != nullptr && mdssamgr->is_valid()) {
        mdssamgr->movePhi(from, to);
    }
}


//Collect all USE expressions of 'def' into 'useset'.
//This function give priority to PRSSA and MDSSA DU chain and then classic
//DU chain in doing collection.
void collectUseSet(IR const* def, MDSSAMgr const* mdssamgr, OUT IRSet * useset)
{
    ASSERT0(def->is_stmt());
    ASSERTN(def->isMemRef(), ("should not query its DU"));
    useset->clean();

    SSAInfo const* prssainfo = def->isPROp() ? def->getSSAInfo() : nullptr;
    if (prssainfo != nullptr) {
        SSAUseIter sc;
        Region * rg = mdssamgr->getRegion();
        for (BSIdx u = SSA_uses(prssainfo).get_first(&sc);
             u != BS_UNDEF; u = SSA_uses(prssainfo).get_next(u, &sc)) {
            ASSERT0_DUMMYUSE(rg->getIR(u));
            useset->bunion(u);
        }
        return;
    }

    if (mdssamgr != nullptr && mdssamgr->is_valid()) {
        MDSSAInfo const* mdssainfo = mdssamgr->getMDSSAInfoIfAny(def);
        if (mdssainfo != nullptr) {
            CollectCtx ctx(COLLECT_CROSS_PHI);
            mdssainfo->collectUse(
                const_cast<MDSSAMgr*>(mdssamgr)->getUseDefMgr(), ctx, useset);
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
void collectDefSet(IR const* use, MDSSAMgr const* mdssamgr, OUT IRSet * defset)
{
    ASSERT0(defset && use->is_exp());
    ASSERTN(use->isMemRef(), ("should not query its DU"));
    defset->clean();

    SSAInfo const* prssainfo = use->isPROp() ? use->getSSAInfo() : nullptr;
    if (prssainfo != nullptr &&
        prssainfo->getDef()) { //PR may not have a DEF, e.g: parameter PR.
        defset->bunion(prssainfo->getDef()->id());
        return;
    }

    MDSSAInfo const* mdssainfo = nullptr;
    if (mdssamgr != nullptr && mdssamgr->is_valid()) {
        mdssainfo = mdssamgr->getMDSSAInfoIfAny(use);
    }
    if (mdssainfo != nullptr) {
        CollectCtx ctx(COLLECT_CROSS_PHI);
        mdssainfo->collectDef(mdssamgr, use->getMustRef(), ctx, defset);
        return;
    }

    if (use->readDUSet() != nullptr) {
        defset->copy((DefSBitSetCore&)*use->readDUSet());
        return;
    }

    ASSERTN(0, ("DU Chain is not available"));
}


//Find the nearest dominated DEF stmt of 'exp'.
//Note RPO of BB must be available.
//exp: given expression.
//defset: DEF stmt set of 'exp'.
//omit_self: true if we do not consider the stmt of 'exp' itself.
IR * findNearestDomDef(IR const* exp, IRSet const& defset, Region const* rg,
                       bool omit_self)
{
    ASSERT0(exp->is_exp());
    IR const* stmt_of_exp = exp->getStmt();
    INT stmt_rpo = stmt_of_exp->getBB()->rpo();
    ASSERT0(stmt_rpo != RPO_UNDEF);
    IR * last = nullptr;
    INT lastrpo = RPO_UNDEF;
    IRSetIter it = nullptr;
    for (BSIdx i = defset.get_first(&it);
         i != BS_UNDEF; i = defset.get_next(i, &it)) {
        IR * d = rg->getIR(i);
        ASSERT0(d->is_stmt());
        if (omit_self && d == stmt_of_exp) {
            continue;
        }

        if (last == nullptr) {
            last = d;
            lastrpo = d->getBB()->rpo();
            ASSERT0(lastrpo >= 0);
            continue;
        }

        IRBB * dbb = d->getBB();
        ASSERT0(dbb);
        ASSERT0(dbb->rpo() >= 0);
        if (dbb->rpo() < stmt_rpo && dbb->rpo() > lastrpo) {
            last = d;
            lastrpo = dbb->rpo();
        } else if (dbb == last->getBB() && dbb->is_dom(last, d, true)) {
            last = d;
            lastrpo = dbb->rpo();
        }
    }
    if (last == nullptr) { return nullptr; }

    IRBB const* last_bb = last->getBB();
    IRBB const* exp_bb = stmt_of_exp->getBB();
    if (!rg->getCFG()->is_dom(last_bb->id(), exp_bb->id())) {
        return nullptr;
    }

    //e.g: *p = *p + 1
    //Def and Use in same stmt, in this situation,
    //the stmt can not be regarded as dom-def.
    if (exp_bb == last_bb && !exp_bb->is_dom(last, stmt_of_exp, true)) {
        return nullptr;
    }
    ASSERT0(last != stmt_of_exp);
    return last;
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
    ASSERT0(ir1->isIsomoTo(ir2, true));
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


bool isLoopIndependent(IR const* ir1, IR const* ir2, bool costly_analysis,
                       LI<IRBB> const* li, Region const* rg)
{
    if (!ir1->isMemRef() || !ir2->isMemRef()) { return false; }
    if (!xoc::isDependent(ir1, ir2, costly_analysis, rg)) { return true; }
    IRBB const* ir1bb = ir1->is_stmt() ? ir1->getBB() : ir1->getStmt()->getBB();
    IRBB const* ir2bb = ir2->is_stmt() ? ir2->getBB() : ir2->getStmt()->getBB();
    ASSERT0(ir1bb && ir2bb);
    if (!li->isInsideLoop(ir1bb->id()) || !li->isInsideLoop(ir2bb->id())) {
        return true;
    }
    MD const* ir1ref = ir1->getExactRef();
    MD const* ir2ref = ir2->getExactRef();
    if (ir1ref && ir2ref && ir1ref == ir2ref) { return true; }
    if (!ir1->isIsomoTo(ir2, true)) {
        //TODO:handle the comparison between ILD/IST and ARRAY.
        return false;
    }
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
    if (ir1->isCallStmt()) {
       return isDependentForCallStmt(ir1, ir2, costly_analysis, rg);
    }
    MD const* must1 = ir1->getMustRef();
    MDSet const* may1 = ir1->getMayRef();
    MD const* must2 = ir2->getMustRef();
    MDSet const* may2 = ir2->getMayRef();
    //Compare MustRef firstly.
    if (must1 != nullptr) {
        if (must2 != nullptr) {
            return must1 == must2 || must1->is_overlap(must2);
        }
        if (may2 != nullptr) {
            if (costly_analysis) {
                //The function will iterate elements in MDSet, which is costly.
                return may2->is_overlap_ex(must1, rg, rg->getMDSystem());
            }
            return may2->is_contain(must1, rg);
        }
        return false;
    }
    if (may1 != nullptr) {
        if (must2 != nullptr) {
            if (costly_analysis) {
                //The function will iterate elements in MDSet, which is costly.
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
        if (use_mdssa && x->isMemRefNonPR()) {
            mdssamgr->findAndSetLiveInDef(x, startir, startbb, oc);
        } else if (use_prssa && x->isReadPR()) {
            prssamgr->findAndSetLiveinDef(x);
        }
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


static inline void removeClassicDUChainForIR(IR * ir, Region * rg,
                                             DUMgr * dumgr,
                                             bool rmprdu, bool rmnonprdu)
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

} //namespace xoc
