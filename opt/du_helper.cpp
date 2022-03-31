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
    ASSERTN(olddef->isMemoryRef() && newdef->isMemoryRef(),
            ("should not query its DU"));
    bool mdssa_changed = false;
    bool prssa_changed = false;
    bool classic_du_changed = false;

    MDSSAMgr * mdssamgr = rg->getMDSSAMgr();
    if (mdssamgr != nullptr && mdssamgr->is_valid() &&
        olddef->isMemoryRefNonPR()) {
        mdssamgr->changeDef(olddef, newdef);
        mdssa_changed = true;
    }

    DUMgr * dumgr = rg->getDUMgr();
    if (dumgr != nullptr) {
        DUSet * oldduset = olddef->getDUSet();
        if (oldduset != nullptr) {
            dumgr->changeDef(newdef, olddef);
            classic_du_changed = true;
        }
    }

    ASSERT0(checkChange(rg, mdssa_changed, prssa_changed, classic_du_changed));
}


static IR * getUniqueDef(IRSet const* defset, Region * rg)
{
    //In the situation, the function just utilizes the Definition
    //Informations to build DU chain between 'newuse' and these
    //definitions.
    ASSERT0(defset);
    IR * unique_def = nullptr;
    IRSetIter it;
    for (INT i = defset->get_first(&it); i != -1;
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
void changeUseEx(IR * olduse, IR * newuse, IRSet const* defset, Region * rg)
{
    ASSERT0(olduse->is_exp() && newuse->is_exp());
    if (newuse->isMemoryOpnd()) {
        ASSERT0(olduse->isMemoryOpnd());
        if (olduse->isMemoryRefNonPR() && newuse->isMemoryRefNonPR()) {
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
 
        if ((olduse->isMemoryRefNonPR() && newuse->isReadPR()) ||
            (newuse->isMemoryRefNonPR() && olduse->isReadPR())) {
            //CASE2:ld x -> $y
            //      $x -> ld y
            //In the situation, the function just utilizes the Definition
            //Informations to build DU chain between 'newuse' and these
            //definitions.
            ASSERT0(defset);
            xoc::removeUseForTree(olduse, rg);

            IRSetIter it;
            for (INT i = defset->get_first(&it); i != -1;
                 i = defset->get_next(i, &it)) {
                IR * def = rg->getIR(i);
                ASSERT0(def && def->is_stmt());
                xoc::buildDUChain(def, newuse, rg);
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
    xoc::removeUseForTree(olduse, rg);
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
    ASSERTN(olduse->isMemoryRef() && newuse->isMemoryRef(),
            ("should not query its DU"));
    bool mdssa_changed = false;
    bool prssa_changed = false;
    bool classic_du_changed = false;

    MDSSAMgr * mdssamgr = rg->getMDSSAMgr();
    if (mdssamgr != nullptr && mdssamgr->is_valid() &&
        olduse->isMemoryRefNonPR()) {
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
        PRSSAMgr * prssamgr = rg->getPRSSAMgr();
        if (prssamgr != nullptr && prssamgr->is_valid()) {
            SSAInfo * oldssainfo = olduse->getSSAInfo();
            SSAInfo * newssainfo = newuse->getSSAInfo();
            ASSERT0(oldssainfo && newssainfo);
            oldssainfo->removeUse(olduse);
            newssainfo->addUse(newuse);
            prssa_changed = true;
        }
    }

    ASSERT0(checkChange(rg, mdssa_changed, prssa_changed, classic_du_changed));
}


//Build DU chain from stmt 'def' to expression 'use'.
//The function establishs DU chain from 'def' to 'use'.
//def: stmt
//use: expression
void buildDUChain(IR * def, IR * use, Region * rg)
{
    ASSERT0(def && use && def->is_stmt() && use->is_exp());
    bool mdssa_changed = false;
    bool prssa_changed = false;
    bool classic_du_changed = false;
    if (def->isMemoryRefNonPR()) {
        MDSSAMgr * mdssamgr = rg->getMDSSAMgr();
        if (mdssamgr != nullptr && mdssamgr->is_valid()) {
            ASSERT0(use->isMemoryRefNonPR());
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
        dumgr->buildDUChain(def, use);
        classic_du_changed = true;
    }

    ASSERT0(checkChange(rg, mdssa_changed, prssa_changed, classic_du_changed));
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
        PRSSAMgr * prssamgr = rg->getPRSSAMgr();
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
        ir->isMemoryRefNonPR()) {
        ASSERTN(mdssamgr->getMDSSAInfoIfAny(ir),
                ("def stmt even not in MDSSA system"));
        change |= mdssamgr->removeExpiredDU(ir);
        mdssa_changed = true;
    }

    ASSERT0(checkChange(rg, mdssa_changed, prssa_changed, classic_du_changed));
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

    if (from->isMemoryRefNonPR()) {
        ASSERT0(to->isMemoryRefNonPR());
        MDSSAMgr * mdssamgr = rg->getMDSSAMgr();
        if (mdssamgr != nullptr && mdssamgr->is_valid()) {
            mdssamgr->coalesceDUChain(from, to);
            mdssa_changed = true;
        }
    }

    ASSERT0(checkChange(rg, mdssa_changed, prssa_changed, classic_du_changed));
}


//Remove Use-Def chain.
//exp: it is the root of IR tree that to be removed.
//e.g: ir = ...
//    = ir //S1
//If S1 will be deleted, ir should be removed from its useset in MDSSAInfo.
//NOTE: If ir is a IR tree, e.g: ild(x, ld(y)), remove ild(x) means
//ld(y) will be removed as well. And ld(y)'s MDSSAInfo will be
//updated as well.
void removeUseForTree(IR const* exp, Region * rg)
{
    ASSERT0(exp && exp->is_exp()); //exp is the root of IR tree.
    bool mdssa_changed = false;
    bool prssa_changed = false;
    bool classic_du_changed = false;

    PRSSAMgr * prssamgr = rg->getPRSSAMgr();
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
        mdssamgr->removeMDSSAOccForTree(exp);
        mdssa_changed = true;
    }

    ASSERT0(checkChange(rg, mdssa_changed, prssa_changed, classic_du_changed));
}


//Remove all DU info of 'stmt'.
void removeStmt(IR * stmt, Region * rg)
{
    ASSERT0(stmt->is_stmt());
    bool mdssa_changed = false;
    bool prssa_changed = false;
    bool classic_du_changed = false;

    DUMgr * dumgr = rg->getDUMgr();
    if (dumgr != nullptr) {
        dumgr->removeIRFromDUMgr(stmt);
        classic_du_changed = true;
    }

    PRSSAMgr * prssamgr = rg->getPRSSAMgr();
    if (prssamgr != nullptr && prssamgr->is_valid()) {
        PRSSAMgr::removePRSSAOcc(stmt);
        prssa_changed = true;
    }

    MDSSAMgr * mdssamgr = rg->getMDSSAMgr();
    if (mdssamgr != nullptr && mdssamgr->is_valid()) {
        //Remove stmt and its RHS.
        mdssamgr->removeMDSSAOccForTree(stmt);
        mdssa_changed = true;
    }

    ASSERT0(checkChange(rg, mdssa_changed, prssa_changed, classic_du_changed));
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
    ASSERT0(to_ir->isMemoryRefNonPR() && from_ir->isMemoryRefNonPR());
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
    if (!to->isMemoryRef() && !to->is_id()) {
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
        from->isMemoryRefNonPR()) {
        addUseInMDSSAMode(to, from, mdssamgr, rg);
        mdssa_changed = true;
    }

    DUMgr * dumgr = rg->getDUMgr();
    if (dumgr != nullptr) {
        dumgr->addUseForTree(to, from);
        classic_du_changed = true;
    }

    ASSERT0(checkChange(rg, mdssa_changed, prssa_changed, classic_du_changed));
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
    IR const* from_ir = xoc::iterInitC(from, cit, false);
    IR * to_ir = xoc::iterInit(to, it, false);
    ASSERT0(cit.get_elem_count() == it.get_elem_count());
    for (; to_ir != nullptr;
         to_ir = xoc::iterNext(it, true), from_ir = xoc::iterNextC(cit, true)) {
        ASSERT0(to_ir->isIREqual(from_ir, true));
        if (!to_ir->isMemoryRef() && !to_ir->is_id()) {
            //Copy MD for IR_ID, some Passes require it, e.g. GVN.
            continue;
        }
        if (use_prssa && from_ir->isReadPR()) {
            addUseInPRSSAMode(to_ir, from_ir);
        }
        if (use_mdssa && from_ir->isMemoryRefNonPR()) {
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
    return const_cast<GVN*>(gvn)->mapIR2VNConst(base);
}


static bool hasSameValueArrayOp(IR const* ir1, IR const* ir2, GVN const* gvn)
{
    ASSERT0(gvn && gvn->is_valid());
    ASSERT0(ir1->isArrayOp() && ir2->isArrayOp());
    IR const* sub1 = ARR_sub_list(ir1);
    IR const* sub2 = ARR_sub_list(ir2);
    for (; sub1 != nullptr && sub2 != nullptr;
         sub1 = sub1->get_next(), sub2 = sub2->get_next()) {
        VN const* v1 = gvn->mapIR2VNConst(sub1);
        VN const* v2 = gvn->mapIR2VNConst(sub2);
        if (v1 != v2) { return false; }
    }

    VN const* v1 = gvn->mapIR2VNConst(ARR_base(ir1));
    VN const* v2 = gvn->mapIR2VNConst(ARR_base(ir2));
    if (v1 != v2) { return false; }

    if (ir1->isCover(ir2, gvn->getRegion())) {
        return true;
    }
    return false;
}


static bool hasSameValueIndirectOp(IR const* ir1, IR const* ir2, GVN const* gvn)
{
    //Find the base expression that is not IndirectOp.
    //e.g: given ILD(ILD(ILD(p))), the function will reason out 'p'.

    //The number of indirect operation.
    //e.g: given ILD(ILD(ILD(p))), indirect_num is 3.
    UINT indirect_num1 = 0;
    VN const* basevn1 = getVNOfIndirectOp(ir1, &indirect_num1, gvn);
    if (basevn1 == nullptr) {
        //No need to keep analyzing if there is no VN provided.
        return false;
    }

    UINT indirect_num2 = 0;
    VN const* basevn2 = getVNOfIndirectOp(ir2, &indirect_num2, gvn);
    if (basevn2 == nullptr) {
        //No need to keep analyzing if there is no VN provided.
        return false;
    }

    //VN is not match OR indirect level is not match
    if (basevn1 == basevn2 && indirect_num1 == indirect_num2) {
        return true;
    }
    return false;
}


//The function try to find the killing-def for 'use'.
//To find the killing-def, the function prefer use SSA info.
IR * findKillingDef(IR const* exp, Region * rg)
{
    ASSERT0(exp->is_exp());
    ASSERTN(exp->isMemoryOpnd(), ("should not query its DU"));
    //Prefer PRSSA and MDSSA DU.
    if (exp->isReadPR()) {
        PRSSAMgr * prssamgr = rg->getPRSSAMgr();
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

    if (exp->isMemoryRefNonPR()) {
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
    ASSERTN(def->isMemoryRef() && use->isMemoryRef(),
            ("should not query its DU"));

    MD const* mustusemd = use->getRefMD();
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
    MD const* mustdefmd = def->getRefMD();
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
    PRSSAMgr * prssamgr = rg->getPRSSAMgr();
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
    ASSERTN(def->isMemoryRef(), ("should not query its DU"));
    useset->clean();

    SSAInfo const* prssainfo = def->getSSAInfo();
    if (prssainfo != nullptr) {
        SSAUseIter sc;
        Region * rg = mdssamgr->getRegion();
        for (INT u = SSA_uses(prssainfo).get_first(&sc);
             u >= 0; u = SSA_uses(prssainfo).get_next(u, &sc)) {
            ASSERT0(rg->getIR(u));
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
    ASSERTN(use->isMemoryRef(), ("should not query its DU"));
    defset->clean();

    SSAInfo const* prssainfo = use->getSSAInfo();
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
        mdssainfo->collectDef(mdssamgr, use->getRefMD(), ctx, defset);
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
    for (INT i = defset.get_first(&it); i >= 0; i = defset.get_next(i, &it)) {
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


//Return true if ir1 is may overlap to phi.
bool isDependent(IR const* ir, MDPhi const* phi)
{
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


//Return true if ir1 is may overlap to ir2.
bool isDependent(IR const* ir1, IR const* ir2, Region const* rg)
{
    MD const* ir1must = ir1->getMustRef();
    MD const* ir2must = ir2->getMustRef();
    MDSet const* ir1may = ir1->getMayRef();
    MDSet const* ir2may = ir2->getMayRef();
    if (ir1must != nullptr && ir2must != nullptr) {
        return ir1must == ir2must ? true : ir1must->is_overlap(ir2must);
    }
    if (ir1must != nullptr) {
        //ir2must is empty.
        if (ir2may != nullptr) {
            if (ir2may->is_overlap(ir1must, rg)) {
                return true;
            }
            if (ir1may != nullptr) {
                return ir1may == ir2may ? true : ir1may->is_intersect(*ir2may);
            }
            return false;
        }
        ASSERTN(0, ("ir2 does not have MDRef"));
    }
    if (ir2must != nullptr) {
        //ir1must is empty.
        if (ir1may != nullptr) {
            if (ir1may->is_overlap(ir2must, rg)) {
                return true;
            }
            if (ir2may != nullptr) {
                return ir1may == ir2may ? true : ir2may->is_intersect(*ir1may);
            }
            return false;
        }
        ASSERTN(0, ("ir1 does not have MDRef"));
    }
    ASSERT0(ir1may && ir2may);
    return ir1may == ir2may ? true : ir2may->is_intersect(*ir1may);
}


void findAndSetLiveInDef(IR * root, IR * startir, IRBB * startbb, Region * rg)
{
    ASSERT0(root->is_exp());
    PRSSAMgr * prssamgr = rg->getPRSSAMgr();
    MDSSAMgr * mdssamgr = rg->getMDSSAMgr();
    bool use_prssa = prssamgr != nullptr && prssamgr->is_valid();
    bool use_mdssa = mdssamgr != nullptr && mdssamgr->is_valid();
    if (!use_prssa && !use_mdssa) { return; }
    IRIter it;
    for (IR * x = iterInit(root, it);
         x != nullptr; x = iterNext(it)) {
        if (use_mdssa && x->isMemoryRefNonPR()) {
            mdssamgr->findAndSetDomLiveInDef(x, startir, startbb);
        } else if (use_prssa && x->isReadPR()) {
            prssamgr->findAndSetLiveinDef(x);
        }
    }
}

} //namespace xoc
