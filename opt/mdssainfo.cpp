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

author: Su Zhenyu
@*/
#include "cominc.h"
#include "comopt.h"

namespace xoc {

//Forward declaration.
static void collectUseCrossPhi(UseDefMgr const* udmgr, MDPhi const* phi,
                               CollectCtx & ctx, OUT IRSet * set);

//
//START VOpndSet
//
VOpnd * VOpndSet::get_unique(MDSSAMgr const* mgr) const
{
    VOpndSetIter it = nullptr;
    BSIdx first = get_first(&it);
    if (first == BS_UNDEF) { return nullptr; }
    BSIdx second = get_next(first, &it);
    return second != BS_UNDEF ? nullptr : mgr->getVOpnd(first);
}


void VOpndSet::dump(Region const* rg) const
{
    if (!rg->isLogMgrInit()) { return; }
    DefSBitSetCore::dump(rg->getLogMgr()->getFileHandler());
}
//END VOpndSet

//
//START MDDef
//
bool MDDef::isRefSameMDWith(IR const* ir) const
{
    MDIdx resmdid = getResult()->mdid();
    MD const* md = ir->getMustRef();
    if (md != nullptr && md->id() == resmdid) { return true; }
    MDSet const* mds = ir->getMayRef();
    if (mds != nullptr && mds->is_contain_pure(resmdid)) { return true; }
    return false;
}


//Note real-use does not include IR_ID.
bool MDDef::hasOutsideLoopRealUse(LI<IRBB> const* li, Region const* rg) const
{
    VMD * res = getResult();
    VMD::UseSetIter it;
    for (INT i = res->getUseSet()->get_first(it);
         !it.end(); i = res->getUseSet()->get_next(it)) {
        IR const* u = rg->getIR(i);
        if (u->is_id()) {
            MDPhi const* phi = ID_phi(u);
            if (phi == this) { continue; }
            if (!li->isInsideLoop(phi->getBB()->id())) { continue; }

            //TBD:Can phi->phi form a cycle?
            if (phi->hasOutsideLoopRealUse(li, rg)) {
                return true;
            }
            continue;
        }
        IR const* ir = rg->getIR(i);
        IRBB const* bb = ir->is_stmt() ? ir->getBB() : ir->getStmt()->getBB();
        ASSERT0(bb);
        if (!li->isInsideLoop(bb->id())) {
            return true;
        }
    }
    return false;
}


//Return true if n is the Next DEF of current DEF.
bool MDDef::isNext(MDDef const* n) const
{
    return getNextSet() != nullptr ? getNextSet()->find(n) : false;
}


//Return true if n is the Next DEF, and n may not be the immediate-next-def to
//current DEF. The function will access all next DEFs recursively.
bool MDDef::isInNextSet(MDDef const* n, UseDefMgr const* mgr) const
{
    #ifdef _DEBUG_
    xcom::TTab<UINT> visited; //to guarantee there is no cycle in NextSet.
    #endif
    List<MDDef const*> wl;
    wl.append_tail(this);
    MDDef const* def;
    while ((def = wl.remove_head()) != nullptr) {
        #ifdef _DEBUG_
        ASSERT0(!visited.find(def->id()));
        visited.append(def->id());
        #endif
        if (def->isNext(n)) { return true; }
        if (def->getNextSet() == nullptr) { continue; }

        MDDefSetIter nit = nullptr;
        for (BSIdx w = def->getNextSet()->get_first(&nit);
            w != BS_UNDEF; w = def->getNextSet()->get_next(w, &nit)) {
            MDDef const* next = mgr->getMDDef(w);
            ASSERTN(next, ("not such MDDef"));
            wl.append_tail(next);
        }
    }
    return false;
}


void MDDef::cleanNextSet(UseDefMgr * mgr)
{
    if (getNextSet() != nullptr) {
        getNextSet()->clean(*mgr->getSBSMgr());
    }
}


IR * MDDef::getOcc() const
{
    ASSERT0(!is_phi());
    return MDDEFSTMT_occ(this);
}


IRBB * MDDef::getBB() const
{
    ASSERT0(is_phi() ? MDPHI_bb(this) != nullptr :
                       (MDDEFSTMT_occ(this) != nullptr &&
                        MDDEFSTMT_occ(this)->is_stmt()));
    return is_phi() ? MDPHI_bb(this) : MDDEFSTMT_occ(this)->getBB();
}
//END MDDef


//
//START MDDefSet
//
//Return true if there is at least one element in MDDefSet that dominates v.
bool MDDefSet::hasAtLeastOneElemDom(MDDef const* v, MDSSAMgr const* mgr) const
{
    MDDefSetIter it = nullptr;
    UseDefMgr * udmgr = const_cast<MDSSAMgr*>(mgr)->getUseDefMgr();
    for (BSIdx i = get_first(&it); i != BS_UNDEF; i = get_next(i, &it)) {
        MDDef const* def = udmgr->getMDDef(i);
        if (mgr->isDom(def, v)) {
            return true;
        }
    }
    return false;
}
//END MDDefSet


//
//START VMDVec
//
//Find the VMD that have MD defined at given BB.
//Return true if the function find DEF in given BB.
bool VMDVec::hasDefInBB(UINT bbid) const
{
    for (VecIdx i = 0; i <= get_last_idx(); i++) {
        VMD * p = get(i);
        if (p == nullptr || p->getDef() == nullptr) { continue; }
        ASSERT0(p->getDef() && p->getDef()->getBB());
        if (p->getDef()->getBB()->id() == bbid) {
            return true;
        }
    }
    return false;
}
//END VMDVec


//
//START MDSSAInfo
//
//Return true if all definition of vopnds can reach 'exp'.
bool MDSSAInfo::isMustDef(UseDefMgr const* udmgr, IR const* exp) const
{
    ASSERT0(udmgr && exp && exp->is_exp());
    VOpndSetIter iter = nullptr;
    MDSSAInfo * pthis = const_cast<MDSSAInfo*>(this);
    for (BSIdx i = pthis->getVOpndSet()->get_first(&iter);
         i != BS_UNDEF; i = pthis->getVOpndSet()->get_next(i, &iter)) {
        VMD * vopnd = (VMD*)udmgr->getVOpnd(i);
        ASSERT0(vopnd && vopnd->is_md());
        if (!vopnd->findUse(exp)) {
            return false;
        }
    }
    return true;
}


//Return true if current ssainfo is equal to src.
bool MDSSAInfo::isEqual(MDSSAInfo const& src) const
{
    return readVOpndSet().is_equal(src.readVOpndSet());
}


void MDSSAInfo::copyVOpndSet(VOpndSet const& src, UseDefMgr * mgr)
{
    getVOpndSet()->copy(src, *mgr->getSBSMgr());
}


void MDSSAInfo::cleanVOpndSet(UseDefMgr * mgr)
{
    m_vopnd_set.clean(*mgr->getSBSMgr());
}


static void collectUseForVOpnd(VMD const* vopnd, UseDefMgr const* udmgr,
                               CollectCtx & ctx, OUT IRSet * set)
{
    VMD::UseSetIter vit;
    bool cross_phi = HAVE_FLAG(ctx.flag, COLLECT_CROSS_PHI);
    VMD * pvopnd = const_cast<VMD*>(vopnd);
    Region * rg = udmgr->getRegion();
    for (INT i = pvopnd->getUseSet()->get_first(vit);
         !vit.end(); i = pvopnd->getUseSet()->get_next(vit)) {
        if (!cross_phi) {
            set->bunion(i);
            continue;
        }
        IR const* ir = rg->getIR(i);
        ASSERT0(ir && !ir->is_undef());
        if (ir->is_id()) {
            MDPhi const* phi = ((CId*)ir)->getMDPhi();
            ASSERT0(phi);
            if (ctx.is_visited(phi->id())) { continue; }
            ctx.set_visited(phi->id());
            collectUseCrossPhi(udmgr, phi, ctx, set);
            continue;
        }
        set->bunion(i);
    }
}


static void collectUseCrossPhi(UseDefMgr const* udmgr, MDPhi const* phi,
                               CollectCtx & ctx, OUT IRSet * set)
{
    ASSERT0(phi && phi->is_phi());
    collectUseForVOpnd(phi->getResult(), udmgr, ctx, set);
}


//Collect all USE, where USE is IR expression.
//Note the function will not clear 'set' because caller may perform unify
//operation.
//ctx: indicates the terminating condition that the function should
//     stop and behaviors what the collector should take when meeting
//     specific IR operator.
void MDSSAInfo::collectUse(UseDefMgr const* udmgr, CollectCtx & ctx,
                           OUT IRSet * set) const
{
    //DO NOT CLEAN SET
    ASSERT0(set && udmgr);
    VOpndSetIter it = nullptr;
    VOpndSet const& vset = readVOpndSet();
    for (BSIdx i = vset.get_first(&it);
         i != BS_UNDEF; i = vset.get_next(i, &it)) {
        VMD const* vopnd = (VMD*)udmgr->getVOpnd(i);
        ASSERT0(vopnd && vopnd->is_md());
        ctx.clean();
        collectUseForVOpnd(vopnd, udmgr, ctx, set);
    }
}


static void collectDefThroughDefChain(MDSSAMgr const* mdssamgr,
                                      MDDef const* def,
                                      OUT IRSet * set)
{
    ASSERT0(def);
    ConstMDDefIter it;
    for (MDDef const* d = mdssamgr->iterDefInitC(def, it);
         d != nullptr; d = mdssamgr->iterDefNextC(it)) {
        if (d->is_phi()) {
            //Nothing to do. The DEF of operand will be iterated at
            //iterDefCHelper().
            continue;
        }
        ASSERT0(d->getOcc());
        set->bunion(d->getOcc()->id());

        //TODO:for now, we have to walk alone with DEF chain to
        //mark almost all DEF to be effect. This may lead to
        //traverse the same DEF many times. Apply DP like algo to reduce
        //the traversal time.
    }
}


//Return true if current MDSSAInfo contains given MD only.
bool MDSSAInfo::containSpecificMDOnly(MDIdx mdid, UseDefMgr const* udmgr) const
{
    VOpndSetIter it = nullptr;
    for (BSIdx i = readVOpndSet().get_first(&it);
         i != BS_UNDEF; i = readVOpndSet().get_next(i, &it)) {
        VMD const* t = (VMD const*)udmgr->getVOpnd(i);
        ASSERT0(t);
        if (t->is_md() && t->mdid() != mdid) { return false; }
    }
    return true;
}


//Collect all DEF that overlapped with 'ref', where DEF is IR expression.
//Note the function will NOT clear 'set' because caller may perform union
//operation.
//ref: given MD, if it is NULL, the function will collect all DEFs.
//collect_flag: if the collection will keep iterating DEF by crossing PHI
//              operand.
void MDSSAInfo::collectDef(MDSSAMgr const* mdssamgr, MD const* ref,
                           CollectCtx const& ctx, OUT IRSet * set) const
{
    //DO NOT CLEAN 'set'.
    UseDefMgr const* udmgr = const_cast<MDSSAMgr*>(mdssamgr)->getUseDefMgr();
    VOpndSetIter it = nullptr;
    bool cross_phi = HAVE_FLAG(ctx.flag, COLLECT_CROSS_PHI);
    for (BSIdx i = readVOpndSet().get_first(&it);
         i != BS_UNDEF; i = readVOpndSet().get_next(i, &it)) {
        VOpnd const* t = udmgr->getVOpnd(i);
        ASSERT0(t && t->is_md());
        MDDef * tdef = ((VMD*)t)->getDef();
        if (tdef == nullptr) { continue; }
        if (tdef->is_phi() && cross_phi) {
            //TODO: iterate phi operands.
            collectDefThroughDefChain(mdssamgr, tdef, set);
            continue;
        }

        IR const* defstmt = tdef->getOcc();
        ASSERT0(defstmt);
        if (defstmt->isCallStmt()) {
            //CASE:call()
            //     ...=USE
            //Call is the only stmt that need to process specially.
            //Because it is not killing-def.
            collectDefThroughDefChain(mdssamgr, tdef, set);
            continue;
        }

        ASSERT0(defstmt->isMemRefNonPR());
        MD const* mustdef = defstmt->getRefMD();
        if (ref != nullptr && mustdef != nullptr &&
            ref->is_exact() && mustdef->is_exact() &&
            (mustdef == ref || mustdef->is_exact_cover(ref))) {
            //defstmt is killing definition of 'ref'.
            set->bunion(defstmt->id());
            continue;
        }

        if (ref != nullptr) {
            //TODO:
            //CASE1:DEF=...
            //      ...=USE
            //CASE2:...=
            //      ...=USE
            //Both cases need to collect all DEFs until
            //meeting the killing-def.
            collectDefThroughDefChain(mdssamgr, tdef, set);
            continue;
        }

        //CASE1:...=
        //         =...
        //CASE2:DEF=...
        //         =...
        //Both cases need to collect all DEFs through def-chain.
        collectDefThroughDefChain(mdssamgr, tdef, set);
    }
}


//Remove vopnd from current MDSSAInfo.
void MDSSAInfo::removeVOpnd(VOpnd const* vopnd, UseDefMgr * mgr)
{
    m_vopnd_set.remove(vopnd, *mgr->getSBSMgr());
}


//Return true if there is VMD renamed.
//Note current MDSSAInfo is the SSA info of 'exp', the VOpndSet will be
//vmd: intent to be swap-in.
bool MDSSAInfo::renameSpecificUse(IR const* exp, MOD VMD * vmd, UseDefMgr * mgr)
{
    bool do_rename = false;
    ASSERT0(exp && exp->is_exp() && mgr);
    ASSERT0(UseDefMgr::getMDSSAInfo(exp) == this);
    VOpndSet * vopndset = getVOpndSet();
    MDIdx vmdid = vmd->mdid();
    VOpndSetIter it = nullptr;
    VOpndSetIter prev_it = nullptr;
    for (BSIdx i = vopndset->get_first(&it); i != BS_UNDEF;
         prev_it = it, i = vopndset->get_next(i, &it)) {
        VMD * vopnd = (VMD*)mgr->getVOpnd(i);
        ASSERT0(vopnd && vopnd->is_md());
        if (vopnd->mdid() == vmdid) {
            vopnd->removeUse(exp);
            //Note here we use SBitSet::remove() rather than
            //removeVOpnd to speedup the accessing of bitset.
            //removeVOpnd(vopnd, mgr);
            vopndset->remove(vopnd, prev_it, it, *mgr->getSBSMgr());
            do_rename = true;
            break;
        }
    }
    if (do_rename) {
        vmd->addUse(exp);
        addVOpnd(vmd, mgr);
    }
    return do_rename;
}


//Remove given IR expression from UseSet of each vopnd in MDSSAInfo.
//Note current MDSSAInfo is the SSA info of 'exp'.
//exp: IR expression to be removed.
void MDSSAInfo::removeSpecificUse(IR const* exp, MDIdx mdid, UseDefMgr * mgr)
{
    ASSERT0(exp && exp->is_exp() && mgr);
    ASSERT0(UseDefMgr::getMDSSAInfo(exp) == this);
    VOpndSet * vopndset = getVOpndSet();
    BSIdx nexti;
    VOpndSetIter it = nullptr;
    VOpndSetIter prev_it = nullptr;
    for (BSIdx i = vopndset->get_first(&it); i != BS_UNDEF; i = nexti) {
        prev_it = it;
        nexti = vopndset->get_next(i, &it);
        VMD * vopnd = (VMD*)mgr->getVOpnd(i);
        ASSERT0(vopnd && vopnd->is_md());
        if (vopnd->mdid() == mdid) {
            vopnd->removeUse(exp);
            vopndset->remove(vopnd, prev_it, it, *mgr->getSBSMgr());
            return;
        }
    }
}


void MDSSAInfo::addUseSet(MDSSAInfo const* src, IN UseDefMgr * mgr)
{
    ASSERT0(src);
    getVOpndSet()->bunion(src->readVOpndSet(), *mgr->getSBSMgr());
}


//Add given IR expression to occurence set.
//exp: IR expression to be added.
void MDSSAInfo::addUse(IR const* exp, IN UseDefMgr * mgr)
{
    ASSERT0(exp && exp->is_exp() && exp->isMemRefNonPR() && mgr);
    VOpndSetIter iter = nullptr;
    for (BSIdx i = getVOpndSet()->get_first(&iter);
         i != BS_UNDEF; i = getVOpndSet()->get_next(i, &iter)) {
        VMD * vopnd = (VMD*)mgr->getVOpnd(i);
        ASSERT0(vopnd && vopnd->is_md());
        vopnd->addUse(exp);
    }
}


VOpnd * MDSSAInfo::getVOpndForMD(UINT mdid, MDSSAMgr const* mgr) const
{
    //Iterate each VOpnd.
    VOpndSetIter iter = nullptr;
    for (BSIdx i = readVOpndSet().get_first(&iter);
         i != BS_UNDEF; i = readVOpndSet().get_next(i, &iter)) {
        VMD * t = (VMD*)const_cast<MDSSAMgr*>(mgr)->
            getUseDefMgr()->getVOpnd(i);
        ASSERT0(t && t->is_md());
        if (t->mdid() == mdid) { return t; }
    }
    return nullptr;
}


void MDSSAInfo::dump(MDSSAMgr const* mgr) const
{
    if (!mgr->getRegion()->isLogMgrInit()) { return; }
    VOpndSetIter iter = nullptr;
    MDSSAInfo * pthis = const_cast<MDSSAInfo*>(this);
    for (BSIdx i = pthis->getVOpndSet()->get_first(&iter);
         i != BS_UNDEF; i = pthis->getVOpndSet()->get_next(i, &iter)) {
        note(mgr->getRegion(), "\nREF:");
        VMD const* vopnd = (VMD*)const_cast<MDSSAMgr*>(mgr)->
            getUseDefMgr()->getVOpnd(i);
        ASSERT0(vopnd && vopnd->is_md());
        vopnd->dump(mgr->getRegion(), const_cast<MDSSAMgr*>(mgr)->
            getUseDefMgr());
    }
}


void MDSSAInfo::addVOpnd(VOpnd const* vopnd, UseDefMgr * mgr)
{
    m_vopnd_set.append(vopnd, *mgr->getSBSMgr());
}
//END MDSSAInfo


//
//START UINT2VMDVec
//
void UINT2VMDVec::set(UINT mdid, VMDVec * vmdvec)
{
    if (mdid < m_threshold) {
        m_mdid2vmdvec_vec.set(mdid, vmdvec);
        return;
    }
    m_mdid2vmdvec_map.set(mdid, vmdvec);
}


size_t UINT2VMDVec::count_mem() const
{
    size_t count = sizeof(UINT2VMDVec);
    count += m_mdid2vmdvec_vec.count_mem();
    count += m_mdid2vmdvec_map.count_mem();
    return count;
}
//END UINT2VMDVec


//
//START VMD
//
//Concisely dump.
void VMD::dump(Region const* rg) const
{
    prt(rg, "MD%dV%d", mdid(), version());
}


void VMD::clean()
{
    VOpnd::clean();
    VMD_def(this) = nullptr;
    VMD_mdid(this) = MD_UNDEF;
    VMD_version(this) = MDSSA_INIT_VERSION;
    VMD_occs(this).clean();
}


void VMD::dump(Region const* rg, UseDefMgr const* mgr) const
{
    if (!rg->isLogMgrInit()) { return; }
    ASSERT0(is_md() && rg);
    //prt(rg, "(");
    prt(rg, "VMD%d:MD%dV%d", id(), mdid(), version());

    //Dump Def
    if (getDef() != nullptr) {
        //TBD: I think MDDef could be PHI.
        //ASSERT0(!getDef()->is_phi());

        if (getDef()->getPrev() != nullptr) {
            prt(rg, ",PrevDEF:MD%dV%d",
                getDef()->getPrev()->getResult()->mdid(),
                getDef()->getPrev()->getResult()->version());
        } else {
            prt(rg, ",-");
        }

        if (getDef()->getNextSet() != nullptr) {
            MDDefSetIter nit = nullptr;
            bool first = true;
            for (BSIdx w = getDef()->getNextSet()->get_first(&nit);
                w != BS_UNDEF; w = getDef()->getNextSet()->get_next(w, &nit)) {
                if (first) {
                    first = false;
                } else {
                    prt(rg, ",");
                }

                MDDef const* use = mgr->getMDDef(w);
                ASSERTN(use, ("not such MDDef"));
                ASSERT0(use->getResult());
                ASSERTN(use->getPrev() == getDef(), ("insanity relation"));
                prt(rg, ",NextDEF:MD%dV%d",
                    use->getResult()->mdid(), use->getResult()->version());
            }
        }
    } else {
        prt(rg, ",-");
    }
    //prt(rg, ")");

    //Dump OccSet
    prt(rg, "|USESET:");
    bool first = true;
    VMD * pthis = const_cast<VMD*>(this);
    VMD::UseSetIter vit;
    for (INT i2 = pthis->getUseSet()->get_first(vit);
         !vit.end(); i2 = pthis->getUseSet()->get_next(vit)) {
        if (first) {
            first = false;
        } else {
            prt(rg, ",");
        }

        IR * use = rg->getIR(i2);
        ASSERT0(use && use->isMemRef());
        prt(rg, "%s(id:%d)", IRNAME(use), use->id());
    }
}
//END VMD


//
//START MDPhi
//
void MDPhi::replaceOpnd(MOD IR * oldopnd, MOD IR * newopnd)
{
    ASSERT0(oldopnd && newopnd);
    xcom::replace(&MDPHI_opnd_list(this), oldopnd, newopnd);
}


//Insert operand at given position.
//pos: position of operand, start at 0.
//     Each operand correspond to in-edge on CFG.
IR * MDPhi::insertOpndAt(MDSSAMgr * mgr, UINT pos, IRBB const* pred,
                         OptCtx const& oc)
{
    Region * rg = mgr->getRegion();
    UINT i = 0;
    IR * marker = MDPHI_opnd_list(this);
    IR * last = nullptr;
    for (; marker != nullptr && i <= pos; marker = marker->get_next(), i++) {
        last = marker;
    }

    //Generate a new ID as operand of PHI.
    MD const* res = rg->getMDSystem()->getMD(getResult()->mdid());
    ASSERT0(res);
    IR * opnd = mgr->getIRMgr()->buildId(res->get_base());
    opnd->setRefMD(res, rg);
    ASSERT0(opnd->getRefMDSet() == nullptr);
    ID_phi(opnd) = this; //Record ID's host PHI.

    //Find the latest live-in version of PHI's operand MD.
    VMD * livein_def = mgr->findDomLiveInDefFrom(res->id(),
        const_cast<IRBB*>(pred)->getLastIR(), pred, oc);
    MDSSAInfo * mdssainfo = nullptr;
    if (livein_def != nullptr) {
        //Generate MDSSAInfo for new operand.
        mdssainfo = mgr->genMDSSAInfoAndVOpnd(opnd, livein_def->version());
    } else {
        //Generate MDSSAInfo for new operand.
        mdssainfo = mgr->genMDSSAInfoAndVOpnd(opnd, MDSSA_INIT_VERSION);
    }

    mgr->getUseDefMgr()->setMDSSAInfo(opnd, mdssainfo);

    //Add current ID into occurrence set of each VOpnd that recorded
    //in 'mdssainfo'.
    mgr->addUseToMDSSAInfo(opnd, mdssainfo);

    if (marker != nullptr) {
        //Insert operand into list.
        xcom::insertbefore(&MDPHI_opnd_list(this), marker, opnd);
        return opnd;
    }

    //Append a new operand to list.
    ASSERT0(pos >= 0 && i == pos);
    //last' may be nullptr, because the operand list may be empty before
    //insertion. During several CFG edge removing and building,
    //there may appear single operand PHI.
    //If CFG optimization perform removing single edge then
    //adding a new edge, the PHI operand is empty when adding the new edge.
    //e.g:Before adding a new edge.
    //  BB13:
    //  Phi: MD13V4 <-| UsedBy :
    xcom::add_next(&MDPHI_opnd_list(this), &last, opnd);
    return opnd;
}


IR * MDPhi::getOpnd(UINT idx) const
{
    UINT i = 0;
    IR * x = MDPHI_opnd_list(this);
    for (; x != nullptr && i < idx; x = x->get_next(), i++) {;}
    return x;
}


//Facility function to make it easier to get the VOpnd of operand of PHI.
VMD * MDPhi::getOpndVMD(IR const* opnd, UseDefMgr const* mgr) const
{
    ASSERTN(xcom::in_list(getOpndList(), opnd), ("not operand of phi"));
    if (!opnd->is_id() && opnd->isMemOpnd()) { return nullptr; }

    ASSERT0(mgr);
    MDSSAInfo * mdssainfo = mgr->getMDSSAInfo(opnd);
    ASSERT0(mdssainfo);
    UINT elemcnt = mdssainfo->getVOpndSet()->get_elem_count();
    if (elemcnt == 0) {
        //Note VOpndSet of 'opnd' may be empty after some optimization.
        //It does not happen when MDSSA just constructed. The USE that
        //without real-DEF will have a virtual-DEF that version is 0.
        //During some increment-maintaining of MDSSA, VOpnd may be removed,
        //and VOpndSet become empty.
        //This means the current USE, 'opnd', does not have real-DEF stmt, the
        //value of 'opnd' always coming from parameter of global value.
        //The ID of PHI should not be removed, because it is regarded
        //as a place-holder of PHI operand, and the place-holder indicates
        //the position of related predecessor of current BB of PHI in CFG.
        return nullptr;
    }
    if (elemcnt > 1){
        //Error occured.
        return nullptr;
    }
    VOpndSetIter iter = nullptr;
    VMD * vopnd = (VMD*)mgr->getVOpnd(mdssainfo->getVOpndSet()->
        get_first(&iter));
    ASSERT0(vopnd->is_md());
    return vopnd;
}


void MDPhi::dumpOpnd(IR const* opnd, IRBB const* pred, Region const* rg,
                     UseDefMgr const* mgr) const
{
    prt(rg, "(");
    switch (opnd->getCode()) {
    case IR_CONST:
        dumpConstContent(opnd, rg);
        break;
    case IR_LDA:
        prt(rg, "LDA");
        break;
    case IR_ID: {
        VMD * vopnd = getOpndVMD(opnd, mgr);
        if (vopnd == nullptr) {
            MD const* ref = opnd->getRefMD();
            ASSERT0_DUMMYUSE(ref);
            prt(rg, "ID id:%u ????", opnd->id());
        } else {
            MD const* ref = opnd->getRefMD();
            ASSERT0_DUMMYUSE(ref);
            ASSERT0(ref->id() == vopnd->mdid());
            prt(rg, "ID id:%u MD%uV%u", opnd->id(), vopnd->mdid(),
                vopnd->version());
        }
        break;
    }
    default: UNREACHABLE();
    }

    if (pred == nullptr) {
        //Predecessor is not match with PHI, error occurred.
        prt(rg, " BB??");
    } else {
        prt(rg, " BB%d", pred->id());
    }
    prt(rg, ")");
}


static void dumpUseSet(VMD const* vmd, Region * rg)
{
    ASSERT0(vmd);
    note(rg, "|USESET:");
    VMD::UseSetIter vit;
    for (INT i = const_cast<VMD*>(vmd)->getUseSet()->get_first(vit);
         !vit.end(); i = const_cast<VMD*>(vmd)->getUseSet()->get_next(vit)) {
        IR const* use = rg->getIR(i);
        ASSERT0(use && use->isMemRef());
        prt(rg, "(%s id:%d) ", IRNAME(use), use->id());
    }
}


void MDPhi::dump(Region const* rg, UseDefMgr const* mgr) const
{
    ASSERT0(rg);
    ASSERT0(is_phi());
    if (!rg->isLogMgrInit()) { return; }

    List<IRBB*> preds;
    IRCFG * cfg = rg->getCFG();
    ASSERT0(cfg);
    cfg->get_preds(preds, getBB());
    IRBB * pred = preds.get_head();

    ASSERT0(getResult());
    prt(rg, "MDPhi%u: MD%uV%u <- ", id(), getResult()->mdid(),
        getResult()->version());
    for (IR const* opnd = getOpndList();
         opnd != nullptr; opnd = opnd->get_next()) {
        if (opnd != getOpndList()) {
            prt(rg, ", ");
        }
        dumpOpnd(opnd, pred, rg, mgr);
        pred = preds.get_next();
    }
    dumpUseSet(getResult(), const_cast<Region*>(rg));
}
//END MDPhi


//
//START UseDefMgr
//
UseDefMgr::UseDefMgr(Region * rg, MDSSAMgr * mgr) :
    m_rg(rg), m_mdssa_mgr(mgr), m_sbs_mgr(mgr->getSBSMgr())
{
    ASSERT0(m_rg && m_mdssa_mgr);
    m_md_sys = m_rg->getMDSystem();
    m_irmgr = m_rg->getIRMgr();

    //Single List Core need user declared a mempool.
    m_vopnd_sc_pool = smpoolCreate(sizeof(xcom::SC<VOpnd*>) * 4,
                                   MEM_CONST_SIZE);
    m_phi_pool = smpoolCreate(sizeof(MDPhi) * 2, MEM_CONST_SIZE);
    m_defstmt_pool = smpoolCreate(sizeof(MDDefStmt) * 2, MEM_CONST_SIZE);
    m_defset_pool = smpoolCreate(sizeof(MDDefSet) * 2, MEM_CONST_SIZE);
    m_vconst_pool = smpoolCreate(sizeof(VConst)*2, MEM_CONST_SIZE);
    m_vmd_pool = smpoolCreate(sizeof(VMD)*2, MEM_CONST_SIZE);
    m_philist_pool = smpoolCreate(sizeof(MDPhiList)*2, MEM_CONST_SIZE);
    m_philist_sc_pool = smpoolCreate(sizeof(xcom::SC<MDPhi*>) * 4,
                                     MEM_CONST_SIZE);
    m_mdssainfo_pool = smpoolCreate(sizeof(MDSSAInfo)*2, MEM_CONST_SIZE);

    m_free_sc_list = nullptr;
    m_def_count = MDDEF_UNDEF + 1;
    m_vopnd_count = VOPND_UNDEF + 1;
    m_philist_vec.set(rg->getBBList()->get_elem_count(), 0);
}


void UseDefMgr::destroyMD2VMDVec()
{
    Vector<VMDVec*> * vec = m_map_md2vmd.getVec();
    if (vec != nullptr) {
        for (VecIdx i = 0; i <= vec->get_last_idx(); i++) {
            Vector<VMD*> * vpv = m_map_md2vmd.get((UINT)i);
            if (vpv != nullptr) {
                delete vpv;
            }
        }
    }

    TMap<UINT, VMDVec*> * map = m_map_md2vmd.getMap();
    if (map != nullptr) {
        TMapIter<UINT, VMDVec*> iter;
        VMDVec * vmdvec;
        for (map->get_first(iter, &vmdvec);
             vmdvec != nullptr; map->get_next(iter, &vmdvec)) {
            delete vmdvec;
        }
    }
}


void UseDefMgr::cleanOrDestroy(bool is_reinit)
{
    ASSERT0(m_rg);
    for (VecIdx i = 0; i <= m_vopnd_vec.get_last_idx(); i++) {
        VOpnd * v = m_vopnd_vec.get((UINT)i);
        if (v != nullptr && v->is_md()) {
            ((VMD*)v)->destroy();
        }
    }

    for (VecIdx i = 0; i <= m_def_vec.get_last_idx(); i++) {
        MDDef * d = m_def_vec.get((UINT)i);
        if (d != nullptr && d->getNextSet() != nullptr) {
            d->getNextSet()->clean(*getSBSMgr());
        }
    }

    for (VecIdx i = 0; i <= m_mdssainfo_vec.get_last_idx(); i++) {
        MDSSAInfo * info = m_mdssainfo_vec.get((UINT)i);
        if (info != nullptr) {
            info->destroy(*getSBSMgr());
        }
    }

    destroyMD2VMDVec();

    if (is_reinit) {
        m_map_md2vmd.destroy();
        m_map_md2vmd.init();
        m_philist_vec.destroy();
        m_philist_vec.init();
        m_def_vec.destroy();
        m_def_vec.init();
        m_mdssainfo_vec.destroy();
        m_mdssainfo_vec.init();
        m_vopnd_vec.destroy();
        m_vopnd_vec.init();
        m_def_count = MDDEF_UNDEF + 1;
        m_vopnd_count = VOPND_UNDEF + 1;
    }

    ASSERT0(m_vopnd_sc_pool);
    smpoolDelete(m_vopnd_sc_pool);

    ASSERT0(m_phi_pool);
    smpoolDelete(m_phi_pool);

    ASSERT0(m_defstmt_pool);
    smpoolDelete(m_defstmt_pool);

    ASSERT0(m_defset_pool);
    smpoolDelete(m_defset_pool);

    ASSERT0(m_vmd_pool);
    smpoolDelete(m_vmd_pool);

    ASSERT0(m_vconst_pool);
    smpoolDelete(m_vconst_pool);

    ASSERT0(m_philist_pool);
    smpoolDelete(m_philist_pool);

    ASSERT0(m_philist_sc_pool);
    smpoolDelete(m_philist_sc_pool);

    ASSERT0(m_mdssainfo_pool);
    smpoolDelete(m_mdssainfo_pool);

    if (is_reinit) {
        m_vopnd_sc_pool = smpoolCreate(sizeof(xcom::SC<VOpnd*>) * 4,
            MEM_CONST_SIZE);
        m_phi_pool = smpoolCreate(sizeof(MDPhi) * 2, MEM_CONST_SIZE);
        m_defstmt_pool = smpoolCreate(sizeof(MDDefStmt) * 2, MEM_CONST_SIZE);
        m_defset_pool = smpoolCreate(sizeof(MDDefSet) * 2, MEM_CONST_SIZE);
        m_vmd_pool = smpoolCreate(sizeof(VMD) * 2, MEM_CONST_SIZE);
        m_vconst_pool = smpoolCreate(sizeof(VConst)*2, MEM_CONST_SIZE);
        m_philist_pool = smpoolCreate(sizeof(MDPhiList)*2, MEM_CONST_SIZE);
        m_philist_sc_pool = smpoolCreate(sizeof(xcom::SC<MDPhi*>) * 4,
            MEM_CONST_SIZE);
        m_mdssainfo_pool = smpoolCreate(sizeof(MDSSAInfo)*4, MEM_CONST_SIZE);
    }
}


void UseDefMgr::setMDSSAInfo(IR * ir, MDSSAInfo * mdssainfo)
{
    ASSERT0(ir && mdssainfo && m_mdssa_mgr->hasMDSSAInfo(ir));
    if (ir->getAI() == nullptr) {
        IR_ai(ir) = m_rg->allocAIContainer();
    }
    IR_ai(ir)->set(mdssainfo, m_rg);
}


void UseDefMgr::cleanMDSSAInfo(IR * ir)
{
    ASSERT0(ir);
    ASSERTN(m_mdssa_mgr->hasMDSSAInfo(ir), ("make decision early"));
    if (ir->getAI() == nullptr) { return; }
    IR_ai(ir)->clean(AI_MD_SSA);
}



MDSSAInfo * UseDefMgr::genMDSSAInfo(MOD IR * ir)
{
    ASSERT0(ir && m_mdssa_mgr->hasMDSSAInfo(ir));
    if (ir->getAI() == nullptr) {
        IR_ai(ir) = m_rg->allocAIContainer();
    }
    MDSSAInfo * mdssainfo = (MDSSAInfo*)ir->getAI()->get(AI_MD_SSA);
    if (mdssainfo == nullptr) {
        mdssainfo = allocMDSSAInfo();
        IR_ai(ir)->init();
        IR_ai(ir)->set(mdssainfo, m_rg);
    }
    return mdssainfo;
}


MDSSAInfo * UseDefMgr::getMDSSAInfo(IR const* ir)
{
    ASSERT0(ir && MDSSAMgr::hasMDSSAInfo(ir));
    if (ir->getAI() == nullptr) {
        return nullptr;
    }

    MDSSAInfo * mdssainfo = (MDSSAInfo*)ir->getAI()->get(AI_MD_SSA);
    if (mdssainfo == nullptr) {
        return nullptr;
    }

    return mdssainfo;
}


//Allocate SSAInfo for specified PR indicated by 'mdid'.
MDSSAInfo * UseDefMgr::allocMDSSAInfo()
{
    ASSERT0(m_mdssainfo_pool);
    MDSSAInfo * p = (MDSSAInfo*)smpoolMallocConstSize(sizeof(MDSSAInfo),
                                                      m_mdssainfo_pool);
    ASSERT0(p);
    ::memset(p, 0, sizeof(MDSSAInfo));
    p->init();
    m_mdssainfo_vec.append(p);
    return p;
}


void UseDefMgr::buildMDPhiOpnd(MDPhi * phi, UINT mdid, UINT num_operands)
{
    ASSERT0(mdid > MD_UNDEF && num_operands > 0);
    VMD const* vmd = allocVMD(mdid, MDSSA_INIT_VERSION);
    ASSERT0(vmd);
    MD const* md = m_md_sys->getMD(mdid);
    ASSERT0(md);
    IR * last = nullptr;
    ASSERT0(getSBSMgr());

    //Generate operand of PHI.
    for (UINT i = 0; i < num_operands; i++) {
        IR * opnd = m_irmgr->buildId(md->get_base());
        opnd->setRefMD(md, m_rg);

        //Generate MDSSAInfo to ID.
        MDSSAInfo * mdssainfo = genMDSSAInfo(opnd);

        //Add VOpnd that ID indicated.
        mdssainfo->addVOpnd(vmd, this);

        xcom::add_next(&MDPHI_opnd_list(phi), &last, opnd);

        ID_phi(opnd) = phi; //Record ID's host PHI.
    }
}


//Allocate MDPhi and initialize with the number of operands.
//Each operands has zero version to mdid.
MDPhi * UseDefMgr::allocMDPhi(UINT mdid)
{
    ASSERT0(mdid > MD_UNDEF);
    //Different from MDDef, MDPhi will be allocated in individual pool.
    MDPhi * phi = (MDPhi*)smpoolMallocConstSize(sizeof(MDPhi), m_phi_pool);
    phi->init();
    MDDEF_id(phi) = m_def_count++;
    m_def_vec.set(MDDEF_id(phi), phi);
    return phi;
}


void UseDefMgr::removeMDDef(MDDef * def)
{
    ASSERT0(def);
    //No need to destroy def's memory overtly.
    m_def_vec.set(def->id(), nullptr);
    def->clean();
}


MDDefStmt * UseDefMgr::allocMDDefStmt()
{
    MDDefStmt * def = (MDDefStmt*)smpoolMallocConstSize(sizeof(MDDefStmt),
                                                        m_defstmt_pool);
    def->init();
    MDDEF_id(def) = m_def_count++;
    m_def_vec.set(MDDEF_id(def), def);
    return def;
}


MDDefSet * UseDefMgr::allocMDDefSet()
{
    MDDefSet * defset = (MDDefSet*)smpoolMallocConstSize(sizeof(MDDefSet),
                                                         m_defset_pool);
    defset->init();
    return defset;
}


xcom::SC<VOpnd*> * UseDefMgr::allocSCVOpnd(VOpnd * opnd)
{
    xcom::SC<VOpnd*> * sc = xcom::removehead_single_list(&m_free_sc_list);
    if (sc != nullptr) {
        sc->init();
        return sc;
    }

    sc = (xcom::SC<VOpnd*>*)smpoolMallocConstSize(sizeof(xcom::SC<VOpnd*>),
                                                  m_vopnd_sc_pool);
    sc->init();
    SC_val(sc) = opnd;
    return sc;
}


VConst * UseDefMgr::allocVConst(IR const* ir)
{
    ASSERTN(m_vconst_pool, ("not init"));
    VConst * p = (VConst*)smpoolMallocConstSize(sizeof(VConst), m_vconst_pool);
    ASSERT0(p);
    ::memset(p, 0, sizeof(VConst));
    VOPND_code(p) = VOPND_CONST;
    VOPND_id(p) = m_vopnd_count++;
    VCONST_val(p) = ir;
    return p;
}


VMD * UseDefMgr::getVMD(UINT mdid, UINT version) const
{
    ASSERT0(mdid > MD_UNDEF);
    Vector<VMD*> * vec = const_cast<UseDefMgr*>(this)->m_map_md2vmd.get(mdid);
    if (vec == nullptr) {
        return nullptr;
    }
    return vec->get(version);
}


//The function remove and clean all information of 'vmd' from MDSSAMgr.
void UseDefMgr::removeVMD(VMD * vmd)
{
    ASSERT0(vmd);
    m_vopnd_vec.set(vmd->id(), nullptr);
    VMDVec * vec = m_map_md2vmd.get(vmd->mdid());
    ASSERT0(vec);
    vec->set(vmd->version(), nullptr);
    vmd->destroy();
}


//Allocate VMD and ensure it is unique according to 'version' and 'mdid'.
VMD * UseDefMgr::allocVMD(UINT mdid, UINT version)
{
    ASSERT0(mdid > MD_UNDEF);
    VMDVec * vec = m_map_md2vmd.get(mdid);
    if (vec == nullptr) {
        vec = new VMDVec();
        m_map_md2vmd.set(mdid, vec);
    }

    VMD * v = vec->get(version);
    if (v != nullptr) {
        return v;
    }

    ASSERTN(m_vmd_pool, ("not init"));
    v = (VMD*)smpoolMallocConstSize(sizeof(VMD), m_vmd_pool);
    ASSERT0(v);
    ::memset(v, 0, sizeof(VMD));
    v->init();
    VOPND_code(v) = VOPND_MD;
    VOPND_id(v) = m_vopnd_count++;
    VMD_mdid(v) = mdid;
    VMD_version(v) = version;
    VMD_def(v) = nullptr;
    vec->set(version, v);
    m_vopnd_vec.set(v->id(), v);
    return v;
}


size_t UseDefMgr::count_mem() const
{
    size_t count = smpoolGetPoolSize(m_mdssainfo_pool);
    count += smpoolGetPoolSize(m_phi_pool);
    count += smpoolGetPoolSize(m_defstmt_pool);
    count += smpoolGetPoolSize(m_defset_pool);
    count += smpoolGetPoolSize(m_vconst_pool);
    count += smpoolGetPoolSize(m_vmd_pool);
    count += smpoolGetPoolSize(m_philist_pool);
    count += smpoolGetPoolSize(m_philist_sc_pool);
    count += m_map_md2vmd.count_mem();
    count += m_vopnd_vec.count_mem();
    count += m_def_vec.count_mem();
    count += sizeof(UseDefMgr);
    return count;
}


MDPhiList * UseDefMgr::genBBPhiList(UINT bbid)
{
    MDPhiList * lst = m_philist_vec.get(bbid);
    if (lst != nullptr) { return lst; }

    lst = (MDPhiList*)smpoolMallocConstSize(sizeof(MDPhiList), m_philist_pool);
    ASSERT0(lst);
    lst->init(m_philist_sc_pool);
    m_philist_vec.set(bbid, lst);
    return lst;
}
//END UseDefMgr

} //namespace xoc
