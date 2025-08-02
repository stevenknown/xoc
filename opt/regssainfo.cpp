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

//
//START VROpndSet
//
VROpnd * VROpndSet::get_unique(RegSSAMgr const* mgr) const
{
    VROpndSetIter it = nullptr;
    BSIdx first = get_first(&it);
    if (first == BS_UNDEF) { return nullptr; }
    BSIdx second = get_next(first, &it);
    return second != BS_UNDEF ? nullptr : mgr->getVROpnd(first);
}


void VROpndSet::dump(Region const* rg) const
{
    if (!rg->isLogMgrInit()) { return; }
    DefSBitSetCore::dump(rg->getLogMgr()->getFileHandler());
}
//END VROpndSet

//
//START RegDef
//
//Note real-use does not include IR_ID.
bool RegDef::hasOutsideLoopRealUse(LI<IRBB> const* li, Region const* rg) const
{
    VReg * res = getResult();
    VReg::UseSetIter it;
    for (UINT i = res->getUseSet()->get_first(it);
         !it.end(); i = res->getUseSet()->get_next(it)) {
        IR const* u = rg->getIR(i);
        if (u->is_id()) {
            RegPhi const* phi = PHYREG_phi(u);
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
bool RegDef::isNext(RegDef const* n) const
{
    return getNextSet() != nullptr ? getNextSet()->find(n) : false;
}


//Return true if n is the Next DEF, and n may not be the immediate-next-def to
//current DEF. The function will access all next DEFs recursively.
bool RegDef::isInNextSet(RegDef const* n, RegDUMgr const* mgr) const
{
    #ifdef _DEBUG_
    xcom::TTab<UINT> visited; //to guarantee there is no cycle in NextSet.
    #endif
    List<RegDef const*> wl;
    wl.append_tail(this);
    RegDef const* def;
    while ((def = wl.remove_head()) != nullptr) {
        #ifdef _DEBUG_
        ASSERT0(!visited.find(def->id()));
        visited.append(def->id());
        #endif
        if (def->isNext(n)) { return true; }
        if (def->getNextSet() == nullptr) { continue; }

        RegDefSetIter nit = nullptr;
        for (BSIdx w = def->getNextSet()->get_first(&nit);
            w != BS_UNDEF; w = def->getNextSet()->get_next(w, &nit)) {
            RegDef const* next = mgr->getRegDef(w);
            ASSERTN(next, ("not such RegDef"));
            wl.append_tail(next);
        }
    }
    return false;
}


void RegDef::cleanNextSet(RegDUMgr * mgr)
{
    if (getNextSet() != nullptr) {
        getNextSet()->clean(*mgr->getSBSMgr());
    }
}


IR * RegDef::getOcc() const
{
    ASSERT0(!is_phi());
    return REGDEFSTMT_occ(this);
}


IRBB * RegDef::getBB() const
{
    ASSERT0(is_phi() ? REGPHI_bb(this) != nullptr :
                       (REGDEFSTMT_occ(this) != nullptr &&
                        REGDEFSTMT_occ(this)->is_stmt()));
    return is_phi() ? REGPHI_bb(this) : REGDEFSTMT_occ(this)->getBB();
}


void RegDef::dump(Region const* rg, RegDUMgr const* mgr) const
{
    ASSERT0(rg);
    if (is_phi()) {
        prt(rg, "RegPhi%u:", id());
    } else {
        prt(rg, "RegDef%u:", id());
    }
    getResult()->dump(mgr->getRA());
}
//END RegDef


//
//START RegDefSet
//
//Return true if there is at least one element in RegDefSet that dominates v.
bool RegDefSet::hasAtLeastOneElemDom(
    RegDef const* v, RegSSAMgr const* mgr) const
{
    RegDefSetIter it = nullptr;
    RegDUMgr * dumgr = const_cast<RegSSAMgr*>(mgr)->getRegDUMgr();
    for (BSIdx i = get_first(&it); i != BS_UNDEF; i = get_next(i, &it)) {
        RegDef const* def = dumgr->getRegDef(i);
        if (mgr->isDom(def, v)) {
            return true;
        }
    }
    return false;
}
//END RegDefSet


//
//START VRegVec
//
//Find the VReg that have Reg defined at given BB.
//Return true if the function find DEF in given BB.
bool VRegVec::hasDefInBB(UINT bbid) const
{
    for (VecIdx i = 0; i <= get_last_idx(); i++) {
        VReg * p = get(i);
        if (p == nullptr || p->getDef() == nullptr) { continue; }
        ASSERT0(p->getDef() && p->getDef()->getBB());
        if (p->getDef()->getBB()->id() == bbid) {
            return true;
        }
    }
    return false;
}
//END VRegVec


//
//START RegSSAInfo
//
//Return true if all definition of vopnds can reach 'exp'.
bool RegSSAInfo::isMustDef(RegDUMgr const* dumgr, IR const* exp) const
{
    ASSERT0(dumgr && exp && exp->is_exp());
    VROpndSetIter iter = nullptr;
    RegSSAInfo * pthis = const_cast<RegSSAInfo*>(this);
    for (BSIdx i = pthis->getVROpndSet()->get_first(&iter);
         i != BS_UNDEF; i = pthis->getVROpndSet()->get_next(i, &iter)) {
        VReg * VROpnd = (VReg*)dumgr->getVROpnd(i);
        ASSERT0(VROpnd && VROpnd->is_reg());
        if (!VROpnd->findUse(exp)) {
            return false;
        }
    }
    return true;
}


//Return true if current ssainfo is equal to src.
bool RegSSAInfo::isEqual(RegSSAInfo const& src) const
{
    return readVROpndSet().is_equal(src.readVROpndSet());
}


//The function looks for the first RegDef that exactly define 'reg'.
RegDef const* RegSSAInfo::findCoverRegDef(RegSSAMgr const* mgr, Reg reg) const
{
    ASSERT0(mgr && reg != REG_UNDEF);
    VROpndSetIter it = nullptr;
    RegDUMgr const* dumgr = const_cast<RegSSAMgr*>(mgr)->getRegDUMgr();
    for (BSIdx i = readVROpndSet().get_first(&it);
         i != BS_UNDEF; i = readVROpndSet().get_next(i, &it)) {
        VReg const* t = (VReg*)dumgr->getVROpnd(i);
        ASSERT0(t && t->is_reg());
        RegDef const* regdef = t->getDef();
        if (regdef == nullptr) { continue; }
        Reg regdef_reg = regdef->getResultReg();
        if (mgr->isExactCover(regdef_reg, reg)) { return regdef; }
    }
    return nullptr;
}


void RegSSAInfo::copyBySpecificReg(
    RegSSAInfo const& src, Reg reg, RegDUMgr * mgr)
{
    VROpndSetIter it = nullptr;
    bool added = false;
    cleanVROpndSet(mgr);
    for (BSIdx i = src.readVROpndSet().get_first(&it);
         i != BS_UNDEF; i = src.readVROpndSet().get_next(i, &it)) {
        VReg const* t = (VReg const*)mgr->getVROpnd(i);
        ASSERT0(t);
        if (t->is_reg() && t->reg() == reg) {
            addVROpnd((VROpnd const*)t, mgr);
            added = true;
        }
    }
    ASSERTN(added, ("no VROpnd corresponded to %s",
            mgr->getRA()->getRegName(reg)));
}


void RegSSAInfo::copyVROpndSet(VROpndSet const& src, RegDUMgr * mgr)
{
    getVROpndSet()->copy(src, *mgr->getSBSMgr());
}


void RegSSAInfo::cleanVROpndSet(RegDUMgr * mgr)
{
    m_vropnd_set.clean(*mgr->getSBSMgr());
}


//Return true if current RegSSAInfo contains given Reg only.
bool RegSSAInfo::containSpecificRegOnly(Reg reg, RegDUMgr const* dumgr) const
{
    VROpndSetIter it = nullptr;
    for (BSIdx i = readVROpndSet().get_first(&it);
         i != BS_UNDEF; i = readVROpndSet().get_next(i, &it)) {
        VReg const* t = (VReg const*)dumgr->getVROpnd(i);
        ASSERT0(t);
        if (t->is_reg() && t->reg() != reg) { return false; }
    }
    return true;
}


//Remove VROpnd from current RegSSAInfo.
void RegSSAInfo::removeVROpnd(VROpnd const* VROpnd, RegDUMgr * mgr)
{
    m_vropnd_set.remove(VROpnd, *mgr->getSBSMgr());
}


//Return true if there is VReg renamed.
//Note current RegSSAInfo is the SSA info of 'exp'.
//vreg: intent to be swap-in.
bool RegSSAInfo::renameOrAddSpecificUse(
    IR const* exp, MOD VReg * vreg, RegDUMgr * mgr)
{
    ASSERT0(exp && exp->is_exp() && mgr);
    ASSERT0(mgr->getRegSSAInfo(exp) == this);
    VROpndSet * vropndset = getVROpndSet();
    Reg vregid = vreg->reg();
    VROpndSetIter it = nullptr;
    VROpndSetIter prev_it = nullptr;
    bool changed = false;
    for (BSIdx i = vropndset->get_first(&it); i != BS_UNDEF;
         prev_it = it, i = vropndset->get_next(i, &it)) {
        VReg * vr = (VReg*)mgr->getVROpnd(i);
        ASSERT0(vr && vr->is_reg());
        if (vr->reg() == vregid) {
            vr->removeUse(exp);
            //Note here we use SBitSet::remove() rather than
            //removeVROpnd to speedup the accessing of bitset.
            //removeVROpnd(VROpnd, mgr);
            vropndset->remove(vr, prev_it, it, *mgr->getSBSMgr());
            changed = true;
            break;
        }
    }
    if (mgr->isRefReg(exp, vregid)) {
        //Note addUse() will build DefUse chain between 'vreg' and 'exp'.
        vreg->addUse(exp);
        addVROpnd(vreg, mgr);
        changed = true;
    }
    return changed;
}


//Remove given IR expression from UseSet of each VROpnd in RegSSAInfo.
//Note current RegSSAInfo is the SSA info of 'exp'.
//exp: IR expression to be removed.
void RegSSAInfo::removeSpecificUse(IR const* exp, Reg reg, RegDUMgr * mgr)
{
    ASSERT0(exp && exp->is_exp() && mgr);
    ASSERT0(mgr->getRegSSAInfo(exp) == this);
    VROpndSet * VROpndSet = getVROpndSet();
    BSIdx nexti;
    VROpndSetIter it = nullptr;
    VROpndSetIter prev_it = nullptr;
    for (BSIdx i = VROpndSet->get_first(&it); i != BS_UNDEF; i = nexti) {
        prev_it = it;
        nexti = VROpndSet->get_next(i, &it);
        VReg * VROpnd = (VReg*)mgr->getVROpnd(i);
        ASSERT0(VROpnd && VROpnd->is_reg());
        if (VROpnd->reg() == reg) {
            VROpnd->removeUse(exp);
            VROpndSet->remove(VROpnd, prev_it, it, *mgr->getSBSMgr());
            return;
        }
    }
}


void RegSSAInfo::addUseSet(RegSSAInfo const* src, IN RegDUMgr * mgr)
{
    ASSERT0(src);
    getVROpndSet()->bunion(src->readVROpndSet(), *mgr->getSBSMgr());
}


void RegSSAInfo::addUseSet(IRSet const& set, IN RegDUMgr * mgr)
{
    IRSetIter it = nullptr;
    Region const* rg = mgr->getRegion();
    for (BSIdx i = set.get_first(&it);
         i != BS_UNDEF; i = set.get_next(i, &it)) {
        IR * exp = rg->getIR(i);
        ASSERT0(exp && exp->is_exp());
        addUse(exp, mgr);
    }
}


//Add given IR expression to occurence set.
//exp: IR expression to be added.
void RegSSAInfo::addUse(IR const* exp, IN RegDUMgr * mgr)
{
    ASSERT0(exp && exp->is_exp() && RegSSAMgr::hasRegSSAInfo(exp) && mgr);
    if (exp->is_id()) {
        //IR_ID represents an individual versioned Reg, thus each IR_ID only
        //can have one VROpnd.
        ASSERT0(getVROpndSet()->get_elem_count() == 1);
    }
    VROpndSetIter iter = nullptr;
    for (BSIdx i = getVROpndSet()->get_first(&iter);
         i != BS_UNDEF; i = getVROpndSet()->get_next(i, &iter)) {
        VReg * VROpnd = (VReg*)mgr->getVROpnd(i);
        ASSERT0(VROpnd && VROpnd->is_reg());
        VROpnd->addUse(exp);
    }
}


bool RegSSAInfo::isUse(
    OUT VReg const** vreg, IR const* ir, RegSSAMgr const* mgr) const
{
    //Iterate each VROpnd.
    VROpndSetIter iter = nullptr;
    for (BSIdx i = readVROpndSet().get_first(&iter);
         i != BS_UNDEF; i = readVROpndSet().get_next(i, &iter)) {
        VReg const* t = (VReg const*)const_cast<RegSSAMgr*>(mgr)->
            getRegDUMgr()->getVROpnd(i);
        ASSERT0(t && t->is_reg());
        if (t->isUse(ir)) {
            if (vreg != nullptr) { *vreg = t; }
            return true;
        }
    }
    return false;
}


VROpnd * RegSSAInfo::getVROpndForReg(Reg reg, RegSSAMgr const* mgr) const
{
    //Iterate each VROpnd.
    VROpndSetIter iter = nullptr;
    for (BSIdx i = readVROpndSet().get_first(&iter);
         i != BS_UNDEF; i = readVROpndSet().get_next(i, &iter)) {
        VReg * t = (VReg*)const_cast<RegSSAMgr*>(mgr)->
            getRegDUMgr()->getVROpnd(i);
        ASSERT0(t && t->is_reg());
        if (t->reg() == reg) { return t; }
    }
    return nullptr;
}


void RegSSAInfo::dump(RegSSAMgr const* mgr) const
{
    if (!mgr->getRegion()->isLogMgrInit()) { return; }
    VROpndSetIter iter = nullptr;
    RegSSAInfo * pthis = const_cast<RegSSAInfo*>(this);
    for (BSIdx i = pthis->getVROpndSet()->get_first(&iter);
         i != BS_UNDEF; i = pthis->getVROpndSet()->get_next(i, &iter)) {
        note(mgr->getRegion(), "\nREF:");
        VReg const* VROpnd = (VReg*)const_cast<RegSSAMgr*>(mgr)->
            getRegDUMgr()->getVROpnd(i);
        ASSERT0(VROpnd && VROpnd->is_reg());
        VROpnd->dump(mgr->getRegion(), const_cast<RegSSAMgr*>(mgr)->
            getRegDUMgr());
    }
}


bool RegSSAInfo::isLiveInVROpndSet(RegSSAMgr const* mgr) const
{
    return isLiveInVROpndSet(const_cast<RegSSAMgr*>(mgr)->getRegDUMgr());
}


bool RegSSAInfo::isLiveInVROpndSet(RegDUMgr const* mgr) const
{
    VROpndSetIter it = nullptr;
    for (BSIdx i = readVROpndSet().get_first(&it);
        i != BS_UNDEF; i = readVROpndSet().get_next(i, &it)) {
        VReg const* VROpnd = (VReg*)mgr->getVROpnd(i);
        ASSERT0(VROpnd && VROpnd->is_reg());
        if (!VROpnd->isLiveIn()) { return false; }
    }
    return true;
}


void RegSSAInfo::addVROpnd(VROpnd const* vr, RegDUMgr * mgr)
{
    m_vropnd_set.append(vr, *mgr->getSBSMgr());
}
//END RegSSAInfo


//
//START UINT2VRegVec
//
void UINT2VRegVec::set(Reg reg, VRegVec * vregvec)
{
    if (reg < m_threshold) {
        m_reg2vregvec_vec.set(reg, vregvec);
        return;
    }
    m_reg2vregvec_map.set(reg, vregvec);
}


size_t UINT2VRegVec::count_mem() const
{
    size_t count = sizeof(UINT2VRegVec);
    count += m_reg2vregvec_vec.count_mem();
    count += m_reg2vregvec_map.count_mem();
    return count;
}
//END UINT2VRegVec


//
//START VReg
//
//Concisely dump.
void VReg::dump(LinearScanRA const* ra) const
{
    ASSERT0(ra);
    Region const* rg = ra->getRegion();
    prt(rg, "VReg%u:", id());
    VReg::dumpRegAndVer(rg, reg(), version(), ra);
}


CHAR const* VReg::dump(LinearScanRA const* ra, OUT VRegFixedStrBuf & buf) const
{
    ASSERT0(ra);
    buf.strcat("VReg%u:", id());
    VReg::dumpRegAndVer(buf, reg(), version(), ra);
    return buf.getBuf();
}


void VReg::clean()
{
    VROpnd::clean();
    VREG_def(this) = nullptr;
    VREG_reg(this) = REG_UNDEF;
    VREG_version(this) = REGSSA_INIT_VERSION;
    VREG_occs(this).clean();
}


static void dumpDefChain(
    VReg const* vreg, RegDUMgr const* mgr, Region const* rg)
{
    //Dump Def
    RegDef const* regdef = vreg->getDef();
    if (regdef == nullptr) {
        prt(rg, ",-");
        return;
    }
    LinearScanRA const* ra = mgr->getRA();
    ASSERT0(ra);

    //TBD: I think RegDef could be PHI.
    //ASSERT0(!getDef()->is_phi());
    if (regdef->getPrev() != nullptr) {
        VReg const* prev_vreg = regdef->getPrev()->getResult();
        ASSERT0(prev_vreg);
        prt(rg, ",PrevDEF:");
        prev_vreg->dump(mgr->getRA());
    } else {
        prt(rg, ",-");
    }
    if (regdef->getNextSet() == nullptr) { return; }
    RegDefSetIter nit = nullptr;
    bool first = true;
    for (BSIdx w = regdef->getNextSet()->get_first(&nit);
        w != BS_UNDEF; w = regdef->getNextSet()->get_next(w, &nit)) {
        if (first) {
            first = false;
        } else {
            prt(rg, ",");
        }
        RegDef const* use = mgr->getRegDef(w);
        ASSERTN(use, ("not such RegDef"));
        ASSERT0(use->getResult());
        ASSERTN(use->getPrev() == regdef, ("insanity relation"));
        VReg const* next_vreg = use->getResult();
        ASSERT0(next_vreg);
        prt(rg, ",NextDEF:");
        next_vreg->dump(mgr->getRA());
    }
}


static void dumpOccSet(VReg const* vreg, Region const* rg)
{
    //Dump OccSet
    prt(rg, "|USESET:");
    bool first = true;
    VReg::UseSetIter vit;
    xcom::DefFixedStrBuf tmp;
    for (UINT i2 = const_cast<VReg*>(vreg)->getUseSet()->get_first(vit);
         !vit.end(); i2 = const_cast<VReg*>(vreg)->getUseSet()->get_next(vit)) {
        if (first) { first = false; }
        else { prt(rg, ","); }
        IR * use = rg->getIR(i2);
        ASSERT0(RegSSAMgr::hasRegSSAInfo(use));
        prt(rg, "%s", xoc::dumpIRName(use, tmp));
    }
}


CHAR const* VReg::dumpRegAndVer(
    OUT VRegFixedStrBuf & buf, Reg r, UINT ver, LinearScanRA const* ra)
{
    REGFILE rf = ra->getRegFile(r);
    buf.strcat("%s(%s)V%u", ra->getRegName(r), ra->getRegFileName(rf), ver);
    return buf.getBuf();
}


void VReg::dumpRegAndVer(
    Region const* rg, Reg r, UINT ver, LinearScanRA const* ra)
{
    REGFILE rf = ra->getRegFile(r);
    prt(rg, "%s(%s)V%u", ra->getRegName(r), ra->getRegFileName(rf), ver);
}


static void dumpVReg(Region const* rg, RegDUMgr const* mgr, VReg const* vr)
{
    prt(rg, "VReg%u:", vr->id());
    VReg::dumpRegAndVer(rg, vr->reg(), vr->version(), mgr->getRA());
    dumpDefChain(vr, mgr, rg);
    dumpOccSet(vr, rg);

}


void VReg::dump(Region const* rg, RegDUMgr const* mgr) const
{
    if (!rg->isLogMgrInit()) { return; }
    ASSERT0(is_reg() && rg);
    dumpVReg(rg, mgr, this);
}


CHAR const* VReg::dumpToBuf(
    OUT StrBuf & outbuf, Region const* rg, RegDUMgr const* mgr,
    UINT indent) const
{
    ASSERT0(rg);
    if (!rg->isLogMgrInit()) { return nullptr; }
    class Dump : public xoc::DumpToBuf {
    public:
        VReg const* vreg;
        RegDUMgr const* mgr;
    public:
        Dump(Region const* rg, xcom::StrBuf & buf, UINT indent)
            : DumpToBuf(rg, buf, indent) {}
        virtual void dumpUserInfo() const override
        {
            ASSERT0(vreg && vreg->is_reg());
            dumpVReg(getRegion(), mgr, vreg);
        }
    };
    Dump d(rg, outbuf, indent);
    d.vreg = this;
    d.mgr = mgr;
    d.dump();
    return outbuf.getBuf();
}


void VReg::addUseSet(IRSet const& set, Region const* rg)
{
    IRSetIter it = nullptr;
    for (BSIdx i = set.get_first(&it);
         i != BS_UNDEF; i = set.get_next(i, &it)) {
        IR * exp = rg->getIR(i);
        ASSERT0(exp && exp->is_exp());
        addUse(exp);
    }
}
//END VReg


//
//START RegPhi
//
void RegPhi::replaceOpnd(MOD IR * oldopnd, MOD IR * newopnd)
{
    ASSERT0(oldopnd && newopnd);
    xcom::replace_one(&REGPHI_opnd_list(this), oldopnd, newopnd);
}


IR * RegPhi::insertOpndAt(
    RegSSAMgr * mgr, UINT pos, IRBB const* pred, OptCtx const& oc,
    OUT RegSSAStatus & st)
{
    Region * rg = mgr->getRegion();
    UINT i = 0;
    IR * marker = REGPHI_opnd_list(this);
    IR * last = nullptr;
    for (; marker != nullptr && i <= pos; marker = marker->get_next(), i++) {
        last = marker;
    }

    //Generate a new ID as operand of PHI.
    MD const* res = rg->getMDSystem()->getMD(getResult()->reg());
    ASSERT0(res);
    IR * opnd = mgr->getIRMgr()->buildId(res->get_base());
    opnd->setRefMD(res, rg);
    ASSERT0(opnd->getRefMDSet() == nullptr);
    PHYREG_phi(opnd) = this; //Record ID's host PHI.

    //Find the latest live-in version of PHI's operand Reg.
    VReg * livein_def = mgr->findDomLiveInDefFrom(res->id(),
        const_cast<IRBB*>(pred)->getLastIR(), pred, oc, st);
    RegSSAInfo * regssainfo = nullptr;
    if (livein_def != nullptr) {
        //Generate RegSSAInfo for new operand.
        regssainfo = mgr->genRegSSAInfoAndSetDedicatedVersionVReg(
            opnd, livein_def->version());
    } else {
        //Still generate a RegSSAInfo to new operand of PHI even if there is no
        //legal VReg. Because we expect to do error recovery and tolerate more
        //following passes untill the RegSSA reconstruction.
        //Generate RegSSAInfo for new operand.
        regssainfo = mgr->genRegSSAInfoAndSetDedicatedVersionVReg(
            opnd, REGSSA_INIT_VERSION);
    }
    mgr->getRegDUMgr()->setRegSSAInfo(opnd, regssainfo);

    //Add current ID into occurrence set of each VROpnd that recorded
    //in 'regssainfo'.
    mgr->addUseToRegSSAInfo(opnd, regssainfo);

    if (marker != nullptr) {
        //Insert operand into list.
        xcom::insertbefore(&REGPHI_opnd_list(this), marker, opnd);
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
    //  Phi: R13V4 <-| UsedBy :
    xcom::add_next(&REGPHI_opnd_list(this), &last, opnd);
    return opnd;
}


IR * RegPhi::getOpnd(UINT idx) const
{
    UINT i = 0;
    IR * x = REGPHI_opnd_list(this);
    for (; x != nullptr && i < idx; x = x->get_next(), i++) {;}
    return x;
}


//Facility function to make it easier to get the VROpnd of operand of PHI.
VReg * RegPhi::getOpndVReg(IR const* opnd, RegDUMgr const* mgr) const
{
    ASSERTN(xcom::in_list(getOpndList(), opnd), ("not operand of phi"));
    if (opnd->getCode() != IR_PHYREG) { return nullptr; }
    ASSERT0(mgr);
    RegSSAInfo * regssainfo = mgr->getRegSSAInfo(opnd);
    ASSERT0(regssainfo);
    UINT elemcnt = regssainfo->getVROpndSet()->get_elem_count();
    if (elemcnt == 0) {
        //Note VROpndSet of 'opnd' may be empty after some optimization.
        //It does not happen when RegSSA just constructed. The USE that
        //without real-DEF will have a virtual-DEF that version is 0.
        //During some increment-maintaining of RegSSA, VROpnd may be removed,
        //and VROpndSet become empty.
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
    VROpndSetIter iter = nullptr;
    VReg * vr = (VReg*)mgr->getVROpnd(regssainfo->getVROpndSet()->
        get_first(&iter));
    ASSERT0(vr->is_reg());
    return vr;
}


void RegPhi::dumpOpnd(
    IR const* opnd, IRBB const* pred, Region const* rg,
    RegDUMgr const* mgr) const
{
    prt(rg, "(");
    switch (opnd->getCode()) {
    case IR_CONST:
        xoc::dumpConstContent(opnd, rg);
        break;
    case IR_LDA:
        xoc::dumpIRName(opnd, rg);
        break;
    case IR_PHYREG: {
        VReg const* vr = getOpndVReg(opnd, mgr);
        xoc::dumpIRName(opnd, rg);
        prt(rg, " ");
        if (vr == nullptr) {
            prt(rg, "????");
        } else {
            ASSERT0(mgr->getReg(opnd) == vr->reg());
            vr->dump(mgr->getRA());
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


static void dumpUseSet(VReg const* vreg, Region * rg)
{
    ASSERT0(vreg);
    note(rg, "|USESET:");
    VReg::UseSetIter vit;
    xcom::DefFixedStrBuf tmp;
    for (UINT i = const_cast<VReg*>(vreg)->getUseSet()->get_first(vit);
         !vit.end(); i = const_cast<VReg*>(vreg)->getUseSet()->get_next(vit)) {
        IR const* use = rg->getIR(i);
        ASSERT0(RegSSAMgr::hasExpRegSSAInfo(use));
        prt(rg, "(%s) ", xoc::dumpIRName(use, tmp));
    }
}


void RegPhi::insertOpndAfter(IR * marker, IR * opnd)
{
    ASSERT0(marker && opnd && opnd->getCode() == IR_PHYREG);
    ASSERT0(xcom::in_list(REGPHI_opnd_list(this), marker));
    ASSERT0(!xcom::in_list(REGPHI_opnd_list(this), opnd));
    xcom::insertafter(&marker, opnd);
    PHYREG_phi(opnd) = this; //Record PhyReg's host PHI.
}


void RegPhi::dump(Region const* rg, RegDUMgr const* mgr) const
{
    ASSERT0(rg);
    ASSERT0(is_phi());
    if (!rg->isLogMgrInit()) { return; }
    List<IRBB*> preds;
    IRCFG * cfg = rg->getCFG();
    ASSERT0(cfg);
    cfg->get_preds(preds, getBB());
    RegDef::dump(rg, mgr);
    prt(rg, " <- ");
    IRBB * pred = preds.get_head();
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
//END RegPhi


//
//START RegDUMgr
//
RegDUMgr::RegDUMgr(Region * rg, RegSSAMgr * mgr) :
    m_rg(rg), m_regssa_mgr(mgr), m_sbs_mgr(mgr->getSBSMgr())
{
    ASSERT0(m_rg && m_regssa_mgr);
    m_irmgr = m_rg->getIRMgr();
    m_timgr = m_rg->getRegionMgr()->getTargInfoMgr();

    //Single List Core need user declared a mempool.
    m_vropnd_sc_pool = smpoolCreate(
        sizeof(xcom::SC<VROpnd*>) * 4, MEM_CONST_SIZE);
    m_phi_pool = smpoolCreate(sizeof(RegPhi) * 2, MEM_CONST_SIZE);
    m_defstmt_pool = smpoolCreate(sizeof(RegDefStmt) * 2, MEM_CONST_SIZE);
    m_defset_pool = smpoolCreate(sizeof(RegDefSet) * 2, MEM_CONST_SIZE);
    m_vrconst_pool = smpoolCreate(sizeof(VRConst)*2, MEM_CONST_SIZE);
    m_vreg_pool = smpoolCreate(sizeof(VReg)*2, MEM_CONST_SIZE);
    m_philist_pool = smpoolCreate(sizeof(RegPhiList)*2, MEM_CONST_SIZE);
    m_philist_sc_pool = smpoolCreate(
        sizeof(xcom::SC<RegPhi*>) * 4, MEM_CONST_SIZE);
    m_regssainfo_pool = smpoolCreate(sizeof(RegSSAInfo)*2, MEM_CONST_SIZE);
    m_free_sc_list = nullptr;
    m_def_count = MDDEF_UNDEF + 1;
    m_vropnd_count = VROPND_UNDEF + 1;
    m_philist_vec.set(rg->getBBList()->get_elem_count(), 0);
}


bool RegDUMgr::isRefReg(IR const* ir, Reg reg) const
{
    ASSERT0(RegSSAMgr::hasRegSSAInfo(ir));
    Reg irreg = getRegSSAMgr()->getReg(ir);
    ASSERT0(irreg != REG_UNDEF);
    return getRegSSAMgr()->isAlias(irreg, reg);
}


void RegDUMgr::destroyReg2VRegVec()
{
    Vector<VRegVec*> * vec = m_map_reg2vreg.getVec();
    if (vec != nullptr) {
        for (VecIdx i = 0; i <= vec->get_last_idx(); i++) {
            Vector<VReg*> * vpv = m_map_reg2vreg.get((UINT)i);
            if (vpv != nullptr) {
                delete vpv;
            }
        }
    }
    TMap<UINT, VRegVec*> * map = m_map_reg2vreg.getMap();
    if (map != nullptr) {
        TMapIter<UINT, VRegVec*> iter;
        VRegVec * VRegVec;
        for (map->get_first(iter, &VRegVec);
             VRegVec != nullptr; map->get_next(iter, &VRegVec)) {
            delete VRegVec;
        }
    }
}


void RegDUMgr::destroyAllRegSSAInfo()
{
    for (VecIdx i = 0; i <= m_regssainfo_vec.get_last_idx(); i++) {
        RegSSAInfo * info = m_regssainfo_vec.get((UINT)i);
        if (info != nullptr) {
            info->destroy(*getSBSMgr());
        }
    }
    m_regssainfo_vec.clean();
}


void RegDUMgr::cleanOrDestroy(bool is_reinit)
{
    ASSERT0(m_rg);
    for (VecIdx i = 0; i <= m_vropnd_vec.get_last_idx(); i++) {
        VROpnd * v = m_vropnd_vec.get((UINT)i);
        if (v != nullptr && v->is_reg()) {
            ((VReg*)v)->destroy();
        }
    }
    for (VecIdx i = 0; i <= m_def_vec.get_last_idx(); i++) {
        RegDef * d = m_def_vec.get((UINT)i);
        if (d != nullptr && d->getNextSet() != nullptr) {
            d->getNextSet()->clean(*getSBSMgr());
        }
    }
    destroyAllRegSSAInfo();
    destroyReg2VRegVec();
    if (is_reinit) {
        m_map_reg2vreg.destroy();
        m_map_reg2vreg.init();
        m_philist_vec.destroy();
        m_philist_vec.init();
        m_def_vec.destroy();
        m_def_vec.init();
        m_regssainfo_vec.destroy();
        m_regssainfo_vec.init();
        m_vropnd_vec.destroy();
        m_vropnd_vec.init();
        m_irid2regssainfo.destroy();
        m_irid2regssainfo.init();
        m_def_count = REGDEF_UNDEF + 1;
        m_vropnd_count = VROPND_UNDEF + 1;
    }

    ASSERT0(m_vropnd_sc_pool);
    smpoolDelete(m_vropnd_sc_pool);

    ASSERT0(m_phi_pool);
    smpoolDelete(m_phi_pool);

    ASSERT0(m_defstmt_pool);
    smpoolDelete(m_defstmt_pool);

    ASSERT0(m_defset_pool);
    smpoolDelete(m_defset_pool);

    ASSERT0(m_vreg_pool);
    smpoolDelete(m_vreg_pool);

    ASSERT0(m_vrconst_pool);
    smpoolDelete(m_vrconst_pool);

    ASSERT0(m_philist_pool);
    smpoolDelete(m_philist_pool);

    ASSERT0(m_philist_sc_pool);
    smpoolDelete(m_philist_sc_pool);

    ASSERT0(m_regssainfo_pool);
    smpoolDelete(m_regssainfo_pool);

    if (is_reinit) {
        m_vropnd_sc_pool = smpoolCreate(sizeof(xcom::SC<VROpnd*>) * 4,
            MEM_CONST_SIZE);
        m_phi_pool = smpoolCreate(sizeof(RegPhi) * 2, MEM_CONST_SIZE);
        m_defstmt_pool = smpoolCreate(sizeof(RegDefStmt) * 2, MEM_CONST_SIZE);
        m_defset_pool = smpoolCreate(sizeof(RegDefSet) * 2, MEM_CONST_SIZE);
        m_vreg_pool = smpoolCreate(sizeof(VReg) * 2, MEM_CONST_SIZE);
        m_vrconst_pool = smpoolCreate(sizeof(VRConst)*2, MEM_CONST_SIZE);
        m_philist_pool = smpoolCreate(sizeof(RegPhiList)*2, MEM_CONST_SIZE);
        m_philist_sc_pool = smpoolCreate(sizeof(xcom::SC<RegPhi*>) * 4,
            MEM_CONST_SIZE);
        m_regssainfo_pool = smpoolCreate(sizeof(RegSSAInfo)*4, MEM_CONST_SIZE);
    }
}


LinearScanRA const* RegDUMgr::getRA() const
{
    return getRegSSAMgr()->getRA();
}


Reg RegDUMgr::getReg(IR const* exp) const
{
    ASSERT0(exp && RegSSAMgr::hasRegSSAInfo(exp));
    return getRegSSAMgr()->getReg(exp);
}


void RegDUMgr::setRegSSAInfo(IR * ir, RegSSAInfo * regssainfo)
{
    ASSERT0(ir && regssainfo && RegSSAMgr::hasRegSSAInfo(ir));
    ASSERT0(m_irid2regssainfo.get(ir->id()) == nullptr);
    m_irid2regssainfo.set(ir->id(), regssainfo);
}


void RegDUMgr::cleanAllRegSSAInfo()
{
    m_irid2regssainfo.clean();
    destroyAllRegSSAInfo();
}


void RegDUMgr::cleanRegSSAInfo(IR * ir)
{
    ASSERT0(ir);
    m_irid2regssainfo.set(ir->id(), nullptr);
}



RegSSAInfo * RegDUMgr::genRegSSAInfo(MOD IR * ir)
{
    ASSERT0(ir && RegSSAMgr::hasRegSSAInfo(ir));
    RegSSAInfo * regssainfo = getRegSSAInfo(ir);
    if (regssainfo == nullptr) {
        regssainfo = allocRegSSAInfo();
        setRegSSAInfo(ir, regssainfo);
    }
    return regssainfo;
}


RegSSAInfo * RegDUMgr::getRegSSAInfo(IR const* ir) const
{
    ASSERT0(ir && RegSSAMgr::hasRegSSAInfo(ir));
    return m_irid2regssainfo.get(ir->id());
}


RegSSAInfo * RegDUMgr::allocRegSSAInfo()
{
    ASSERT0(m_regssainfo_pool);
    RegSSAInfo * p = (RegSSAInfo*)smpoolMallocConstSize(
        sizeof(RegSSAInfo), m_regssainfo_pool);
    ASSERT0(p);
    ::memset((void*)p, 0, sizeof(RegSSAInfo));
    p->init();
    m_regssainfo_vec.append(p);
    return p;
}


void RegDUMgr::buildRegPhiOpnd(RegPhi * phi, Reg reg, UINT num_operands)
{
    ASSERT0(reg > REG_UNDEF && num_operands > 0);
    VReg const* vreg = allocVReg(reg, REGSSA_INIT_VERSION);
    ASSERT0(vreg);
    IR * last = nullptr;

    //Generate operand of PHI.
    for (UINT i = 0; i < num_operands; i++) {
        IR * opnd = getIRMgrExt()->buildPhyReg(reg, phi);

        //Generate RegSSAInfo to ID.
        RegSSAInfo * regssainfo = genRegSSAInfo(opnd);

        //Add VROpnd that ID indicated.
        regssainfo->addVROpnd(vreg, this);
        xcom::add_next(&REGPHI_opnd_list(phi), &last, opnd);
    }
}


//Allocate RegPhi and initialize with the number of operands.
//Each operands has zero version to reg.
RegPhi * RegDUMgr::allocRegPhi(Reg reg)
{
    ASSERT0(reg > REG_UNDEF);
    //Different from RegDef, RegPhi will be allocated in individual pool.
    RegPhi * phi = (RegPhi*)smpoolMallocConstSize(sizeof(RegPhi), m_phi_pool);
    phi->init();
    REGDEF_id(phi) = m_def_count++;
    m_def_vec.set(REGDEF_id(phi), phi);
    return phi;
}


void RegDUMgr::removeRegDef(RegDef * def)
{
    ASSERT0(def);
    //No need to destroy def's memory overtly.
    m_def_vec.set(def->id(), nullptr);
    def->clean();
}


RegDefStmt * RegDUMgr::allocRegDefStmt()
{
    RegDefStmt * def = (RegDefStmt*)smpoolMallocConstSize(
        sizeof(RegDefStmt), m_defstmt_pool);
    def->init();
    REGDEF_id(def) = m_def_count++;
    m_def_vec.set(REGDEF_id(def), def);
    return def;
}


RegDefSet * RegDUMgr::allocRegDefSet()
{
    RegDefSet * defset = (RegDefSet*)smpoolMallocConstSize(
        sizeof(RegDefSet), m_defset_pool);
    defset->init();
    return defset;
}


xcom::SC<VROpnd*> * RegDUMgr::allocSCVROpnd(VROpnd * opnd)
{
    xcom::SC<VROpnd*> * sc = xcom::removehead_single_list(&m_free_sc_list);
    if (sc != nullptr) {
        sc->init();
        return sc;
    }
    sc = (xcom::SC<VROpnd*>*)smpoolMallocConstSize(
        sizeof(xcom::SC<VROpnd*>), m_vropnd_sc_pool);
    sc->init();
    SC_val(sc) = opnd;
    return sc;
}


VRConst * RegDUMgr::allocVRConst(IR const* ir)
{
    ASSERTN(m_vrconst_pool, ("not init"));
    VRConst * p = (VRConst*)smpoolMallocConstSize(
        sizeof(VRConst), m_vrconst_pool);
    ASSERT0(p);
    ::memset((void*)p, 0, sizeof(VRConst));
    VROPND_code(p) = VROPND_CONST;
    VROPND_id(p) = m_vropnd_count++;
    VCONST_val(p) = ir;
    return p;
}


VReg * RegDUMgr::getVReg(Reg reg, UINT version) const
{
    ASSERT0(reg > REG_UNDEF);
    Vector<VReg*> * vec = const_cast<RegDUMgr*>(this)->m_map_reg2vreg.get(reg);
    if (vec == nullptr) {
        return nullptr;
    }
    return vec->get(version);
}


//The function remove and clean all information of 'vreg' from RegSSAMgr.
void RegDUMgr::removeVReg(VReg * vreg)
{
    ASSERT0(vreg);
    m_vropnd_vec.set(vreg->id(), nullptr);
    VRegVec * vec = m_map_reg2vreg.get(vreg->reg());
    ASSERT0(vec);
    vec->set(vreg->version(), nullptr);
    vreg->destroy();
}


//Allocate VReg and ensure it is unique according to 'version' and 'reg'.
VReg * RegDUMgr::allocVReg(Reg reg, UINT version)
{
    ASSERT0(reg > REG_UNDEF);
    VRegVec * vec = m_map_reg2vreg.get(reg);
    if (vec == nullptr) {
        vec = new VRegVec();
        m_map_reg2vreg.set(reg, vec);
    }

    VReg * v = vec->get(version);
    if (v != nullptr) {
        return v;
    }

    ASSERTN(m_vreg_pool, ("not init"));
    v = (VReg*)smpoolMallocConstSize(sizeof(VReg), m_vreg_pool);
    ASSERT0(v);
    ::memset((void*)v, 0, sizeof(VReg));
    v->init();
    VROPND_code(v) = VROPND_REG;
    VROPND_id(v) = m_vropnd_count++;
    VREG_reg(v) = reg;
    VREG_version(v) = version;
    VREG_def(v) = nullptr;
    vec->set(version, v);
    m_vropnd_vec.set(v->id(), v);
    return v;
}


size_t RegDUMgr::count_mem() const
{
    size_t count = smpoolGetPoolSize(m_regssainfo_pool);
    count += smpoolGetPoolSize(m_phi_pool);
    count += smpoolGetPoolSize(m_defstmt_pool);
    count += smpoolGetPoolSize(m_defset_pool);
    count += smpoolGetPoolSize(m_vrconst_pool);
    count += smpoolGetPoolSize(m_vreg_pool);
    count += smpoolGetPoolSize(m_philist_pool);
    count += smpoolGetPoolSize(m_philist_sc_pool);
    count += m_map_reg2vreg.count_mem();
    count += m_vropnd_vec.count_mem();
    count += m_def_vec.count_mem();
    count += sizeof(RegDUMgr);
    return count;
}


RegPhiList * RegDUMgr::genBBPhiList(UINT bbid)
{
    RegPhiList * lst = m_philist_vec.get(bbid);
    if (lst != nullptr) { return lst; }
    lst = (RegPhiList*)smpoolMallocConstSize(
        sizeof(RegPhiList), m_philist_pool);
    ASSERT0(lst);
    lst->init(m_philist_sc_pool);
    m_philist_vec.set(bbid, lst);
    return lst;
}
//END RegDUMgr

} //namespace xoc
