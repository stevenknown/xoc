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

static CHAR const* g_parting_line_char = "----------------";
static CHAR const* g_msg_no_mdssainfo = " NOMDSSAINFO!!";

//
//START LiveSet
//
void LiveSet::dump(MDSSAMgr const* mgr) const
{
    note(mgr->getRegion(), "\n==-- DUMP LiveSet --==");
    VOpndSetIter it;
    for (BSIdx i = m_set.get_first(&it);
         i != BS_UNDEF; i = m_set.get_next(i, &it)) {
        VMD * t = (VMD*)const_cast<MDSSAMgr*>(mgr)->getUseDefMgr()->getVOpnd(i);
        ASSERT0(t && t->is_md());
        note(mgr->getRegion(), "\n");
        t->dump(mgr->getRegion());
    }
}
//END LiveSet


//
//START VMD::UstSet
//
void VMD::UseSet::dump(Region const* rg) const
{
    if (!rg->isLogMgrInit()) { return; }
    VMD::UseSetIter it;
    note(rg, "\nVMD::UseSet:");
    for (UINT i = get_first(it); !it.end(); i = get_next(it)) {
        IR const* ir = rg->getIR(i);
        ASSERT0(ir);
        prt(rg, "<%s,id:%d> ", IRNAME(ir), i);
    }
}
//END


//
//START BB2DefMDSet
//
void BB2DefMDSet::dump(Region const* rg) const
{
    if (!rg->isLogMgrInit()) { return; }
    note(rg, "\n==-- DUMP BB2DefMDSet --==");
    BBList * bbl = rg->getBBList();
    for (IRBB const* bb = bbl->get_head();
         bb != nullptr; bb = bbl->get_next()) {
        DefMDSet * defmds = get(bb->id());
        note(rg, "\nBB%d DefinedMDSet:", bb->id());
        if (defmds == nullptr) { continue; }
        defmds->dump(rg->getLogMgr()->getFileHandler());
    }
}
//END


//
//START MD2VMDStack
//
void MD2VMDStack::dump(Region const* rg) const
{
    if (!rg->isLogMgrInit()) { return; }
    note(rg, "\n==-- DUMP MD2VMDStack --==");
    for (INT i = MD_UNDEF + 1; i <= get_last_idx(); i++) {
        xcom::Stack<VMD*> * s = get(i);
        note(rg, "\nMD%u:", i);
        if (s == nullptr) {
            continue;
        }
        for (VMD * vmd = s->get_bottom(); vmd != nullptr; vmd = s->get_next()) {
            ASSERT0(vmd->mdid() == (UINT)i);
            prt(rg, "v%u|", vmd->version());
        }
    }
}


void MD2VMDStack::destroy()
{
    for (INT i = 0; i <= get_last_idx(); i++) {
        xcom::Stack<VMD*> * s = get(i);
        if (s != nullptr) { delete s; }
    }
    Vector<Stack<VMD*>*>::destroy();
}


Stack<VMD*> * MD2VMDStack::gen(UINT mdid)
{
    Stack<VMD*> * stk = get(mdid);
    if (stk == nullptr) {
        stk = new Stack<VMD*>();
        set(mdid, stk);
    }
    return stk;
}


VMD * MD2VMDStack::get_top(UINT mdid) const
{
    Stack<VMD*> * stk = get(mdid);
    if (stk == nullptr) { return nullptr; }
    return stk->get_top();
}


void MD2VMDStack::push(UINT mdid, VMD * vmd)
{
    ASSERT0(mdid != MD_UNDEF && vmd && vmd->mdid() == mdid);
    Stack<VMD*> * stk = gen(mdid);
    ASSERT0(stk);
    stk->push(vmd);
}
//END MD2VMDStack


//
//START MDSSAMgr
//
size_t MDSSAMgr::count_mem() const
{
    size_t count = m_map_md2stack.count_mem();
    count += m_max_version.count_mem();
    count += m_usedef_mgr.count_mem();
    count += sizeof(*this);
    return count;
}


void MDSSAMgr::destroy()
{
    if (m_usedef_mgr.m_mdssainfo_pool == nullptr) { return; }

    //CAUTION: If you do not finish out-of-SSA prior to destory(),
    //the reference to IR's MDSSA info will lead to undefined behaviors.
    //ASSERTN(!m_is_valid,
    //        ("Still in ssa mode, you should do out of "
    //         "SSA before destroy"));

    freePhiList();
}


void MDSSAMgr::freeBBPhiList(IRBB * bb)
{
    MDPhiList * philist = getPhiList(bb);
    if (philist == nullptr) { return; }
    for (MDPhiListIter it = philist->get_head();
         it != philist->end(); it = philist->get_next(it)) {
        MDPhi * phi = it->val();
        ASSERT0(phi && phi->is_phi());
        m_rg->freeIRTreeList(phi->getOpndList());
        MDPHI_opnd_list(phi) = nullptr;
    }
}


void MDSSAMgr::freePhiList()
{
    for (IRBB * bb = m_rg->getBBList()->get_head();
         bb != nullptr; bb = m_rg->getBBList()->get_next()) {
        freeBBPhiList(bb);
    }
    m_usedef_mgr.m_philist_vec.destroy();
    m_usedef_mgr.m_philist_vec.init();
}


//The function destroy data structures that allocated during SSA
//construction, and these data structures are only useful in construction.
void MDSSAMgr::cleanLocalUsedData()
{
    //Do NOT clean max_version of each MD, because some passes will generate
    //DEF stmt for individual MD, which need new version of the MD.
    //MD's max verison is often used to update MDSSAInfo incrementally.
    //m_max_version.destroy();
    //m_max_version.init();
}


CHAR * MDSSAMgr::dumpVMD(IN VMD * v, OUT CHAR * buf)
{
    sprintf(buf, "MD%dV%d", v->mdid(), v->version());
    return buf;
}


//This function dumps VMD structure and SSA DU info.
void MDSSAMgr::dumpAllVMD()
{
    if (!m_rg->isLogMgrInit()) { return; }
    note(getRegion(), "\n\n==---- DUMP MDSSAMgr: ALL VMD '%s'----==",
         m_rg->getRegionName());
    VOpndVec * vec = getUseDefMgr()->getVOpndVec();
    for (INT i = 1; i <= vec->get_last_idx(); i++) {
        VMD * v = (VMD*)vec->get(i);
        if (v == nullptr) {
            //Some pass, e.g:ir_refinement, will remove ir and related VMD.
            continue;
        }
        note(getRegion(), "\nVMD%d:MD%dV%d: ",
             v->id(), v->mdid(), v->version());
        MDDef * mddef = v->getDef();
        //Print DEF.
        if (v->version() != MDSSA_INIT_VERSION) {
            //After renaming, MD must have defstmt if its version is nonzero.
            ASSERT0(mddef);
        }
        if (mddef != nullptr) {
            IR const* stmt = mddef->getOcc();
            if (stmt == nullptr) {
                ASSERT0(mddef->is_phi() && mddef->getBB());
                prt(getRegion(), "DEF:(phi,BB%d)", mddef->getBB()->id());
            } else {
                //The stmt may have been removed, and the VMD is obsoleted.
                //If the stmt removed, its UseSet should be empty.
                //ASSERT0(stmt->is_stmt() && !stmt->isWritePR());
                prt(getRegion(), "DEF:(%s,id:%d)", IRNAME(stmt), stmt->id());
            }
        } else {
            prt(getRegion(), "DEF:---");
        }

        //Print USEs.
        prt(getRegion(), "\tUSE:");
        VMD::UseSetIter it;
        INT nexti = 0;
        for (BSIdx j = v->getUseSet()->get_first(it); !it.end(); j = nexti) {
            nexti = v->getUseSet()->get_next(it);
            IR * use = m_rg->getIR(j);
            ASSERT0(use && !use->isReadPR());
            prt(getRegion(), "(%s,id:%d)", IRNAME(use), use->id());
            if (nexti >= 0) {
                prt(getRegion(), ",");
            }
        }
    }
    note(getRegion(), "\n");
}


//Before removing BB or change BB successor,
//you need remove the related PHI operand if BB successor has PHI.
void MDSSAMgr::removeSuccessorDesignatedPhiOpnd(IRBB const* succ, UINT pos)
{
    MDPhiList * philist = getPhiList(succ);
    if (philist == nullptr) { return; }
    for (MDPhiListIter it = philist->get_head();
         it != philist->end(); it = philist->get_next(it)) {
        MDPhi * phi = it->val();
        ASSERT0(phi && phi->is_phi());
        IR * opnd_head = phi->getOpndList();

        //CASE:CFG optimization may have already remove the predecessor of
        //'succ' before call the function.
        //ASSERT0(xcom::cnt_list(opnd_head) == succ->getNumOfPred(m_cfg));
        if (opnd_head == nullptr) {
            //MDPHI does not contain any operand.
            continue;
        }

        IR * opnd = nullptr;
        UINT lpos = pos;
        for (opnd = opnd_head; lpos != 0; opnd = opnd->get_next()) {
            ASSERT0(opnd);
            lpos--;
        }
        removeMDSSAOccForTree(opnd);
        phi->removeOpnd(opnd);
        m_rg->freeIRTree(opnd);
    }
}


//The function try to find the unique MDDef for given def that is outside
//of the loop.
//Return the MDDef if found, otherwise nullptr.
MDDef const* MDSSAMgr::findUniqueOutsideLoopDef(MDDef const* phi,
                                                LI<IRBB> const* li) const
{
    ASSERT0(phi->is_phi());
    UINT num_outside_def = 0;
    MDDef const* ret = nullptr;
    for (IR const* opnd = MDPHI_opnd_list(phi);
         opnd != nullptr; opnd = opnd->get_next()) {
        VMD * opndvmd = ((MDPhi*)phi)->getOpndVMD(opnd,
            const_cast<MDSSAMgr*>(this)->getUseDefMgr());
        MDDef const* def = opndvmd->getDef();
        if (def == nullptr) { continue; }
        ASSERT0(def->getBB());
        if (li->isInsideLoop(def->getBB()->id())) { continue; }
        ret = def;
        num_outside_def++;
        if (num_outside_def > 1) { return nullptr; }
    }
    return ret;
}


//Find VMD from ir list and phi list.
VMD * MDSSAMgr::findLastMayDef(IRBB const* bb, MDIdx mdid) const
{
    return findLastMayDefFrom(bb, BB_last_ir(const_cast<IRBB*>(bb)), mdid);
}


VMD * MDSSAMgr::findVMDFromPhiList(IRBB const* bb, MDIdx mdid) const
{
    MDPhiList * philist = getPhiList(bb);
    if (philist == nullptr) { return nullptr; }
    for (MDPhiListIter pit = philist->get_head();
         pit != philist->end(); pit = philist->get_next(pit)) {
        MDPhi * phi = pit->val();
        ASSERT0(phi && phi->is_phi());
        VMD * res = phi->getResult();
        ASSERT0(res);
        if (res->mdid() == mdid) { return res; }
    }
    return nullptr;
}


//Find VMD from ir list and phi list.
//start: the start position, if it is NULL, the function will scan the phi list.
VMD * MDSSAMgr::findLastMayDefFrom(IRBB const* bb, IR const* start,
                                   MDIdx mdid) const
{
    if (start != nullptr) {
        BBIRListIter it = nullptr;
        BBIRList const& irlist = const_cast<IRBB*>(bb)->getIRList();
        irlist.find(const_cast<IR*>(start), &it);
        ASSERTN(it, ("IR%d is not belong to BB%d", start->id(), bb->id()));
        for (; it != nullptr; it = irlist.get_prev(it)) {
            IR const* ir = it->val();
            VMD * vmd;
            if (MDSSAMgr::hasMDSSAInfo(ir) &&
                (vmd = findMayRef(ir, mdid)) != nullptr) {
                return vmd;
            }
        }
    }
    return findVMDFromPhiList(bb, mdid);
}


//The function do searching that begin at the IDom BB of marker.
//Note DOM info must be available.
VMD * MDSSAMgr::findDomLiveInDefFromIDomOf(IRBB const* marker, MDIdx mdid) const
{
    UINT idom = ((DGraph*)m_cfg)->get_idom(marker->id());
    ASSERTN(idom != VERTEX_UNDEF, ("no idom"));
    ASSERT0(m_cfg->isVertex(idom));
    IRBB * bb = m_cfg->getBB(idom);
    ASSERT0(bb);
    return findDomLiveInDefFrom(mdid, bb->getLastIR(), bb);
}


//Find livein def-stmt through given start IR at start BB.
//Note DOM info must be available.
//startir: the start position in 'startbb', it can be NULL.
//         If it is NULL, the function first finding the Phi list of
//         'startbb', then keep finding its predecessors until the
//         CFG entry.
//startbb: the BB that begin to do searching.
VMD * MDSSAMgr::findDomLiveInDefFrom(MDIdx mdid, IR const* startir,
                                     IRBB const* startbb) const
{
    ASSERT0(startbb);
    //NOTE startir may be have already removed from startbb.
    //ASSERT0(startir == nullptr ||
    //        const_cast<IRBB*>(startbb)->getIRList()->find(
    //        const_cast<IR*>(startir)));
    IRBB const* meetup = m_cfg->getEntry();
    ASSERT0(meetup);
    VMDVec const* vmdvec = m_usedef_mgr.getVMDVec(mdid);
    xcom::List<IRBB const*> wl;
    wl.append_tail(startbb);
    xcom::TTab<UINT> visited;
    while (wl.get_elem_count() != 0) {
        IRBB const* t = wl.remove_head();
        if (t == startbb) {
            if (startir != nullptr) {
                VMD * livein = findLastMayDefFrom(t, startir, mdid);
                if (livein != nullptr) {
                    return livein;
                }
            } else {
                VMD * livein = findVMDFromPhiList(t, mdid);
                if (livein != nullptr) {
                    return livein;
                }
            }
            //Keep finding through predecessors.
        } else if (vmdvec != nullptr && vmdvec->hasDefInBB(t->id())) {
            VMD * livein = findLastMayDef(t, mdid);
            ASSERT0(livein);
            return livein;
        } else {
            //Keep finding through predecessors.
        }

        visited.append(t->id());
        if (t == meetup) { continue; }

        UINT dom = ((DGraph*)m_cfg)->get_idom(t->id());
        if (dom == VERTEX_UNDEF) { continue; }

        ASSERTN(m_cfg->isVertex(dom), ("miss DomInfo"));
        if (!visited.find(dom)) {
            ASSERT0(m_cfg->getBB(dom));
            wl.append_tail(m_cfg->getBB(dom));
        }
    }
    return nullptr;
}


//After adding BB or change BB successor,
//you need add the related PHI operand if BB successor has PHI stmt.
void MDSSAMgr::addSuccessorDesignatedPhiOpnd(IRBB * bb, IRBB * succ)
{
    MDPhiList * philist = getPhiList(succ);
    if (philist == nullptr) { return; }

    UINT const pos = m_cfg->WhichPred(bb, succ);
    for (MDPhiListIter it = philist->get_head();
         it != philist->end(); it = philist->get_next(it)) {
        MDPhi * phi = it->val();
        ASSERT0(phi && phi->is_phi());
        phi->insertOpndAt(this, pos, bb);
        ASSERT0(xcom::cnt_list(phi->getOpndList()) ==
                succ->getNumOfPred(m_cfg));
    }
}


void MDSSAMgr::dumpPhiList(MDPhiList const* philist) const
{
    if (philist == nullptr) { return; }
    for (MDPhiListIter it = philist->get_head();
         it != philist->end(); it = philist->get_next(it)) {
        MDPhi const* phi = it->val();
        ASSERT0(phi && phi->is_phi());
        note(getRegion(), "\n");
        phi->dump(m_rg, &m_usedef_mgr);
    }
}


void MDSSAMgr::dumpIRWithMDSSAForStmt(IR const* ir, UINT flag,
                                      bool & parting_line) const
{
    if (!ir->is_stmt() || (!ir->isMemoryRefNonPR() && !ir->isCallStmt())) {
        return;
    }
    VOpndSetIter iter = nullptr;
    if (!parting_line) {
        note(getRegion(), "\n----");
        parting_line = true;
    }
    dumpIR(ir, m_rg, nullptr, IR_DUMP_DEF);

    MDSSAInfo * mdssainfo = getMDSSAInfoIfAny(ir);
    if (mdssainfo == nullptr) {
        prt(getRegion(), "%s", g_msg_no_mdssainfo);
    } else {
        for (BSIdx i = mdssainfo->getVOpndSet()->get_first(&iter);
            i != BS_UNDEF; i = mdssainfo->getVOpndSet()->get_next(i, &iter)) {
            note(getRegion(), "\n--DEF:");
            VMD * vopnd = (VMD*)m_usedef_mgr.getVOpnd(i);
            ASSERT0(vopnd && vopnd->is_md());
            if (vopnd->getDef() != nullptr) {
                ASSERT0(vopnd->getDef()->getOcc() == ir);
            }
            vopnd->dump(m_rg, &m_usedef_mgr);
        }
    }
}


void MDSSAMgr::dumpIRWithMDSSAForExp(IR const* ir, UINT flag,
                                     bool & parting_line) const
{
    List<IR const*> lst;
    List<IR const*> opnd_lst;
    for (IR const* opnd = iterInitC(ir, lst);
         opnd != nullptr; opnd = iterNextC(lst)) {
        if (!opnd->isMemoryRefNonPR() || opnd->is_stmt()) {
            continue;
        }
        VOpndSetIter iter = nullptr;
        if (!parting_line) {
            note(getRegion(), "\n----");
            parting_line = true;
        }

        dumpIR(opnd, m_rg, nullptr, IR_DUMP_DEF);

        note(getRegion(), "\n--USE:");
        bool first = true;
        MDSSAInfo * mdssainfo = getMDSSAInfoIfAny(opnd);
        if (mdssainfo == nullptr) {
            prt(getRegion(), "%s", g_msg_no_mdssainfo);
            continue;
        }

        for (BSIdx i = mdssainfo->getVOpndSet()->get_first(&iter);
             i != BS_UNDEF; i = mdssainfo->getVOpndSet()->get_next(i, &iter)) {
            VMD * vopnd = (VMD*)m_usedef_mgr.getVOpnd(i);
            ASSERT0(vopnd && vopnd->is_md());
            if (first) {
                first = false;
            } else {
                prt(getRegion(), ",");
            }
            prt(getRegion(), "MD%dV%d", vopnd->mdid(), vopnd->version());
        }
    }
}


//Dump IR tree and MDSSAInfo if any.
//ir: can be stmt or expression.
//flag: the flag to dump IR.
void MDSSAMgr::dumpIRWithMDSSA(IR const* ir, UINT flag) const
{
    if (!m_rg->isLogMgrInit()) { return; }
    ASSERT0(ir);
    dumpIR(ir, m_rg, nullptr, flag);

    bool parting_line = false;
    m_rg->getLogMgr()->incIndent(2);
    //ir->dumpRef(m_rg, 0); //Dump REF may make dumpinfo in a mess.
    dumpIRWithMDSSAForStmt(ir, flag, parting_line);
    dumpIRWithMDSSAForExp(ir, flag, parting_line);
    m_rg->getLogMgr()->decIndent(2);
}


void MDSSAMgr::dumpVOpndRef() const
{
    Region * rg = getRegion();
    if (!rg->isLogMgrInit()) { return; }
    note(rg, "\n==-- DUMP MDSSAMgr VOpndRef '%s' --==\n", rg->getRegionName());
    BBList * bbl = m_rg->getBBList();
    for (IRBB * bb = bbl->get_head(); bb != nullptr; bb = bbl->get_next()) {
        note(rg, "\n--- BB%d ---", bb->id());
        dumpPhiList(getPhiList(bb));
        for (IR * ir = BB_first_ir(bb); ir != nullptr; ir = BB_next_ir(bb)) {
            note(rg, "\n");
            dumpIRWithMDSSA(ir);
        }
    }
}


bool MDSSAMgr::dump() const
{
    if (!m_rg->isLogMgrInit()) { return false; }
    START_TIMER(t, "MDSSA: Dump After Pass");
    note(getRegion(), "\n==---- DUMP %s '%s' ----==",
         getPassName(), m_rg->getRegionName());
    getRegion()->getLogMgr()->incIndent(2);
    m_md_sys->dump(true);
    dumpVOpndRef();
    dumpDUChain();
    getRegion()->getLogMgr()->decIndent(2);
    END_TIMER(t, "MDSSA: Dump After Pass");
    return true;
}


//Find the VOpnd if 'ir' must OR may referenced 'md'.
//Return the VMD if found.
VMD * MDSSAMgr::findMayRef(IR const* ir, MDIdx mdid) const
{
    ASSERT0(ir->isMemoryRef());
    ASSERT0(ir->is_stmt() || ir->is_exp());
    MDSSAInfo const* mdssainfo = getMDSSAInfoIfAny(ir);
    ASSERTN(mdssainfo, ("miss MDSSAInfo"));
    VOpndSetIter iter = nullptr;
    VOpndSet const& vset = mdssainfo->readVOpndSet();
    for (BSIdx i = vset.get_first(&iter);
         i != BS_UNDEF; i = vset.get_next(i, &iter)) {
        VMD * t = (VMD*)m_usedef_mgr.getVOpnd(i);
        ASSERT0(t && t->is_md());
        if (t->mdid() == mdid) { return t; }
    }
    return nullptr;
}


//Find the MustDef of 'ir'.
MDDef * MDSSAMgr::findMustDef(IR const* ir) const
{
    ASSERT0(ir && ir->is_exp() && ir->isMemoryOpnd());
    MD const* mustuse = ir->getMustRef();
    if (mustuse == nullptr || (!mustuse->is_exact() && !mustuse->is_range())) {
        return nullptr;
    }
    MDSSAInfo const* mdssainfo = getMDSSAInfoIfAny(ir);
    ASSERTN(mdssainfo, ("miss MDSSAInfo"));
    VOpndSetIter iter = nullptr;
    for (BSIdx i = mdssainfo->readVOpndSet().get_first(&iter);
         i != BS_UNDEF; i = mdssainfo->readVOpndSet().get_next(i, &iter)) {
        VMD * t = (VMD*)m_usedef_mgr.getVOpnd(i);
        ASSERT0(t && t->is_md());
        MDDef * tdef = t->getDef();
        if (tdef != nullptr && tdef->getResultMD(m_md_sys) == mustuse) {
            return tdef;
        }
    }
    return nullptr;
}


//Find nearest virtual DEF in VOpndSet of 'ir'.
MDDef * MDSSAMgr::findNearestDef(IR const* ir) const
{
    ASSERT0(ir && ir->is_exp() && ir->isMemoryOpnd());
    MDSSAInfo const* mdssainfo = getMDSSAInfoIfAny(ir);
    ASSERTN(mdssainfo, ("miss MDSSAInfo"));
    VOpndSetIter iter = nullptr;
    INT lastrpo = -1;
    MDDef * last = nullptr;
    for (BSIdx i = mdssainfo->readVOpndSet().get_first(&iter);
         i != BS_UNDEF; i = mdssainfo->readVOpndSet().get_next(i, &iter)) {
        VMD * t = (VMD*)m_usedef_mgr.getVOpnd(i);
        ASSERT0(t && t->is_md());
        MDDef * tdef = t->getDef();
        if (last == nullptr) {
            if (tdef != nullptr) {
                last = tdef;
                ASSERT0(tdef->getBB());
                lastrpo = BB_rpo(last->getBB());
                ASSERT0(lastrpo >= 0);
            } else {
                ASSERT0(t->isLiveIn());
                //Regard the virtual def at the entry of region,
                //it is the farmost def.
            }
            continue;
        }

        if (tdef == nullptr) { continue; }

        ASSERT0(tdef->getResult() && tdef->getResult()->is_md());

        IRBB * tbb = tdef->getBB();
        ASSERT0(tbb);
        ASSERT0(BB_rpo(tbb) >= 0);
        if (BB_rpo(tbb) > lastrpo) {
            //tdef is near more than 'last', then update 'last'.
            last = tdef;
            //Update nearest BB's rpo.
            lastrpo = BB_rpo(tbb);
            continue;
        }
        if (tbb != last->getBB()) {
            //last is near more than tdef, so nothing to do.
            continue;
        }

        //tdef' and 'last' are placed in same BB.
        if (tdef->is_phi()) {
            ; //last is near more than tdef, so nothing to do.
            if (tdef->getResult()->mdid() == last->getResult()->mdid()) {
                ASSERT0(tdef == last || !last->is_phi());
            }
            continue;
        }
        if (last->is_phi()) {
            if (tdef->getResult()->mdid() == last->getResult()->mdid()) {
                ASSERT0(tdef == last || !tdef->is_phi());
            }

            //tdef is near more than 'last', then update 'last'.
            last = tdef;
            ASSERTN(lastrpo == BB_rpo(tbb), ("lastrpo should be updated"));
            continue;
        }
        if (tbb->is_dom(last->getOcc(), tdef->getOcc(), true)) {
            //tdef is near more than 'last', then update 'last'.
            last = tdef;
        }
    }
    return last;
}


//Find killing must-def IR stmt for expression ir.
//Return the IR stmt if found.
//e.g: g is global variable, it is exact.
//x is a pointer that we do not know where it pointed to.
//    1. *x += 1; # *x may overlapped with g
//    2. g = 0; # exactly defined g
//    3. call foo(); # foo may overlapped with g
//    4. return g;
//In the case, the last reference of g in stmt 4 may be defined by
//stmt 1, 2, 3, there is no nearest killing def.

IR * MDSSAMgr::findKillingDefStmt(IR const* ir) const
{
    MDDef * mddef = findKillingMDDef(ir);
    if (mddef != nullptr && !mddef->is_phi()) {
        ASSERT0(mddef->getOcc());
        return mddef->getOcc();
    }
    return nullptr;
}


//Find killing must-def Virtual-DEF for expression ir.
//Return the MDDef if found.
//e.g: g is global variable, it is exact.
//x is a pointer that we do not know where it pointed to.
//    1. *x += 1; # *x may overlapped with g
//    2. g = 0; # exactly defined g
//    3. call foo(); # foo may overlapped with g
//    4. return g;
//In the case, the last reference of g in stmt 4 may be defined by
//stmt 1, 2, 3, there is no nearest killing def.
MDDef * MDSSAMgr::findKillingMDDef(IR const* ir) const
{
    ASSERT0(ir && ir->is_exp() && ir->isMemoryOpnd());
    MD const* opndmd = ir->getMustRef();
    if (opndmd == nullptr || (!opndmd->is_exact() && !opndmd->is_range())) {
        //TBD: For those exp who do not have MustRef, must they not
        //have killing-def?
        return nullptr;
    }

    MDDef * def = findMustDef(ir);
    //MDDef * def = findNearestDef(ir);
    if (def == nullptr || def->is_phi()) { return nullptr; }

    ASSERT0(def->getOcc());
    MD const* defmd = def->getOcc()->getRefMD();
    if (defmd != nullptr && isKillingDef(defmd, opndmd)) { return def; }
    return nullptr;
}


static void dumpDef(MDDef const* def, MD const* vopndmd, UseDefMgr const* mgr,
                    Region * rg, xcom::BitSet & visited_def,
                    MOD List<MDDef const*> & wl,
                    MOD IRSet & visited, MOD bool & has_dump_something)
{
    if (def->is_phi()) {
        if (has_dump_something) {
            prt(rg, " ");
        }
        prt(rg, "mdphi%d", def->id());
        has_dump_something = true;

        //Collect opnd of PHI to go forward to
        //retrieve corresponding DEFs.
        for (IR const* opnd = MDPHI_opnd_list(def);
             opnd != nullptr; opnd = opnd->get_next()) {
            if (opnd->is_const()) {
                //CONST does not have VMD info.
                continue;
            }

            VMD * opndvmd = ((MDPhi*)def)->getOpndVMD(opnd, mgr);

            //CASE:Do NOT assert VOpnd here.
            //  sometime VOpnd of ID will be NULL before reconstruction.
            //  st x_1 = ...     st x_2 = ...
            //      \           /
            //      x_3 = phi(x_1, x_2)
            //  If some pass removed 'st x_1', the PHI will be
            //       x_3 = phi(--, x_2)
            //  where the first operand is missed, and the illegal PHI will
            //  be recomputed until MDSSA reconstructed.
            //  The situation will be checked during verifyPhi().
            //  Thus just omit the operand of PHI if it is NULL.
            //ASSERT0(opndvmd);

            if (opndvmd != nullptr && opndvmd->getDef() != nullptr &&
                !visited_def.is_contain(opndvmd->getDef()->id())) {
                //Keep walking previous DEF.
                wl.append_tail(opndvmd->getDef());
            }
        }
        return;
    }

    //def is normal IR stmt.
    ASSERT0(def->getOcc());
    if (!visited.find(def->getOcc())) {
        visited.append(def->getOcc());
        if (has_dump_something) {
            prt(rg, " ");
        }
        prt(rg, "(%s id:%d)",
            IRNAME(def->getOcc()), def->getOcc()->id());
        has_dump_something = true;
    }

    MD const* defmd = def->getOcc()->getRefMD();
    if (defmd != nullptr && defmd->is_exact() && vopndmd->is_exact() &&
        (defmd == vopndmd || defmd->is_exact_cover(vopndmd))) {
        //Stop iteration. def is killing may-def.
        return;
    }
    if (def->getPrev() != nullptr &&
        !visited_def.is_contain(def->getPrev()->id())) {
        //Keep walking previous DEF.
        wl.append_tail(def->getPrev());
    }
}


//The function dump all possible DEF of 'vopnd' by walking through the
//Def Chain.
void MDSSAMgr::dumpDefByWalkDefChain(List<MDDef const*> & wl, IRSet & visited,
                                     VMD const* vopnd) const
{
    if (vopnd->getDef() == nullptr) { return; }
    MD const* vopndmd = m_rg->getMDSystem()->getMD(vopnd->mdid());
    ASSERT0(vopndmd);
    wl.clean();
    wl.append_tail(vopnd->getDef());
    xcom::BitSet visited_def;
    bool has_dump_something = false;
    for (MDDef const* def = wl.remove_head();
         def != nullptr; def = wl.remove_head()) {
        visited_def.bunion(def->id());
        ASSERT0(def->getResult()->mdid() == vopnd->mdid());
        dumpDef(def, vopndmd, const_cast<MDSSAMgr*>(this)->getUseDefMgr(),
                m_rg, visited_def, wl, visited, has_dump_something);
    }
}


//Return true if exist USE to 'ir'.
//This is a helper function to provid simple query, an example to
//show how to retrieval VOpnd and USE occurences as well.
//ir: stmt
bool MDSSAMgr::hasUse(IR const* ir) const
{
    ASSERT0(ir && ir->is_stmt());
    MDSSAInfo const* info = const_cast<MDSSAMgr*>(this)->getMDSSAInfoIfAny(ir);
    if (info == nullptr) { return false; }

    VOpndSetIter iter = nullptr;
    for (BSIdx i = info->readVOpndSet().get_first(&iter);
         i != BS_UNDEF; i = info->readVOpndSet().get_next(i, &iter)) {
        VOpnd const* vopnd = getVOpnd(i);
        ASSERT0(vopnd && vopnd->is_md());
        if (((VMD*)vopnd)->hasUse()) {
            return true;
        }
    }
    return false;
}


//lst: for local used.
void MDSSAMgr::dumpExpDUChainIter(IR const* ir, List<IR*> & lst,
                                  List<IR*> & opnd_lst,
                                  OUT bool * parting_line) const
{
    IRSet visited(getSBSMgr()->getSegMgr());
    xcom::List<MDDef const*> wl;
    lst.clean();
    opnd_lst.clean();
    for (IR const* opnd = xoc::iterInit(const_cast<IR*>(ir), lst);
         opnd != nullptr; opnd = xoc::iterNext(lst)) {
        if (!opnd->isMemoryRefNonPR() || opnd->is_stmt()) {
            continue;
        }

        VOpndSetIter iter = nullptr;
        if (!(*parting_line)) {
            note(getRegion(), "\n%s", g_parting_line_char);
            (*parting_line) = true;
        }
        note(getRegion(), "\n");
        prt(getRegion(), "%s(id:%d)", IRNAME(opnd), opnd->id());

        MDSSAInfo * mdssainfo = getMDSSAInfoIfAny(opnd);
        if (mdssainfo == nullptr) {
            prt(getRegion(), " MISS MDSSAInfo");
            continue;
        }
        MDDef * kdef = findKillingMDDef(opnd);
        if (kdef != nullptr) {
            prt(getRegion(), " KDEF:%s(id:%d)", IRNAME(kdef->getOcc()),
                kdef->getOcc()->id());
        }

        //Not found killing def, thus dump total define-chain.
        //Define-chain represents the may-def list.
        prt(getRegion(), " DEFSET:");
        visited.clean();
        m_rg->getLogMgr()->incIndent(2);
        for (BSIdx i = mdssainfo->getVOpndSet()->get_first(&iter);
             i != BS_UNDEF; i = mdssainfo->getVOpndSet()->get_next(i, &iter)) {
            VMD * vopnd = (VMD*)m_usedef_mgr.getVOpnd(i);
            ASSERT0(vopnd && vopnd->is_md());
            note(getRegion(), "\nMD%uV%u:", vopnd->mdid(), vopnd->version());
            dumpDefByWalkDefChain(wl, visited, vopnd);
        }
        m_rg->getLogMgr()->decIndent(2);
    }
}


//Dump all USE.
static void dumpUseSet(VMD const* vmd, Region * rg)
{
    ASSERT0(vmd);
    VMD::UseSetIter vit;
    for (BSIdx i = const_cast<VMD*>(vmd)->getUseSet()->get_first(vit);
         !vit.end(); i = const_cast<VMD*>(vmd)->getUseSet()->get_next(vit)) {
        IR const* use = rg->getIR(i);
        ASSERT0(use && (use->isMemoryRef() || use->is_id()));
        prt(rg, "(%s id:%d) ", IRNAME(use), use->id());
        //prt(rg, "(%s id:%d MD%uV%u) ", IRNAME(use), use->id(),
        //    vmd->mdid(), vmd->version());
    }
}


void MDSSAMgr::dumpDUChainForStmt(IR const* ir, bool & parting_line) const
{
    ASSERT0(ir->isMemoryRefNonPR() || ir->isCallStmt());
    VOpndSetIter iter = nullptr;
    Region * rg = getRegion();
    if (!parting_line) {
        note(rg, "\n%s", g_parting_line_char);
        parting_line = true;
    }
    
    note(rg, "\n");
    prt(rg, "%s(id:%d)", IRNAME(ir), ir->id());

    MDSSAMgr * pmgr = const_cast<MDSSAMgr*>(this);
    MDSSAInfo * mdssainfo = pmgr->getMDSSAInfoIfAny(ir);
    if (mdssainfo == nullptr) {
        prt(rg, " MISS MDSSAInfo");
        return;
    }

    prt(rg, " USESET:");
    //Dump VOpnd and the USE List for each VOpnd.
    rg->getLogMgr()->incIndent(2);
    for (BSIdx i = mdssainfo->getVOpndSet()->get_first(&iter);
         i != BS_UNDEF; i = mdssainfo->getVOpndSet()->get_next(i, &iter)) {
        VMD * vopnd = (VMD*)pmgr->getUseDefMgr()->getVOpnd(i);
        ASSERT0(vopnd && vopnd->is_md());
        if (vopnd->getDef() != nullptr) {
            ASSERT0(vopnd->getDef()->getOcc() == ir);
        }
        note(rg, "\nMD%uV%u:", vopnd->mdid(), vopnd->version());
        //Dump all USE.
        dumpUseSet(vopnd, rg);
    }
    rg->getLogMgr()->decIndent(2);
}


void MDSSAMgr::dumpDUChainForStmt(IR const* ir,
                                  xcom::List<IR*> & lst,
                                  xcom::List<IR*> & opnd_lst) const
{
    ASSERT0(ir->is_stmt());
    dumpIR(ir, getRegion());
    getRegion()->getLogMgr()->incIndent(2);
    bool parting_line = false;
    //Handle stmt.
    if (ir->isMemoryRefNonPR() || ir->isCallStmt()) {
        dumpDUChainForStmt(ir, parting_line);
    }
    //Handle expression.
    dumpExpDUChainIter(ir, lst, opnd_lst, &parting_line);
    if (parting_line) {
        note(getRegion(), "\n%s", g_parting_line_char);
        note(getRegion(), "\n");
    }
    getRegion()->getLogMgr()->decIndent(2);
}


void MDSSAMgr::dumpDUChain() const
{
    Region * rg = getRegion();
    if (!rg->isLogMgrInit()) { return; }
    note(rg, "\n==-- DUMP MDSSAMgr DU CHAIN '%s' --==\n", rg->getRegionName());
    BBList * bbl = rg->getBBList();
    xcom::List<IR*> lst;
    xcom::List<IR*> opnd_lst;
    for (IRBB * bb = bbl->get_head(); bb != nullptr; bb = bbl->get_next()) {
        note(rg, "\n--- BB%d ---", bb->id());
        bb->dumpLabelList(m_rg);
        bb->dumpAttr(m_rg);
        dumpPhiList(getPhiList(bb));
        for (IR * ir = BB_first_ir(bb); ir != nullptr; ir = BB_next_ir(bb)) {
            dumpDUChainForStmt(ir, lst, opnd_lst);
        }
    }
}


//Generate MDSSAInfo and generate VOpnd for referrenced MD that both include
//must-ref MD and may-ref MDs.
MDSSAInfo * MDSSAMgr::genMDSSAInfoAndNewVesionVMD(IR * ir)
{
    ASSERT0(ir);
    //Note ir already has VOpndSet, keep it unchanged, then generate new one.
    MDSSAInfo * mdssainfo = genMDSSAInfo(ir);
    mdssainfo->cleanVOpndSet(getUseDefMgr());

    //Generate new VMD according to MD reference.
    MD const* ref = ir->getRefMD();
    MDIdx mustmd = MD_UNDEF;
    if (ref != nullptr &&
        !ref->is_pr()) { //ir may be Call stmt, its result is PR.
        mustmd = MD_id(ref);
        VMD * vmd = genNewVersionVMD(MD_id(ref));
        ASSERT0(getSBSMgr());
        ASSERT0(vmd->getDef() == nullptr);
        VMD_def(vmd) = genMDDef(ir, vmd);
        mdssainfo->addVOpnd(vmd, getUseDefMgr());
    }

    MDSet const* refset = ir->getRefMDSet();
    if (refset != nullptr) {
        MDSetIter iter;
        for (BSIdx i = refset->get_first(&iter);
             i != BS_UNDEF; i = refset->get_next((UINT)i, &iter)) {
            if ((MDIdx)i == mustmd) { continue; }
            MD * md = m_md_sys->getMD(i);
            ASSERTN(md && !md->is_pr(), ("PR should not in MaySet"));
            VMD * vmd2 = genNewVersionVMD(MD_id(md));
            ASSERT0(getSBSMgr());
            VMD_def(vmd2) = genMDDef(ir, vmd2);
            mdssainfo->addVOpnd(vmd2, getUseDefMgr());
        }
    }
    return mdssainfo;
}


//Generate MDSSAInfo and generate VOpnd for referrenced MD that both include
//must-ref MD and may-ref MDs.
MDSSAInfo * MDSSAMgr::genMDSSAInfoAndVOpnd(IR * ir, UINT version)
{
    ASSERT0(ir);
    MDSSAInfo * mdssainfo = genMDSSAInfo(ir);
    MD const* ref = ir->getRefMD();
    if (ref != nullptr &&
        !ref->is_pr()) { //ir may be Call stmt, its result is PR.
        VMD const* vmd = genVMD(MD_id(ref), version);
        ASSERT0(getSBSMgr());
        mdssainfo->addVOpnd(vmd, getUseDefMgr());
    }

    MDSet const* refset = ir->getRefMDSet();
    if (refset != nullptr) {
        MDSetIter iter;
        for (BSIdx i = refset->get_first(&iter);
             i != BS_UNDEF; i = refset->get_next((UINT)i, &iter)) {
            MD * md = m_md_sys->getMD(i);
            ASSERTN(md && !md->is_pr(), ("PR should not in MaySet"));
            VMD const* vmd2 = genVMD(MD_id(md), version);
            ASSERT0(getSBSMgr());
            mdssainfo->addVOpnd(vmd2, getUseDefMgr());
        }
    }
    return mdssainfo;
}


//Return true if all vopnds of 'def' can reach 'exp'.
bool MDSSAMgr::isMustDef(IR const* def, IR const* exp) const
{
    MDSSAInfo const* mdssainfo = getMDSSAInfoIfAny(def);
    return mdssainfo != nullptr && mdssainfo->isMustDef(
        const_cast<MDSSAMgr*>(this)->getUseDefMgr(), exp);
}


//Return true if def1 dominates def2.
bool MDSSAMgr::isDom(MDDef const* def1, MDDef const* def2) const
{
    ASSERT0(def1 != def2);
    IRBB const* bb1 = def1->getBB();
    IRBB const* bb2 = def2->getBB();
    if (bb1 != bb2) { return m_cfg->is_dom(bb1->id(), bb2->id()); }
    if (def1->is_phi()) {
        if (def2->is_phi()) {
            //PHIs that are in same BB do not dominate each other.
            return false;
        }
        return true;
    }
    if (def2->is_phi()) { return false; }
    ASSERT0(def1->getOcc() && def2->getOcc());
    return bb1->is_dom(def1->getOcc(), def2->getOcc(), true);
}


//Generate VMD for stmt and its kid expressions that reference memory.
void MDSSAMgr::initVMD(IN IR * ir, OUT DefMDSet & maydef)
{
    m_iter.clean();
    ASSERT0(ir->is_stmt());
    if (ir->isMemoryRefNonPR() ||
        (ir->isCallStmt() && !ir->isReadOnly())) {
        MD const* ref = ir->getRefMD();
        if (ref != nullptr &&
           !ref->is_pr()) { //MustRef of CallStmt may be PR.
            maydef.bunion(MD_id(ref));
        }
        MDSet const* refset = ir->getRefMDSet();
        if (refset != nullptr) {
            maydef.bunion((DefSBitSet&)*refset);
        }
        genMDSSAInfoAndVOpnd(ir, MDSSA_INIT_VERSION);
    }

    for (IR * t = xoc::iterExpInit(ir, m_iter);
         t != nullptr; t = xoc::iterExpNext(m_iter)) {
        ASSERT0(t->is_exp());
        if (t->isMemoryRefNonPR()) {
            genMDSSAInfoAndVOpnd(t, MDSSA_INIT_VERSION);
        }
    }
}


void MDSSAMgr::collectUseMD(IR const* ir, OUT LiveInMDTab & livein_md)
{
    ASSERT0(ir);
    MD const* ref = ir->getRefMD();
    if (ref != nullptr &&
        !ref->is_pr()) { //ir may be Call stmt, its result is PR.
        livein_md.append(ref->id());
    }

    MDSet const* refset = ir->getRefMDSet();
    if (refset == nullptr) { return; }

    MDSetIter iter;
    for (BSIdx i = refset->get_first(&iter);
         i != BS_UNDEF; i = refset->get_next((UINT)i, &iter)) {
        MD * md = m_md_sys->getMD(i);
        ASSERTN(md && !md->is_pr(), ("PR should not in MayBeSet"));
        livein_md.append(md->id());
    }
}


void MDSSAMgr::computeLiveInMD(IRBB const* bb, OUT LiveInMDTab & livein_md)
{
    livein_md.clean();
    ConstIRIter irit;
    IRBB * pbb = const_cast<IRBB*>(bb);
    for (IR const* ir = BB_last_ir(pbb); ir != nullptr; ir = BB_prev_ir(pbb)) {
        irit.clean();
        ASSERT0(ir->is_stmt());
        for (IR const* t = iterExpInitC(ir, irit);
             t != nullptr; t = iterExpNextC(irit)) {
            ASSERT0(t->is_exp());
            if (t->isMemoryRefNonPR()) {
                collectUseMD(t, livein_md);
            }
        }

        if (!ir->isMemoryRefNonPR()) { continue; }

        MD const* ref = ir->getRefMD();
        if (ref == nullptr || !ref->is_exact()) { continue; }

        ASSERT0(!ref->is_pr());
        LiveInMDTabIter it;
        for (UINT mdid = livein_md.get_first(it);
             mdid != MD_UNDEF; mdid = livein_md.get_next(it)) {
            MD const* md = m_md_sys->getMD(mdid);
            ASSERT0(md);
            if (ref->is_exact_cover(md)) {
                //ir kills the value of md.
                livein_md.remove(ref->id());
            }
        }
    }
}


//maydef_md: record MDs that defined in 'bb'.
void MDSSAMgr::collectDefinedMDAndInitVMD(IN IRBB * bb,
                                          OUT DefMDSet & maydef)
{
    for (IR * ir = BB_first_ir(bb); ir != nullptr; ir = BB_next_ir(bb)) {
        initVMD(ir, maydef);
    }
}


void MDSSAMgr::insertPhi(UINT mdid, IN IRBB * bb)
{
    UINT num_opnd = m_cfg->getVertex(bb->id())->getInDegree();

    //Here each operand and result of phi set to same type.
    //They will be revised to correct type during renaming.
    MDPhi * phi = genMDPhi(mdid, num_opnd, bb, genInitVersionVMD(mdid));
    m_usedef_mgr.genBBPhiList(bb->id())->append_head(phi);
}


//Insert phi for VMD.
//defbbs: record BBs which defined the VMD identified by 'mdid'.
//visited: record visited BB id
void MDSSAMgr::placePhiForMD(UINT mdid, List<IRBB*> const* defbbs,
                             DfMgr const& dfm, xcom::BitSet & visited,
                             List<IRBB*> & wl,
                             BB2DefMDSet & defmds_vec)
{
    ASSERT0(defbbs && mdid != MD_UNDEF);
    visited.clean();
    wl.clean();
    C<IRBB*> * bbit;
    for (IRBB * defbb = defbbs->get_head(&bbit);
         defbb != nullptr; defbb = defbbs->get_next(&bbit)) {
        wl.append_tail(defbb);
        //visited.bunion(defbb->id());
    }

    while (wl.get_elem_count() != 0) {
        IRBB * bb = wl.remove_head();

        //Each basic block in dfcs is in dominance frontier of 'bb'.
        xcom::BitSet const* dfcs = dfm.getDFControlSet(bb->id());
        if (dfcs == nullptr) { continue; }

        for (BSIdx i = dfcs->get_first(); i != BS_UNDEF;
             i = dfcs->get_next(i)) {
            if (visited.is_contain(i)) {
                //Already insert phi for 'mdid' into BB i.
                //TODO:ensure the phi for same PR does NOT be
                //inserted multiple times.
                continue;
            }

            visited.bunion(i);

            IRBB * ibb = m_cfg->getBB(i);
            ASSERT0(ibb);

            //Redundant phi will be removed during refinePhi().
            insertPhi(mdid, ibb);

            ASSERT0(defmds_vec.get(i));
            defmds_vec.get(i)->bunion(mdid);

            wl.append_tail(ibb);
        }
    }
}


//Return true if phi is redundant, otherwise return false.
//CASE1: if all opnds have same defintion or defined by current phi,
//then phi is redundant.
//common_def: record the common_def if the definition of all opnd is the same.
//TODO: p=phi(m,p), if the only use of p is phi, then phi is redundant.
bool MDSSAMgr::doOpndHaveSameDef(MDPhi const* phi, OUT VMD ** common_def) const
{
    MDDef * def = nullptr;
    bool same_def = true; //indicate all DEF of operands are the same stmt.
    MDDef * liveindef = (MDDef*)(-1);
    for (IR const* opnd = phi->getOpndList();
         opnd != nullptr; opnd = opnd->get_next()) {
        VMD * v = phi->getOpndVMD(opnd, &m_usedef_mgr);
        if (v == nullptr) {
            //VOpnd may have been removed from MDSSAMgr, thus the VOpnd that
            //corresponding to the ID is NULL.
            continue;
        }
        ASSERT0(v->is_md());

        MDDef * vdef = nullptr;
        if (v->getDef() != nullptr) {
            vdef = v->getDef();
        } else {
            //DEF of v is the region live-in MD.
            vdef = liveindef;
        }

        if (def == nullptr) {
            def = vdef;
        } else if (def != vdef && phi != vdef) {
            same_def = false;
            break;
        }
    }
    ASSERT0(common_def);
    *common_def = def == nullptr || def == liveindef ?
                  nullptr : def->getResult();
    return same_def;
}


//Return true if one of phi's operand have valid DEF, otherwise return false.
//CASE1: if all opnds's DEF are invalid, then phi is redundant.
//If opnd of phi do not have any valid DEF, then phi is redundant,
//otherwise phi can NOT be removed even if there are only one
//opnd have valid DEF.
//e.g1:Phi: MD14V2 <- MD14V1, MD14V3
//     MD14V1 is valid, MD14V3 is invalid, then Phi can not be removed.
//e.g2:dce6.c, after DCE, there are two BBs. Both PHI prepended at BB
//     are redundant becase their opnd do not have DEF, and it is invalid.
//  --- BB7 ---
//  --- BB5 ---
//  Phi: MD14V2 <- MD14V0(id:60)(BB7)|UsedBy:
//  Phi: MD13V2 <- MD13V1(id:58)(BB7)|UsedBy:
//  Phi: MD10V4 <- MD10V3(id:56)(BB7)|UsedBy:
//  Phi: MD9V4 <- MD9V3(id:54)(BB7)|UsedBy:
//  Phi: MD7V4 <- MD7V3(id:52)(BB7)|UsedBy:
//  starray (i8, ety:i8) id:32 attachinfo:Dbx,MDSSA
//  return id:47 attachinfo:Dbx
bool MDSSAMgr::doOpndHaveValidDef(MDPhi const* phi) const
{
    if (phi->hasNoOpnd()) { return false; }
    //Indicate if there exist a valid DEF for operands
    bool has_valid_def = false;
    for (IR const* opnd = phi->getOpndList();
         opnd != nullptr; opnd = opnd->get_next()) {
        VMD * v = phi->getOpndVMD(opnd, &m_usedef_mgr);
        if (v == nullptr) { continue; }
        ASSERT0(v->is_md());
        if (v->getDef() != nullptr) {
            has_valid_def = true;
            break;
        }
    }
    return has_valid_def;
}


//Record all modified MDs which will be versioned later.
void MDSSAMgr::recordEffectMD(IRBB const* bb, OUT DefMDSet & effect_md)
{
    LiveInMDTab livein_md;
    computeLiveInMD(bb, livein_md);
    LiveInMDTabIter iter;
    for (UINT mdid = livein_md.get_first(iter);
         mdid != MD_UNDEF; mdid = livein_md.get_next(iter)) {
        effect_md.bunion(mdid);
    }
}


//Place phi and assign the v0 for each PR.
//effect_prs: record the MD which need to versioning.
void MDSSAMgr::placePhi(DfMgr const& dfm, OUT DefMDSet & effect_md,
                        DefMiscBitSetMgr & bs_mgr,
                        BB2DefMDSet & defined_md_vec,
                        List<IRBB*> & wl)
{
    START_TIMER(t, "MDSSA: Place phi");

    //Record BBs which modified each MD.
    BBList * bblst = m_rg->getBBList();
    //All objects allocated and recorded in md2defbb are used for local purpose,
    //and will be destoied before leaving this function.
    Vector<List<IRBB*>*> md2defbb(bblst->get_elem_count());
    for (IRBB * bb = bblst->get_head(); bb != nullptr; bb = bblst->get_next()) {
        DefSBitSet * bs = bs_mgr.allocSBitSet();
        defined_md_vec.set(bb->id(), bs);
        collectDefinedMDAndInitVMD(bb, *bs);
        if (m_is_semi_pruned) {
            recordEffectMD(bb, effect_md);
        } else {
            //Record all modified MDs which will be versioned later.
            effect_md.bunion(*bs);
        }

        //Record which BB defined these effect mds.
        DefSBitSetIter cur = nullptr;
        for (BSIdx i = bs->get_first(&cur); i != BS_UNDEF;
             i = bs->get_next(i, &cur)) {
            List<IRBB*> * bbs = md2defbb.get(i);
            if (bbs == nullptr) {
                bbs = new List<IRBB*>();
                md2defbb.set(i, bbs);
            }
            bbs->append_tail(bb);
        }
    }

    //Place phi for lived MD.
    xcom::BitSet visited((bblst->get_elem_count() / BITS_PER_BYTE) + 1);
    DefSBitSetIter cur = nullptr;
    for (BSIdx i = effect_md.get_first(&cur);
         i != BS_UNDEF; i = effect_md.get_next(i, &cur)) {
        //effect_md includes MDs that have not been defined. These MDs's
        //defbbs is empty.
        List<IRBB*> const* defbbs = md2defbb.get(i);
        if (defbbs != nullptr) {
            placePhiForMD(i, defbbs, dfm, visited, wl, defined_md_vec);
        }
    }
    END_TIMER(t, "MDSSA: Place phi");

    //Free local used objects.
    for (INT i = 0; i <= md2defbb.get_last_idx(); i++) {
        List<IRBB*> * bbs = md2defbb.get(i);
        if (bbs == nullptr) { continue; }
        delete bbs;
    }
}


//Note call stmt is a specical case in renaming because it regards MayDef
//as MayUse.
void MDSSAMgr::renameUse(IR * ir, MD2VMDStack & md2vmdstk)
{
    ASSERT0(ir);
    ASSERT0(ir->is_exp());
    MDSSAInfo * mdssainfo = genMDSSAInfo(ir);
    ASSERT0(mdssainfo);
    VOpndSetIter iter;
    VOpndSet * set = mdssainfo->getVOpndSet();
    VOpndSet removed;
    VOpndSet added;
    INT next;
    for (BSIdx i = set->get_first(&iter); i != BS_UNDEF; i = next) {
        next = set->get_next(i, &iter);
        VMD * vopnd = (VMD*)m_usedef_mgr.getVOpnd(i);
        ASSERT0(vopnd && vopnd->is_md() && vopnd->id() == (UINT)i);

        //Get the top-version on stack.
        VMD * topv = md2vmdstk.get_top(vopnd->mdid());
        if (topv == nullptr) {
            //MD does not have top-version, it has no def,
            //and may be parameter.
            continue;
        }

        //e.g: MD1 = MD2(VMD1)
        //    VMD1 will be renamed to VMD2, so VMD1 will not
        //    be there in current IR any more.

        //Set latest version of VMD be the USE of current opnd.
        if (topv->version() == MDSSA_INIT_VERSION) {
            //Do nothing.
            ASSERT0(vopnd == topv);
        } else if (vopnd != topv) {
            //vopnd may be ver0.
            //Current ir does not refer the old version VMD any more.
            ASSERT0(vopnd->version() == MDSSA_INIT_VERSION ||
                    vopnd->findUse(ir));
            ASSERT0(vopnd->version() == MDSSA_INIT_VERSION || vopnd->getDef());
            ASSERT0(!topv->findUse(ir));

            set->remove(vopnd, *getSBSMgr());
            added.append(topv, *getSBSMgr());
        }

        topv->addUse(ir);
    }

    set->bunion(added, *getSBSMgr());
    added.clean(*getSBSMgr());
}


MDPhi * MDSSAMgr::genMDPhi(MDIdx mdid, IR * opnd_list, IRBB * bb, VMD * result)
{
    MDPhi * phi = m_usedef_mgr.allocMDPhi(mdid);
    //Allocate MDSSAInfo for given operands.
    for (IR * opnd = opnd_list; opnd != nullptr; opnd = opnd->get_next()) {
        ASSERT0(opnd->getMustRef() && opnd->getMustRef()->id() == mdid);
        //Generate MDSSAInfo to ID.
        MDSSAInfo const* mdssainfo = getMDSSAInfoIfAny(opnd);
        ASSERT0(mdssainfo && !mdssainfo->readVOpndSet().is_empty());
        ASSERT0(mdssainfo->containSpecificMDOnly(mdid, getUseDefMgr()));
        ID_phi(opnd) = phi; //Record ID's host PHI.
    }
    MDPHI_opnd_list(phi) = opnd_list;
    MDDEF_bb(phi) = bb;
    MDDEF_result(phi) = result;
    //Do NOT set DEF of result here because result's version may be zero.
    //VMD_def(result) = phi;
    return phi;
}


MDPhi * MDSSAMgr::genMDPhi(MDIdx mdid, UINT num_opnd, IRBB * bb, VMD * result)
{
    MDPhi * phi = m_usedef_mgr.allocMDPhi(mdid);
    m_usedef_mgr.buildMDPhiOpnd(phi, mdid, num_opnd);
    MDDEF_bb(phi) = bb;
    MDDEF_result(phi) = result;
    //Do NOT set DEF of result here because result's version may be zero.
    //VMD_def(result) = phi;
    return phi;
}


MDDef * MDSSAMgr::genMDDef(IR * ir, VMD * result)
{
    ASSERT0(ir && ir->is_stmt());
    MDDef * mddef = m_usedef_mgr.allocMDDef();
    MDDEF_bb(mddef) = ir->getBB();
    MDDEF_result(mddef) = result;
    MDDEF_is_phi(mddef) = false;
    MDDEF_occ(mddef) = ir;
    return mddef;
}


void MDSSAMgr::renameDef(IR * ir, IRBB * bb, MD2VMDStack & md2vmdstk)
{
    ASSERT0(ir && ir->is_stmt());
    MDSSAInfo * mdssainfo = genMDSSAInfo(ir);
    ASSERT0(mdssainfo);

    VOpndSetIter iter;
    VOpndSet * set = mdssainfo->getVOpndSet();
    VOpndSet added;
    INT next;
    for (BSIdx i = set->get_first(&iter); i != BS_UNDEF; i = next) {
        next = set->get_next(i, &iter);
        VMD * vopnd = (VMD*)m_usedef_mgr.getVOpnd(i);
        ASSERT0(vopnd && vopnd->is_md() && vopnd->id() == (UINT)i);
        ASSERTN(vopnd->version() == MDSSA_INIT_VERSION,
                ("should be first meet"));

        //Update versioned MD.
        VMD * newv = genNewVersionVMD(vopnd->mdid());
        VMD * nearestv = md2vmdstk.get_top(vopnd->mdid());
        md2vmdstk.push(vopnd->mdid(), newv);

        MDDef * mddef = genMDDef(ir, newv);
        if (nearestv != nullptr && nearestv->getDef() != nullptr) {
            addDefChain(nearestv->getDef(), mddef);
        }
        VMD_def(newv) = mddef;
        set->remove(vopnd, *getSBSMgr());
        added.append(newv, *getSBSMgr());
    }

    set->bunion(added, *getSBSMgr());
    added.clean(*getSBSMgr());
}


//Cut off the DU chain between 'def' and its predecessors.
void MDSSAMgr::cutoffDefChain(MDDef * def)
{
    MDDef * prev = def->getPrev();
    if (prev != nullptr) {
        ASSERT0(prev->getNextSet() && prev->getNextSet()->find(def));
        prev->getNextSet()->remove(def, *getSBSMgr());
    }
    MDDEF_prev(def) = nullptr;
}


//Return true if VMDs of stmt cross version when moving stmt previous to marker
//at tgtbb.
//marker: if marker is nullptr, 'stmt' will append tail of tgtbb, otherwise
//        stmt will insert before marker.
bool MDSSAMgr::crossVersion(IR const* stmt, IRBB const* tgtbb, IR const* marker,
                            OptCtx const& oc) const
{
    ASSERT0(oc.is_dom_valid());
    ASSERT0(stmt->is_stmt());
    MDSSAInfo * info = getMDSSAInfoIfAny(stmt);
    if (info == nullptr) { return false; }
    VOpndSet const& vmdset = info->readVOpndSet();
    UseDefMgr const* udmgr = const_cast<MDSSAMgr*>(this)->getUseDefMgr();
    if (marker == nullptr) {
        //stmt will be appended at the tail of tgtbb.
        VOpndSetIter vit = nullptr;
        for (BSIdx i = vmdset.get_first(&vit);
             i != BS_UNDEF; i = vmdset.get_next(i, &vit)) {
            VMD const* t = (VMD*)udmgr->getVOpnd(i);
            ASSERT0(t && t->is_md());
            if (t->getDef() == nullptr) { continue; }
            MDDef const* prev = t->getDef()->getPrev();
            if (prev == nullptr) { continue; }
            if (m_cfg->is_dom(tgtbb->id(), prev->getBB()->id())) {
                return true;
            }
        }
        return false;
    }

    ASSERT0(marker->getBB() == tgtbb);
    VOpndSetIter vit = nullptr;
    for (BSIdx i = vmdset.get_first(&vit);
         i != BS_UNDEF; i = vmdset.get_next(i, &vit)) {
        VMD const* t = (VMD*)udmgr->getVOpnd(i);
        ASSERT0(t && t->is_md());
        if (t->getDef() == nullptr) { continue; }
        MDDef const* prev = t->getDef()->getPrev();
        if (prev == nullptr) { continue; }
        if (m_cfg->is_dom(tgtbb->id(), prev->getBB()->id())) {
            return true;
        }
        if (prev->getBB() != tgtbb) { continue; }
        if (prev->is_phi()) {
            //Phi always prior to marker.
            continue;
        }
        if (!prev->getBB()->is_dom(prev->getOcc(), marker, true)) {
            return false;
        }
    }
    return false;
}


//Insert def1 in front of def2.
//Note def1 should dominate def2.
void MDSSAMgr::insertDefChain(MDDef * def1, MDDef * def2)
{
    ASSERT0(def1 && def2);
    ASSERT0(isDom(def1, def2));

    //CASE: def4 may be have been inserted between other DefDef chain.
    //     BB11:def1
    //      |
    //      v
    //   --BB14:def4---
    //  |              |
    //  v              v
    //  BB16:def2     BB20:def3
    //When inserting stmt at BB14, whereas need to insert MDDef between
    //def1->def2, and def1->def3.
    ASSERT0(def1->getPrev() == nullptr || def1->getPrev() == def2->getPrev());

    //CASE:The new inserted stmt may processed in previous updating, and
    //the related VMD has been inserted into some DD chain.
    //If it is that, just maintain the DD chain.
    //ASSERTN(def1->getNextSet() == nullptr, ("def1 have to be new"));
    MDDef * prev = def2->getPrev();
    if (prev != nullptr) {
        ASSERT0(prev->getNextSet() && prev->getNextSet()->find(def2));
        prev->getNextSet()->remove(def2, *getSBSMgr());
        prev->getNextSet()->append(def1, *getSBSMgr());

        ASSERTN(isDom(prev, def1),
                ("def1 must be placed between prev and def2"));
        MDDEF_prev(def1) = prev;
        if (MDDEF_nextset(def1) == nullptr) {
            MDDEF_nextset(def1) = m_usedef_mgr.allocMDDefSet();
        } else {
            ASSERT0(!MDDEF_nextset(def1)->isElemDom(def2, this));
        }
        def1->getNextSet()->append(def2, *getSBSMgr());

        MDDEF_prev(def2) = def1;
        return;
    }
    if (MDDEF_nextset(def1) == nullptr) {
        MDDEF_nextset(def1) = m_usedef_mgr.allocMDDefSet();
    } else {
        ASSERT0(!MDDEF_nextset(def1)->isElemDom(def2, this));
    }
    def1->getNextSet()->append(def2, *getSBSMgr());
    MDDEF_prev(def2) = def1;
}


//Add relation to def1->def2 where def1 dominated def2.
void MDSSAMgr::addDefChain(MDDef * def1, MDDef * def2)
{
    ASSERT0(def1 && def2);
    ASSERTN(def2->getPrev() == nullptr,
            ("should cutoff outdated def-relation"));
    if (def1->getNextSet() == nullptr) {
        MDDEF_nextset(def1) = m_usedef_mgr.allocMDDefSet();
    }
    def1->getNextSet()->append(def2, *getSBSMgr());
    MDDEF_prev(def2) = def1;
}


//Rename VMD from current version to the top-version on stack if it exist.
void MDSSAMgr::renamePhiResult(IN IRBB * bb, MD2VMDStack & md2vmdstk)
{
    ASSERT0(bb);
    MDPhiList * philist = getPhiList(bb);
    if (philist == nullptr) { return; }

    for (MDPhiListIter it = philist->get_head();
         it != philist->end(); it = philist->get_next(it)) {
        MDPhi * phi = it->val();
        ASSERT0(phi && phi->is_phi() && phi->getBB() == bb);

        //Rename phi result.
        VMD * vopnd = phi->getResult();
        ASSERT0(vopnd && vopnd->is_md());

        //Update versioned MD.
        VMD * newv = genNewVersionVMD(vopnd->mdid());
        md2vmdstk.push(vopnd->mdid(), newv);

        MDDEF_result(phi) = newv;
        cutoffDefChain(phi);

        VMD_def(newv) = phi;
    }
}


//Rename VMD from current version to the top-version on stack if it exist.
void MDSSAMgr::renameBB(IN IRBB * bb, MD2VMDStack & md2vmdstk)
{
    renamePhiResult(bb, md2vmdstk);
    for (IR * ir = BB_first_ir(bb); ir != nullptr; ir = BB_next_ir(bb)) {
        //Rename opnd, not include phi.
        //Walk through rhs expression IR tree to rename memory's VMD.
        m_iter.clean();
        for (IR * opnd = xoc::iterInit(ir, m_iter);
             opnd != nullptr; opnd = xoc::iterNext(m_iter)) {
            if (!opnd->isMemoryOpnd() || opnd->isReadPR()) {
                continue;
            }

            //In memory SSA, rename the MD even if it is ineffect to
            //keep sound DU chain, e.g:
            //  int bar(int * p, int * q, int * m, int * n)
            //  {
            //    *p = *q + 20; *p define MD2V1
            //    *m = *n - 64; *n use MD2V1
            //    return 0;
            //  }
            renameUse(opnd, md2vmdstk);
        }

        if (!ir->isMemoryRef() || ir->isWritePR()) { continue; }

        //Rename result.
        renameDef(ir, bb, md2vmdstk);
    }
}


void MDSSAMgr::renamePhiOpndInSuccBB(IRBB * bb, MD2VMDStack & md2vmdstk)
{
    ASSERT0(m_cfg->getVertex(bb->id()));
    for (EdgeC const* bbel = m_cfg->getVertex(bb->id())->getOutList();
         bbel != nullptr; bbel = bbel->get_next()) {
        UINT opnd_idx = 0; //the index of corresponding predecessor.
        Vertex const* succv = bbel->getTo();
        EdgeC const* sel;
        for (sel = succv->getInList();
             sel != nullptr; sel = sel->get_next(), opnd_idx++) {
            if (sel->getFromId() == bb->id()) {
                break;
            }
        }
        ASSERTN(sel, ("not found related pred"));
        //Replace opnd of PHI of 'succ' with top SSA version.
        handlePhiInSuccBB(m_cfg->getBB(succv->id()), opnd_idx, md2vmdstk);
    }
}


//Replace opnd of PHI of 'succ' with top SSA version.
void MDSSAMgr::handlePhiInSuccBB(IRBB * succ, UINT opnd_idx,
                                 MD2VMDStack & md2vmdstk)
{
    MDPhiList * philist = getPhiList(succ);
    if (philist == nullptr) { return; }

    for (MDPhiListIter it = philist->get_head();
         it != philist->end(); it = philist->get_next(it)) {
        MDPhi * phi = it->val();
        ASSERT0(phi && phi->is_phi());

        UINT j = 0;
        IR * opnd;
        for (opnd = phi->getOpndList();
             opnd != nullptr && j < opnd_idx; opnd = opnd->get_next(), j++) {}
        ASSERT0(j == opnd_idx && opnd && opnd->is_id());

        ASSERT0(opnd->getRefMD());
        VMD * topv = md2vmdstk.get_top(opnd->getRefMD()->id());
        ASSERTN(topv, ("miss def-stmt to operand of phi"));

        MDSSAInfo * opnd_ssainfo = getMDSSAInfoIfAny(opnd);
        ASSERT0(opnd_ssainfo);
        opnd_ssainfo->cleanVOpndSet(getUseDefMgr());
        opnd_ssainfo->addVOpnd(topv, getUseDefMgr());
        topv->addUse(opnd);
    }
}


void MDSSAMgr::handleBBRename(IRBB * bb, DefMDSet const& effect_mds,
                              DefMDSet const& defed_mds,
                              MOD BB2VMDMap & bb2vmdmap,
                              MD2VMDStack & md2vmdstk)
{
    ASSERT0(bb2vmdmap.get(bb->id()) == nullptr);
    MD2VMD * mdid2vmd = bb2vmdmap.gen(bb->id());
    DefMDSetIter it = nullptr;
    for (INT mdid = defed_mds.get_first(&it);
         mdid >= 0; mdid = defed_mds.get_next(mdid, &it)) {
        VMD * vmd = md2vmdstk.get_top(mdid);
        ASSERT0(vmd || !effect_mds.is_contain(mdid));
        if (vmd != nullptr) {
            mdid2vmd->set(vmd->mdid(), vmd);
        }
    }
    renameBB(bb, md2vmdstk);
    renamePhiOpndInSuccBB(bb, md2vmdstk);
}


//defed_prs_vec: for each BB, indicate PRs which has been defined.
void MDSSAMgr::renameInDomTreeOrder(DefMDSet const& effect_mds,
                                    IRBB * root, DomTree const& domtree,
                                    BB2DefMDSet & defed_mds_vec,
                                    MD2VMDStack & md2vmdstk)
{
    xcom::Stack<IRBB*> stk;
    UINT n = m_rg->getBBList()->get_elem_count();
    xcom::BitSet visited(n / BIT_PER_BYTE);
    BB2VMDMap bb2vmdmap(n);
    IRBB * v;
    stk.push(root);
    while ((v = stk.get_top()) != nullptr) {
        if (!visited.is_contain(v->id())) {
            visited.bunion(v->id());
            DefMDSet const* defed_mds = defed_mds_vec.get(v->id());
            ASSERT0(defed_mds);
            handleBBRename(v, effect_mds, *defed_mds, bb2vmdmap, md2vmdstk);
        }

        xcom::Vertex const* bbv = domtree.getVertex(v->id());
        bool all_visited = true;
        for (xcom::EdgeC const* c = VERTEX_out_list(bbv);
             c != nullptr; c = EC_next(c)) {
            xcom::Vertex * dom_succ = c->getTo();
            if (dom_succ == bbv) { continue; }
            if (!visited.is_contain(dom_succ->id())) {
                ASSERT0(m_cfg->getBB(dom_succ->id()));
                all_visited = false;
                stk.push(m_cfg->getBB(dom_succ->id()));
                break;
            }
        }

        if (all_visited) {
            stk.pop();

            //Do post-processing while all kids of BB has been processed.
            MD2VMD * mdid2vmd = bb2vmdmap.get(v->id());
            ASSERT0(mdid2vmd);
            xcom::DefSBitSet * defed_mds = defed_mds_vec.get(v->id());
            ASSERT0(defed_mds);

            DefSBitSetIter cur = nullptr;
            for (BSIdx i = defed_mds->get_first(&cur);
                 i != BS_UNDEF; i = defed_mds->get_next(i, &cur)) {
                Stack<VMD*> * vs = md2vmdstk.get(i);
                ASSERT0(vs->get_bottom());
                ASSERT0(vs->get_top()->mdid() == (MDIdx)i);
                VMD * vmd = mdid2vmd->get(i);
                while (vs->get_top() != vmd) {
                    vs->pop();
                }
            }

            //vmdmap is useless from now on.
            bb2vmdmap.erase(v->id());
        }
    }
}


//Rename variables.
void MDSSAMgr::rename(DefMDSet const& effect_mds,
                      BB2DefMDSet & defed_mds_vec,
                      DomTree & domtree,
                      MD2VMDStack & md2vmdstk)
{
    START_TIMER(t, "MDSSA: Rename");
    BBList * bblst = m_rg->getBBList();
    if (bblst->get_elem_count() == 0) { return; }

    DefMDSetIter it = nullptr;
    for (INT mdid = effect_mds.get_first(&it);
         mdid >= 0; mdid = effect_mds.get_next(mdid, &it)) {
        md2vmdstk.push(mdid, genInitVersionVMD(mdid));
    }

    ASSERT0(m_cfg->getEntry());
    renameInDomTreeOrder(effect_mds, m_cfg->getEntry(), domtree,
                         defed_mds_vec, md2vmdstk);
    END_TIMER(t, "MDSSA: Rename");
}


void MDSSAMgr::cleanIRSSAInfo(IRBB * bb)
{
    IRListIter lit;
    IRIter it;
    for (IR * ir = BB_irlist(bb).get_head(&lit);
         ir != nullptr; ir = BB_irlist(bb).get_next(&lit)) {
        it.clean();
        for (IR * k = xoc::iterInit(ir, it);
             k != nullptr; k = xoc::iterNext(it)) {
            if (hasMDSSAInfo(k)) {
                cleanMDSSAInfo(k);
            }
        }
    }
}


void MDSSAMgr::destructBBSSAInfo(IRBB * bb)
{
    cleanIRSSAInfo(bb);
    freeBBPhiList(bb);
}


void MDSSAMgr::destructionInDomTreeOrder(IRBB * root, DomTree & domtree)
{
    xcom::Stack<IRBB*> stk;
    UINT n = m_rg->getBBList()->get_elem_count();
    xcom::BitSet visited(n / BIT_PER_BYTE);
    BB2VMDMap bb2vmdmap(n);
    IRBB * v = nullptr;
    stk.push(root);
    while ((v = stk.get_top()) != nullptr) {
        if (!visited.is_contain(v->id())) {
            visited.bunion(v->id());
            destructBBSSAInfo(v);
        }

        xcom::Vertex * bbv = domtree.getVertex(v->id());
        ASSERTN(bbv, ("dom tree is invalid."));

        xcom::EdgeC * c = VERTEX_out_list(bbv);
        bool all_visited = true;
        while (c != nullptr) {
            xcom::Vertex * dom_succ = c->getTo();
            if (dom_succ == bbv) { continue; }
            if (!visited.is_contain(dom_succ->id())) {
                ASSERT0(m_cfg->getBB(dom_succ->id()));
                all_visited = false;
                stk.push(m_cfg->getBB(dom_succ->id()));
                break;
            }
            c = EC_next(c);
        }

        if (all_visited) {
            stk.pop();
            //Do post-processing while all kids of BB has been processed.
        }
    }
}


//Destruction of MDSSA.
//The function perform SSA destruction via scanning BB in preorder
//traverse dominator tree.
//Return true if inserting copy at the head of fallthrough BB
//of current BB's predessor.
void MDSSAMgr::destruction(DomTree & domtree)
{
    START_TIMER(t, "MDSSA: destruction in dom tree order");

    BBList * bblst = m_rg->getBBList();
    if (bblst->get_elem_count() == 0) { return; }
    ASSERT0(m_cfg->getEntry());
    destructionInDomTreeOrder(m_cfg->getEntry(), domtree);
    m_is_valid = false;

    END_TIMER(t, "MDSSA: destruction in dom tree order");
}


bool MDSSAMgr::verifyDDChain() const
{
    START_TIMER(tverify, "MDSSA: Verify DefDef Chain");
    MDSSAMgr * pthis = const_cast<MDSSAMgr*>(this);
    MDDefVec const* mddefvec = pthis->getUseDefMgr()->getMDDefVec();
    for (INT i = 0; i <= mddefvec->get_last_idx(); i++) {
        MDDef const* mddef = mddefvec->get(i);
        if (mddef == nullptr) { continue; }

        MDDef const* prev = mddef->getPrev();
        if (prev != nullptr) {
            ASSERT0(prev->getNextSet());
            ASSERT0(prev->getNextSet()->find(mddef));
        }
        if (mddef->is_phi() && ((MDPhi*)mddef)->hasNumOfOpndAtLeast(2)) {
            //Note if MDDef indicates PHI, it does not have Previous DEF,
            //because PHI has multiple Previous DEFs rather than single DEF.
            ASSERT0(prev == nullptr);
        }
        //CASE: Be careful that 'prev' should not belong to the NextSet of
        //mddef', otherwise the union operation of prev and mddef's succ DEF
        //will construct a cycle in Def-Def chain, which is illegal.
        //e.g: for (i = 0; i < 10; i++) {;}, where i's MD is MD5.
        //  MD5V2 <-- PHI(MD5V--, MD5V3)
        //  MD5V3 <-- MD5V2 + 1
        // If we regard MD5V3 as the common-def, PHI is 'mddef', a cycle
        // will appeared.
        if (prev != nullptr && mddef->getNextSet() != nullptr) {
            ASSERTN(!mddef->getNextSet()->find(prev),
                    ("prev should NOT be the NEXT of mddef"));
        }
    }
    END_TIMER(tverify, "MDSSA: Verify DefDef Chain");
    return true;
}


bool MDSSAMgr::verifyPhiOpndList(MDPhi const* phi, UINT prednum) const
{
    MDSSAMgr * pthis = const_cast<MDSSAMgr*>(this);
    VMD * res = phi->getResult();
    CHECK0_DUMMYUSE(res->is_md());
    UINT opndnum = 0;
    for (IR const* opnd = phi->getOpndList();
         opnd != nullptr; opnd = opnd->get_next()) {
        opndnum++;
        if (!opnd->is_id()) {
            ASSERT0(opnd->is_const() || opnd->is_lda());
            continue;
        }
        ASSERTN(ID_phi(opnd) == phi, ("opnd is not an operand of phi"));

        //CASE1:Opnd may be ID, CONST or LDA.
        MD const* opnd_md = opnd->getMustRef();
        CHECK0_DUMMYUSE(opnd_md);
        ASSERTN(MD_id(opnd_md) == res->mdid(), ("mdid of VMD is unmatched"));

        //CASE2:An individual ID can NOT represent multiple versioned MD, thus
        //the VOpnd of ID must be unique.
        MDSSAInfo const* opnd_mdssainfo = getMDSSAInfoIfAny(opnd);
        ASSERT0(opnd_mdssainfo);
        UINT vopndnum = opnd_mdssainfo->readVOpndSet().get_elem_count();
        ASSERT0(vopndnum == 1);

        //CASE3:some pass, e.g:DCE, will remove MDPhi step by step, thus
        //do NOT invoke the function during the removing.
        VMD * opndvmd = ((MDPhi*)phi)->getOpndVMD(opnd, pthis->getUseDefMgr());
        ASSERTN(opndvmd, ("miss VOpnd"));

        //CASE4:Version 0 does not have MDDef.
        //ASSERT0(VMD_version(opndvmd) > 0);
    }
    //CASE5:check the number of phi opnds.
    ASSERTN(opndnum == prednum,
            ("The number of phi operand must same with "
             "the number of BB predecessors."));
    return true;
}


//The function verify the operand and VMD info for MDPhi.
//NOTE: some pass, e.g:DCE, will remove MDPhi step by step, thus
//do NOT invoke the function during the removing.
bool MDSSAMgr::verifyPhi() const
{
    BBList * bblst = m_rg->getBBList();
    List<IRBB*> preds;
    for (IRBB * bb = bblst->get_head(); bb != nullptr;
         bb = bblst->get_next()) {
        m_cfg->get_preds(preds, bb);
        MDPhiList * philist = getPhiList(bb);
        if (philist == nullptr) { continue; }

        UINT prednum = bb->getNumOfPred(m_cfg);
        for (MDPhiListIter it = philist->get_head();
             it != philist->end(); it = philist->get_next(it)) {
            MDPhi * phi = it->val();
            ASSERT0(phi);
            verifyPhiOpndList(phi, prednum);
        }
    }
    return true;
}


void MDSSAMgr::collectDefinedMD(IRBB const* bb, OUT DefMDSet & maydef) const
{
    MDPhiList const* philist = getPhiList(bb);
    if (philist != nullptr) {
        for (MDPhiListIter it = philist->get_head();
             it != philist->end(); it = philist->get_next(it)) {
            ASSERT0(it->val());
            maydef.bunion(it->val()->getResult()->mdid());
        }
    }
    for (IR const* ir = BB_first_ir(const_cast<IRBB*>(bb));
         ir != nullptr; ir = BB_next_ir(const_cast<IRBB*>(bb))) {
        if (ir->isCallReadOnly() || !MDSSAMgr::hasMDSSAInfo(ir)) {
            continue;
        }
        MDSSAInfo const* info = getMDSSAInfoIfAny(ir);
        if (info == nullptr) { continue; }
        VOpndSetIter it;
        VOpndSet const& set = info->readVOpndSet();
        for (BSIdx i = set.get_first(&it); i != BS_UNDEF;
             i = set.get_next(i, &it)) {
            VMD const* vmd = (VMD*)getVOpnd(i);
            ASSERT0(vmd && vmd->is_md());
            ASSERT0(vmd->version() != MDSSA_INIT_VERSION);
            ASSERT0(vmd->getDef());
            maydef.bunion(vmd->mdid());
        }
    }
}


void MDSSAMgr::collectDefinedMDForBBList(MOD DefMiscBitSetMgr & bs_mgr,
                                         OUT BB2DefMDSet & bb2defmds) const
{
    BBList const* bblst = m_rg->getBBList();
    BBListIter it;
    for (IRBB const* bb = bblst->get_head(&it);
         bb != nullptr; bb = bblst->get_next(&it)) {
        DefMDSet * bs = bs_mgr.allocSBitSet();
        bb2defmds.set(bb->id(), bs);
        collectDefinedMD(bb, *bs);
    }
}


static bool verifyVerPhiInSuccBB(IRBB const* succ, UINT opnd_idx,
                                 MD2VMDStack const& md2verstk,
                                 MDSSAMgr const* mgr)
{
    MDPhiList * philist = mgr->getPhiList(succ);
    if (philist == nullptr) { return true; }

    MDSSAMgr * pmgr = const_cast<MDSSAMgr*>(mgr);
    for (MDPhiListIter it = philist->get_head();
         it != philist->end(); it = philist->get_next(it)) {
        MDPhi const* phi = it->val();
        ASSERT0(phi && phi->is_phi());

        UINT j = 0;
        IR * opnd;
        for (opnd = phi->getOpndList();
             opnd != nullptr && j < opnd_idx; opnd = opnd->get_next(), j++) {}
        ASSERT0(j == opnd_idx && opnd && opnd->is_id());

        VMD * vmd = ((MDPhi*)phi)->getOpndVMD(opnd, pmgr->getUseDefMgr());
        ASSERTN(vmd, ("miss VOpnd"));
        VMD const* topvmd = md2verstk.get_top(vmd);
        ASSERTN(vmd == topvmd ||
                (vmd->version() == MDSSA_INIT_VERSION && topvmd == nullptr),
                ("use invalid version"));
    }
    return true;
}


static bool verifyVerPhiResult(IRBB const* bb, MOD MD2VMDStack & md2verstk,
                               MDSSAMgr const* mgr)
{
    ASSERT0(bb);
    MDPhiList const* philist = mgr->getPhiList(bb);
    if (philist == nullptr) { return true; }
    for (MDPhiListIter it = philist->get_head();
         it != philist->end(); it = philist->get_next(it)) {
        MDPhi * phi = it->val();
        ASSERT0(phi && phi->is_phi() && phi->getBB() == bb);
        VMD * resvmd = phi->getResult();
        ASSERT0(resvmd && resvmd->is_md());
        md2verstk.push(resvmd);
    }
    return true;
}


static bool verifyVerUse(IR const* ir, MD2VMDStack const& md2verstk,
                         MDSSAMgr const* mgr)
{
    ASSERT0(ir->is_exp());
    MDSSAInfo const* info = mgr->getMDSSAInfoIfAny(ir);
    ASSERT0(info);
    VOpndSetIter it;
    VOpndSet const& set = info->readVOpndSet();
    for (BSIdx i = set.get_first(&it); i != BS_UNDEF;
         i = set.get_next(i, &it)) {
        VMD const* vmd = (VMD*)mgr->getVOpnd(i);
        ASSERT0(vmd && vmd->is_md() && vmd->id() == (UINT)i);
        if (vmd->version() == MDSSA_INIT_VERSION) { continue; }
        VMD const* topvmd = md2verstk.get_top(vmd);
        ASSERTN(vmd == topvmd, ("use invalid version"));
    }
    return true;
}


static bool verifyVerDef(IR const* ir, MOD MD2VMDStack & md2verstk,
                         MDSSAMgr const* mgr)
{
    ASSERT0(ir->is_stmt());
    MDSSAInfo const* info = mgr->getMDSSAInfoIfAny(ir);
    ASSERT0(info);
    VOpndSetIter it;
    VOpndSet const& set = info->readVOpndSet();
    for (BSIdx i = set.get_first(&it); i != BS_UNDEF; i = set.get_next(i, &it)) {
        VMD * vmd = (VMD*)mgr->getVOpnd(i);
        ASSERT0(vmd && vmd->is_md() && vmd->id() == (UINT)i);
        ASSERT0(vmd->version() != MDSSA_INIT_VERSION);
        md2verstk.push(vmd);
    }
    return true;
}


static bool verifyPhiOpndInSuccBB(IRBB const* bb, MD2VMDStack & md2verstk,
                                  MDSSAMgr const* mgr)
{
    IRCFG * cfg = mgr->getRegion()->getCFG();
    ASSERT0(cfg->getVertex(bb->id()));
    for (EdgeC const* bbel = cfg->getVertex(bb->id())->getOutList();
         bbel != nullptr; bbel = bbel->get_next()) {
        UINT opnd_idx = 0; //the index of corresponding predecessor.
        Vertex const* succv = bbel->getTo();
        EdgeC const* sel;
        for (sel = succv->getInList();
             sel != nullptr; sel = sel->get_next(), opnd_idx++) {
            if (sel->getFromId() == bb->id()) {
                break;
            }
        }
        ASSERTN(sel, ("not found related pred"));
        verifyVerPhiInSuccBB(cfg->getBB(succv->id()), opnd_idx, md2verstk, mgr);
    }
    return true;
}


static bool verifyVerBB(IRBB const* bb, MD2VMDStack & md2verstk,
                        MDSSAMgr const* mgr)
{
    verifyVerPhiResult(bb, md2verstk, mgr);
    ConstIRIter irit;
    for (IR const* ir = BB_first_ir(const_cast<IRBB*>(bb));
         ir != nullptr; ir = BB_next_ir(const_cast<IRBB*>(bb))) {
        //Rename opnd, not include phi.
        //Walk through rhs expression IR tree to rename memory's VMD.
        irit.clean();
        for (IR const* opnd = xoc::iterInitC(ir, irit);
             opnd != nullptr; opnd = xoc::iterNextC(irit)) {
            if (!opnd->isMemoryOpnd() || opnd->isReadPR()) {
                continue;
            }
            verifyVerUse(opnd, md2verstk, mgr);
        }

        if (!ir->isMemoryRef() || ir->isWritePR()) { continue; }

        //Rename result.
        verifyVerDef(ir, md2verstk, mgr);
    }
    verifyPhiOpndInSuccBB(bb, md2verstk, mgr);
    return true;
}


static void recordTopVer(IRBB const* bb, DefMDSet const* defed_mds,
                         MD2VMDStack const& md2verstk,
                         MOD BB2VMDMap & bb2vmd)
{
    ASSERT0(bb2vmd.get(bb->id()) == nullptr);
    MD2VMD * mdid2vmd = bb2vmd.gen(bb->id());
    DefMDSetIter it = nullptr;
    for (INT mdid = defed_mds->get_first(&it);
         mdid >= 0; mdid = defed_mds->get_next(mdid, &it)) {
        VMD * vmd = md2verstk.get_top(mdid);
        if (vmd != nullptr) {
            mdid2vmd->set(vmd->mdid(), vmd);
        }
    }
}


static bool verifyVerImpl(DomTree const& domtree, MDSSAMgr const* mgr)
{
    DefMiscBitSetMgr bs_mgr;
    BB2DefMDSet bb2defmds;
    mgr->collectDefinedMDForBBList(bs_mgr, bb2defmds);

    IRCFG const* cfg = mgr->getRegion()->getCFG();
    IRBB const* root = cfg->getEntry();
    xcom::Stack<IRBB const*> stk;
    UINT n = mgr->getRegion()->getBBList()->get_elem_count();
    xcom::BitSet visited(n / BIT_PER_BYTE);
    BB2VMDMap bb2vmd(n);
    IRBB const* bb;
    stk.push(root);
    MD2VMDStack md2verstk;
    while ((bb = stk.get_top()) != nullptr) {
        if (!visited.is_contain(bb->id())) {
            visited.bunion(bb->id());
            DefMDSet const* mds = bb2defmds.get(bb->id());
            ASSERT0(mds);
            recordTopVer(bb, mds, md2verstk, bb2vmd);
            verifyVerBB(bb, md2verstk, mgr);
        }
        xcom::Vertex const* bbv = domtree.getVertex(bb->id());
        bool all_visited = true;
        for (xcom::EdgeC const* c = bbv->getOutList();
             c != nullptr; c = c->get_next()) {
            xcom::Vertex const* dom_succ = c->getTo();
            if (dom_succ == bbv) { continue; }
            if (!visited.is_contain(dom_succ->id())) {
                ASSERT0(cfg->getBB(dom_succ->id()));
                all_visited = false;
                stk.push(cfg->getBB(dom_succ->id()));
                break;
            }
        }

        if (all_visited) {
            stk.pop();

            //Do post-processing while all kids of BB has been processed.
            MD2VMD * mdid2vmd = bb2vmd.get(bb->id());
            ASSERT0(mdid2vmd);
            xcom::DefSBitSet * defmds = bb2defmds.get(bb->id());
            ASSERT0(defmds);

            DefSBitSetIter it = nullptr;
            for (BSIdx i = defmds->get_first(&it);
                 i != BS_UNDEF; i = defmds->get_next(i, &it)) {
                Stack<VMD*> * verstk = md2verstk.get(i);
                ASSERT0(verstk && verstk->get_bottom());
                VMD const* vmd = mdid2vmd->get(i);
                while (verstk->get_top() != vmd) {
                    verstk->pop();
                }
            }

            //vmdmap is useless from now on.
            bb2vmd.erase(bb->id());
        }
    }
    return true;
}


//Note the verification is relatively slow.
bool MDSSAMgr::verifyVersion() const
{
    //Extract dominate tree of CFG.
    START_TIMER(t, "MDSSA: verifyVersion");
    ASSERT0(getOptCtx()->is_dom_valid());
    DomTree domtree;
    m_cfg->genDomTree(domtree);
    if (!verifyVerImpl(domtree, this)) {
        return false;
    }
    END_TIMER(t, "MDSSA: verifyVersion");
    return true;
}


bool MDSSAMgr::verifyVMD() const
{
    START_TIMER(tverify, "MDSSA: Verify VMD After Pass");

    MDSSAMgr * pthis = const_cast<MDSSAMgr*>(this);
    //Check version for each VMD.
    xcom::BitSet defset;
    VOpndVec * vec = pthis->getUseDefMgr()->getVOpndVec();
    for (INT i = 1; i <= vec->get_last_idx(); i++) {
        VMD * v = (VMD*)vec->get(i);
        if (v == nullptr) {
            //VMD may have been removed.
            continue;
        }
        if (!v->is_md()) { continue; }

        MDDef * def = v->getDef();
        if (def == nullptr) {
            //ver0 used to indicate the Region live-in MD.
            //It may be parameter or outer region MD.
            ASSERTN(v->version() == MDSSA_INIT_VERSION,
                    ("Nondef vp's version must be MDSSA_INIT_VERSION"));
        } else {
            ASSERTN(def->getResult() == v, ("def is not the DEF of v"));
            ASSERTN(v->version() != MDSSA_INIT_VERSION,
                    ("version can not be MDSSA_INIT_VERSION"));
            ASSERTN(!defset.is_contain(def->id()),
                    ("DEF for each md+version must be unique."));
            defset.bunion(def->id());
        }

        VMD * res = nullptr;
        if (def != nullptr) {
            res = def->getResult();
            ASSERT0(res);
            verifyDef(def, v);
        }

        if (res != nullptr) {
            verifyUseSet(res);
        }
    }
    END_TIMER(tverify, "MDSSA: Verify VMD After Pass");
    return true;
}


void MDSSAMgr::verifyDef(MDDef const* def, VMD const* vopnd) const
{
    ASSERT0(def);
    ASSERT0(def->getResult());
    if (def->is_phi()) {
        //TODO: verify PHI.
        return;
    }

    ASSERT0(def->getOcc());
    if (def->getOcc()->is_undef()) {
        //The stmt may have been removed, and the VMD is obsoleted.
        //If the stmt removed, its UseSet should be empty, otherwise there is
        //an illegal missing-DEF error.
        ASSERT0(!vopnd->hasUse());
    }

    bool findref = false;
    if (def->getOcc()->getRefMD() != nullptr &&
        MD_id(def->getOcc()->getRefMD()) == vopnd->mdid()) {
        findref = true;
    }
    if (def->getOcc()->getRefMDSet() != nullptr &&
        def->getOcc()->getRefMDSet()->is_contain_pure(vopnd->mdid())) {
        findref = true;
    }

    //Do NOT ASSERT if not found reference.
    //Some transformation, such as IR Refinement, may change
    //the MDSet contents. This might lead to the inaccurate and
    //redundant memory dependence. But the correctness of
    //dependence is garanteed.
    //e.g:
    //ist:*<4> id:18 //:MD11, MD12, MD14, MD15
    //    lda: *<4> 'r'
    //    ild: i32  //MMD13: MD16
    //        ld: *<4> 'q' //MMD18
    //=> after IR combination: ist(lda) => st
    //st:*<4> 'r' //MMD12
    //    ild : i32 //MMD13 : MD16
    //        ld : *<4> 'q'    //MMD18
    //ist changed to st. The reference MDSet changed to single MD as well.
    //CHECK0_DUMMYUSE(findref);
    DUMMYUSE(findref);
}


//Check SSA uses.
void MDSSAMgr::verifyUseSet(VMD const* vopnd) const
{
    //Check if USE of vopnd references the correct MD/MDSet.
    VMD::UseSetIter iter2;
    for (INT j = const_cast<VMD*>(vopnd)->getUseSet()->get_first(iter2);
         !iter2.end();
         j = const_cast<VMD*>(vopnd)->getUseSet()->get_next(iter2)) {
        IR const* use = (IR*)m_rg->getIR(j);
        ASSERT0(use);
        ASSERT0(use->isMemoryRef() || use->is_id());
        bool findref = false;
        if (use->getRefMD() != nullptr &&
            use->getRefMD()->id() == vopnd->mdid()) {
            findref = true;
        }
        if (use->getRefMDSet() != nullptr &&
            use->getRefMDSet()->is_contain_pure(vopnd->mdid())) {
            findref = true;
        }

        //Do NOT ASSERT if not found reference MD at USE point which
        //should correspond to vopnd->md().
        //Some transformation, such as IR Refinement, may change
        //the USE's MDSet. This might lead to the inaccurate and
        //redundant MDSSA DU Chain. So the MDSSA DU Chain is conservative,
        //but the correctness of MDSSA dependence is garanteed.
        //e.g:
        //  ist:*<4> id:18 //:MD11, MD12, MD14, MD15
        //    lda: *<4> 'r'
        //    ild: i32 //MMD13: MD16
        //      ld: *<4> 'q' //MMD18
        //=> After IR combination: ist(lda) transformed to st
        //  st:*<4> 'r' //MMD12
        //    ild : i32 //MMD13 : MD16
        //      ld : *<4> 'q'    //MMD18
        //ist transformed to st. This reduce referenced MDSet to a single MD
        //as well.
        //CHECK0_DUMMYUSE(findref);
        DUMMYUSE(findref);

        //VOpndSet of each USE should contain vopnd.
        MDSSAInfo * mdssainfo = getMDSSAInfoIfAny(use);
        ASSERT0(mdssainfo);
        ASSERT0(mdssainfo->getVOpndSet()->find(vopnd));
    }
}


void MDSSAMgr::verifyMDSSAInfoForIR(IR const* ir) const
{
    ASSERT0(ir);
    MDSSAInfo * mdssainfo = getMDSSAInfoIfAny(ir);
    ASSERT0(mdssainfo);
    VOpndSetIter iter = nullptr;
    VOpndSet * set = mdssainfo->getVOpndSet();
    for (BSIdx i = set->get_first(&iter); i != BS_UNDEF;
         i = set->get_next(i, &iter)) {
        VMD * vopnd = (VMD*)m_usedef_mgr.getVOpnd(i);
        ASSERTN(vopnd, ("vopnd may have been removed, "
                        "VOpndSet have not been updated in time"));
        ASSERT0(vopnd && vopnd->is_md());
        MDDef * def = vopnd->getDef();
        if (ir->is_stmt()) {
            ASSERTN(vopnd->version() != MDSSA_INIT_VERSION,
                    ("not yet perform renaming"));
            ASSERTN(def && def->getOcc() == ir, ("IR stmt should have MDDef"));
            ASSERT0(def->is_valid());
            continue;
        }

        //ir is expression.
        if (def != nullptr) {
            ASSERT0(vopnd->findUse(ir));
        } else {
            //The DEF of vopnd is NULL, it should be initial version of MD.
            ASSERT0(vopnd->version() == MDSSA_INIT_VERSION);
        }
        //VMD's Def and UseSet verification will processed in verifyVMD().
    }
}


bool MDSSAMgr::verifyMDSSAInfoUniqueness() const
{
    xcom::TMap<MDSSAInfo const*, IR const*> ir2mdssainfo;
    for (INT i = 0; i <= m_rg->getIRVec()->get_last_idx(); i++) {
        IR const* ir = m_rg->getIR(i);
        if (ir == nullptr) { continue; }
        MDSSAInfo const* mdssainfo = getMDSSAInfoIfAny(ir);
        if (mdssainfo != nullptr) {
            ir2mdssainfo.set(mdssainfo, ir);
        }
    }
    return true;
}


bool MDSSAMgr::verifyDUChainAndOccForPhi(MDPhi const* phi) const
{
    for (IR const* opnd = phi->getOpndList();
         opnd != nullptr; opnd = opnd->get_next()) {
        if (!opnd->is_id()) {
            ASSERT0(opnd->is_const() || opnd->is_lda());
            continue;
        }
        MD const* opnd_md = opnd->getMustRef();
        CHECK0_DUMMYUSE(opnd_md);
        ASSERTN(opnd_md->id() == phi->getResult()->mdid(), ("MD not matched"));
        verifyMDSSAInfoForIR(opnd);
    }
    return true;
}


bool MDSSAMgr::verifyDUChainAndOcc() const
{
    MDSSAMgr * pthis = const_cast<MDSSAMgr*>(this);
    //Check version for each VMD.
    BBList * bbl = m_rg->getBBList();
    BBListIter ct = nullptr;
    for (bbl->get_head(&ct); ct != bbl->end(); ct = bbl->get_next(ct)) {
        IRBB * bb = ct->val();
        //Verify PHI list.
        MDPhiList * philist = pthis->getPhiList(bb);
        if (philist != nullptr) {
            for (MDPhiListIter it = philist->get_head();
                 it != philist->end(); it = philist->get_next(it)) {
                MDPhi const* phi = it->val();
                ASSERT0(phi && phi->is_phi() && phi->getResult());
                verifyDUChainAndOccForPhi(phi);
            }
        }

        IRListIter ctir = nullptr;
        for (BB_irlist(bb).get_head(&ctir);
             ctir != BB_irlist(bb).end();
             ctir = BB_irlist(bb).get_next(ctir)) {
            IR * ir = ctir->val();
            pthis->m_iter.clean();
            for (IR const* x = xoc::iterInit(ir, pthis->m_iter);
                 x != nullptr; x = xoc::iterNext(pthis->m_iter)) {
                if (x->isMemoryRefNonPR()) {
                    verifyMDSSAInfoForIR(x);
                }
            }
        }
    }
    return true;
}


//The verification check the DU info in SSA form.
//Current IR must be in SSA form.
bool MDSSAMgr::verify() const
{
    START_TIMER(tverify, "MDSSA: Verify After Pass");
    ASSERT0(verifyDDChain());
    ASSERT0(verifyVMD());
    ASSERT0(verifyDUChainAndOcc());
    ASSERT0(verifyMDSSAInfoUniqueness());
    END_TIMER(tverify, "MDSSA: Verify After Pass");
    return true;
}


//DU chain operation.
//Change Def stmt from 'olddef' to previous-def in the DefDef chain.
//olddef: original stmt.
//e.g: olddef->{USE1,USE2} change to prevdef->{USE1,USE2}.
bool MDSSAMgr::tryChangeDefToPrev(IR * olddef)
{
    ASSERT0(olddef->is_stmt());
    MDSSAInfo const* mdssainfo = getMDSSAInfoIfAny(olddef);
    ASSERTN(mdssainfo, ("miss MDSSAInfo"));
    VOpndSetIter it = nullptr;
    VOpndSet const& vset = mdssainfo->readVOpndSet();
    for (BSIdx i = vset.get_first(&it);
         i != BS_UNDEF; i = vset.get_next(i, &it)) {
        VMD * t = (VMD*)getVOpnd(i);
        ASSERT0(t && t->is_md());
        MDDef * def = t->getDef();
        ASSERT0(def || t->version() == MDSSA_INIT_VERSION);
        if (def == nullptr || def->getPrev() == nullptr) {
            ASSERT0(def || t->version() == MDSSA_INIT_VERSION);
            changeVMD(t, genInitVersionVMD(t->mdid()));
            continue;
        }
        if (def->getPrev() != nullptr) {
            changeVMD(t, def->getPrev()->getResult());
            continue;
        }
        ASSERT0(def->is_phi());
        return false;
    }
    return true;
}


void MDSSAMgr::changeVMD(VMD * oldvmd, VMD * newvmd)
{
    ASSERT0(oldvmd && newvmd && oldvmd->is_md() && newvmd->is_md());
    ASSERT0(oldvmd != newvmd);
    VMD::UseSet * oldus = oldvmd->getUseSet();
    VMD::UseSet * newus = newvmd->getUseSet();
    VMD::UseSetIter it;
    for (INT j = oldus->get_first(it); !it.end(); j = oldus->get_next(it)) {
        IR * use = m_rg->getIR(j);
        MDSSAInfo * usessainfo = getMDSSAInfoIfAny(use);
        ASSERT0(usessainfo);
        usessainfo->removeVOpnd(oldvmd, getUseDefMgr());
        usessainfo->addVOpnd(newvmd, getUseDefMgr());
        newus->append(j);
    }
    oldvmd->cleanUseSet();
}


//DU chain operation.
//Change Def stmt from 'olddef' to 'newdef'.
//olddef: original stmt.
//newdef: target stmt.
//e.g: olddef->{USE1,USE2} change to newdef->{USE1,USE2}.
void MDSSAMgr::changeDef(IR * olddef, IR * newdef)
{
    ASSERT0(olddef && newdef && olddef->is_stmt() && newdef->is_stmt());
    ASSERT0(olddef != newdef);
    ASSERT0(olddef->isMemoryRefNonPR() && newdef->isMemoryRefNonPR());
    MDSSAInfo * oldmdssainfo = getMDSSAInfoIfAny(olddef);
    ASSERT0(oldmdssainfo);

    VOpndSetIter iter = nullptr;
    for (BSIdx i = oldmdssainfo->getVOpndSet()->get_first(&iter);
         i != BS_UNDEF; i = oldmdssainfo->getVOpndSet()->get_next(i, &iter)) {
        VMD * vopnd = (VMD*)getUseDefMgr()->getVOpnd(i);
        ASSERT0(vopnd && vopnd->is_md());
        ASSERT0(vopnd->getDef()->getOcc() == olddef);
        MDDEF_occ(VMD_def(vopnd)) = newdef;
    }

    MDSSAInfo * newmdssainfo = getMDSSAInfoIfAny(newdef);
    if (newmdssainfo == nullptr) {
        copyMDSSAInfo(newdef, olddef);
        return;
    }
    //newdef may have different VOpnd than olddef. Thus appending VOpnd of old
    //to newdef.
    newmdssainfo->addUseSet(oldmdssainfo, getUseDefMgr());
}


//Generate MDSSAInfo and generate VMD for referrenced MD that both include
//The function will generate MDSSAInfo for 'exp' according to the refinfo.
//that defined inside li. The new info for 'exp' will be VMD that defined
//outside of li or the initial version of VMD.
void MDSSAMgr::genMDSSAInfoToOutsideLoopDef(IR * exp,
                                            MDSSAInfo const* refinfo,
                                            LI<IRBB> const* li)
{
    MDSSAInfo * info = genMDSSAInfo(exp);
    ASSERT0(info);
    VOpndSetIter vit = nullptr;
    List<VMD*> newvmds;
    for (BSIdx i = refinfo->readVOpndSet().get_first(&vit);
         i != BS_UNDEF; i = refinfo->readVOpndSet().get_next(i, &vit)) {
        VMD * vmd = (VMD*)getVOpnd(i);
        ASSERT0(vmd->is_md());
        vmd->removeUse(exp);
        MDDef const* newdef = nullptr;
        if (vmd->getDef() == nullptr) {
            ASSERT0(vmd->version() == MDSSA_INIT_VERSION);
        } else if (vmd->getDef()->is_phi()) {
            newdef = findUniqueOutsideLoopDef(vmd->getDef(), li);
        } else {
            for (newdef = vmd->getDef(); newdef != nullptr &&
                 li->isInsideLoop(newdef->getBB()->id());
                 newdef = newdef->getPrev()) {
            }
        }
        if (newdef != nullptr) {
            newvmds.append_tail(newdef->getResult());
        } else {
            //Need add init-version VMD to represent the existence of MD.
            newvmds.append_tail(genInitVersionVMD(vmd->mdid()));
        }
    }
    info->cleanVOpndSet(getUseDefMgr());
    for (VMD * v = newvmds.get_head();
         v != nullptr; v = newvmds.get_next()) {
        if (v->version() != MDSSA_INIT_VERSION) {
            v->addUse(exp);
        }
        info->addVOpnd(v, getUseDefMgr());
    }
}


void MDSSAMgr::changeDefToOutsideLoopDefForTree(IR * exp, LI<IRBB> const* li)
{
    IRIter it;
    for (IR * x = iterInit(exp, it); x != nullptr; x = iterNext(it)) {
        if (x->isMemoryRefNonPR()) {
            changeDefToOutsideLoopDef(x, li);
        }
    }
}


void MDSSAMgr::changeDefToOutsideLoopDef(IR * exp, LI<IRBB> const* li)
{
    ASSERT0(exp->is_exp() && exp->isMemoryRefNonPR());
    MDSSAInfo * info = getMDSSAInfoIfAny(exp);
    ASSERT0(info && !info->readVOpndSet().is_empty());
    genMDSSAInfoToOutsideLoopDef(exp, info, li);
}


//Note DOM info must be available.
void MDSSAMgr::findAndSetLiveInDefForTree(IR * exp, IR const* startir,
                                          IRBB const* startbb)
{
    IRIter it;
    for (IR * x = iterInit(exp, it); x != nullptr; x = iterNext(it)) {
        if (x->isMemoryRefNonPR()) {
            findAndSetDomLiveInDef(x, startir, startbb);
        }
    }
}


//Note DOM info must be available.
void MDSSAMgr::findAndSetDomLiveInDef(IR * exp, IR const* startir,
                                      IRBB const* startbb)
{
    ASSERT0(startir == nullptr || startir->getBB() == startbb);
    ASSERT0(exp->is_exp() && exp->isMemoryRefNonPR());
    MDSSAInfo * info = genMDSSAInfo(exp);
    ASSERT0(info);
    List<VMD*> newvmds;
    MD const* must = exp->getMustRef();
    if (must != nullptr) {
        VMD * newvmd = findDomLiveInDefFrom(must->id(), startir, startbb);
        if (newvmd != nullptr) {
            newvmds.append_tail(newvmd);
        } else {
            //Need add init-version VMD to represent the existence of MD.
            newvmds.append_tail(genInitVersionVMD(must->id()));
        }
    }
    MDSet const* may = exp->getMayRef();
    if (may != nullptr) {
        MDSetIter it;
        for (BSIdx i = may->get_first(&it); i != BS_UNDEF;
             i = may->get_next(i, &it)) {
            VMD * newvmd = findDomLiveInDefFrom(i, startir, startbb);
            if (newvmd != nullptr) {
                newvmds.append_tail(newvmd);
            } else {
                //Need add init-version VMD to represent the existence of MD.
                newvmds.append_tail(genInitVersionVMD(i));
            }
        }
    }
    removeExpFromAllVOpnd(exp);
    info->cleanVOpndSet(getUseDefMgr());
    for (VMD * v = newvmds.get_head();
         v != nullptr; v = newvmds.get_next()) {
        if (v->version() != MDSSA_INIT_VERSION) {
            v->addUse(exp);
        }
        info->addVOpnd(v, getUseDefMgr());
    }
}


//DU chain operation.
//Change Use expression from 'olduse' to 'newuse'.
//olduse: single source expression.
//newuse: single target expression.
//e.g: Change MDSSA DU chain DEF->olduse to DEF->newuse.
void MDSSAMgr::changeUse(IR * olduse, IR * newuse)
{
    ASSERT0(olduse && newuse && olduse->is_exp() && newuse->is_exp());
    ASSERTN(olduse != newuse, ("redundant operation"));
    ASSERT0(olduse->isMemoryRefNonPR() && newuse->isMemoryRefNonPR());
    MDSSAInfo * oldinfo = getMDSSAInfoIfAny(olduse);
    ASSERT0(oldinfo);
    MDSSAInfo * newinfo = copyAndAddMDSSAOcc(newuse, oldinfo);
    ASSERT0(newinfo);
    removeExpFromAllVOpnd(olduse);
}


//DU chain operation.
//Change Use expression from 'olduse' to 'newuse'.
//olduse: source expression as tree root.
//newuse: target expression as tree root.
//e.g: Change MDSSA DU chain DEF->olduse to DEF->newuse.
void MDSSAMgr::changeUseForTree(IR * olduse, IR * newuse)
{
    ASSERT0(olduse && newuse && olduse->is_exp() && newuse->is_exp());
    ASSERTN(olduse != newuse, ("redundant operation"));
    ASSERT0(olduse->isMemoryRefNonPR() && newuse->isMemoryRefNonPR());
    addMDSSAOccForTree(newuse, olduse);
    removeMDSSAOccForTree(olduse);
}


//Coalesce DU chain, actually the version of MD, from 'src' to 'tgt'.
//'src' and 'tgt' refered the same MD.
//This function replace definition of USE of src to tgt's defintion.
//There is always removeStmt() following the function call.
//'src' and 'tgt' is the form of copy operation.
//e.g: p0 =...
//     p1 = p0
//     ...= p1
//=> after coalescing, p1 is src, p0 is tgt
//     p0 = ...
//     ------ //removed
//     ... = p0
void MDSSAMgr::coalesceDUChain(IR const* src, IR const* tgt)
{
    ASSERT0(src && tgt);
    ASSERT0(src->is_stmt() && tgt->is_exp() && tgt->getStmt() == src);
    MDSSAInfo * src_mdssainfo = getMDSSAInfoIfAny(src);
    MDSSAInfo * tgt_mdssainfo = getMDSSAInfoIfAny(tgt);
    ASSERT0(src_mdssainfo && tgt_mdssainfo);
    VOpndSetIter iter1 = nullptr;
    for (BSIdx i = src_mdssainfo->getVOpndSet()->get_first(&iter1);
         i != BS_UNDEF; i = src_mdssainfo->getVOpndSet()->get_next(i, &iter1)) {
        VMD * src_vopnd = (VMD*)getUseDefMgr()->getVOpnd(i);
        ASSERT0(src_vopnd && src_vopnd->is_md());

        //Find the MD in tgt's vopnd-set which has same mdid with src's
        //except the distinct version.
        //e.g: src has MD6Vx, find MD6Vy in tgt vopnd set.
        VOpndSetIter iter2 = nullptr;
        VMD * tgt_vopnd = nullptr;
        for (BSIdx j = tgt_mdssainfo->getVOpndSet()->get_first(&iter2);
             j != BS_UNDEF;
             j = tgt_mdssainfo->getVOpndSet()->get_next(j, &iter2)) {
            VMD * t = (VMD*)getUseDefMgr()->getVOpnd(j);
            ASSERT0(t && t->is_md());
            if (t->mdid() == src_vopnd->mdid()) {
                ASSERT0(t != src_vopnd);
                tgt_vopnd = t;
                break;
            }
        }

        if (tgt_vopnd == nullptr) {
            //Not find related tgt VMD that has same MD to src VMD.
            //Just skip it because there is no version MD to coalesce.
            continue;
        }

        ASSERTN(tgt_vopnd->version() != src_vopnd->version(),
                ("DEF and USE reference same version MD"));
        //Replace the USE of src to tgt.
        replaceVOpndForAllUse(tgt_vopnd, src_vopnd);
    }
    //Do NOT clean VOpndSet of src here because the subsequent removeStmt()
    //need VOpndSet information.
    //src_mdssainfo->cleanVOpndSet(getUseDefMgr());
}


//Add ir to given mdssainfo as an USE.
//ir: occurence to be added.
//mdssainfo: add ir to the UseSet of VOpnd that recorded in 'mdssainfo'.
//Note mdssainfo must be unique for each IR.
void MDSSAMgr::addUseToMDSSAInfo(IR const* ir, MDSSAInfo * mdssainfo)
{
    ASSERT0(mdssainfo);
    mdssainfo->addUse(ir, getUseDefMgr());
}


//The function copy MDSSAInfo from 'src' to ir. Then add ir as an USE of the
//new MDSSAInfo, and return the new MDSSAInfo.
MDSSAInfo * MDSSAMgr::copyAndAddMDSSAOcc(IR * ir, MDSSAInfo const* src)
{
    //User may retain IRTree in transformation, however the MDSSAInfo has been
    //removed. Thus for the sake of convenient, permit src to be empty here.
    //ASSERT0(!src->readVOpndSet().is_empty());
    MDSSAInfo * irmdssainfo = genMDSSAInfo(ir);
    if (irmdssainfo != src) {
        //Set ir references VOpnd as if src has.
        irmdssainfo->copy(*src, getUseDefMgr());
    }
    //Set VOpnd references ir as occurrence.
    addUseToMDSSAInfo(ir, irmdssainfo);
    return irmdssainfo;
}


void MDSSAMgr::copyMDSSAInfo(IR * tgt, IR const* src)
{
    ASSERT0(MDSSAMgr::hasMDSSAInfo(tgt));
    MDSSAInfo * tgtinfo = genMDSSAInfo(tgt);
    MDSSAInfo const* srcinfo = MDSSAMgr::getMDSSAInfoIfAny(src);
    ASSERT0(srcinfo);
    tgtinfo->copy(*srcinfo, getUseDefMgr());
}


//The function copy MDSSAInfo from tree 'src' to tree tgt.
//Note src and tgt must be isomorphic.
void MDSSAMgr::copyMDSSAInfoForTree(IR * tgt, IR const* src)
{
    IRIter it;
    ConstIRIter it2;
    IR * x;
    IR const* y;
    ASSERT0(tgt->isIRIsomo(src, true));
    for (x = iterInit(tgt, it), y = iterInitC(src, it2);
         x != nullptr; x = iterNext(it), y = iterNextC(it2)) {
        if (x->isMemoryRefNonPR()) {
            copyMDSSAInfo(x, y);
        }
    }
}


void MDSSAMgr::addStmtToMDSSAMgr(IR * ir, IR const* ref)
{
    ASSERT0(0);//TODO:
}


//Build DU chain from 'def' to 'exp'.
//The function will add VOpnd of phi to 'exp'.
void MDSSAMgr::buildDUChain(MDDef const* def, MOD IR * exp)
{
    ASSERT0(exp->is_exp());
    ASSERTN(def->getResult(), ("does not have occurrence"));
    def->getResult()->addUse(exp);
    MDSSAInfo * info = genMDSSAInfo(exp);
    ASSERT0(info);
    //Note the function does NOT check whether the remainder VOpnd in info
    //is conflict with 'def'. Users have to guarantee it by themself.
    info->addVOpnd(def->getResult(), getUseDefMgr());
}


//Add occurence to each VOpnd in mdssainfo.
//ir: occurence to be added.
//ref: the reference that is isomorphic to 'ir'.
//     It is used to retrieve MDSSAInfo.
void MDSSAMgr::addMDSSAOccForTree(IR * ir, IR const* ref)
{
    ASSERT0(ir->isIREqual(ref, false));
    if (ir->is_stmt()) {
        addStmtToMDSSAMgr(ir, ref);
    } else {
        MDSSAInfo * mdssainfo = getMDSSAInfoIfAny(ref);
        if (mdssainfo != nullptr) {
            copyAndAddMDSSAOcc(ir, mdssainfo);
        } else {
            ASSERT0(!hasMDSSAInfo(ref));
        }
    }
    for (UINT i = 0; i < IR_MAX_KID_NUM(ir); i++) {
        IR const* refkid = ref->getKid(i);
        IR * x = ir->getKid(i);
        for (; x != nullptr; x = x->get_next(), refkid = refkid->get_next()) {
            ASSERTN(refkid, ("ir is not isomorphic to ref"));
            addMDSSAOccForTree(x, refkid);
        }
    }
}


void MDSSAMgr::removeMDSSAOccForTree(IR const* ir)
{
    if (hasMDSSAInfo(ir)) {
        if (ir->is_stmt()) {
            removeStmtFromMDSSAMgr(ir);
        } else {
            removeExpFromAllVOpnd(ir);
        }
    }
    for (UINT i = 0; i < IR_MAX_KID_NUM(ir); i++) {
        for (IR * x = ir->getKid(i); x != nullptr; x = x->get_next()) {
            removeMDSSAOccForTree(x);
        }
    }
}


//Remove DU chain from 'def' to 'exp'.
//The function will add VOpnd of phi to 'exp'.
void MDSSAMgr::removeDUChain(MDDef const* def, IR * exp)
{
    ASSERT0(exp->is_exp());
    def->getResult()->removeUse(exp);
    MDSSAInfo * info = genMDSSAInfo(exp);
    ASSERT0(info);
    //Note the function does NOT check whether the remainder VOpnd in info
    //is conflict with 'def'. Users have to guarantee it by themself.
    info->removeVOpnd(def->getResult(), getUseDefMgr());
}


//Remove DU chain if exist in between 'stmt' and 'exp'.
//The function will remove 'exp' from occurence set.
//stmt: IR stmt that may be DEF of 'exp'.
//exp: IR expression to be removed.
void MDSSAMgr::removeDUChain(IR const* stmt, IR const* exp)
{
    ASSERT0(stmt && exp && stmt->is_stmt() && exp->is_exp());
    MDSSAInfo * mdssainfo = getMDSSAInfoIfAny(exp);
    if (mdssainfo == nullptr) { return; }
    VOpndSetIter iter = nullptr;
    INT next_i = -1;
    for (BSIdx i = mdssainfo->getVOpndSet()->get_first(&iter);
         i != BS_UNDEF; i = next_i) {
        next_i = mdssainfo->getVOpndSet()->get_next(i, &iter);
        VMD * vopnd = (VMD*)getUseDefMgr()->getVOpnd(i);
        ASSERT0(vopnd && vopnd->is_md());
        if (vopnd->getDef() == nullptr) {
            ASSERTN(vopnd->version() == MDSSA_INIT_VERSION,
                    ("Only zero version MD has no DEF"));
            continue;
        }
        if (vopnd->getDef()->getOcc() != stmt) { continue; }
        vopnd->removeUse(exp);
        mdssainfo->removeVOpnd(vopnd, getUseDefMgr());
    }
}


//Remove all virtual USEs of 'stmt'.
//stmt will have not any USE expression when function returned.
void MDSSAMgr::removeAllUse(IR const* stmt)
{
    ASSERT0(stmt && stmt->is_stmt());
    MDSSAInfo * mdssainfo = getMDSSAInfoIfAny(stmt);
    if (mdssainfo == nullptr) { return; }

    VOpndSetIter iter = nullptr;
    INT next_i = -1;
    for (BSIdx i = mdssainfo->getVOpndSet()->get_first(&iter);
         i != BS_UNDEF; i = next_i) {
        next_i = mdssainfo->getVOpndSet()->get_next(i, &iter);
        VMD * vopnd = (VMD*)getUseDefMgr()->getVOpnd(i);
        ASSERT0(vopnd && vopnd->is_md());
        if (vopnd->getDef() == nullptr) {
            ASSERTN(vopnd->version() == MDSSA_INIT_VERSION,
                    ("Only zero version MD has no DEF"));
            continue;
        }
        if (vopnd->getDef()->getOcc() != stmt) { continue; }

        //Iterate all USEs.
        removeVOpndForAllUse(vopnd);
        mdssainfo->removeVOpnd(vopnd, getUseDefMgr());
    }
}


//The function changes VOpnd of 'from' to 'to', for each elements in 'from'
//UseSet.
//Note all elements in UseSet of 'from' will be removed.
void MDSSAMgr::replaceVOpndForAllUse(MOD VMD * to, MOD VMD * from)
{
    ASSERT0(to->is_md() && from->is_md());
    //Replace the USE of src to tgt.
    VMD::UseSetIter it;
    for (INT k = from->getUseSet()->get_first(it);
         !it.end(); k = from->getUseSet()->get_next(it)) {
        IR const* use = (IR*)m_rg->getIR(k);
        MDSSAInfo * use_mdssainfo = getMDSSAInfoIfAny(use);
        ASSERTN(use_mdssainfo, ("use miss MDSSAInfo"));
        use_mdssainfo->removeVOpnd(from, getUseDefMgr());
        use_mdssainfo->addVOpnd(to, getUseDefMgr());
        to->addUse(use);
    }
    from->cleanUseSet();
}


//id: input ID.
//ssainfo: MDSSAInfo of id.
//olddef: old DEF of id.
void MDSSAMgr::findNewDefForID(IR * id, MDSSAInfo * ssainfo, MDDef * olddef)
{
    ASSERT0(id->is_id());
    if (ID_phi(id) == nullptr) { return; }

    ASSERT0(getMDSSAInfoIfAny(id) == ssainfo);
    //CASE: to avoid assertions that raised by verify() which is used
    //to guanrantee operand of MDPhi is not NULL, replace the removed
    //vopnd of operand with initial-versiond vopnd.
    ASSERT0(olddef);
    VMD * oldres = olddef->getResult();
    MDIdx defmdid = oldres->mdid();
    ASSERT0(defmdid != MD_UNDEF);
    IR const* start = nullptr;
    VMD * livein = nullptr;
    if (getOptCtx()->is_dom_valid()) {
        //DOM info must be available.
        if (olddef->is_phi()) {
            livein = findDomLiveInDefFromIDomOf(olddef->getBB(), defmdid);
        } else {
            start = olddef->getBB()->getPrevIR(olddef->getOcc());
            livein = findDomLiveInDefFrom(defmdid, start, olddef->getBB());
        }
    }
    if (livein == nullptr || livein == oldres) {
        livein = genInitVersionVMD(defmdid);
        ssainfo->addVOpnd(livein, getUseDefMgr());
        return;
    }
    buildDUChain(livein->getDef(), id);
}


//The function remove 'vopnd' from MDSSAInfo of each ir its UseSet.
//Note the UseSet will be clean.
void MDSSAMgr::removeVOpndForAllUse(MOD VMD * vopnd)
{
    ASSERT0(vopnd && vopnd->is_md());
    VMD::UseSet * useset = vopnd->getUseSet();
    VMD::UseSetIter it;
    MDDef * vopnddef = vopnd->getDef();
    for (BSIdx i = useset->get_first(it); !it.end(); i = useset->get_next(it)) {
        IR * use = m_rg->getIR(i);
        ASSERT0(use && (use->isMemoryRef() || use->is_id()));
        MDSSAInfo * mdssainfo = getMDSSAInfoIfAny(use);
        ASSERT0(mdssainfo);
        if (use->is_id()) {
            //An individual ID can NOT represent multiple versioned MD, thus
            //the VOpnd of ID must be unique.
            mdssainfo->cleanVOpndSet(getUseDefMgr());
        } else {
            mdssainfo->removeVOpnd(vopnd, getUseDefMgr());
        }
        if (!use->is_id()) { continue; }
        //Note VOpndSet of 'vopnd' may be empty after the removing.
        //It does not happen when MDSSA just constructed. The USE that
        //without real-DEF will have a virtual-DEF that version is 0.
        //During some increment-maintaining of MDSSA, 'vopnd' may be removed,
        //just like what current function does.
        //This means the current USE, 'use', does not have real-DEF stmt, the
        //value of 'use' always coming from parameter of global value.
        findNewDefForID(use, mdssainfo, vopnddef);
    }
    vopnd->cleanUseSet();
}


//The function handle the DU chain and cut off the DU chain between MDPHI
//and its USE expression.
//Remove 'phi' from its use's vopnd-list.
//e.g:u1, u2 are its use expressions.
//cut off the DU chain between def->u1 and def->u2.
void MDSSAMgr::removeDefFromUseSet(MDPhi const* phi)
{
    removeVOpndForAllUse(phi->getResult());
}


//The function removes 'phi' from MDSSAMgr.
//It will cut off DU chain of each phi's operands, and the DU chain of phi
//itself as well, then free all resource.
//phi: to be removed.
//prev: previous DEF that is used to maintain Def-Def chain, and it can be
//      NULL if there is no previous DEF.
void MDSSAMgr::removePhiFromMDSSAMgr(MDPhi * phi, MDDef * prev)
{
    ASSERT0(phi && phi->is_phi());
    for (IR * opnd = phi->getOpndList(); opnd != nullptr;
         opnd = opnd->get_next()) {
        //Update the MDSSAInfo for each phi opnd.
        removeMDSSAOccForTree(opnd);
    }

    VMD * vopnd = phi->getResult();
    //Note VOpnd should be removed first of all because it use DD-Chain.
    removeVOpndForAllUse(vopnd);
    removePhiFromDDChain(phi, prev);
    m_rg->freeIRTreeList(phi->getOpndList());
    MDPHI_opnd_list(phi) = nullptr;
    removeVMD(vopnd);
}


//The function remove all MDPhis in 'bb'.
//Note caller should guarrantee phi is useless and removable.
void MDSSAMgr::removePhiFromBB(IRBB * bb)
{
    MDPhiList * philist = getPhiList(bb);
    if (philist == nullptr) { return; }
    MDPhiListIter next;
    for (MDPhiListIter it = philist->get_head();
         it != philist->end(); it = next) {
        next = philist->get_next(it);
        MDPhi * phi = it->val();
        ASSERT0(phi && phi->is_phi());
        removePhiFromMDSSAMgr(phi, nullptr);
        philist->remove_head();
    }
    //Do NOT free PhiList here even if it is empty because philist is
    //allocated from memory pool.
}


//Remove given IR expression from UseSet of each vopnd in MDSSAInfo.
//Note current MDSSAInfo is the SSA info of 'exp', the VOpndSet will be
//emtpy when exp is removed from all VOpnd's useset.
//exp: IR expression to be removed.
//NOTE: the function only process exp itself.
void MDSSAMgr::removeExpFromAllVOpnd(IR const* exp)
{
    ASSERT0(exp && exp->is_exp() && hasMDSSAInfo(exp));
    MDSSAInfo * expssainfo = UseDefMgr::getMDSSAInfo(exp);
    ASSERT0(expssainfo);
    VOpndSetIter it = nullptr;
    VOpndSet * vopndset = expssainfo->getVOpndSet();
    UseDefMgr * mgr = getUseDefMgr();
    for (BSIdx i = vopndset->get_first(&it);
         i != BS_UNDEF; i = vopndset->get_next(i, &it)) {
        VMD * vopnd = (VMD*)mgr->getVOpnd(i);
        ASSERT0(vopnd && vopnd->is_md());
        vopnd->removeUse(exp);
    }
    expssainfo->cleanVOpndSet(mgr);
}


//Remove Use-Def chain.
//exp: the expression to be removed.
//e.g: ir = ...
//    = ir //S1
//If S1 will be deleted, ir should be removed from its useset in MDSSAInfo.
//NOTE: the function only process exp itself.
void MDSSAMgr::removeUse(IR const* exp)
{
    if (hasMDSSAInfo(exp)) {
        removeExpFromAllVOpnd(exp);
    }
}


//Remove all VMD in set from MDSSAMgr. The function will clean all information
//about these VMDs.
void MDSSAMgr::removeVMDInSet(VOpndSet const& set)
{
    VOpndSetIter it = nullptr;
    for (BSIdx i = set.get_first(&it); i != BS_UNDEF;
         i = set.get_next(i, &it)) {
        VMD * vmd = (VMD*)getUseDefMgr()->getVOpnd(i);
        ASSERT0(vmd && vmd->is_md());
        //The function only remove VMD out of MDSSAMgr.
        removeVMD(vmd);
    }
}


//Remove the MDSSAInfo related info of 'stmt' from MDSSAMgr.
//The MDSSAInfo includes Def and UseSet info.
//Note this function only handle stmt's MDSSAInfo, thus it will not
//process its RHS expression.
void MDSSAMgr::removeStmtFromMDSSAMgr(IR const* stmt)
{
    ASSERT0(stmt && stmt->is_stmt());
    MDSSAInfo * stmtmdssainfo = getMDSSAInfoIfAny(stmt);
    ASSERT0(stmtmdssainfo);
    VOpndSetIter iter = nullptr;
    INT next_i = -1;
    for (BSIdx i = stmtmdssainfo->getVOpndSet()->get_first(&iter);
         i != BS_UNDEF; i = next_i) {
        next_i = stmtmdssainfo->getVOpndSet()->get_next(i, &iter);
        VMD * vopnd = (VMD*)getUseDefMgr()->getVOpnd(i);
        ASSERT0(vopnd && vopnd->is_md());
        if (vopnd->getDef() == nullptr) {
            ASSERTN(vopnd->version() == MDSSA_INIT_VERSION,
                    ("Only zero version MD has no DEF"));
            continue;
        }
        ASSERT0(vopnd->getDef()->getOcc() == stmt);

        //Iterate all USEs and remove 'vopnd' from its MDSSAInfo.
        removeVOpndForAllUse(vopnd);

        //Iterate Def-Def chain.
        removeDefFromDDChain(vopnd->getDef());

        //Remove 'vopnd' from current stmt.
        stmtmdssainfo->removeVOpnd(vopnd, getUseDefMgr());

        //Clear DEF info of 'vopnd'
        VMD_def(vopnd) = nullptr;

        //Because all info has been eliminated, the vopnd is out
        //of date and can be removed from MDSSAMgr.
        removeVMD(vopnd);
    }
}


//Union successors in NextSet from 'from' to 'to'.
void MDSSAMgr::unionSuccessors(MDDef const* from, MDDef const* to)
{
    if (from->is_phi()) {
        if (to->getNextSet() == nullptr) {
            if (from->getNextSet() != nullptr) {
                //Note if MDDef indicates PHI, it does not have Previous DEF,
                //because PHI has multiple Previous DEFs rather than single DEF.
                MDDEF_nextset(to) = m_usedef_mgr.allocMDDefSet();
                to->getNextSet()->bunion(*from->getNextSet(), *getSBSMgr());
            }
            return;
        }
        if (from->getNextSet() != nullptr) {
            //Note if MDDef indicates PHI, it does not have Previous DEF,
            //because PHI has multiple Previous DEFs rather than single DEF.
            to->getNextSet()->bunion(*from->getNextSet(), *getSBSMgr());
        }
        return;
    }
    if (to->getNextSet() == nullptr || from->getNextSet() == nullptr) {
        return;
    }
    to->getNextSet()->bunion(*from->getNextSet(), *getSBSMgr());
}


//Remove MDDef from Def-Def chain.
//Note the function does not deal with MDSSAInfo of IR occurrence, and just
//process Def-Def chain that built on MDDef.
//mddef: will be removed from Def-Def chain, and be modified as well.
//prev: previous Def to mddef, and will be modified.
//e.g:D1->D2
//     |->D3
//     |  |->D5
//     |  |->D6
//     |->D4
//  where D1 is predecessor of D2, D3 and D4; D3 is predecssor of D5, D6.
//  After remove D3:
//e.g:D1->D2
//     |->D5
//     |->D6
//     |->D4
//  where D1 is predecessor of D2, D5, D6, D4.
void MDSSAMgr::removeDefFromDDChainHelper(MDDef * mddef, MDDef * prev)
{
    ASSERT0(mddef);
    if (mddef->getNextSet() == nullptr) {
        if (prev != nullptr) {
            if (prev->getNextSet() != nullptr) {
                //Cutoff def-def chain between 'mddef' to its predecessor.
                prev->getNextSet()->remove(mddef, *getSBSMgr());
            } else {
                //Note if mddef indicates PHI, it does not have Previous DEF,
                //because PHI has multiple Previous DEFs rather than single
                //DEF.
                ASSERT0(mddef->is_phi());
            }
        }
        MDDEF_prev(mddef) = nullptr;
        getUseDefMgr()->removeMDDef(mddef);
        return;
    }

    if (prev != nullptr) {
        //CASE: Be careful that 'prev' should not belong to the NextSet of
        //mddef', otherwise the union operation of prev and mddef's succ DEF
        //will construct a cycle in Def-Def chain, which is illegal.
        //e.g: for (i = 0; i < 10; i++) {;}, where i's MD is MD5.
        //  MD5V2 <-- PHI(MD5V--, MD5V3)
        //  MD5V3 <-- MD5V2 + 1
        // If we regard MD5V3 as the common-def, PHI is 'mddef', a cycle
        // will appeared.
        ASSERTN(!mddef->getNextSet()->find(prev),
                ("prev is actually the NEXT of mddef"));

        //Union successors of 'mddef' to its predecessor's next-set.
        unionSuccessors(mddef, prev);
        if (prev->getNextSet() != nullptr) {
            prev->getNextSet()->remove(mddef, *getSBSMgr());
        }
    }

    //Update successor's predecesor.
    MDDefSetIter nit = nullptr;
    for (INT w = mddef->getNextSet()->get_first(&nit);
         w >= 0; w = mddef->getNextSet()->get_next(w, &nit)) {
        MDDef const* use = getUseDefMgr()->getMDDef(w);
        ASSERTN(use->getPrev() == mddef, ("insanity DD chain"));
        MDDEF_prev(use) = prev;
    }
    MDDEF_prev(mddef) = nullptr;
    mddef->cleanNextSet(getUseDefMgr());
    getUseDefMgr()->removeMDDef(mddef);
}


//Remove MDDef from Def-Def chain.
//mddef: will be removed from Def-Def chain, and be modified as well.
//e.g:D1->D2
//     |->D3
//     |  |->D5
//     |  |->D6
//     |->D4
//  where D1 is predecessor of D2, D3 and D4; D3 is predecssor of D5, D6.
//  After remove D3:
//e.g:D1->D2
//     |->D5
//     |->D6
//     |->D4
//  where D1 is predecessor of D2, D5, D6, D4.
void MDSSAMgr::removeDefFromDDChain(MDDef * mddef)
{
    ASSERT0(mddef);
    MDDef * prev = mddef->getPrev();
    removeDefFromDDChainHelper(mddef, prev);
}


//Remove PHI that without any USE.
//Return true if any PHI was removed, otherwise return false.
bool MDSSAMgr::removePhiNoUse(MDPhi * phi)
{
    ASSERT0(phi && phi->is_phi() && phi->getBB());
    VMD * vopnd = phi->getResult();
    ASSERT0(vopnd && vopnd->is_md());
    ASSERT0(phi == vopnd->getDef());
    if (vopnd->hasUse()) { return false; }

    MD const* phi_result_md = vopnd->getMD(m_md_sys);
    ASSERT0(phi_result_md);

    if (!phi_result_md->is_exact()) {
        //Inexact MD indicates a non-killing-def, and the phi which is
        //non-killing-def always used to pass through DEF in def-chain.
        //Thus the phi is the connection point to other DEF, and can not be
        //removed.
        return false;
    }

    if (phi->getNextSet() != nullptr) {
        MDDefSetIter nit = nullptr;
        for (BSIdx i = phi->getNextSet()->get_first(&nit);
             i != BS_UNDEF; i = phi->getNextSet()->get_next(i, &nit)) {
            MDDef const* next_def = getUseDefMgr()->getMDDef(i);
            ASSERTN(next_def && next_def->getPrev() == phi,
                    ("insanity DD chain"));
            MD const* next_result_md = next_def->getResultMD(m_md_sys);
            if (!next_result_md->is_exact_cover(phi_result_md)) {
                //There are DEFs represented by phi and its operand may pass
                //through 'def'. These DEFs belong to the MayDef set of
                //followed USE.
                return false;
            }
        }
    }

    removePhiFromMDSSAMgr(phi, phi->getPrev());
    return true;
}


//Check each USE|DEF of ir, remove the expired one which is not reference
//the memory any more that ir defined.
//Return true if DU changed.
bool MDSSAMgr::removeExpiredDU(IR const* ir)
{
    //TODO: Do NOT attempt to remove if not found reference MD at USE point
    //which should correspond to vopnd->md().
    //Some transformation, such as IR Refinement, may change
    //the USE's MDSet. This might lead to the inaccurate and
    //redundant MDSSA DU Chain. So the MDSSA DU Chain is conservative,
    //but the correctness of MDSSA dependence is garanteed.
    //e.g:
    //  ist:*<4> id:18 //:MD11, MD12, MD14, MD15
    //    lda: *<4> 'r'
    //    ild: i32 //MMD13: MD16
    //      ld: *<4> 'q' //MMD18
    //=> After IR combination: ist(lda) transformed to st
    //  st:*<4> 'r' //MMD12
    //    ild : i32 //MMD13 : MD16
    //      ld : *<4> 'q'    //MMD18
    //ist transformed to st. This reduce referenced MDSet to a single MD
    //as well.
    return false;
}


//Remove MDDef from Def-Def chain.
//phi: will be removed from Def-Def chain, and be modified as well.
//prev: designated previous DEF.
//e.g:D1<->D2
//     |<->D3
//     |   |<->D5
//     |   |<->D6
//     |->D4
//  where predecessor of D3 is D1, successors of D3 are D5, D6
//  After remove D3:
//    D1<->D2
//     |<->D5
//     |<->D6
//     |<->D4
//    D3<->nullptr
//  where predecessor of D5, D6 is D1, successor of D1 includes D5, D6.
void MDSSAMgr::removePhiFromDDChain(MDPhi * phi, MDDef * prev)
{
    ASSERT0(phi && phi->is_phi());
    removeDefFromDDChainHelper(phi, prev);
}


//This function perform SSA destruction via scanning BB in sequential order.
void MDSSAMgr::destruction(OptCtx * oc)
{
    BBList * bblst = m_rg->getBBList();
    if (bblst->get_elem_count() == 0) { return; }
    UINT bbnum = bblst->get_elem_count();
    BBListIter bbct;
    for (bblst->get_head(&bbct);
         bbct != bblst->end(); bbct = bblst->get_next(bbct)) {
        ASSERT0(bbct->val());
        destructBBSSAInfo(bbct->val());
    }
    if (bbnum != bblst->get_elem_count()) {
        oc->setInvalidIfCFGChanged();
    }
    m_is_valid = false;
}


//wl: is an optional parameter to record BB which expected to deal with.
//    It is a work-list that is used to drive iterative collection and
//    elimination of redundant PHI elmination.
//Return true if phi removed.
bool MDSSAMgr::removePhiHasCommonDef(List<IRBB*> * wl, MDPhi * phi)
{
    ASSERT0(phi);
    VMD * common_def = nullptr;
    if (!doOpndHaveSameDef(phi, &common_def)) {
        return false;
    }

    //commond_def may be NULL.
    //e.g:Phi: MD10V3 <- MD10V0
    //  The only operand of PHI is livein MD, thus the
    //  commond_def is NULL.
    for (IR * opnd = phi->getOpndList();
         opnd != nullptr; opnd = opnd->get_next()) {
        VMD * vopnd = phi->getOpndVMD(opnd, &m_usedef_mgr);
        //if (vopnd == nullptr) {
        //    //VOpnd may have been removed from MDSSAMgr, thus the VOpnd that
        //    //corresponding to current ID is NULL.
        //    continue;
        //}
        ASSERTN(vopnd, ("init-version should be placed if vopnd removed"));

        ASSERT0(vopnd->is_md());
        if (wl != nullptr && vopnd->getDef() != nullptr) {
            ASSERT0(vopnd->getDef()->getBB());
            wl->append_tail(vopnd->getDef()->getBB());
        }
    }

    if (common_def != phi->getResult() && common_def != nullptr) {
        //Change DEF from PHI to common_def to UseList.
        ASSERT0(common_def->is_md());

        //CASE: Be careful that 'prev' should not belong to the NextSet of
        //mddef', otherwise the union operation of prev and mddef's succ DEF
        //will construct a cycle in Def-Def chain, which is illegal.
        //e.g: for (i = 0; i < 10; i++) {;}, where i's MD is MD5.
        //  MD5V2 <-- PHI(MD5V--, MD5V3)
        //  MD5V3 <-- MD5V2 + 1
        // If we regard MD5V3 as the common-def, PHI as the 'mddef', a cycle
        // will appeared.
        MDDef * prev = common_def->getDef();
        ASSERT0(prev);
        if (phi->isInNextSet(prev, getUseDefMgr())) {
            prev = nullptr;
        }
        removePhiFromMDSSAMgr(phi, prev);
        return true;
    }

    removePhiFromMDSSAMgr(phi, nullptr);
    return true;
}


//wl: is an optional parameter to record BB which expected to deal with.
//    It is a work-list that is used to drive iterative collection and
//    elimination of redundant PHI elmination.
//Return true if phi removed.
bool MDSSAMgr::removePhiHasNoValidDef(List<IRBB*> * wl, MDPhi * phi)
{
    ASSERT0(phi);
    if (doOpndHaveValidDef(phi)) {
        return false;
    }

    for (IR * opnd = phi->getOpndList();
         opnd != nullptr; opnd = opnd->get_next()) {
        VMD * vopnd = phi->getOpndVMD(opnd, &m_usedef_mgr);
        //if (vopnd == nullptr) {
        //    //VOpnd may be have been removed from MDSSAMgr, thus the VOpnd
        //    //that corresponding to current ID is NULL.
        //    continue;
        //}
        ASSERTN(vopnd, ("init-version should be placed if vopnd removed"));

        ASSERT0(vopnd->is_md());
        if (wl != nullptr && vopnd->getDef() != nullptr) {
            ASSERT0(vopnd->getDef()->getBB());
            wl->append_tail(vopnd->getDef()->getBB());
        }
    }

    removePhiFromMDSSAMgr(phi, phi->getPrev());
    return true;
}


//wl: work list for temporary used.
//Return true if any PHI was removed.
bool MDSSAMgr::prunePhiForBB(IRBB const* bb, List<IRBB*> * wl)
{
    ASSERT0(bb);
    MDPhiList * philist = getPhiList(bb);
    if (philist == nullptr) { return false; }
    bool remove = false;
    MDPhiListIter prev = nullptr;
    MDPhiListIter next = nullptr;
    for (MDPhiListIter it = philist->get_head();
         it != philist->end(); it = next) {
        next = philist->get_next(it);
        MDPhi * phi = it->val();
        ASSERT0(phi);
        if (removePhiHasCommonDef(wl, phi)) {
            remove = true;
            philist->remove(prev, it);
            continue;
        }

        if (removePhiHasNoValidDef(wl, phi)) {
            remove = true;
            philist->remove(prev, it);
            continue;
        }

        //Remove PHI that without any USE.
        //TBD: PHI that without USE could not removed in some case:
        //e.g:for (...) { S1<---will insert PHI of x
        //      x[i]=0;   S2
        //    }
        //    x[j]=...;   S3
        //  where x[j] is NOT killing-def of x[i].
        //  ----
        //  MDSSAMgr inserted PHI of x, it is the previous DEF of S3.
        //  If we remove PHI at S1, S2 will lost USE at opnd of PHI, thus
        //  will be removed finally. It is incorrect.
        if (removePhiNoUse(phi)) {
            remove = true;
            philist->remove(prev, it);
            continue;
        }
        prev = it;
    }
    return remove;
}


//Remove redundant phi.
//Return true if any PHI was removed.
bool MDSSAMgr::removeRedundantPhi()
{
    List<IRBB*> wl;
    return prunePhi(wl);
}


//Remove redundant phi.
//wl: work list for temporary used.
//Return true if any PHI was removed.
bool MDSSAMgr::prunePhi(List<IRBB*> & wl)
{
    START_TIMER(t, "MDSSA: Prune phi");
    BBList * bblst = m_rg->getBBList();
    BBListIter ct;

    wl.clean();
    for (bblst->get_head(&ct); ct != bblst->end(); ct = bblst->get_next(ct)) {
        IRBB * bb = ct->val();
        ASSERT0(bb);
        wl.append_tail(bb);
    }

    bool remove = false;
    IRBB * bb = nullptr;
    while ((bb = wl.remove_head()) != nullptr) {
        remove |= prunePhiForBB(bb, &wl);
    }
    END_TIMER(t, "MDSSA: Prune phi");
    return remove;
}


static void iterDefCHelperPhi(MDDef const* def, IR const* use,
                              MDSSAMgr const* mgr, OUT ConstMDDefIter & it)
{
    ASSERT0(def->is_phi());
    //Iter phi's opnd
    for (IR const* opnd = MDPHI_opnd_list(def);
         opnd != nullptr; opnd = opnd->get_next()) {
        if (opnd->is_const()) {
            //CONST does not have VMD info.
            continue;
        }
        VMD const* vmd = ((MDPhi const*)def)->getOpndVMD(opnd,
            const_cast<MDSSAMgr*>(mgr)->getUseDefMgr());
       if (vmd == nullptr) {
            //Note VOpndSet of 'opnd' may be empty after some optimization.
            //It does not happen when MDSSA just constructed. The USE that
            //without real-DEF will have a virtual-DEF that version is 0.
            //During some increment-maintaining of MDSSA, VOpnd may be removed,
            //and VOpndSet become empty.
            //This means the current USE, 'opnd', does not have real-DEF stmt,
            //the value of 'opnd' always coming from parameter of global value.
            //The ID of PHI should not be removed, because it is be regarded
            //as a place-holder of PHI, and the place-holder indicates the
            //position of related predecessor of current BB of PHI in CFG.
            continue;
        }

        MDDef * vmd_tdef = vmd->getDef();
        if (vmd_tdef == nullptr || vmd_tdef == def ||
            it.is_visited(vmd_tdef)) {
            continue;
        }
        it.set_visited(vmd_tdef);
        it.append_tail(vmd_tdef);
    }
}


static void iterDefCHelper(MDDef const* def, IR const* use,
                           MDSSAMgr const* mgr, OUT ConstMDDefIter & it)
{
    ASSERT0(def);
    if (def->is_phi()) {
        iterDefCHelperPhi(def, use, mgr, it);
        return;
    }

    ASSERT0(def->getOcc());
    if (use != nullptr && isKillingDef(def->getOcc(), use, nullptr)) {
        //Stop the iteration until meeting the killing DEF real stmt.
        return;
    }
    MDDef const* prev = def->getPrev();
    if (prev == nullptr || it.is_visited(prev)) { return; }
    it.set_visited(prev);
    it.append_tail(prev);
}


//Iterative access USE in MDSSAInfo.
//The USE always an IR occurrence that describes a memory expression.
//This funtion initialize the iterator.
//def: the MDDef of the chain.
//it: iterator. It should be clean already.
//Readonly function.
//Note this function may iterate same IR multiple times because it may
//belong different VOpnd.
//e.g: global int g; local int b;
//     g = b;
//The MDSSA info of ST is:
//  st:i32 'g'
//    --DEFVMD:(MD2V2, PrevDEF:MD2V1, NextDEF : MD2V3) | UsedBy : ld b(id:15)
//    --DEFVMD : (MD5V2, PrevDEF:MD5V1) | UsedBy : ld b(id:15), id(id:23)
//  ld b is both USE of VOpnd(MD2V2) and VOpnd(MD5V2).
IR const* MDSSAMgr::iterUseInitC(IR const* def,
                                 OUT ConstMDSSAUSEIRIter & it) const
{
    MDSSAMgr * pthis = const_cast<MDSSAMgr*>(this);
    MDSSAInfo * info = pthis->getMDSSAInfoIfAny(def);
    ASSERT0(info);
    it.vopndset_iter = nullptr;
    it.vopndset = info->getVOpndSet();
    //Find the first iter and position in VOpndSet.
    for (it.current_pos_in_vopndset = it.vopndset->get_first(
            &it.vopndset_iter);
         it.current_pos_in_vopndset >= 0;
         it.current_pos_in_vopndset = it.vopndset->get_next(
            it.current_pos_in_vopndset, &it.vopndset_iter)) {
        VMD * vopnd = (VMD*)pthis->getUseDefMgr()->getVOpnd(
            it.current_pos_in_vopndset);
        ASSERT0(vopnd && vopnd->is_md());
        it.current_useset = vopnd->getUseSet();
        it.useset_iter.clean();
        //Find the first iter and position in UseSet.
        for (it.current_pos_in_useset = it.current_useset->get_first(
                 it.useset_iter);
             !it.useset_iter.end();
             it.current_pos_in_useset = it.current_useset->get_next(
                 it.useset_iter)) {
            IR * use = m_rg->getIR(it.current_pos_in_useset);
            ASSERT0(use && !use->isReadPR());
            return use;
        }
    }
    return nullptr;
}


//Iterative access USE in MDSSAInfo. The USE always an IR occurrence that
//describes a memory expression.
//This function return the next USE accroding to 'it'.
//it: iterator.
//Readonly function.
//Note this function may iterate same IR multiple times because it may
//belong different VOpnd.
//e.g: global int g; local int b;
//     g = b;
//The MDSSA info of ST is:
//  st:i32 'g'
//    --DEFVMD:(MD2V2, PrevDEF:MD2V1, NextDEF : MD2V3) | UsedBy : ld b(id:15)
//    --DEFVMD:(MD5V2, PrevDEF:MD5V1) | UsedBy : ld b(id:15), id(id:23)
//  ld b is both USE of VOpnd(MD2V2) and VOpnd(MD5V2).
IR const* MDSSAMgr::iterUseNextC(OUT ConstMDSSAUSEIRIter & it) const
{
    MDSSAMgr * pthis = const_cast<MDSSAMgr*>(this);
    //Update iter and position in UseSet.
    for (; !it.useset_iter.end(); UNREACHABLE()) {
        //Find next USE.
        it.current_pos_in_useset = it.current_useset->get_next(it.useset_iter);
        if (it.useset_iter.end()) {
            //Prepare next VOpnd.
            it.current_pos_in_vopndset = it.vopndset->get_next(
                it.current_pos_in_vopndset, &it.vopndset_iter);
            //Step into next VOpnd.
            break;
        } else {
            IR * use = m_rg->getIR(it.current_pos_in_useset);
            ASSERT0(use && !use->isReadPR());
            return use;
        }
    }

    //Update iter and position in VOpndSet.
    for (; it.current_pos_in_vopndset >= 0;
         it.current_pos_in_vopndset = it.vopndset->get_next(
             it.current_pos_in_vopndset, &it.vopndset_iter)) {
        VMD * vopnd = (VMD*)pthis->getUseDefMgr()->getVOpnd(
            it.current_pos_in_vopndset);
        ASSERT0(vopnd && vopnd->is_md());
        it.current_useset = vopnd->getUseSet();
        it.useset_iter.clean();
        //Find the first iter and position in UseSet.
        for (it.current_pos_in_useset = it.current_useset->get_first(
                 it.useset_iter);
             !it.useset_iter.end(); UNREACHABLE()) {
            IR * use = m_rg->getIR(it.current_pos_in_useset);
            ASSERT0(use && !use->isReadPR());
            return use;
        }
    }
    return nullptr;
}


//Iterative access MDDef chain.
//This funtion initialize the iterator.
//def: the beginning MDDef of the chain.
//it: iterator. It should be clean already.
//Readonly function.
MDDef const* MDSSAMgr::iterDefInitC(MDDef const* def,
                                    OUT ConstMDDefIter & it) const
{
    ASSERT0(def);
    it.set_visited(def);
    iterDefCHelper(def, nullptr, this, it);
    return def;
}


//Iterative access MDDef chain.
//This function return the next MDDef node accroding to 'it'.
//it: iterator.
//Readonly function.
MDDef const* MDSSAMgr::iterDefNextC(MOD ConstMDDefIter & it) const
{
    MDDef const* def = it.remove_head();
    if (def == nullptr) { return nullptr; }
    iterDefCHelper(def, nullptr, this, it);
    return def;
}


//Iterative access MDDef chain.
//This funtion initialize the iterator.
//def: the beginning MDDef of the chain.
//use: indicate the USE expression of the 'def'.
//it: iterator. It should be clean already.
//Readonly function.
MDDef const* MDSSAMgr::iterDefInitCTillKillingDef(MDDef const* def,
                                                  IR const* use,
                                                  OUT ConstMDDefIter & it) const
{
    ASSERT0(def && use && use->is_exp());
    it.set_visited(def);
    iterDefCHelper(def, use, this, it);
    return def;
}


//Iterative access MDDef chain.
//This function return the next MDDef node accroding to 'it'.
//it: iterator.
//use: indicate the USE expression of the 'def'.
//Readonly function.
MDDef const* MDSSAMgr::iterDefNextCTillKillingDef(IR const* use,
                                                  OUT ConstMDDefIter & it) const
{
    ASSERT0(use && use->is_exp());
    MDDef const* def = it.remove_head();
    if (def == nullptr) { return nullptr; }
    iterDefCHelper(def, use, this, it);
    return def;
}


//Clean MDSSAInfo AI of all IR.
void MDSSAMgr::cleanMDSSAInfoAI()
{
    for (INT i = 0; i <= m_rg->getIRVec()->get_last_idx(); i++) {
        IR * ir = m_rg->getIR(i);
        if (ir != nullptr && ir->getAI() != nullptr &&
            ir->getAI()->is_init()) {
            IR_ai(ir)->clean(AI_MD_SSA);
        }
    }
}


//Reinitialize MD SSA manager.
void MDSSAMgr::reinit()
{
    destroy();
    cleanMDSSAInfoAI();
    m_max_version.destroy();
    m_max_version.init();
    m_usedef_mgr.reinit();
    init();
}


//The function will attempt to remove the USE that located in outside loop BB.
//Note the function will NOT cross MDPhi.
bool MDSSAMgr::tryRemoveOutsideLoopUse(MDDef * def, LI<IRBB> const* li)
{
    VMD * res = def->getResult();
    VMD::UseSetIter it;
    UINT next_i;
    bool removed = false;
    VMD::UseSet * uset = res->getUseSet();
    for (UINT i = uset->get_first(it); !it.end(); i = next_i) {
        next_i = uset->get_next(it);
        IR * u = m_rg->getIR(i);
        ASSERT0(u);
        if (u->is_id()) {
            MDPhi const* phi = ID_phi(u);
            if (li->isInsideLoop(phi->getBB()->id())) { continue; }
        } else {
            IRBB const* bb = u->getStmt()->getBB();
            ASSERT0(bb);
            if (li->isInsideLoop(bb->id())) { continue; }
        }
        removeDUChain(def, u);
        removed = true;
    }
    return removed;
}


bool MDSSAMgr::verifyMDSSAInfo(Region const* rg)
{
    MDSSAMgr * ssamgr = (MDSSAMgr*)(rg->getPassMgr()->
        queryPass(PASS_MD_SSA_MGR));
    if (ssamgr != nullptr && ssamgr->is_valid()) {
        ASSERT0(ssamgr->verify());
        ASSERT0(ssamgr->verifyPhi());
        if (ssamgr->getOptCtx()->is_dom_valid()) {
            ASSERT0(ssamgr->verifyVersion());
        }
    }
    return true;
}


//Duplicate Phi operand that is at the given position, and insert after
//given position sequently.
//pos: given position
//num: the number of duplication.
//Note caller should guarrentee the number of operand is equal to the
//number predecessors of BB of Phi.
void MDSSAMgr::dupAndInsertPhiOpnd(IRBB const* bb, UINT pos, UINT num)
{
    ASSERT0(bb && num >= 1);
    MDPhiList * philist = getPhiList(bb);
    if (philist == nullptr || philist->get_elem_count() == 0) {
        return;
    }
    ASSERTN(0, ("TO BE CHECK"));
    for (MDPhiListIter it = philist->get_head();
         it != philist->end(); it = philist->get_next(it)) {
        MDPhi * phi = it->val();
        ASSERT0(xcom::cnt_list(phi->getOpndList()) == bb->getNumOfPred(m_cfg));
        UINT lpos = pos;
        IR * opnd = phi->getOpndList();
        for (; lpos != 0; opnd = opnd->get_next()) {
            ASSERT0(opnd);
            lpos--;
        }
        ASSERTN(opnd, ("MDPHI does not contain such many operands."));
        MDSSAInfo * opndinfo = getMDSSAInfoIfAny(opnd);
        ASSERT0(opndinfo || !opnd->is_id());
        for (UINT i = 0; i < num; i++) {
            IR * newopnd = m_rg->dupIR(opnd);
            phi->insertOpndAfter(opnd, newopnd);
            if (opndinfo != nullptr) {
                addUseToMDSSAInfo(newopnd, opndinfo);
            }
        }
    }
}


//Return true if stmt dominates use's stmt, otherwise return false.
bool MDSSAMgr::isStmtDomUseInsideLoop(IR const* stmt, IR const* use,
                                      LI<IRBB> const* li) const
{
    IRBB const* usestmtbb = nullptr;
    if (use->is_id()) {
        MDPhi const* phi = ID_phi(use);
        ASSERT0(phi && phi->is_phi());
        usestmtbb = phi->getBB();
    } else {
        ASSERT0(use->getStmt());
        usestmtbb = use->getStmt()->getBB();
    }
    ASSERT0(usestmtbb);

    if (!li->isInsideLoop(usestmtbb->id())) {
        //Only check dominiation info inside loop.
        return true;
    }

    IRBB const* defstmtbb = stmt->getBB();
    ASSERT0(defstmtbb);
    if (defstmtbb != usestmtbb &&
        m_cfg->is_dom(defstmtbb->id(), usestmtbb->id())) {
        return true;
    }
    if (defstmtbb == usestmtbb) {
        if (use->is_id()) { //use's stmt is MDPhi
            //stmt can not dominate PHI because PHI is always
            //in the front of BB.
            return false;
        }
        IR const* ustmt = use->getStmt();
        ASSERT0(ustmt);
        return defstmtbb->is_dom(stmt, ustmt, true);
    }

    return false;
}


//Return true if ir dominates all its USE expressions which inside loop.
//In ssa mode, stmt's USE may be placed in operand list of PHI.
bool MDSSAMgr::isStmtDomAllUseInsideLoop(IR const* ir, LI<IRBB> const* li) const
{
    ASSERT0(ir && ir->getBB());
    ConstMDSSAUSEIRIter it;
    for (IR const* use = iterUseInitC(ir, it);
        use != nullptr; use = iterUseNextC(it)) {
        if (!isStmtDomUseInsideLoop(ir, use, li)) {
            return false;
        }
    }
    return true;
}


//Move MDPhi from 'from' to 'to'.
//This function often used in updating PHI when adding new dominater
//BB to 'to'.
void MDSSAMgr::movePhi(IRBB * from, IRBB * to)
{
    ASSERT0(from && to && from != to);
    MDPhiList * from_philist = getPhiList(from);
    if (from_philist == nullptr || from_philist->get_elem_count() == 0) {
        return;
    }

    MDPhiList * to_philist = m_usedef_mgr.genBBPhiList(to->id());
    MDPhiListIter to_it = to_philist->get_head();
    for (MDPhiListIter from_it = from_philist->get_head();
         from_it != from_philist->end();
         from_it = from_philist->get_next(from_it)) {

        //Move MDPhi from 'from' to 'to'.
        MDPhi * phi = from_it->val();
        MDDEF_bb(phi) = to;
        if (to_it == nullptr) {
            //'to' BB does not have PHI list.
            to_it = to_philist->append_head(phi);
        } else {
            //Make sure phi's order in 'to' is same with 'from'.
            to_it = to_philist->insert_after(phi, to_it);
        }
    }
    from_philist->clean();
}


class Vertex2LiveSet {
    COPY_CONSTRUCTOR(Vertex2LiveSet);
    TMap<UINT, LiveSet*> m_bbid2set;
    xcom::DefMiscBitSetMgr m_sbsmgr;
public:
    Vertex2LiveSet() {}
    ~Vertex2LiveSet()
    {
        TMapIter<UINT, LiveSet*> it;
        LiveSet * set;
        for (m_bbid2set.get_first(it, &set); set != nullptr;
             m_bbid2set.get_next(it, &set)) {
            delete set;
        }
    }

    LiveSet * genAndCopy(UINT bbid, LiveSet const& src)
    { return genAndCopy(bbid, const_cast<LiveSet&>(src).getSet()); }
    LiveSet * genAndCopy(UINT bbid, VOpndSet const& src)
    {
        LiveSet * liveset = new LiveSet(src, &m_sbsmgr);
        ASSERT0(m_bbid2set.get(bbid) == nullptr);
        m_bbid2set.set(bbid, liveset);
        return liveset;
    }
    LiveSet * get(UINT bbid) const { return m_bbid2set.get(bbid); }
    void free(UINT bbid)
    {
        LiveSet * set = m_bbid2set.remove(bbid);
        if (set != nullptr) { delete set; }
    }
};


//
//START RenameTillNextDef
//
class RenameTillNextDef {
    COPY_CONSTRUCTOR(RenameTillNextDef);
    IR const* m_newstmt;
    DomTree const& m_domtree;
    LiveSet * m_liveset;
    MDSSAMgr * m_mgr;
    IRCFG * m_cfg;
    Vertex2LiveSet m_vex2liveset;
private:
    void iterBBPhiListToKillLivedVMD(IRBB const* bb, LiveSet & liveset);
    void iterSuccBBPhiListToRename(IRBB const* succ, UINT opnd_idx,
                                   LiveSet const& liveset);
    //v: vertex on DomTree.
    void iterSuccBB(Vertex const* v, LiveSet const& liveset);

    void killLivedVMD(MDPhi const* phi, MOD LiveSet & liveset);

    void renamePhiOpnd(MDPhi const* phi, UINT opnd_idx, MOD VMD * vmd);

    //stmtbb: the BB of inserted stmt
    //newinfo: MDSSAInfo that intent to be swap-in.
    bool renameVMDForDesignatedPhiOpnd(MDPhi * phi, UINT opnd_pos,
                                       LiveSet & liveset);
    //vmd: intent to be swap-in.
    //irtree: may be stmt or exp.
    //irit: for local used.
    void renameVMDForIRTree(IR * irtree, VMD * vmd, MOD IRIter & irit,
                            bool & no_exp_has_ssainfo);
    //ir: may be stmt or exp
    //irit: for local used.
    void renameLivedVMDForIRTree(IR * ir, MOD IRIter & irit,
                                 LiveSet const& liveset);

    //stmtbbid: indicates the BB of inserted stmt
    //bb: the BB that to be renamed
    //dompred: indicates the predecessor of 'bb' in DomTree
    //Note stmtbbid have to dominate 'bb'.
    void renameUseInBBTillNextDef(Vertex const* v, IRBB const* bb,
                                  BBIRListIter & irlistit,
                                  OUT LiveSet & liveset);
    void renameFollowUseIntraBBTillNextDef(Vertex const* stmtbbvex,
                                           MOD LiveSet & stmtliveset);
    void renameFollowUseInterBBTillNextDef(Vertex const* stmtbbvex,
                                           MOD LiveSet & stmtliveset);

    bool tryInsertDDChainForDesigatedVMD(IR * ir, VMD * vmd,
                                         MOD LiveSet & liveset);
    bool tryInsertDDChainForStmt(IR * ir, MOD LiveSet & liveset);
public:
    RenameTillNextDef(IR const* stmt, DomTree const& dt, MDSSAMgr * mgr) :
        m_newstmt(stmt), m_domtree(dt), m_liveset(nullptr), m_mgr(mgr)
    { m_cfg = m_mgr->getCFG(); }

    MDSSAMgr * getMgr() const { return m_mgr; }

    void perform();
};


void RenameTillNextDef::renamePhiOpnd(MDPhi const* phi, UINT opnd_idx,
                                      MOD VMD * vmd)
{
    ASSERT0(phi->is_phi());
    UseDefMgr * udmgr = m_mgr->getUseDefMgr();
    UINT i = 0;
    IR * opnd = nullptr;
    for (opnd = phi->getOpndList();
         opnd != nullptr && i != opnd_idx; opnd = opnd->get_next(), i++) {;}
    ASSERT0(opnd);
    MDSSAInfo * info = m_mgr->getMDSSAInfoIfAny(opnd);
    info->renameSpecificUse(opnd, vmd, udmgr);
}


//stmtbb: the BB of inserted stmt
//newinfo: MDSSAInfo that intent to be swap-in.
bool RenameTillNextDef::renameVMDForDesignatedPhiOpnd(MDPhi * phi,
                                                      UINT opnd_pos,
                                                      MOD LiveSet & liveset)
{
    UseDefMgr * udmgr = m_mgr->getUseDefMgr();
    MDIdx phimdid = phi->getResult()->mdid();
    VOpndSet const& vmdset = const_cast<LiveSet&>(liveset).getSet();
    VOpndSetIter it = nullptr;
    for (BSIdx i = vmdset.get_first(&it); i != BS_UNDEF;
         i = vmdset.get_next(i, &it)) {
        VMD * t = (VMD*)udmgr->getVOpnd(i);
        ASSERT0(t && t->is_md());
        if (t->mdid() == phimdid) {
            renamePhiOpnd(phi, opnd_pos, t);
            liveset.set_killed(t->id());
            return true;
        }
    }
    return false;
}


//vmd: intent to be swap-in.
//irtree: may be stmt or exp.
//irit: for local used.
void RenameTillNextDef::renameVMDForIRTree(IR * irtree, VMD * vmd,
                                           MOD IRIter & irit,
                                           bool & no_exp_has_ssainfo)
{
    irit.clean();
    for (IR * e = iterExpInit(irtree, irit, true);
         e != nullptr; e = iterExpNext(irit, true)) {
        if (MDSSAMgr::hasMDSSAInfo(e)) {
            no_exp_has_ssainfo = false;
            MDSSAInfo * einfo = m_mgr->getMDSSAInfoIfAny(e);
            ASSERT0(einfo);
            einfo->renameSpecificUse(e, vmd, m_mgr->getUseDefMgr());
        }
    }
}


//ir: may be stmt or exp
//irit: for local used.
void RenameTillNextDef::renameLivedVMDForIRTree(IR * ir, MOD IRIter & irit,
                                                LiveSet const& liveset)
{
    VOpndSetIter it = nullptr;
    UseDefMgr * udmgr = m_mgr->getUseDefMgr();
    VOpndSet const& vmdset = const_cast<LiveSet&>(liveset).getSet();
    bool no_exp_has_ssainfo = true;
    for (BSIdx i = vmdset.get_first(&it); i != BS_UNDEF;
         i = vmdset.get_next(i, &it)) {
        VMD * t = (VMD*)udmgr->getVOpnd(i);
        ASSERT0(t && t->is_md());
        if (liveset.is_live(i)) {
            renameVMDForIRTree(ir, t, irit, no_exp_has_ssainfo);
            if (no_exp_has_ssainfo) { return; }
        }
    }
}


bool RenameTillNextDef::tryInsertDDChainForDesigatedVMD(IR * ir, VMD * vmd,
                                                        MOD LiveSet & liveset)
{
    ASSERT0(ir->is_stmt());
    MDSSAInfo * irinfo = m_mgr->getMDSSAInfoIfAny(ir);
    ASSERT0(irinfo);
    VOpndSetIter vit = nullptr;
    UseDefMgr * udmgr = m_mgr->getUseDefMgr();
    MDIdx vmdid = vmd->mdid();
    VOpndSet const& irvmdset = irinfo->readVOpndSet();
    for (BSIdx i = irvmdset.get_first(&vit);
         i != BS_UNDEF; i = irvmdset.get_next(i, &vit)) {
        VMD * t = (VMD*)udmgr->getVOpnd(i);
        ASSERT0(t && t->is_md());
        if (t->mdid() == vmdid) {
            liveset.set_killed(vmd->id());
            m_mgr->insertDefChain(vmd->getDef(), t->getDef());
            return true;
        }
    }
    return false;
}


bool RenameTillNextDef::tryInsertDDChainForStmt(IR * ir, MOD LiveSet & liveset)
{
    ASSERT0(ir->is_stmt());
    VOpndSetIter it = nullptr;
    UseDefMgr * udmgr = m_mgr->getUseDefMgr();
    bool inserted = false;
    VOpndSet const& vmdset = liveset.getSet();
    INT nexti;
    for (BSIdx i = vmdset.get_first(&it); i != BS_UNDEF; i = nexti) {
        nexti = vmdset.get_next(i, &it); //i may be removed.
        VMD * t = (VMD*)udmgr->getVOpnd(i);
        ASSERT0(t && t->is_md());
        if (liveset.is_live(i)) {
            inserted |= tryInsertDDChainForDesigatedVMD(ir, t, liveset);
        }
    }
    return inserted;
}


void RenameTillNextDef::killLivedVMD(MDPhi const* phi, MOD LiveSet & liveset)
{
    UseDefMgr * udmgr = m_mgr->getUseDefMgr();
    MDIdx phimdid = phi->getResult()->mdid();
    VOpndSet const& vmdset = liveset.getSet();
    VOpndSetIter it = nullptr;
    for (BSIdx i = vmdset.get_first(&it); i != BS_UNDEF;
         i = vmdset.get_next(i, &it)) {
        VMD * t = (VMD*)udmgr->getVOpnd(i);
        ASSERT0(t && t->is_md());
        if (t->mdid() == phimdid) {
            liveset.set_killed(t->id());
        }
    }
}


void RenameTillNextDef::iterSuccBBPhiListToRename(IRBB const* succ,
                                                  UINT opnd_idx,
                                                  LiveSet const& liveset)
{
    ASSERT0(succ);
    MDPhiList * philist = m_mgr->getPhiList(succ);
    if (philist == nullptr) { return; }
    LiveSet * succliveset = m_vex2liveset.get(succ->id());
    if (succliveset == nullptr) {
        succliveset = m_vex2liveset.genAndCopy(succ->id(), liveset);
    } else {
        succliveset->copy(liveset);
    }
    for (MDPhiListIter it = philist->get_head();
         it != philist->end(); it = philist->get_next(it)) {
        MDPhi * phi = it->val();
        ASSERT0(phi);
        renameVMDForDesignatedPhiOpnd(phi, opnd_idx, *succliveset);
        if (succliveset->all_killed()) {
            return;
        }
    }
}


void RenameTillNextDef::iterSuccBB(Vertex const* v, LiveSet const& liveset)
{
    Vertex const* cfgv = m_cfg->getVertex(v->id());
    ASSERT0(cfgv);
    for (EdgeC const* el = cfgv->getOutList();
         el != nullptr; el = el->get_next()) {
        UINT opnd_idx = 0; //the index of corresponding predecessor.
        Vertex const* succv = el->getTo();
        EdgeC const* sel;
        for (sel = succv->getInList();
             sel != nullptr; sel = sel->get_next(), opnd_idx++) {
            if (sel->getFromId() == cfgv->id()) {
                break;
            }
        }
        ASSERTN(sel, ("not found related pred"));
        //Replace opnd of PHI of 'succ' with lived SSA version.
        iterSuccBBPhiListToRename(m_cfg->getBB(succv->id()), opnd_idx, liveset);
    }
}


void RenameTillNextDef::iterBBPhiListToKillLivedVMD(IRBB const* bb,
                                                    LiveSet & liveset)
{
    ASSERT0(bb);
    MDPhiList * philist = m_mgr->getPhiList(bb);
    if (philist == nullptr) { return; }
    for (MDPhiListIter it = philist->get_head();
         it != philist->end(); it = philist->get_next(it)) {
        MDPhi * phi = it->val();
        ASSERT0(phi);
        killLivedVMD(phi, liveset);
        if (liveset.all_killed()) {
            return;
        }
    }
}


//stmtbbid: indicates the BB of inserted stmt
//bb: the BB that to be renamed
//dompred: indicates the predecessor of 'bb' in DomTree
//Note stmtbbid have to dominate 'bb'.
void RenameTillNextDef::renameUseInBBTillNextDef(Vertex const* v,
                                                 IRBB const* bb,
                                                 BBIRListIter & irlistit,
                                                 MOD LiveSet & liveset)
{
    iterBBPhiListToKillLivedVMD(bb, liveset);
    if (liveset.all_killed()) { return; }
    IRIter irit;
    BBIRList & irlist = const_cast<IRBB*>(bb)->getIRList();
    for (; irlistit != nullptr; irlistit = irlist.get_next(irlistit)) {
        IR * n = irlistit->val();
        renameLivedVMDForIRTree(n, irit, liveset);
        if (!MDSSAMgr::hasMDSSAInfo(n)) { continue; }
        tryInsertDDChainForStmt(n, liveset);
        if (liveset.all_killed()) {
            return;
        }
    }
    iterSuccBB(v, liveset);
}


void RenameTillNextDef::renameFollowUseIntraBBTillNextDef(
    Vertex const* stmtbbvex, MOD LiveSet & stmtliveset)
{
    IRBB * bb = m_newstmt->getBB();
    ASSERT0(bb);
    BBIRListIter irlistit = nullptr;
    BBIRList & irlist = bb->getIRList();
    irlist.find(const_cast<IR*>(m_newstmt), &irlistit);
    ASSERT0(irlistit);
    irlistit = irlist.get_next(irlistit);
    renameUseInBBTillNextDef(stmtbbvex, bb, irlistit, stmtliveset);
}


void RenameTillNextDef::renameFollowUseInterBBTillNextDef(
    Vertex const* stmtbbvex, MOD LiveSet & stmtliveset)
{
    xcom::Stack<Vertex const*> stk;
    xcom::TTab<UINT> visited;
    stk.push(stmtbbvex);
    IRBB const* newstmtbb = m_newstmt->getBB();
    visited.append(newstmtbb->id());
    Vertex const* v;
    while ((v = stk.get_top()) != nullptr) {
        if (!visited.find(v->id())) {
            visited.append(v->id());
            ASSERTN(newstmtbb->id() != v->id(), ("domtree contains a cycle"));

            //Init liveset for given vertex.
            LiveSet * tliveset = m_vex2liveset.get(v->id());
            if (tliveset == nullptr) {
                Vertex const* parent = m_domtree.getParent(v);
                ASSERT0(parent);
                LiveSet const* pset = m_vex2liveset.get(parent->id());
                ASSERT0(pset);
                tliveset = m_vex2liveset.genAndCopy(v->id(), *pset);
            } else if (tliveset->all_killed()) {
                stk.pop();
                m_vex2liveset.free(v->id());
                continue;
            }

            IRBB * vbb = m_cfg->getBB(v->id());
            ASSERT0(vbb);
            BBIRListIter irlistit;
            vbb->getIRList().get_head(&irlistit);
            renameUseInBBTillNextDef(v, vbb, irlistit, *tliveset);
            if (tliveset->all_killed()) {
                stk.pop();
                m_vex2liveset.free(v->id());
                continue;
            }
        }
        bool all_visited = true;
        for (EdgeC const* c = v->getOutList();
             c != nullptr; c = c->get_next()) {
            Vertex const* dom_succ = c->getTo();
            if (dom_succ == v) { continue; }
            if (!visited.find(dom_succ->id())) {
                all_visited = false;
                stk.push(dom_succ);
                break;
            }
        }

        if (all_visited) {
            stk.pop();
            m_vex2liveset.free(v->id());
        }
    }
}


void RenameTillNextDef::perform()
{
    IRBB const* newstmtbb = m_newstmt->getBB();
    MDSSAInfo const* info = m_mgr->getMDSSAInfoIfAny(m_newstmt);
    ASSERT0(info && info->readVOpndSet().get_elem_count() > 0);

    ASSERT0(m_vex2liveset.get(newstmtbb->id()) == nullptr);
    LiveSet * stmtliveset = m_vex2liveset.genAndCopy(newstmtbb->id(),
                                                     info->readVOpndSet());
    Vertex const* stmtbbvex = m_domtree.getVertex(newstmtbb->id());
    ASSERTN(stmtbbvex, ("miss vertex on domtree"));
    renameFollowUseIntraBBTillNextDef(stmtbbvex, *stmtliveset);
    if (stmtliveset->all_killed()) { return; }
    renameFollowUseInterBBTillNextDef(stmtbbvex, *stmtliveset);
}
//END RenameTillNextDef


void MDSSAMgr::insertDefStmt(IR * stmt, DomTree const& domtree)
{
    ASSERT0(stmt && hasMDSSAInfo(stmt));
    MDSSAInfo const* info = genMDSSAInfoAndNewVesionVMD(stmt);
    ASSERT0(info->readVOpndSet().get_elem_count() > 0);
    RenameTillNextDef rtnd(stmt, domtree, this);
    rtnd.perform();
}


void MDSSAMgr::construction(OptCtx & oc)
{
    START_TIMER(t0, "MDSSA: Construction");
    m_rg->getPassMgr()->checkValidAndRecompute(&oc, PASS_DOM, PASS_UNDEF);
    ASSERT0(oc.is_ref_valid());
    ASSERT0(oc.is_dom_valid());
    reinit();

    //Extract dominate tree of CFG.
    START_TIMER(t1, "MDSSA: Extract Dom Tree");
    DomTree domtree;
    m_cfg->genDomTree(domtree);
    END_TIMER(t1, "MDSSA: Extract Dom Tree");

    if (!construction(domtree, oc)) {
        return;
    }
    m_is_valid = true;
    END_TIMER(t0, "MDSSA: Construction");
}


bool MDSSAMgr::construction(DomTree & domtree, OptCtx const& oc)
{
    ASSERT0(m_rg);
    START_TIMER(t1, "MDSSA: Build dominance frontier");
    DfMgr dfm;
    dfm.build((xcom::DGraph&)*m_cfg); //Build dominance frontier.
    END_TIMER(t1, "MDSSA: Build dominance frontier");
    if (dfm.hasHighDFDensityVertex((xcom::DGraph&)*m_cfg)) {
        return false;
    }

    m_oc = &oc;
    MD2VMDStack md2vmdstk;
    List<IRBB*> wl;
    DefMiscBitSetMgr bs_mgr;
    DefMDSet effect_mds(bs_mgr.getSegMgr());
    BB2DefMDSet defed_mds;
    placePhi(dfm, effect_mds, bs_mgr, defed_mds, wl);
    rename(effect_mds, defed_mds, domtree, md2vmdstk);
    //Note you can clean version stack after renaming.
    ASSERT0(verifyPhi());
    prunePhi(wl);
    cleanLocalUsedData();
    if (g_dump_opt.isDumpAfterPass() && g_dump_opt.isDumpMDSSAMgr()) {
        dump();
    }
    ASSERT0(verify());
    ASSERT0(verifyIRandBB(m_rg->getBBList(), m_rg));
    ASSERT0(verifyPhi() && verifyVMD() && verifyVersion());
    m_is_valid = true;
    return true;
}
//END MDSSAMgr

} //namespace xoc
