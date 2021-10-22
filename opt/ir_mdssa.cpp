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

    cleanMD2Stack();
    freePhiList();
}


void MDSSAMgr::freeBBPhiList(IRBB * bb)
{
    MDPhiList * philist = getPhiList(bb);
    if (philist == nullptr) { return; }

    for (MDPhiListIter sct = philist->get_head();
         sct != philist->end(); sct = philist->get_next(sct)) {
        MDPhi * phi = sct->val();
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
    cleanMD2Stack();

    //Do NOT clean max_version of each MD, because some passes will generate
    //DEF stmt for individual MD, which need new version of the MD.
    //MD's max verison is often used to update MDSSAInfo incrementally.
    //m_max_version.destroy();
    //m_max_version.init();
}


//Clean version stack.
void MDSSAMgr::cleanMD2Stack()
{
    for (INT i = 0; i <= m_map_md2stack.get_last_idx(); i++) {
        xcom::Stack<VMD*> * s = m_map_md2stack.get(i);
        if (s != nullptr) { delete s; }
    }
    m_map_md2stack.destroy();
    m_map_md2stack.init();
}


//Dump MDSSA DU stmt graph.
void MDSSAMgr::dumpSSAGraph(CHAR *)
{
    //MDSSAGraph sa(m_rg, this);
    //sa.dump(name, true);
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
                ASSERT0(stmt->is_stmt() && !stmt->isWritePR());
                prt(getRegion(), "DEF:(%s,id:%d)", IRNAME(stmt), stmt->id());
            }
        } else {
            prt(getRegion(), "DEF:---");
        }

        //Print USEs.
        prt(getRegion(), "\tUSE:");
        VMD::UseSetIter it;
        INT nexti = 0;
        for (INT j = v->getUseSet()->get_first(it); !it.end(); j = nexti) {
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
void MDSSAMgr::removeSuccessorDesignatePhiOpnd(IRBB * bb, IRBB * succ)
{
    MDPhiList * philist = getPhiList(succ);
    if (philist == nullptr) { return; }

    UINT pos = m_cfg->WhichPred(bb, succ);
    for (MDPhiListIter  sct = philist->get_head();
         sct != philist->end(); sct = philist->get_next(sct)) {
        MDPhi * phi = sct->val();
        ASSERT0(phi && phi->is_phi());
        IR * opnd_head = phi->getOpndList();

        ASSERT0(xcom::cnt_list(opnd_head) == succ->getNumOfPred(m_cfg));
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
        removeMDSSAOcc(opnd);
        phi->removeOpnd(opnd);
        m_rg->freeIRTree(opnd);
    }
}


//Find livein def-stmt through given 'start' BB.
//start: the BB that begin to do searching.
static bool findLiveInDef(IRCFG * cfg,
                          VMDVec const* vmdvec,
                          IRBB const* start,
                          VMD ** livein_def)
{
    ASSERT0(livein_def && start);
    IRBB const* meetup = cfg->getEntry();
    ASSERT0(meetup);
    xcom::List<IRBB const*> wl;
    cfg->get_preds(wl, start);
    xcom::TTab<UINT> visited;
    while (wl.get_elem_count() != 0) {
        IRBB const* t = wl.remove_head();
        if (t == meetup) { continue; }

        VMD * vmd = vmdvec->findVMD(t->id());
        if (vmd != nullptr) {
            *livein_def = vmd;
            return true;
        }

        visited.append(t->id());
        for (xcom::EdgeC * el = cfg->getVertex(t->id())->getInList();
             el != nullptr; el = el->get_next()) {
            UINT pred = (UINT)el->getFromId();
            if (!visited.find(pred)) {
                wl.append_tail(cfg->getBB(pred));
            }
        }
    }
    return false;
}


//Find livein def-stmt through given 'start' BB.
//This function will iterate dom tree bottom up from 'start' until the
//anticipated BB found.
//start: the BB that begin to do searching.
static void findLiveInDefInDomSet(IRCFG * cfg, VMDVec const* vmdvec,
                                  IRBB const* start, VMD ** livein_def)
{
    //Find the latest live-in version of PHI's operand MD.
    for (UINT bbid = start->id(); bbid != BBID_UNDEF;
         bbid = ((DGraph*)cfg)->get_idom(bbid)) {
        VMD * v = vmdvec->findVMD(bbid);
        if (v != nullptr) {
            *livein_def = v;
            return;
        }
    }
}


//Insert operand at given position.
//pos: position of operand, start at 0.
//     Each operand correspond to in-edge on CFG.
IR * MDPhi::insertOpndAt(MDSSAMgr * mgr, UINT pos, IRBB const* pred)
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
    IR * opnd = rg->buildId(res->get_base());
    opnd->setRefMD(res, rg);
    ASSERT0(opnd->getRefMDSet() == nullptr);
    ID_phi(opnd) = this; //Record ID's host PHI.

    VMDVec * vmdvec = mgr->getUseDefMgr()->getVMDVec(res->id());
    IRCFG * cfg = rg->getCFG();

    //Find the latest live-in version of PHI's operand MD.
    VMD * livein_def = nullptr;
    DUMMYUSE(findLiveInDefInDomSet);
    //findLiveInDefInDomSet(m_cfg, vmdvec, pred, &livein_def);
    findLiveInDef(cfg, vmdvec, pred, &livein_def);

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


//After adding BB or change BB successor,
//you need add the related PHI operand if BB successor has PHI stmt.
void MDSSAMgr::addSuccessorDesignatePhiOpnd(IRBB * bb, IRBB * succ)
{
    MDPhiList * philist = getPhiList(succ);
    if (philist == nullptr) { return; }

    UINT const pos = m_cfg->WhichPred(bb, succ);
    for (MDPhiListIter  sct = philist->get_head();
         sct != philist->end(); sct = philist->get_next(sct)) {
        MDPhi * phi = sct->val();
        ASSERT0(phi && phi->is_phi());
        phi->insertOpndAt(this, pos, bb);
        ASSERT0(xcom::cnt_list(phi->getOpndList()) ==
                succ->getNumOfPred(m_cfg));
    }
}


void MDSSAMgr::dumpPhiList(MDPhiList const* philist) const
{
    ASSERT0(philist);
    for (MDPhiListIter  sct = philist->get_head();
         sct != philist->end(); sct = philist->get_next(sct)) {
        MDPhi const* phi = sct->val();
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
        prt(getRegion(), "NOMDSSAINFO!!");
    } else {
        for (INT i = mdssainfo->getVOpndSet()->get_first(&iter);
            i >= 0; i = mdssainfo->getVOpndSet()->get_next(i, &iter)) {
            note(getRegion(), "\n--DEFVMD:");
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

        note(getRegion(), "\n--USEVMD:");
        bool first = true;
        MDSSAInfo * mdssainfo = getMDSSAInfoIfAny(opnd);
        if (mdssainfo == nullptr) {
            prt(getRegion(), "NOMDSSAINFO!!");
            continue;
        }

        for (INT i = mdssainfo->getVOpndSet()->get_first(&iter);
             i >= 0; i = mdssainfo->getVOpndSet()->get_next(i, &iter)) {
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
        MDPhiList * philist = getPhiList(bb);
        if (philist != nullptr) {
            dumpPhiList(philist);
        }
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
    m_md_sys->dump(true);
    dumpVOpndRef();
    dumpDUChain();
    END_TIMER(t, "MDSSA: Dump After Pass");
    return true;
}


//Find nearest virtual DEF in VOpndSet of 'ir'.
MDDef * MDSSAMgr::findNearestDef(IR const* ir) const
{
    ASSERT0(ir);
    MDSSAInfo const* mdssainfo = getMDSSAInfoIfAny(ir);
    ASSERTN(mdssainfo, ("miss MDSSAInfo"));
    VOpndSetIter iter = nullptr;
    INT lastrpo = -1;
    MDDef * last = nullptr;
    for (INT i = mdssainfo->readVOpndSet()->get_first(&iter);
         i >= 0; i = mdssainfo->readVOpndSet()->get_next(i, &iter)) {
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
        if (tbb->is_dom(tdef->getOcc(), last->getOcc(), true)) {
            //tdef is near more than 'last', then update 'last'.
            last = tdef;
        }
    }
    return last;
}


//Find killing must-def for expression ir.
//e.g: g is global variable, it is exact.
//x is a pointer that we do not know where it pointed to.
//    1. *x += 1; # *x may overlapped with g
//    2. g = 0; # exactly defined g
//    3. call foo(); # foo may overlapped with g
//    4. return g;
//In the case, the last reference of g in stmt 4 may be defined by
//stmt 1, 2, 3, there is no nearest killing def.
MDDef * MDSSAMgr::findKillingDef(IR const* ir) const
{
    ASSERT0(ir && ir->is_exp() && ir->isMemoryOpnd());

    MD const* opndmd = ir->getRefMD();
    if (opndmd == nullptr || !opndmd->is_exact()) { return nullptr; }

    MDDef * def = findNearestDef(ir);
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
    for (INT i = info->readVOpndSet()->get_first(&iter);
         i >= 0; i = info->readVOpndSet()->get_next(i, &iter)) {
        VOpnd const* vopnd = getVOpnd(i);
        ASSERT0(vopnd && vopnd->is_md());
        if (!((VMD*)vopnd)->getUseSet()->is_empty()) {
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
    IRSet visited(m_sbs_mgr->getSegMgr());
    xcom::List<MDDef const*> wl;
    lst.clean();
    opnd_lst.clean();
    for (IR const* opnd = iterInit(const_cast<IR*>(ir), lst);
         opnd != nullptr; opnd = iterNext(lst)) {
        if (!opnd->isMemoryRefNonPR() || opnd->is_stmt()) {
            continue;
        }

        VOpndSetIter iter = nullptr;
        if (!(*parting_line)) {
            note(getRegion(), "\n----------------");
            (*parting_line) = true;
        }
        note(getRegion(), "\n");
        prt(getRegion(), "%s(id:%d)", IRNAME(opnd), opnd->id());

        MDSSAInfo * mdssainfo = getMDSSAInfoIfAny(opnd);
        if (mdssainfo == nullptr) {
            prt(getRegion(), " MISS MDSSAInfo");
            continue;
        }

        MDDef * kdef = findKillingDef(opnd);
        if (kdef != nullptr) {
            prt(getRegion(), " KDEF:%s(id:%d)", IRNAME(kdef->getOcc()),
                kdef->getOcc()->id());
            continue;
        }

        //Not found killing def, thus dump total define-chain.
        //Define-chain represents the may-def list.
        prt(getRegion(), " DEFSET:");
        visited.clean();
        bool first = true;
        m_rg->getLogMgr()->incIndent(2);
        for (INT i = mdssainfo->getVOpndSet()->get_first(&iter);
             i >= 0; i = mdssainfo->getVOpndSet()->get_next(i, &iter)) {
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
    for (INT i = const_cast<VMD*>(vmd)->getUseSet()->get_first(vit);
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
        note(rg, "\n----------------");
        parting_line = true;
    }
    
    note(rg, "\n");
    prt(rg, "%s(id:%d)", IRNAME(ir), ir->id());

    bool first = true;
    MDSSAMgr * pmgr = const_cast<MDSSAMgr*>(this);
    MDSSAInfo * mdssainfo = pmgr->getMDSSAInfoIfAny(ir);
    if (mdssainfo == nullptr) {
        prt(rg, " MISS MDSSAInfo");
        return;
    }

    prt(rg, " USESET:");
    //Dump VOpnd and the USE List for each VOpnd.
    rg->getLogMgr()->incIndent(2);
    for (INT i = mdssainfo->getVOpndSet()->get_first(&iter);
         i >= 0; i = mdssainfo->getVOpndSet()->get_next(i, &iter)) {
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


void MDSSAMgr::dumpDUChainForIR(IR const* ir,
                                xcom::List<IR*> & lst,
                                xcom::List<IR*> & opnd_lst) const
{
    dumpIR(ir, getRegion());
    bool parting_line = false;
    //Handle stmt.
    MDSSAMgr * pmgr = const_cast<MDSSAMgr*>(this);
    if (ir->isMemoryRefNonPR() || ir->isCallStmt()) {
        dumpDUChainForStmt(ir, parting_line);
    }
    //Handle expression.
    dumpExpDUChainIter(ir, lst, opnd_lst, &parting_line);
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
        MDPhiList * philist = getPhiList(bb);
        if (philist != nullptr) {
            for (MDPhiListIter  sct = philist->get_head();
                 sct != philist->end(); sct = philist->get_next(sct)) {
                MDPhi * phi = sct->val();
                ASSERT0(phi && phi->is_phi());
                note(rg, "\n");
                phi->dump(rg, &m_usedef_mgr);
            }
        }
        for (IR * ir = BB_first_ir(bb); ir != nullptr; ir = BB_next_ir(bb)) {
            dumpDUChainForIR(ir, lst, opnd_lst);
        }
    }
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
        VMD const* vmd = m_usedef_mgr.allocVMD(MD_id(ref), version);
        ASSERT0(m_sbs_mgr);
        mdssainfo->addVOpnd(vmd, getUseDefMgr());
    }

    MDSet const* refset = ir->getRefMDSet();
    if (refset != nullptr) {
        MDSetIter iter;
        for (INT i = refset->get_first(&iter);
             i >= 0; i = refset->get_next((UINT)i, &iter)) {
            MD * md = m_md_sys->getMD(i);
            ASSERTN(md && !md->is_pr(), ("PR should not in MaySet"));
            VMD const* vmd2 = m_usedef_mgr.allocVMD(MD_id(md), version);
            ASSERT0(m_sbs_mgr);
            mdssainfo->addVOpnd(vmd2, getUseDefMgr());
        }
    }
    return mdssainfo;
}


//Generate VMD for stmt and its kid expressions that reference memory.
void MDSSAMgr::initVMD(IN IR * ir, OUT DefSBitSet & maydef_md)
{
    m_iter.clean();
    ASSERT0(ir->is_stmt());
    if (ir->isMemoryRefNonPR() ||
        (ir->isCallStmt() && !ir->isReadOnly())) {
        MD const* ref = ir->getRefMD();
        if (ref != nullptr && !ref->is_pr()) {
            maydef_md.bunion(MD_id(ref));
        }
        MDSet const* refset = ir->getRefMDSet();
        if (refset != nullptr) {
            maydef_md.bunion((DefSBitSet&)*refset);
        }
        genMDSSAInfoAndVOpnd(ir, MDSSA_INIT_VERSION);
    }

    for (IR * t = iterRhsInit(ir, m_iter);
         t != nullptr; t = iterRhsNext(m_iter)) {
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
    for (INT i = refset->get_first(&iter);
         i >= 0; i = refset->get_next((UINT)i, &iter)) {
        MD * md = m_md_sys->getMD(i);
        ASSERTN(md && !md->is_pr(), ("PR should not in MayBeSet"));
        livein_md.append(md->id());
    }
}


//maydef_md: record MDs that defined in 'bb'.
void MDSSAMgr::computeLiveInMD(IRBB const* bb, OUT LiveInMDTab & livein_md)
{
    livein_md.clean();
    ConstIRIter iter; //for tmp use.
    IRBB * pbb = const_cast<IRBB*>(bb);
    for (IR const* ir = BB_last_ir(pbb); ir != nullptr; ir = BB_prev_ir(pbb)) {
        iter.clean();
        ASSERT0(ir->is_stmt());
        for (IR const* t = iterRhsInitC(ir, iter);
             t != nullptr; t = iterRhsNextC(iter)) {
            ASSERT0(t->is_exp());
            if (t->isMemoryRefNonPR()) {
                collectUseMD(t, livein_md);
            }
        }

        if (!ir->isMemoryRefNonPR()) { continue; }

        MD const* ref = ir->getRefMD();
        if (ref == nullptr || !ref->is_exact()) { continue; }

        ASSERT0(!ref->is_pr());
        LiveInMDTabIter iter;
        for (UINT mdid = livein_md.get_first(iter);
             mdid != MD_UNDEF; mdid = livein_md.get_next(iter)) {
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
                                          OUT DefSBitSet & maydef_md)
{
    for (IR * ir = BB_first_ir(bb); ir != nullptr; ir = BB_next_ir(bb)) {
        initVMD(ir, maydef_md);
    }
}


void MDSSAMgr::insertPhi(UINT mdid, IN IRBB * bb)
{
    UINT num_opnd = m_cfg->getInDegree(m_cfg->getVertex(bb->id()));

    //Here each operand and result of phi set to same type.
    //They will be revised to correct type during renaming.
    MDPhi * phi = m_usedef_mgr.allocMDPhi(mdid, num_opnd);
    MDDEF_bb(phi) = bb;
    MDDEF_result(phi) = m_usedef_mgr.allocVMD(mdid, MDSSA_INIT_VERSION);
    ASSERT0(phi);

    m_usedef_mgr.genBBPhiList(bb->id())->append_head(phi);
}


//Insert phi for VMD.
//defbbs: record BBs which defined the VMD identified by 'mdid'.
//visited: record visited BB id
void MDSSAMgr::placePhiForMD(UINT mdid, List<IRBB*> const* defbbs,
                             DfMgr const& dfm, xcom::BitSet & visited,
                             List<IRBB*> & wl,
                             Vector<DefSBitSet*> & defmds_vec)
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

        for (INT i = dfcs->get_first(); i >= 0; i = dfcs->get_next(i)) {
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
    VMD * def = nullptr;
    bool same_def = true; //indicate all DEF of operands are the same stmt.
    for (IR const* opnd = phi->getOpndList();
         opnd != nullptr; opnd = opnd->get_next()) {
        VMD * v = phi->getOpndVMD(opnd, &m_usedef_mgr);
        if (v == nullptr) {
            //VOpnd may have been removed from MDSSAMgr, thus the VOpnd that
            //corresponding to the ID is NULL.
            continue;
        }
        ASSERT0(v->is_md());

        if (v->getDef() != nullptr) {
            if (def == nullptr) {
                def = v->getDef()->getResult();
            } else if (def != v->getDef()->getResult() &&
                       def != phi->getResult()) {
                same_def = false;
                break;
            }
            continue;
        }

        //DEF is dummy value to inidcate the region live-in MD.
        same_def = false;
        break;
    }
    ASSERT0(common_def);
    *common_def = def;
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
void MDSSAMgr::recordEffectMD(IRBB const* bb, OUT DefSBitSet & effect_md)
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
void MDSSAMgr::placePhi(DfMgr const& dfm, OUT DefSBitSet & effect_md,
                        DefMiscBitSetMgr & bs_mgr,
                        Vector<DefSBitSet*> & defined_md_vec,
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
        for (INT i = bs->get_first(&cur); i >= 0; i = bs->get_next(i, &cur)) {
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
    for (INT i = effect_md.get_first(&cur);
         i >= 0; i = effect_md.get_next(i, &cur)) {
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
void MDSSAMgr::renameUse(IR * ir)
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
    for (INT i = set->get_first(&iter); i >= 0; i = next) {
        next = set->get_next(i, &iter);
        VMD * vopnd = (VMD*)m_usedef_mgr.getVOpnd(i);
        ASSERT0(vopnd && vopnd->is_md() && vopnd->id() == (UINT)i);

        //Get the top-version on stack.
        Stack<VMD*> * vs = mapMD2VMDStack(vopnd->mdid());
        ASSERT0(vs);
        VMD * topv = vs->get_top();
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

            set->remove(vopnd, *m_sbs_mgr);
            added.append(topv, *m_sbs_mgr);
        }

        topv->addUse(ir);
    }

    set->bunion(added, *m_sbs_mgr);
    added.clean(*m_sbs_mgr);
}


void MDSSAMgr::renameDef(IR * ir, IRBB * bb)
{
    ASSERT0(ir && ir->is_stmt());
    MDSSAInfo * mdssainfo = genMDSSAInfo(ir);
    ASSERT0(mdssainfo);

    VOpndSetIter iter;
    VOpndSet * set = mdssainfo->getVOpndSet();
    VOpndSet added;
    INT next;
    for (INT i = set->get_first(&iter); i >= 0; i = next) {
        next = set->get_next(i, &iter);
        VMD * vopnd = (VMD*)m_usedef_mgr.getVOpnd(i);
        ASSERT0(vopnd && vopnd->is_md() && vopnd->id() == (UINT)i);
        ASSERTN(vopnd->version() == MDSSA_INIT_VERSION,
                ("should be first meet"));

        //Update versioned MD.
        VMD * newv = m_usedef_mgr.allocVMD(vopnd->mdid(),
                                           genNewVersion(vopnd->mdid()));
        VMD * nearestv = mapMD2VMDStack(vopnd->mdid())->get_top();
        mapMD2VMDStack(vopnd->mdid())->push(newv);

        MDDef * mddef = m_usedef_mgr.allocMDDef();
        MDDEF_bb(mddef) = bb;
        MDDEF_result(mddef) = newv;
        MDDEF_is_phi(mddef) = false;
        if (nearestv != nullptr && nearestv->getDef() != nullptr) {
            addDefChain(nearestv->getDef(), mddef);
        }

        MDDEF_occ(mddef) = ir;

        VMD_def(newv) = mddef;

        set->remove(vopnd, *m_sbs_mgr);
        added.append(newv, *m_sbs_mgr);
    }

    set->bunion(added, *m_sbs_mgr);
    added.clean(*m_sbs_mgr);
}


//Cut off the DU chain between 'def' and its predecessors.
void MDSSAMgr::cutoffDefChain(MDDef * def)
{
    MDDef * prev = def->getPrev();
    if (prev != nullptr) {
        ASSERT0(prev->getNextSet() && prev->getNextSet()->find(def));
        prev->getNextSet()->remove(def, *m_sbs_mgr);
    }
    MDDEF_prev(def) = nullptr;
}


//Add relation to def1->def2 where def1 dominated def2.
void MDSSAMgr::addDefChain(MDDef * def1, MDDef * def2)
{
    ASSERT0(def1 && def2);
    ASSERTN(def2->getPrev() == nullptr, ("should cutoff outdated def-relation"));
    if (def1->getNextSet() == nullptr) {
        MDDEF_nextset(def1) = m_usedef_mgr.allocMDDefSet();
    }
    def1->getNextSet()->append(def2, *m_sbs_mgr);
    MDDEF_prev(def2) = def1;
}


//Rename VMD from current version to the top-version on stack if it exist.
void MDSSAMgr::renamePhiResult(IN IRBB * bb)
{
    ASSERT0(bb);
    MDPhiList * philist = getPhiList(bb);
    if (philist == nullptr) { return; }

    for (MDPhiListIter  sct = philist->get_head();
         sct != philist->end(); sct = philist->get_next(sct)) {
        MDPhi * phi = sct->val();
        ASSERT0(phi && phi->is_phi() && phi->getBB() == bb);

        //Rename phi result.
        VMD * vopnd = phi->getResult();
        ASSERT0(vopnd && vopnd->is_md());

        //Update versioned MD.
        VMD * newv = m_usedef_mgr.allocVMD(vopnd->mdid(),
                                           genNewVersion(vopnd->mdid()));
        mapMD2VMDStack(vopnd->mdid())->push(newv);

        MDDEF_result(phi) = newv;
        cutoffDefChain(phi);

        VMD_def(newv) = phi;
    }
}


//Rename VMD from current version to the top-version on stack if it exist.
void MDSSAMgr::renameBB(IN IRBB * bb)
{
    renamePhiResult(bb);
    for (IR * ir = BB_first_ir(bb); ir != nullptr; ir = BB_next_ir(bb)) {
        //Rename opnd, not include phi.
        //Walk through rhs expression IR tree to rename memory's VMD.
        m_iter.clean();
        for (IR * opnd = iterInit(ir, m_iter);
             opnd != nullptr; opnd = iterNext(m_iter)) {
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
            renameUse(opnd);
        }

        if (!ir->isMemoryRef() || ir->isWritePR()) { continue; }

        //Rename result.
        renameDef(ir, bb);
    }
}


Stack<VMD*> * MDSSAMgr::mapMD2VMDStack(UINT mdid)
{
    Stack<VMD*> * stack = m_map_md2stack.get(mdid);
    if (stack == nullptr) {
        stack = new Stack<VMD*>();
        m_map_md2stack.set(mdid, stack);
    }
    return stack;
}


void MDSSAMgr::renamePhiOpndInSuccBB(IRBB * bb)
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
        handlePhiInSuccBB(m_cfg->getBB(succv->id()), opnd_idx);
    } 
}


//Replace opnd of PHI of 'succ' with top SSA version.
void MDSSAMgr::handlePhiInSuccBB(IRBB * succ, UINT opnd_idx)
{
    MDPhiList * philist = getPhiList(succ);
    if (philist == nullptr) { return; }

    for (MDPhiListIter sct = philist->get_head();
         sct != philist->end(); sct = philist->get_next(sct)) {
        MDPhi * phi = sct->val();
        ASSERT0(phi && phi->is_phi());

        UINT j = 0;
        IR * opnd;
        for (opnd = phi->getOpndList();
             opnd != nullptr && j < opnd_idx; opnd = opnd->get_next(), j++) {}
        ASSERT0(j == opnd_idx && opnd && opnd->is_id());

        ASSERT0(opnd->getRefMD());
        VMD * topv = mapMD2VMDStack(opnd->getRefMD()->id())->get_top();
        ASSERTN(topv, ("miss def-stmt to operand of phi"));

        MDSSAInfo * opnd_ssainfo = getMDSSAInfoIfAny(opnd);
        ASSERT0(opnd_ssainfo);
        opnd_ssainfo->cleanVOpndSet(getUseDefMgr());
        opnd_ssainfo->addVOpnd(topv, getUseDefMgr());
        topv->addUse(opnd);
    }
}


void MDSSAMgr::handleBBRename(IRBB * bb, xcom::DefSBitSet const& effect_mds,
                              DefSBitSet & defed_mds,
                              MOD BB2VMDMap & bb2vmdmap)
{
    ASSERT0(bb2vmdmap.get(bb->id()) == nullptr);
    xcom::TMap<UINT, VMD*> * mdid2vmd = new xcom::TMap<UINT, VMD*>();
    bb2vmdmap.set(bb->id(), mdid2vmd);

    DefSBitSetIter cur = nullptr;
    for (INT mdid = defed_mds.get_first(&cur);
         mdid >= 0; mdid = defed_mds.get_next(mdid, &cur)) {
        VMD * vmd = mapMD2VMDStack(mdid)->get_top();
        //ASSERT0(vmd);
        ASSERT0(vmd || !effect_mds.is_contain(mdid));
        if (vmd != nullptr) {
            mdid2vmd->set(VMD_mdid(vmd), vmd);
        }
    }
    renameBB(bb);
    renamePhiOpndInSuccBB(bb);
}


//defed_prs_vec: for each BB, indicate PRs which has been defined.
void MDSSAMgr::renameInDomTreeOrder(xcom::DefSBitSet const& effect_mds,
                                    IRBB * root, xcom::Graph & domtree,
                                    Vector<DefSBitSet*> & defed_mds_vec)
{
    xcom::Stack<IRBB*> stk;
    UINT n = m_rg->getBBList()->get_elem_count();
    xcom::BitSet visited(n / BIT_PER_BYTE);
    BB2VMDMap bb2vmdmap;
    IRBB * v;
    stk.push(root);
    List<IR*> lst; //for tmp use.
    while ((v = stk.get_top()) != nullptr) {
        if (!visited.is_contain(v->id())) {
            visited.bunion(v->id());
            xcom::DefSBitSet * defed_mds = defed_mds_vec.get(v->id());
            ASSERT0(defed_mds);
            handleBBRename(v, effect_mds, *defed_mds, bb2vmdmap);
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
            xcom::TMap<UINT, VMD*> * mdid2vmd = bb2vmdmap.get(v->id());
            ASSERT0(mdid2vmd);
            xcom::DefSBitSet * defed_mds = defed_mds_vec.get(v->id());
            ASSERT0(defed_mds);

            DefSBitSetIter cur = nullptr;
            for (INT i = defed_mds->get_first(&cur);
                 i >= 0; i = defed_mds->get_next(i, &cur)) {
                Stack<VMD*> * vs = mapMD2VMDStack(i);
                ASSERT0(vs->get_bottom());
                VMD * vmd = mdid2vmd->get(VMD_mdid(vs->get_top()));
                while (vs->get_top() != vmd) {
                    vs->pop();
                }
            }

            //vmdmap is useless from now on.
            bb2vmdmap.set(v->id(), nullptr);
            delete mdid2vmd;
        }
    }

    #ifdef _DEBUG_
    //Verify if vpmap of each BB has been deleted.
    for (INT i = 0; i <= bb2vmdmap.get_last_idx(); i++) {
        ASSERT0(bb2vmdmap.get(i) == nullptr);
    }
    #endif
}


//Replace oldir with newir.
void MDSSAMgr::changeIR(IR * oldir, IR * newir)
{
    ASSERT0(oldir && newir);
    if (oldir->is_stmt()) {
        ASSERT0(newir->is_stmt());
        changeDef(oldir, newir);
    } else {
        ASSERT0(oldir->is_exp() && newir->is_exp());
        changeUse(oldir, newir);
    }
    IR_ai(newir) = IR_ai(oldir);
    IR_ai(oldir) = nullptr;
}


//Rename variables.
void MDSSAMgr::rename(xcom::DefSBitSet const& effect_mds,
                      Vector<DefSBitSet*> & defed_mds_vec,
                      xcom::Graph & domtree)
{
    START_TIMER(t, "MDSSA: Rename");
    BBList * bblst = m_rg->getBBList();
    if (bblst->get_elem_count() == 0) { return; }

    DefSBitSetIter cur = nullptr;
    for (INT mdid = effect_mds.get_first(&cur);
         mdid >= 0; mdid = effect_mds.get_next(mdid, &cur)) {
        VMD * v = m_usedef_mgr.allocVMD(mdid, MDSSA_INIT_VERSION);
        mapMD2VMDStack(mdid)->push(v);
    }

    ASSERT0(m_cfg->getEntry());
    renameInDomTreeOrder(effect_mds, m_cfg->getEntry(), domtree, defed_mds_vec);
    END_TIMER(t, "MDSSA: Rename");
}


void MDSSAMgr::cleanIRSSAInfo(IRBB * bb)
{
    IRListIter lit;
    IRIter it;
    for (IR * ir = BB_irlist(bb).get_head(&lit);
         ir != nullptr; ir = BB_irlist(bb).get_next(&lit)) {
        it.clean();
        for (IR * k = iterInit(ir, it); k != nullptr; k = iterNext(it)) {
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


void MDSSAMgr::destructionInDomTreeOrder(IRBB * root, xcom::Graph & domtree)
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


//This function perform SSA destruction via scanning BB in preorder
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
        if (mddef->is_phi()) {
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
    END_TIMER(tverify, "MDSSA: Verify After Pass");
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
        UINT vopndnum = opnd_mdssainfo->readVOpndSet()->get_elem_count();
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
    MDSSAMgr * pthis = const_cast<MDSSAMgr*>(this);
    for (IRBB * bb = bblst->get_head(); bb != nullptr;
         bb = bblst->get_next()) {
        m_cfg->get_preds(preds, bb);
        MDPhiList * philist = getPhiList(bb);
        if (philist == nullptr) { continue; }

        UINT prednum = bb->getNumOfPred(m_cfg);
        for (MDPhiListIter sct = philist->get_head();
             sct != philist->end(); sct = philist->get_next(sct)) {
            MDPhi * phi = sct->val();
            ASSERT0(phi);
            verifyPhiOpndList(phi, prednum);
        }
    }
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

    ASSERT0(def->getOcc() && def->getOcc()->is_stmt());
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
    for (INT i = set->get_first(&iter); i >= 0; i = set->get_next(i, &iter)) {
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
            bool find;
            IR const* mapped = ir2mdssainfo.get(mdssainfo, &find);
            ASSERTN(!find, ("mdssainfo is not unique."));
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
            for (MDPhiListIter sct = philist->get_head();
                 sct != philist->end(); sct = philist->get_next(sct)) {
                MDPhi const* phi = sct->val();
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
            for (IR const* x = iterInit(ir, pthis->m_iter);
                 x != nullptr; x = iterNext(pthis->m_iter)) {
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
//Change Def stmt from 'olddef' to 'newdef'.
//olddef: source stmt.
//newdef: target stmt.
//e.g: oldef->USE change to newdef->USE.
void MDSSAMgr::changeDef(IR * olddef, IR * newdef)
{
    ASSERT0(olddef && newdef && olddef->is_stmt() && newdef->is_stmt());
    ASSERT0(olddef != newdef);
    ASSERT0(newdef->isMemoryRefNonPR());
    MDSSAInfo * oldmdssainfo = getMDSSAInfoIfAny(olddef);
    ASSERT0(oldmdssainfo);

    VOpndSetIter iter = nullptr;
    for (INT i = oldmdssainfo->getVOpndSet()->get_first(&iter);
         i >= 0; i = oldmdssainfo->getVOpndSet()->get_next(i, &iter)) {
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


//DU chain operation.
//Change Use expression from 'olduse' to 'newuse'.
//olduse: source expression.
//newuse: target expression.
//e.g: Change MDSSA DU chain DEF->olduse to DEF->newuse.
void MDSSAMgr::changeUse(IR * olduse, IR * newuse)
{
    ASSERT0(olduse && newuse && olduse->is_exp() && newuse->is_exp());
    ASSERT0(olduse != newuse);
    MDSSAInfo * oldmdssainfo = getMDSSAInfoIfAny(olduse);
    ASSERT0(oldmdssainfo);

    MDSSAInfo * newmdssainfo = genMDSSAInfo(newuse);
    ASSERT0(newmdssainfo);
    VOpndSetIter iter = nullptr;
    INT next = -1;
    for (INT i = oldmdssainfo->getVOpndSet()->get_first(&iter);
         i >= 0; i = next) {
        next = oldmdssainfo->getVOpndSet()->get_next(i, &iter);
        VMD * oldvopnd = (VMD*)getUseDefMgr()->getVOpnd(i);
        ASSERT0(oldvopnd && oldvopnd->is_md());
        if (!oldvopnd->findUse(olduse)) { continue; }

        //Update VOpnd's UseSet for olduse's MDSSAInfo.
        oldvopnd->removeUse(olduse);

        //Update ir's VOpndSet.
        oldmdssainfo->removeVOpnd(oldvopnd, getUseDefMgr());
    }

    for (INT i = newmdssainfo->getVOpndSet()->get_first(&iter);
         i >= 0; i = newmdssainfo->getVOpndSet()->get_next(i, &iter)) {
        VMD * newvopnd = (VMD*)getUseDefMgr()->getVOpnd(i);
        ASSERT0(newvopnd && newvopnd->is_md());
        //Update VOpnd's UseSet for newuse's MDSSAInfo.
        newvopnd->addUse(newuse);
    }
}


//Coalesce DU chain, actually the version of MD, from 'src' to 'tgt'.
//'src' and 'tgt' refered the same MD.
//This function replace definition of USE of src to tgt's defintion.
//src' and 'tgt' is the form of copy operation.
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
    for (INT i = src_mdssainfo->getVOpndSet()->get_first(&iter1);
         i >= 0; i = src_mdssainfo->getVOpndSet()->get_next(i, &iter1)) {
        VMD * src_vopnd = (VMD*)getUseDefMgr()->getVOpnd(i);
        ASSERT0(src_vopnd && src_vopnd->is_md());

        //Find the MD in tgt's vopnd-set which has same mdid with src's
        //except the distinct version.
        //e.g: src has MD6Vx, find MD6Vy in tgt vopnd set.
        VOpndSetIter iter2 = nullptr;
        VMD * tgt_vopnd = nullptr;
        for (INT j = tgt_mdssainfo->getVOpndSet()->get_first(&iter2);
             j >= 0; j = tgt_mdssainfo->getVOpndSet()->get_next(j, &iter2)) {
            VMD * t = (VMD*)getUseDefMgr()->getVOpnd(j);
            ASSERT0(t && t->is_md());
            if (t->mdid() == src_vopnd->mdid()) {
                ASSERT0(t != src_vopnd);
                tgt_vopnd = t;
                break;
            }
        }

        ASSERTN(tgt_vopnd, ("no MD correspond to src"));
        ASSERTN(tgt_vopnd->version() != src_vopnd->version(),
                ("DEF and USE reference same version MD"));
        //Replace the USE of src to tgt.
        replaceVOpndForAllUse(tgt_vopnd, src_vopnd);
    }
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
//new MDSSAInfo.
void MDSSAMgr::copyAndAddMDSSAOcc(IR * ir, MDSSAInfo const* src)
{
    MDSSAInfo * irmdssainfo = genMDSSAInfo(ir);
    if (irmdssainfo != src) {
        irmdssainfo->copy(*src, getUseDefMgr());
    }
    addUseToMDSSAInfo(ir, irmdssainfo);
}


void MDSSAMgr::copyMDSSAInfo(IR * tgt, IR const* src)
{
    ASSERT0(MDSSAMgr::hasMDSSAInfo(tgt));
    MDSSAInfo * tgtinfo = genMDSSAInfo(tgt);
    MDSSAInfo const* srcinfo = MDSSAMgr::getMDSSAInfoIfAny(src);
    ASSERT0(srcinfo);
    tgtinfo->copy(*srcinfo, getUseDefMgr());
}


void MDSSAMgr::addStmtToMDSSAMgr(IR * ir, IR const* ref)
{
    ASSERT0(0);//TODO:
}


//Add occurence to each VOpnd in mdssainfo.
//ir: occurence to be added.
//ref: the reference that is isomorphic to 'ir'.
//     It is used to retrieve MDSSAInfo.
void MDSSAMgr::addMDSSAOcc(IR * ir, IR const* ref)
{
    ASSERT0(ir->isIREqual(ref, false));
    if (ir->is_stmt()) {
        addStmtToMDSSAMgr(ir, ref);
    } else {
        MDSSAInfo * mdssainfo = getMDSSAInfoIfAny(ref);
        if (mdssainfo != nullptr) {
            copyAndAddMDSSAOcc(ir, mdssainfo);
        }
    }
    for (UINT i = 0; i < IR_MAX_KID_NUM(ir); i++) {
        IR const* refkid = ref->getKid(i);
        IR * x = ir->getKid(i);
        for (; x != nullptr; x = x->get_next(), refkid = refkid->get_next()) {
            ASSERTN(refkid, ("ir is not isomorphic to ref"));
            addMDSSAOcc(x, refkid);
        }
    }
}


//Remove MDSSA Use-Def chain for all memory references in IR Tree
//that rooted by 'ir'. Note ir can be stmt or expression.
//e.g:ir = ...
//    ... = ir //S1
//If S1 deleted, ir should be removed from its useset in MDSSAInfo.
//NOTE: If ir is an IR tree, e.g: ild(x, ld(y)), removing ild(x) means
//ld(y) will be removed as well. Therefore ld(y)'s MDSSAInfo will be
//updated as well.
void MDSSAMgr::removeMDSSAOcc(IR * ir)
{
    if (ir->is_stmt()) {
        //removeAllUse(ir);
        removeStmtFromMDSSAMgr(ir);
    } else {
        MDSSAInfo * mdssainfo = getMDSSAInfoIfAny(ir);
        if (mdssainfo != nullptr) {
            mdssainfo->removeUse(ir, getUseDefMgr());
        }
    }

    //TBD: Why do you remove PRSSA info in MDSSAMgr.
    //if ((prssainfo = ir->getSSAInfo()) != nullptr) {
    //    //Whole IR tree may be removed via this function recursively.
    //    //Maintain the SSAInfo of read-pr/write-pr operation.
    //    prssainfo->removeUse(ir);
    //}
    for (UINT i = 0; i < IR_MAX_KID_NUM(ir); i++) {
        for (IR * x = ir->getKid(i); x != nullptr; x = x->get_next()) {
            removeMDSSAOcc(x);
        }
    }
}


//Remove DEF-USE chain if exist in between 'stmt' and 'exp'.
//This function will remove 'exp' from occurence set.
//stmt: IR stmt that is DEF of 'exp'.
//exp: IR expression to be removed.
void MDSSAMgr::removeDUChain(IR const* stmt, IR const* exp)
{
    ASSERT0(stmt && exp && stmt->is_stmt() && exp->is_exp());
    MDSSAInfo * mdssainfo = getMDSSAInfoIfAny(exp);
    if (mdssainfo == nullptr) { return; }
    VOpndSetIter iter = nullptr;
    INT next_i = -1;
    for (INT i = mdssainfo->getVOpndSet()->get_first(&iter);
         i >= 0; i = next_i) {
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
//stmt' will have not any USE expression when function returned.
void MDSSAMgr::removeAllUse(IR const* stmt)
{
    ASSERT0(stmt && stmt->is_stmt());
    MDSSAInfo * mdssainfo = getMDSSAInfoIfAny(stmt);
    if (mdssainfo == nullptr) { return; }

    VOpndSetIter iter = nullptr;
    INT next_i = -1;
    for (INT i = mdssainfo->getVOpndSet()->get_first(&iter);
         i >= 0; i = next_i) {
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


//The function change VOpnd from 'from' to 'to', for each IR ir in 'from'
//UseSet.
//Note the UseSet of 'from' will be clean.
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


//The function remove 'vopnd' from MDSSAInfo of each ir its UseSet.
//Note the UseSet will be clean.
void MDSSAMgr::removeVOpndForAllUse(MOD VMD * vopnd)
{
    ASSERT0(vopnd->is_md());
    VMD::UseSet * useset = vopnd->getUseSet();
    VMD::UseSetIter vit;
    for (INT i = useset->get_first(vit);
         !vit.end(); i = useset->get_next(vit)) {
        IR * ir = m_rg->getIR(i);
        ASSERT0(ir && (ir->isMemoryRef() || ir->is_id()));
        MDSSAInfo * mdssainfo = getMDSSAInfoIfAny(ir);
        ASSERT0(mdssainfo);

        if (ir->is_id()) {
            //An individual ID can NOT represent multiple versioned MD, thus
            //the VOpnd of ID must be unique.
            mdssainfo->cleanVOpndSet(getUseDefMgr());
        } else {
            mdssainfo->removeVOpnd(vopnd, getUseDefMgr());
        }

        //Note VOpndSet of 'vopnd' may be empty after the removing.
        //It does not happen when MDSSA just constructed. The USE that
        //without real-DEF will have a virtual-DEF that version is 0.
        //During some increment-maintaining of MDSSA, 'vopnd' may be removed,
        //just like what current function does.
        //This means the current USE, 'ir', does not have real-DEF stmt, the
        //value of 'ir' always coming from parameter of global value.
        if (ir->is_id() && ID_phi(ir) != nullptr) {
            //CASE: to avoid assertions that raised by verify() which is used
            //to guanrantee VOpnd of MDPhi is not NULL, replace the removed
            //vopnd with initial-version vopnd.
            ASSERT0(vopnd->is_md());
            mdssainfo->addVOpnd(genInitVersionVMD(((VMD const*)vopnd)->mdid()),
                                getUseDefMgr());
        }
    }
    vopnd->cleanUseSet();
}


//This function removes 'phi' from MDSSAMgr.
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
        removeMDSSAOcc(opnd);
    }

    VMD * vopnd = phi->getResult();
    removePhiFromDDChain(phi, prev);
    m_rg->freeIRTreeList(phi->getOpndList());
    MDPHI_opnd_list(phi) = nullptr;
    removeVOpndForAllUse(vopnd);
    removeVMD(vopnd);
}


//Remove the MDSSAInfo related info of 'stmt' from MDSSAMgr.
//The MDSSAInfo includes Def and UseSet info.
//Note this function only handle stmt's MDSSAInfo, thus it will not
//process its RHS expression.
void MDSSAMgr::removeStmtFromMDSSAMgr(IR const* stmt)
{
    ASSERT0(stmt && stmt->is_stmt());
    MDSSAInfo * stmtmdssainfo = getMDSSAInfoIfAny(stmt);
    if (stmtmdssainfo == nullptr) { return; }

    VOpndSetIter iter = nullptr;
    INT next_i = -1;
    for (INT i = stmtmdssainfo->getVOpndSet()->get_first(&iter);
         i >= 0; i = next_i) {
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
                to->getNextSet()->bunion(*from->getNextSet(), *m_sbs_mgr);
            }
            return;
        }
        if (from->getNextSet() != nullptr) {
            //Note if MDDef indicates PHI, it does not have Previous DEF,
            //because PHI has multiple Previous DEFs rather than single DEF.
            to->getNextSet()->bunion(*from->getNextSet(), *m_sbs_mgr);
        }
        return;
    }
    if (to->getNextSet() == nullptr || from->getNextSet() == nullptr) { return; }
    to->getNextSet()->bunion(*from->getNextSet(), *m_sbs_mgr);
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
                prev->getNextSet()->remove(mddef, *m_sbs_mgr);
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
            prev->getNextSet()->remove(mddef, *m_sbs_mgr);
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

    if (!vopnd->getUseSet()->is_empty()) { return false; }

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
        for (INT i = phi->getNextSet()->get_first(&nit);
             i >= 0; i = phi->getNextSet()->get_next(i, &nit)) {
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
bool MDSSAMgr::removeExpiredDU(IR * ir)
{
    //TODO: Do NOT attempt to remove if not found reference MD at USE point which
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
bool MDSSAMgr::removePhiHasCommonDef(List<IRBB*> * wl, MDPhi * phi)
{
    ASSERT0(phi);
    VMD * common_def = nullptr;
    if (!doOpndHaveSameDef(phi, &common_def)) {
        return false;
    }

    //commond_def may be NULL.
    //e.g:Phi: MD10V3 <- MD10V--(id:69)(BB3)
    //  The only operand of PHI has been removed by some passes, thus the
    //  commond_def is NULL.
    for (IR * opnd = phi->getOpndList();
         opnd != nullptr; opnd = opnd->get_next()) {
        VMD * vopnd = phi->getOpndVMD(opnd, &m_usedef_mgr);
        if (vopnd == nullptr) {
            //VOpnd may have been removed from MDSSAMgr, thus the VOpnd that
            //corresponding to current ID is NULL.
            continue;
        }

        ASSERT0(vopnd->is_md());
        if (wl != nullptr && vopnd->getDef() != nullptr) {
            ASSERT0(vopnd->getDef()->getBB());
            wl->append_tail(vopnd->getDef()->getBB());
        }

        vopnd->removeUse(opnd);
    }

    if (common_def == nullptr) {
        removePhiFromMDSSAMgr(phi, nullptr);
        return true;
    }

    if (common_def != phi->getResult()) {
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
    }

    return true;
}


//wl: is an optional parameter to record BB which expected to deal with.
//    It is a work-list that is used to drive iterative collection and
//    elimination of redundant PHI elmination.
bool MDSSAMgr::removePhiHasNoValidDef(List<IRBB*> * wl, MDPhi * phi)
{
    ASSERT0(phi);
    if (doOpndHaveValidDef(phi)) {
        return false;
    }

    for (IR * opnd = phi->getOpndList();
         opnd != nullptr; opnd = opnd->get_next()) {
        VMD * vopnd = phi->getOpndVMD(opnd, &m_usedef_mgr);
        if (vopnd == nullptr) {
            //VOpnd may be have been removed from MDSSAMgr, thus the VOpnd that
            //corresponding to current ID is NULL.
            continue;
        }

        ASSERT0(vopnd->is_md());
        if (wl != nullptr && vopnd->getDef() != nullptr) {
            ASSERT0(vopnd->getDef()->getBB());
            wl->append_tail(vopnd->getDef()->getBB());
        }

        vopnd->removeUse(opnd);
    }

    //TODO: remove following commented code.
    ////Remove the VOpnd that PHI described of its UseSet's VOpnd set.
    //IRSetIter iter = nullptr;
    //IRSet * useset = phi->getResult()->getUseSet();
    //for (INT i = useset->get_first(&iter);
    //     i >= 0; i = useset->get_next(i, &iter)) {
    //    IR * u = m_rg->getIR(i);
    //    ASSERT0(u && getMDSSAInfoIfAny(u));
    //
    //    //Change DEF.
    //    getMDSSAInfoIfAny(u)->removeVOpnd(phi->getResult(), getUseDefMgr());
    //}
    //removeDefFromDDChain(phi);
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
    for (MDPhiListIter sct = philist->get_head();
         sct != philist->end(); sct = next) {
        next = philist->get_next(sct);
        MDPhi * phi = sct->val();
        ASSERT0(phi);

        if (removePhiHasCommonDef(wl, phi)) {
            remove = true;
            philist->remove(prev, sct);
            continue;
        }

        if (removePhiHasNoValidDef(wl, phi)) {
            remove = true;
            philist->remove(prev, sct);
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
            philist->remove(prev, sct);
            continue;
        }
        prev = sct;
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


void MDSSAMgr::construction(OptCtx & oc)
{
    START_TIMER(t0, "MDSSA: Construction");
    m_rg->getPassMgr()->checkValidAndRecompute(&oc, PASS_DOM, PASS_UNDEF);
    ASSERT0(oc.is_ref_valid());
    ASSERT0(OC_is_dom_valid(oc));
    reinit();

    //Extract dominate tree of CFG.
    START_TIMER(t1, "MDSSA: Extract Dom Tree");
    DomTree domtree;
    m_cfg->get_dom_tree(domtree);
    END_TIMER(t1, "MDSSA: Extract Dom Tree");

    if (!construction(domtree)) {
        return;
    }
    m_is_valid = true;
    END_TIMER(t0, "MDSSA: Construction");
}


bool MDSSAMgr::construction(DomTree & domtree)
{
    ASSERT0(m_rg);
    START_TIMER(t1, "MDSSA: Build dominance frontier");
    DfMgr dfm;
    dfm.build((xcom::DGraph&)*m_cfg); //Build dominance frontier.
    END_TIMER(t1, "MDSSA: Build dominance frontier");
    if (dfm.hasHighDFDensityVertex((xcom::DGraph&)*m_cfg)) {
        return false;
    }

    List<IRBB*> wl;
    DefMiscBitSetMgr bs_mgr;
    DefSBitSet effect_mds(bs_mgr.getSegMgr());
    Vector<DefSBitSet*> defed_mds_vec;
    placePhi(dfm, effect_mds, bs_mgr, defed_mds_vec, wl);
    rename(effect_mds, defed_mds_vec, domtree);
    //Note you can clean version stack after renaming.
    ASSERT0(verifyPhi());
    prunePhi(wl);
    cleanLocalUsedData();

    if (g_is_dump_after_pass && g_dump_opt.isDumpMDSSAMgr()) {
        dump();
    }

    ASSERT0(verify());
    ASSERT0(verifyIRandBB(m_rg->getBBList(), m_rg));
    ASSERT0(verifyPhi() && verifyVMD());
    m_is_valid = true;
    return true;
}


bool MDSSAMgr::verifyMDSSAInfo(Region const* rg)
{
    MDSSAMgr * ssamgr = (MDSSAMgr*)(rg->getPassMgr()->
        queryPass(PASS_MD_SSA_MGR));
    if (ssamgr != nullptr && ssamgr->is_valid()) {
        ASSERT0(ssamgr->verify());
        ASSERT0(ssamgr->verifyPhi());
    }
    return true;
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
    MDPhiListIter  to_sct = to_philist->get_head();
    for (MDPhiListIter  from_sct = from_philist->get_head();
         from_sct != from_philist->end();
         from_sct = from_philist->get_next(from_sct)) {

        //Move MDPhi from 'from' to 'to'.
        MDPhi * phi = from_sct->val();
        MDDEF_bb(phi) = to;
        if (to_sct == nullptr) {
            //'to' BB does not have PHI list.
            to_sct = to_philist->append_head(phi);
        } else {
            //Make sure phi's order in 'to' is same with 'from'.
            to_sct = to_philist->insert_after(phi, to_sct);
        }
    }
    from_philist->clean();
}
//END MDSSAMgr

} //namespace xoc
