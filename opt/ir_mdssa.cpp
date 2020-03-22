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

author: Su Zhenyu
@*/
#include "cominc.h"
#include "comopt.h"

namespace xoc {

//
//START MDSSAMgr
//
size_t MDSSAMgr::count_mem()
{
    size_t count = 0;
    count += m_map_md2stack.count_mem();
    count += m_max_version.count_mem();
    count += m_usedef_mgr.count_mem();
    count += sizeof(MDSSAMgr);
    return count;
}


void MDSSAMgr::destroy()
{
    if (m_usedef_mgr.m_mdssainfo_pool == NULL) { return; }

    //CAUTION: If you do not finish out-of-SSA prior to destory(),
    //the reference to IR's MDSSA info will lead to undefined behaviors.
    //ASSERTN(!m_is_ssa_constructed,
    //   ("Still in ssa mode, you should do out of "
    //    "SSA before destroy"));

    cleanMD2Stack();
    freePhiList();
}


void MDSSAMgr::freePhiList()
{
    for (IRBB * bb = m_rg->getBBList()->get_head();
         bb != NULL; bb = m_rg->getBBList()->get_next()) {
        MDPhiList * philist = m_usedef_mgr.genBBPhiList(BB_id(bb));
        if (philist == NULL) { continue; }

        for (xcom::SC<MDPhi*> * sct = philist->get_head();
             sct != philist->end(); sct = philist->get_next(sct)) {
            MDPhi * phi = sct->val();
            ASSERT0(phi && phi->is_phi());
            m_rg->freeIRTreeList(phi->getOpndList());
        }
    }
}


//Clean version stack.
void MDSSAMgr::cleanMD2Stack()
{
    for (INT i = 0; i <= m_map_md2stack.get_last_idx(); i++) {
        xcom::Stack<VMD*> * s = m_map_md2stack.get(i);
        if (s != NULL) { delete s; }
    }
    m_map_md2stack.clean();
}


//Dump Region's IR BB list.
//DUMP ALL BBList DEF/USE/OVERLAP_DEF/OVERLAP_USE.
void MDSSAMgr::dumpRef(UINT indent)
{
    if (g_tfile == NULL) { return; }
    note("\n\n==---- DUMP MDSSAMgr: IR REFERENCE '%s' ----==\n",
         m_rg->getRegionName());
    BBList * bbs = m_rg->getBBList();
    ASSERT0(bbs);
    if (bbs->get_elem_count() != 0) {
        m_md_sys->dump(false);
    }

    //Dump imported variables referenced.
    note("\n==----==");
    MDSet * ru_maydef = m_rg->getMayDef();
    if (ru_maydef != NULL) {
        note("\nRegionMayDef(OuterRegion):");
        ru_maydef->dump(m_md_sys, true);
    }

    MDSet * ru_mayuse = m_rg->getMayUse();
    if (ru_mayuse != NULL) {
        note("\nRegionMayUse(OuterRegion):");
        ru_mayuse->dump(m_md_sys, true);
    }

    for (IRBB * bb = bbs->get_head(); bb != NULL; bb = bbs->get_next()) {
        note("\n--- BB%d ---", BB_id(bb));
        dumpBBRef(bb, indent);
    }
}


void MDSSAMgr::dumpBBRef(IN IRBB * bb, UINT indent)
{
    if (g_tfile == NULL) { return; }
    for (IR * ir = BB_first_ir(bb); ir != NULL; ir = BB_next_ir(bb)) {
        ir->dumpRef(m_rg, indent);
    }
}


//Dump ssa du stmt graph.
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
//have_renamed: set true if PRs have been renamed in construction.
void MDSSAMgr::dumpAllVMD()
{
    //if (g_tfile == NULL) { return; }
    //note("\n==---- DUMP MDSSAMgr:VMD ----==\n");
    //
    //for (INT i = 1; i <= getVMDVec()->get_last_idx(); i++) {
    //    VMD * v = getVMDVec()->get(i);
    //    ASSERT0(v);
    //    note("\nVMD%d:MD%dV%d: ",
    //        VOPND_id(v), VOPND_mdid(v), VOPND_ver(v));
    //    IR * def = VOPND_def(v);
    //    if (VOPND_ver(v) != 0) {
    //        //After renaming, MD must have defstmt if its version is nonzero.
    //        ASSERT0(def);
    //    }
    //    if (def != NULL) {
    //        ASSERT0(def->is_stmt() && !def->isWritePR());
    //        prt("DEF:%s,id%d", IRNAME(def), def->id());
    //    } else {
    //        prt("DEF:---");
    //    }
    //
    //    prt("\tUSE:");
    //
    //    MDSSAUseIter it = NULL;
    //    INT nexti = 0;
    //    for (INT j = VMD_uses(v).get_first(&it); it != NULL; j = nexti) {
    //        nexti = VMD_uses(v).get_next(j, &it);
    //        IR * use = m_rg->getIR(j);
    //        ASSERT0(!use->isReadPR());
    //
    //        prt("(%s,id%d)", IRNAME(use), IR_id(use));
    //
    //        if (nexti >= 0) {
    //            prt(",");
    //        }
    //    }
    //}
    fflush(g_tfile);
}


void MDSSAMgr::dump()
{
    if (g_tfile == NULL) { return; }
    note("\n\n==---- DUMP MDSSAMgr '%s'----==\n",
         m_rg->getRegionName());

    BBList * bbl = m_rg->getBBList();
    List<IR const*> lst;
    List<IR const*> opnd_lst;
    UINT const indent = 2;
    g_indent = 0;
    for (IRBB * bb = bbl->get_head(); bb != NULL; bb = bbl->get_next()) {
        note("\n--- BB%d ---", BB_id(bb));

        MDPhiList * philist = m_usedef_mgr.genBBPhiList(BB_id(bb));
        ASSERT0(philist);
        for (xcom::SC<MDPhi*> * sct = philist->get_head();
             sct != philist->end(); sct = philist->get_next(sct)) {
            MDPhi * phi = sct->val();
            ASSERT0(phi && phi->is_phi());
            note("\n");
            phi->dump(m_rg, &m_usedef_mgr);
        }

        for (IR * ir = BB_first_ir(bb); ir != NULL; ir = BB_next_ir(bb)) {
            note("\n");
            dumpIR(ir, m_rg);
            g_indent += indent;
            ir->dumpRef(m_rg, g_indent);

            bool parting_line = false;
            //Result
            if (ir->isMemoryRefNotOperatePR() || ir->isCallStmt()) {
                MDSSAInfo * mdssainfo = m_usedef_mgr.getMDSSAInfo(ir);
                ASSERT0(mdssainfo);
                SEGIter * iter = NULL;
                if (!parting_line) {
                    note("\n----");
                    parting_line = true;
                }
                dumpIR(ir, m_rg, NULL, false);

                for (INT i = mdssainfo->getVOpndSet()->get_first(&iter);
                     i >= 0; i = mdssainfo->getVOpndSet()->get_next(i, &iter)) {
                    note("\n--DEFREF:");
                    VMD * vopnd = (VMD*)m_usedef_mgr.getVOpnd(i);
                    ASSERT0(vopnd && vopnd->is_md());
                    if (vopnd->getDef() != NULL) {
                        ASSERT0(vopnd->getDef()->getOcc() == ir);
                    }
                    vopnd->dump(m_rg, &m_usedef_mgr);
                }
            }

            //Operand
            lst.clean();
            opnd_lst.clean();
            for (IR const* opnd = iterInitC(ir, lst);
                 opnd != NULL; opnd = iterNextC(lst)) {
                if (!opnd->isMemoryRefNotOperatePR() || opnd->is_stmt()) {
                    continue;
                }

                MDSSAInfo * mdssainfo = m_usedef_mgr.getMDSSAInfo(opnd);
                ASSERT0(mdssainfo);
                SEGIter * iter = NULL;
                if (!parting_line) {
                    note("\n----");
                    parting_line = true;
                }
                dumpIR(opnd, m_rg, NULL, false);
                note("\n--USEREF:");
                bool first = true;
                for (INT i = mdssainfo->getVOpndSet()->get_first(&iter);
                     i >= 0; i = mdssainfo->getVOpndSet()->get_next(i, &iter)) {
                    VMD * vopnd = (VMD*)m_usedef_mgr.getVOpnd(i);
                    ASSERT0(vopnd && vopnd->is_md());
                    if (first) {
                        first = false;
                    } else {
                        prt(",");
                    }
                    prt("MD%dV%d", vopnd->mdid(), vopnd->version());
                }
            }
            g_indent -= indent;
        }
    }

    fflush(g_tfile);
}


MDDef * MDSSAMgr::findNearestDef(IR const* ir)
{
    ASSERT0(ir);
    MDSSAInfo const* mdssainfo = m_usedef_mgr.getMDSSAInfo(ir);
    ASSERTN(mdssainfo, ("miss MDSSAInfo"));
    SEGIter * iter = NULL;
    INT lastrpo = -1;
    MDDef * last = NULL;
    for (INT i = mdssainfo->readVOpndSet()->get_first(&iter);
         i >= 0; i = mdssainfo->readVOpndSet()->get_next(i, &iter)) {
        VMD * t = (VMD*)m_usedef_mgr.getVOpnd(i);
        ASSERT0(t && t->is_md());
        MDDef * tdef = t->getDef();
        if (last == NULL) {
            if (tdef != NULL) {
                last = tdef;
                ASSERT0(tdef->getBB());
                lastrpo = BB_rpo(last->getBB());
                ASSERT0(lastrpo >= 0);
            } else {
                ASSERT0(t->version() == 0);
                //Regard the virtual def at the entry of region,
                //it is the farest def.
            }
            continue;
        }

        if (tdef == NULL) { continue; }

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

        //'tdef' and 'last' are placed in same BB.
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
MDDef * MDSSAMgr::findKillingDef(IR const* ir)
{
    ASSERT0(ir && ir->is_exp() && ir->isMemoryOpnd());

    MD const* opndmd = ir->getRefMD();
    if (opndmd == NULL || !opndmd->is_exact()) { return NULL; }

    MDDef * def = findNearestDef(ir);
    if (def == NULL || def->is_phi()) { return NULL; }

    ASSERT0(def->getOcc());

    MD const* defmd = def->getOcc()->getRefMD();
    if (defmd != NULL &&
        (defmd == opndmd ||
         (defmd->is_exact() && defmd->is_cover(opndmd)))) {
        //def is killing must-def.
        return def;
    }

    return NULL;
}


void MDSSAMgr::dumpDefChain(List<MDDef const*> & wl,
                            IRSet & visited,
                            VMD const* vopnd)
{
    if (vopnd->getDef() == NULL) { return; }
    MD const* vopndmd = m_rg->getMDSystem()->getMD(vopnd->mdid());
    ASSERT0(vopndmd);
    wl.clean();
    wl.append_tail(vopnd->getDef());
    xcom::BitSet visited_def;
    bool prt_left_parenthesis = false;
    bool need_comma = false;
    for (MDDef const* def = wl.remove_head();
         def != NULL; def = wl.remove_head()) {
        visited_def.bunion(def->id());
        if (need_comma) {
            need_comma = false;
            prt(",");
        }

        if (def->is_phi()) {
            ASSERT0(def->getResult()->mdid() == vopnd->mdid());
            if (!prt_left_parenthesis) {
                prt("(");
                prt_left_parenthesis = true;
            }
            prt("phi");
            need_comma = true;
            //Collect opnd of PHI to forward to
            //retrieve corresponding DEFs.
            for (IR const* phiopnd = MDPHI_opnd_list(def);
                 phiopnd != NULL; phiopnd = phiopnd->get_next()) {
                VMD * opndvmd = ((MDPhi*)def)->getOpndVMD(
                    phiopnd, &m_usedef_mgr);
                ASSERT0(opndvmd);
                if (opndvmd->getDef() != NULL &&
                    !visited_def.is_contain(
                        opndvmd->getDef()->id())) {
                    wl.append_tail(opndvmd->getDef());
                }
            }
            continue;
        }
        ASSERT0(def->getOcc());
        if (!visited.find(def->getOcc())) {
            visited.append(def->getOcc());
            if (!prt_left_parenthesis) {
                prt("(");
                prt_left_parenthesis = true;
            }
            prt("%s(id:%d)",
                IRNAME(def->getOcc()), def->getOcc()->id());
            need_comma = true;
        }

        MD const* defmd = def->getOcc()->getRefMD();
        if (defmd != NULL &&
            defmd->is_exact() &&
            vopndmd->is_exact() &&
            (defmd == vopndmd || defmd->is_cover(vopndmd))) {
            ; //def is killing may-def.
        } else {
            if (def->getPrev() != NULL &&
                !visited_def.is_contain(def->getPrev()->id())) {
                wl.append_tail(def->getPrev());
            }
        }
    }
    if (prt_left_parenthesis) {
        prt(")");
    }
}


void MDSSAMgr::dumpExpDUChainIter(IR const* ir,
                                  List<IR const*> & lst,
                                  List<IR const*> & opnd_lst,
                                  OUT bool * parting_line)
{

    IRSet visited(m_sbs_mgr->getSegMgr());
    xcom::List<MDDef const*> wl;
    lst.clean();
    opnd_lst.clean();
    for (IR const* opnd = iterInitC(ir, lst);
         opnd != NULL; opnd = iterNextC(lst)) {
        if (!opnd->isMemoryRefNotOperatePR() || opnd->is_stmt()) {
            continue;
        }

        MDSSAInfo * mdssainfo = m_usedef_mgr.getMDSSAInfo(opnd);
        ASSERT0(mdssainfo);
        SEGIter * iter = NULL;
        if (!(*parting_line)) {
            note("\n----------------");
            (*parting_line) = true;
        }

        note("\n");
        prt("%s(id:%d) --DEF LIST:", IRNAME(opnd), opnd->id());

        MDDef * kdef = findKillingDef(opnd);
        if (kdef != NULL) {
            prt("%s(id:%d)",
                IRNAME(kdef->getOcc()), kdef->getOcc()->id());
            continue;
        }

        visited.clean();
        bool first = true;
        for (INT i = mdssainfo->getVOpndSet()->get_first(&iter);
             i >= 0; i = mdssainfo->getVOpndSet()->get_next(i, &iter)) {
            VMD * vopnd = (VMD*)m_usedef_mgr.getVOpnd(i);
            ASSERT0(vopnd && vopnd->is_md());
            if (first) { first = false; }
            else { prt(","); }
            prt("MD%dV%d", vopnd->mdid(), vopnd->version());
            dumpDefChain(wl, visited, vopnd);
        }
    }
}


void MDSSAMgr::dumpDUChain()
{
    if (g_tfile == NULL) { return; }
    note("\n==---- DUMP MDSSAMgr DU CHAIN '%s' ----==\n",
         m_rg->getRegionName());

    BBList * bbl = m_rg->getBBList();
    xcom::List<IR const*> lst;
    xcom::List<IR const*> opnd_lst;

    g_indent = 0;
    for (IRBB * bb = bbl->get_head(); bb != NULL; bb = bbl->get_next()) {
        note("\n--- BB%d ---", BB_id(bb));
        MDPhiList * philist = m_usedef_mgr.genBBPhiList(BB_id(bb));
        ASSERT0(philist);
        for (xcom::SC<MDPhi*> * sct = philist->get_head();
             sct != philist->end(); sct = philist->get_next(sct)) {
            MDPhi * phi = sct->val();
            ASSERT0(phi && phi->is_phi());
            note("\n");
            phi->dump(m_rg, &m_usedef_mgr);
        }

        for (IR * ir = BB_first_ir(bb);
             ir != NULL; ir = BB_next_ir(bb)) {
            dumpIR(ir, m_rg);

            bool parting_line = false;
            //Result
            if (ir->isMemoryRefNotOperatePR() || ir->isCallStmt()) {
                MDSSAInfo * mdssainfo = m_usedef_mgr.getMDSSAInfo(ir);
                ASSERT0(mdssainfo);
                xcom::SEGIter * iter = NULL;
                if (!parting_line) {
                    note("\n----------------");
                    parting_line = true;
                }
                note("\n");
                prt("%s(id:%d) --USE LIST:", IRNAME(ir), ir->id());
                bool first = true;
                for (INT i = mdssainfo->getVOpndSet()->get_first(&iter);
                     i >= 0; i = mdssainfo->getVOpndSet()->get_next(i, &iter)) {
                    VMD * vopnd = (VMD*)m_usedef_mgr.getVOpnd(i);
                    ASSERT0(vopnd && vopnd->is_md());
                    if (vopnd->getDef() != NULL) {
                        ASSERT0(vopnd->getDef()->getOcc() == ir);
                    }

                    xcom::SEGIter * vit = NULL;
                    for (INT i2 = vopnd->getUseSet()->get_first(&vit);
                         i2 >= 0; i2 = vopnd->getUseSet()->get_next(i2, &vit)) {
                        if (first) {
                            first = false;
                        } else {
                            prt(",");
                        }
                        IR * use = m_rg->getIR(i2);
                        ASSERT0(use && (use->isMemoryRef() || use->is_id()));
                        prt("%s(id:%d(", IRNAME(use), use->id());
                        vopnd->dump();
                        prt("))");
                    }
                }
            }

            //Operand
            dumpExpDUChainIter(ir, lst, opnd_lst, &parting_line);
        }
    }

    fflush(g_tfile);
}

//Generate VMD for stmt and exp which reference memory.
void MDSSAMgr::initVMD(IN IR * ir, OUT DefSBitSet & maydef_md)
{
    m_iter.clean();
    for (IR * t = iterInit(ir, m_iter); t != NULL; t = iterNext(m_iter)) {
        if (!t->isMemoryRefNotOperatePR() && !t->isCallStmt()) {
            continue;
        }
        MDSSAInfo * mdssainfo = m_usedef_mgr.genMDSSAInfo(t);
        MD const* ref = t->getRefMD();
        if (ref != NULL && !ref->is_pr()) {
            VMD const* vmd = m_usedef_mgr.allocVMD(MD_id(ref), 0);
            ASSERT0(m_sbs_mgr);
            mdssainfo->getVOpndSet()->append(vmd, *m_sbs_mgr);
            if (t->is_stmt()) {
                maydef_md.bunion(MD_id(ref));
            }
        }

        MDSet const* refset = t->getRefMDSet();
        if (refset != NULL) {
            xcom::SEGIter * iter;
            for (INT i = refset->get_first(&iter);
                 i >= 0; i = refset->get_next((UINT)i, &iter)) {
                MD * md = m_md_sys->getMD(i);
                ASSERTN(md && !md->is_pr(), ("PR should not in MayBeSet"));
                VMD const* vmd2 = m_usedef_mgr.allocVMD(MD_id(md), 0);
                ASSERT0(m_sbs_mgr);
                mdssainfo->getVOpndSet()->append(vmd2, *m_sbs_mgr);
                if (t->is_stmt()) {
                    maydef_md.bunion(MD_id(md));
                }
            }
        }
    }
}


//'maydef_md': record MDs that defined in 'bb'.
void MDSSAMgr::collectDefinedMD(IN IRBB * bb, OUT DefSBitSet & maydef_md)
{
    for (IR * ir = BB_first_ir(bb); ir != NULL; ir = BB_next_ir(bb)) {
        initVMD(ir, maydef_md);
    }
}


void MDSSAMgr::insertPhi(UINT mdid, IN IRBB * bb)
{
    UINT num_opnd = m_cfg->getInDegree(m_cfg->getVertex(BB_id(bb)));

    //Here each operand and result of phi set to same type.
    //They will be revised to correct type during renaming.
    MDPhi * phi = m_usedef_mgr.allocMDPhi(mdid, num_opnd);
    MDDEF_bb(phi) = bb;
    MDDEF_result(phi) = m_usedef_mgr.allocVMD(mdid, 0);
    ASSERT0(phi);

    m_usedef_mgr.genBBPhiList(bb->id())->append_head(phi);
}


//Insert phi for VMD.
//defbbs: record BBs which defined the VMD identified by 'mdid'.
void MDSSAMgr::placePhiForMD(UINT mdid,
                             IN List<IRBB*> * defbbs,
                             DfMgr const& dfm,
                             xcom::BitSet & visited,
                             List<IRBB*> & wl,
                             Vector<DefSBitSet*> & defmds_vec)
{
    visited.clean();
    wl.clean();
    for (IRBB * defbb = defbbs->get_head();
         defbb != NULL; defbb = defbbs->get_next()) {
        wl.append_tail(defbb);
        //visited.bunion(BB_id(defbb));
    }

    while (wl.get_elem_count() != 0) {
        IRBB * bb = wl.remove_head();

        //Each basic block in dfcs is in dominance frontier of 'bb'.
        xcom::BitSet const* dfcs = dfm.getDFControlSet(BB_id(bb));
        if (dfcs == NULL) { continue; }

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
//If all opnds have same defintion or defined by current
//phi, the phi is redundant.
//common_def: record the common_def if the definition of all opnd is the same.
//TODO: p=phi(m,p), the only use of p is phi. the phi is redundant.
bool MDSSAMgr::isRedundantPHI(MDPhi const* phi, OUT VMD ** common_def) const
{
    ASSERT0(phi && common_def);

    if (xcom::cnt_list(phi->getOpndList()) == 0) { return true; }

    #define DUMMY_DEF_ADDR    0x1234
    VMD * def = NULL;
    bool same_def = true; //indicate all DEF of operands are the same stmt.

    for (IR const* opnd = phi->getOpndList();
         opnd != NULL; opnd = opnd->get_next()) {
        VMD * v = phi->getOpndVMD(opnd, &m_usedef_mgr);
        if (v == NULL) { continue; }
        ASSERT0(v->is_md());

        if (v->getDef() != NULL) {
            if (def == NULL) {
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


//Place phi and assign the v0 for each PR.
//'effect_prs': record the MD which need to versioning.
void MDSSAMgr::placePhi(DfMgr const& dfm,
                        OUT DefSBitSet & effect_md,
                        DefMiscBitSetMgr & bs_mgr,
                        Vector<DefSBitSet*> & defined_md_vec,
                        List<IRBB*> & wl)
{
    START_TIMER(t, "MDSSA: Place phi");

    //Record BBs which modified each MD.
    BBList * bblst = m_rg->getBBList();
    Vector<List<IRBB*>*> md2defbb(bblst->get_elem_count()); //for local used.

    for (IRBB * bb = bblst->get_head(); bb != NULL; bb = bblst->get_next()) {
        DefSBitSet * bs = bs_mgr.allocSBitSet();
        defined_md_vec.set(BB_id(bb), bs);
        collectDefinedMD(bb, *bs);

        //Record all modified MDs which will be versioned later.
        effect_md.bunion(*bs);

        //Record which BB defined these effect mds.
        SEGIter * cur = NULL;
        for (INT i = bs->get_first(&cur); i >= 0; i = bs->get_next(i, &cur)) {
            List<IRBB*> * bbs = md2defbb.get(i);
            if (bbs == NULL) {
                bbs = new List<IRBB*>();
                md2defbb.set(i, bbs);
            }
            bbs->append_tail(bb);
        }
    }

    //Place phi for lived MD.
    xcom::BitSet visited((bblst->get_elem_count()/8)+1);
    SEGIter * cur = NULL;
    for (INT i = effect_md.get_first(&cur);
         i >= 0; i = effect_md.get_next(i, &cur)) {
        placePhiForMD(i, md2defbb.get(i), dfm, visited, wl, defined_md_vec);
    }
    END_TIMER(t, "MDSSA: Place phi");

    //Free local used objects.
    for (INT i = 0; i <= md2defbb.get_last_idx(); i++) {
        List<IRBB*> * bbs = md2defbb.get(i);
        if (bbs == NULL) { continue; }
        delete bbs;
    }
}


void MDSSAMgr::renameUse(IR * ir)
{
    ASSERT0(ir && ir->is_exp());

    MDSSAInfo * mdssainfo = m_usedef_mgr.genMDSSAInfo(ir);
    ASSERT0(mdssainfo);

    SEGIter * iter;
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
        if (topv == NULL) {
            //MD does not have top-version, it has no def,
            //and may be parameter.
            continue;
        }

        //e.g: MD1 = MD2(VMD1)
        //    VMD1 will be renamed to VMD2, so VMD1 will not
        //    be there in current IR any more.

        //Set latest version of VMD be the USE of current opnd.
        if (topv->version() == 0) {
            //Do nothing.
            ASSERT0(vopnd == topv);
        } else if (vopnd != topv) {
            //vopnd may be ver0.
            //Current ir does not refer the old version VMD any more.
            ASSERT0(vopnd->version() == 0 || vopnd->getUseSet()->find(ir));
            ASSERT0(vopnd->version() == 0 || vopnd->getDef());
            ASSERT0(!topv->getUseSet()->find(ir));

            set->remove(vopnd, *m_sbs_mgr);
            added.append(topv, *m_sbs_mgr);
        }

        topv->getUseSet()->append(ir);
    }

    set->bunion(added, *m_sbs_mgr);
    added.clean(*m_sbs_mgr);
}


void MDSSAMgr::renameDef(IR * ir, IRBB * bb)
{
    ASSERT0(ir && ir->is_stmt());

    MDSSAInfo * mdssainfo = m_usedef_mgr.genMDSSAInfo(ir);
    ASSERT0(mdssainfo);

    SEGIter * iter;
    VOpndSet * set = mdssainfo->getVOpndSet();
    VOpndSet added;
    INT next;
    for (INT i = set->get_first(&iter); i >= 0; i = next) {
        next = set->get_next(i, &iter);
        VMD * vopnd = (VMD*)m_usedef_mgr.getVOpnd(i);
        ASSERT0(vopnd && vopnd->is_md() && vopnd->id() == (UINT)i);
        ASSERTN(vopnd->version() == 0, ("should be first meet"));

        UINT maxv = m_max_version.get(vopnd->mdid());
        VMD * newv = m_usedef_mgr.allocVMD(vopnd->mdid(), maxv + 1);
        m_max_version.set(vopnd->mdid(), maxv + 1);

        VMD * nearestv = mapMD2VMDStack(vopnd->mdid())->get_top();

        mapMD2VMDStack(vopnd->mdid())->push(newv);

        MDDef * mddef = m_usedef_mgr.allocMDDef();
        MDDEF_bb(mddef) = bb;
        MDDEF_result(mddef) = newv;
        MDDEF_is_phi(mddef) = false;
        if (nearestv != NULL && nearestv->getDef() != NULL) {
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


void MDSSAMgr::cutoffDefChain(MDDef * def)
{
    MDDef * prev = def->getPrev();
    if (prev != NULL) {
        ASSERT0(prev->getNextSet() && prev->getNextSet()->find(def));
        prev->getNextSet()->remove(def, *m_sbs_mgr);
    }
    MDDEF_prev(def) = NULL;
}


//Add relation to def1->def2 where def1 dominated def2.
void MDSSAMgr::addDefChain(MDDef * def1, MDDef * def2)
{
    ASSERT0(def1 && def2);
    ASSERTN(def2->getPrev() == NULL, ("should cutoff outdated def-relation"));
    if (def1->getNextSet() == NULL) {
        MDDEF_nextset(def1) = m_usedef_mgr.allocMDDefSet();
    }
    def1->getNextSet()->append(def2, *m_sbs_mgr);
    MDDEF_prev(def2) = def1;
}


//Rename VMD from current version to the top-version on stack if it exist.
void MDSSAMgr::renamePhiResult(IN IRBB * bb)
{
    ASSERT0(bb);
    MDPhiList * philist = m_usedef_mgr.genBBPhiList(BB_id(bb));
    ASSERT0(philist);
    for (xcom::SC<MDPhi*> * sct = philist->get_head();
         sct != philist->end(); sct = philist->get_next(sct)) {
        MDPhi * phi = sct->val();
        ASSERT0(phi && phi->is_phi() && phi->getBB() == bb);

        //Rename phi result.
        VMD * vopnd = phi->getResult();
        ASSERT0(vopnd && vopnd->is_md());

        UINT maxv = m_max_version.get(vopnd->mdid());
        VMD * newv = m_usedef_mgr.allocVMD(vopnd->mdid(), maxv + 1);
        m_max_version.set(vopnd->mdid(), maxv + 1);

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
    for (IR * ir = BB_first_ir(bb); ir != NULL; ir = BB_next_ir(bb)) {
        //Rename opnd, not include phi.
        //Walk through rhs expression IR tree to rename IR_PR's VMD.
        m_iter.clean();
        for (IR * opnd = iterInit(ir, m_iter);
             opnd != NULL; opnd = iterNext(m_iter)) {
            if (!opnd->isMemoryOpnd() || opnd->isReadPR()) {
                continue;
            }

            MD const* ref = opnd->getRefMD();
            if (ref != NULL) {
                renameUse(opnd);
            }

            MDSet const* refset = opnd->getRefMDSet();
            if (refset != NULL) {
                SEGIter * iter;
                for (INT i = refset->get_first(&iter);
                     i >= 0; i = refset->get_next((UINT)i, &iter)) {
                    MD * md = m_md_sys->getMD(i);
                    CHECK_DUMMYUSE(md);

                    //In memory SSA, rename the MD even
                    //if it is ineffect to keep DU relation, e.g:
                    //  int bar(int * p, int * q, int * m, int * n)
                    //  {
                    //      *p = *q + 20; *p define MD2V1
                    //      *m = *n - 64; *n use MD2V1
                    //      return 0;
                    //  }
                    //if (!md->is_effect()) { continue; }

                    renameUse(opnd);
                }
            }
        }

        if (!ir->isMemoryRef() || ir->isWritePR()) { continue; }

        //Rename result.
        renameDef(ir, bb);
    }
}


Stack<VMD*> * MDSSAMgr::mapMD2VMDStack(UINT mdid)
{
    Stack<VMD*> * stack = m_map_md2stack.get(mdid);
    if (stack == NULL) {
        stack = new Stack<VMD*>();
        m_map_md2stack.set(mdid, stack);
    }
    return stack;
}


void MDSSAMgr::handleBBRename(IRBB * bb,
                              DefSBitSet & defed_mds,
                              IN OUT BB2VMDMap & bb2vmdmap)
{
    ASSERT0(bb2vmdmap.get(BB_id(bb)) == NULL);
    xcom::TMap<UINT, VMD*> * mdid2vmd = new xcom::TMap<UINT, VMD*>();
    bb2vmdmap.set(BB_id(bb), mdid2vmd);

    xcom::SEGIter * cur = NULL;
    for (INT mdid = defed_mds.get_first(&cur);
         mdid >= 0; mdid = defed_mds.get_next(mdid, &cur)) {
        VMD * vmd = mapMD2VMDStack(mdid)->get_top();
        ASSERT0(vmd);
        mdid2vmd->set(VMD_mdid(vmd), vmd);
    }

    renameBB(bb);

    //Rename PHI opnd in successor BB.
    List<IRBB*> succs;
    m_cfg->get_succs(succs, bb);
    if (succs.get_elem_count() ==0) { return; }

    //Replace the jth opnd of PHI with 'topv' which in bb's successor.
    List<IRBB*> preds;
    for (IRBB * succ = succs.get_head();
         succ != NULL; succ = succs.get_next()) {
        //Compute which predecessor 'bb' is with respect to its successor.
        m_cfg->get_preds(preds, succ);
        UINT idx = 0; //the index of corresponding predecessor.
        IRBB * p;
        for (p = preds.get_head(); p != NULL; p = preds.get_next(), idx++) {
            if (p == bb) {
                break;
            }
        }
        ASSERT0(p);

        //Replace opnd of PHI of 'succ' with top SSA version.
        MDPhiList * philist = m_usedef_mgr.genBBPhiList(BB_id(succ));
        ASSERT0(philist);
        for (xcom::SC<MDPhi*> * sct = philist->get_head();
             sct != philist->end(); sct = philist->get_next(sct)) {
            MDPhi * phi = sct->val();
            ASSERT0(phi && phi->is_phi());

            UINT j = 0;
            IR * opnd;
            for (opnd = phi->getOpndList();
                 opnd != NULL && j < idx; opnd = opnd->get_next(), j++) {}
            ASSERT0(j == idx && opnd && opnd->is_id());

            ASSERT0(opnd->getRefMD());
            VMD * topv = mapMD2VMDStack(opnd->getRefMD()->id())->get_top();
            ASSERTN(topv, ("miss def-stmt to operand of phi"));

            MDSSAInfo * opnd_ssainfo = m_usedef_mgr.getMDSSAInfo(opnd);
            ASSERT0(opnd_ssainfo);

            opnd_ssainfo->getVOpndSet()->clean(*m_sbs_mgr);
            opnd_ssainfo->getVOpndSet()->append(topv, *m_sbs_mgr);
            topv->getUseSet()->append(opnd);
        }
    }
}


//defed_prs_vec: for each BB, indicate PRs which has been defined.
void MDSSAMgr::renameInDomTreeOrder(IRBB * root,
                                    xcom::Graph & domtree,
                                    Vector<DefSBitSet*> & defed_mds_vec)
{
    xcom::Stack<IRBB*> stk;
    UINT n = m_rg->getBBList()->get_elem_count();
    xcom::BitSet visited(n / BIT_PER_BYTE);
    BB2VMDMap bb2vmdmap;
    IRBB * v;
    stk.push(root);
    List<IR*> lst; //for tmp use.
    while ((v = stk.get_top()) != NULL) {
        if (!visited.is_contain(BB_id(v))) {
            visited.bunion(BB_id(v));
            xcom::DefSBitSet * defed_mds = defed_mds_vec.get(BB_id(v));
            ASSERT0(defed_mds);
            handleBBRename(v, *defed_mds, bb2vmdmap);
        }

        xcom::Vertex const* bbv = domtree.getVertex(BB_id(v));
        bool all_visited = true;
        for (xcom::EdgeC const* c = VERTEX_out_list(bbv);
             c != NULL; c = EC_next(c)) {
            xcom::Vertex * dom_succ = EDGE_to(EC_edge(c));
            if (dom_succ == bbv) { continue; }
            if (!visited.is_contain(VERTEX_id(dom_succ))) {
                ASSERT0(m_cfg->getBB(VERTEX_id(dom_succ)));
                all_visited = false;
                stk.push(m_cfg->getBB(VERTEX_id(dom_succ)));
                break;
            }
        }

        if (all_visited) {
            stk.pop();

            //Do post-processing while all kids of BB has been processed.
            xcom::TMap<UINT, VMD*> * mdid2vmd = bb2vmdmap.get(BB_id(v));
            ASSERT0(mdid2vmd);
            xcom::DefSBitSet * defed_mds = defed_mds_vec.get(BB_id(v));
            ASSERT0(defed_mds);

            xcom::SEGIter * cur = NULL;
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
            bb2vmdmap.set(BB_id(v), NULL);
            delete mdid2vmd;
        }
    }

    #ifdef _DEBUG_
    //Verify if vpmap of each BB has been deleted.
    for (INT i = 0; i <= bb2vmdmap.get_last_idx(); i++) {
        ASSERT0(bb2vmdmap.get(i) == NULL);
    }
    #endif
}


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
    IR_ai(oldir) = NULL;
}


//Rename variables.
void MDSSAMgr::rename(xcom::DefSBitSet & effect_mds,
                      Vector<DefSBitSet*> & defed_mds_vec,
                      xcom::Graph & domtree)
{
    START_TIMER(t, "MDSSA: Rename");
    BBList * bblst = m_rg->getBBList();
    if (bblst->get_elem_count() == 0) { return; }

    xcom::SEGIter * cur = NULL;
    for (INT mdid = effect_mds.get_first(&cur);
         mdid >= 0; mdid = effect_mds.get_next(mdid, &cur)) {
        VMD * v = m_usedef_mgr.allocVMD(mdid, 0);
        mapMD2VMDStack(mdid)->push(v);
    }

    ASSERT0(m_cfg->get_entry());
    renameInDomTreeOrder(m_cfg->get_entry(), domtree, defed_mds_vec);
    END_TIMER(t, "MDSSA: Rename");
}


void MDSSAMgr::destructBBSSAInfo(IRBB * bb)
{
    MDPhiList * philist = m_usedef_mgr.genBBPhiList(BB_id(bb));
    ASSERT0(philist);
    for (xcom::SC<MDPhi*> * sct = philist->get_head();
         sct != philist->end(); sct = philist->get_next(sct)) {
        MDPhi * phi = sct->val();
        ASSERT0(phi && phi->is_phi());
        stripPhi(phi);
    }
}


void MDSSAMgr::destructionInDomTreeOrder(IRBB * root, xcom::Graph & domtree)
{
    xcom::Stack<IRBB*> stk;
    UINT n = m_rg->getBBList()->get_elem_count();
    xcom::BitSet visited(n / BIT_PER_BYTE);
    BB2VMDMap bb2vmdmap(n);
    IRBB * v = NULL;
    stk.push(root);
    while ((v = stk.get_top()) != NULL) {
        if (!visited.is_contain(BB_id(v))) {
            visited.bunion(BB_id(v));
            destructBBSSAInfo(v);
        }

        xcom::Vertex * bbv = domtree.getVertex(BB_id(v));
        ASSERTN(bbv, ("dom tree is invalid."));

        xcom::EdgeC * c = VERTEX_out_list(bbv);
        bool all_visited = true;
        while (c != NULL) {
            xcom::Vertex * dom_succ = EDGE_to(EC_edge(c));
            if (dom_succ == bbv) { continue; }
            if (!visited.is_contain(VERTEX_id(dom_succ))) {
                ASSERT0(m_cfg->getBB(VERTEX_id(dom_succ)));
                all_visited = false;
                stk.push(m_cfg->getBB(VERTEX_id(dom_succ)));
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
    ASSERT0(m_cfg->get_entry());
    destructionInDomTreeOrder(m_cfg->get_entry(), domtree);
    m_is_ssa_constructed = false;

    END_TIMER(t, "MDSSA: destruction in dom tree order");
}


//Return true if inserting copy at the head of fallthrough BB
//of current BB's predessor.
//Note that do not free phi at this function, it will be freed
//by user.
void MDSSAMgr::stripPhi(MDPhi * phi)
{
    CHECK_DUMMYUSE(phi);
    //IRBB * bb = phi->getBB();
    //ASSERT0(bb);
    //
    //xcom::Vertex const* vex = m_cfg->getVertex(BB_id(bb));
    //ASSERT0(vex);
    //
    ////Temprarory RP to hold the result of PHI.
    //IR * phicopy = m_rg->buildPR(phi->getType());
    //phicopy->setRefMD(m_rg->genMDforPR(
    //    PR_no(phicopy), phicopy->getType()), m_rg);
    //phicopy->cleanRefMDSet();
    //IR * opnd = PHI_opnd_list(phi);
    //
    ////opnd may be CONST, LDA, PR.
    ////ASSERT0(opnd->is_pr());
    //ASSERT0(PHI_ssainfo(phi));
    //
    //UINT pos = 0;
    //for (xcom::EdgeC * el = VERTEX_in_list(vex), * nextel = NULL;
    //     el != NULL; el = nextel, opnd = opnd->get_next(), pos++) {
    //    ASSERT0(find_position(VERTEX_in_list(vex), el) == pos);
    //
    //    nextel = EC_next(el);
    //    INT pred = VERTEX_id(EDGE_from(EC_edge(el)));
    //
    //    ASSERT0(opnd);
    //    IR * opndcopy = m_rg->dupIRTree(opnd);
    //    if (opndcopy->is_pr()) {
    //        opndcopy->copyRef(opnd, m_rg);
    //    }
    //
    //    //The copy will be inserted into related predecessor.
    //    IR * store_to_phicopy = m_rg->buildStorePR(
    //        PR_no(phicopy), phicopy->getType(), opndcopy);
    //    store_to_phicopy->copyRef(phicopy, m_rg);
    //
    //    IRBB * p = m_cfg->getBB(pred);
    //    ASSERT0(p);
    //    IR * plast = BB_last_ir(p);
    //
    //    //In PHI node elimination to insert the copy in the predecessor block,
    //    //there is a check if last IR of BB is not a call then
    //    //place the copy there only.
    //    //However for call BB terminator, the copy will be placed at the start
    //    //of fallthrough BB.
    //    if (plast != NULL && plast->isCallStmt()) {
    //        IRBB * fallthrough = m_cfg->getFallThroughBB(p);
    //        if (!plast->is_terminate()) {
    //            //Fallthrough BB does not exist if the last ir is terminate.
    //            ASSERTN(fallthrough, ("invalid control flow graph."));
    //            if (BB_irlist(fallthrough).get_head() == NULL ||
    //                !BB_irlist(fallthrough).get_head()->is_phi()) {
    //                BB_irlist(fallthrough).append_head(store_to_phicopy);
    //            } else {
    //                //Insert block to hold the copy.
    //                IRBB * newbb = m_rg->allocBB();
    //                m_rg->getBBList()->insert_after(newbb, p);
    //                m_cfg->addBB(newbb);
    //                m_cfg->insertVertexBetween(
    //                    BB_id(p), BB_id(fallthrough), BB_id(newbb));
    //                BB_is_fallthrough(newbb) = true;
    //
    //                //Then append the copy.
    //                BB_irlist(newbb).append_head(store_to_phicopy);
    //            }
    //        }
    //    } else {
    //        BB_irlist(p).append_tail_ex(store_to_phicopy);
    //    }
    //
    //    //Remove the SSA DU chain between opnd and its DEF stmt.
    //    if (opnd->is_pr()) {
    //        ASSERT0(MD_ssainfo(opnd));
    //        MDSSA_uses(MD_ssainfo(opnd)).remove(opnd);
    //    }
    //}
    //
    //IR * substitue_phi = m_rg->buildStorePR(
    //    PHI_prno(phi), phi->getType(), phicopy);
    //substitue_phi->copyRef(phi, m_rg);
    //
    //BB_irlist(bb).insert_before(substitue_phi, phict);
    //
    //PHI_ssainfo(phi) = NULL;
}


//This function verify def/use information of PHI stmt.
//If vpinfo is available, the function also check VOPND_mdid of phi operands.
//is_vpinfo_avail: set true if VMD information is available.
bool MDSSAMgr::verifyPhi(bool is_vpinfo_avail)
{
    DUMMYUSE(is_vpinfo_avail);
    BBList * bblst = m_rg->getBBList();
    List<IRBB*> preds;
    for (IRBB * bb = bblst->get_head(); bb != NULL; bb = bblst->get_next()) {
        m_cfg->get_preds(preds, bb);

        MDPhiList * philist = m_usedef_mgr.genBBPhiList(BB_id(bb));
        if (philist == NULL) { continue; }

        for (xcom::SC<MDPhi*> * sct = philist->get_head();
             sct != philist->end(); sct = philist->get_next(sct)) {
            MDPhi * phi = sct->val();
            ASSERT0(phi);

            //Check phi result.
            VMD * res = phi->getResult();
            CHECK_DUMMYUSE(res->is_md());

            //Check the number of phi opnds.
            UINT num_opnd = 0;

            for (IR const* opnd = phi->getOpndList();
                 opnd != NULL; opnd = opnd->get_next()) {
                num_opnd++;
                if (!opnd->is_id()) { continue; }

                //Opnd may be ID, CONST or LDA.
                MD const* opnd_md = opnd->getRefMD();
                CHECK_DUMMYUSE(opnd_md);
                ASSERTN(MD_id(opnd_md) == res->mdid(),
                        ("mdid of VMD is unmatched"));

                //Ver0 is input parameter, and it has no MDSSA_def.
                //ASSERT0(VOPND_ver(MD_ssainfo(opnd)) > 0);

                ASSERTN(ID_phi(opnd) == phi, ("opnd is not an operand of phi"));
            }

            ASSERTN(num_opnd == preds.get_elem_count(),
                   ("The number of phi operand must same with "
                    "the number of BB predecessors."));
        }
    }
    return true;
}


bool MDSSAMgr::verifyVMD()
{
    START_TIMER(tverify, "MDSSA: Verify VMD After Pass");

    //Check version for each vp.
    xcom::BitSet defset;
    Vector<VOpnd*> * vec = m_usedef_mgr.getVOpndVec();
    for (INT i = 1; i <= vec->get_last_idx(); i++) {
        VMD * v = (VMD*)vec->get(i);
        ASSERT0(v);

        if (!v->is_md()) { continue; }

        MDDef * def = v->getDef();
        if (def == NULL) {
            //ver0 used to indicate the Region live-in PR.
            //It may be a parameter.
            ASSERTN(v->version() == 0, ("Nondef vp's version must be 0"));
        } else {
            ASSERTN(v->version() != 0, ("version can not be 0"));
            ASSERTN(!defset.is_contain(def->id()),
                    ("DEF for each md+version must be unique."));
            defset.bunion(def->id());
        }

        VMD * res = NULL;
        if (def != NULL) {
            res = def->getResult();
            ASSERT0(res);
            if (!def->is_phi()) {
                ASSERT0(def->getOcc() && def->getOcc()->is_stmt() &&
                        def->getOcc()->isMemoryRef());
                bool findref = false;
                if (def->getOcc()->getRefMD() != NULL &&
                    MD_id(def->getOcc()->getRefMD()) == res->mdid()) {
                    findref = true;
                }
                if (def->getOcc()->getRefMDSet() != NULL &&
                    def->getOcc()->getRefMDSet()->
                        is_contain_pure(res->mdid())) {
                    findref = true;
                }
                CHECK_DUMMYUSE(findref);
            }
        }

        if (res != NULL) {
            SEGIter * vit = NULL;
            for (INT i2 = res->getUseSet()->get_first(&vit);
                 i2 >= 0; i2 = res->getUseSet()->get_next(i2, &vit)) {
                IR * use = m_rg->getIR(i2);

                ASSERT0(use->isMemoryRef() || use->is_id());

                bool findref = false;
                if (use->getRefMD() != NULL &&
                    MD_id(use->getRefMD()) == res->mdid()) {
                    findref = true;
                }
                if (use->getRefMDSet() != NULL &&
                    use->getRefMDSet()->is_contain_pure(res->mdid())) {
                    findref = true;
                }
                CHECK_DUMMYUSE(findref);
            }
        }
    }
    END_TIMER(tverify, "MDSSA: Verify VMD After Pass");
    return true;
}


void MDSSAMgr::verifySSAInfo(IR const* ir)
{
    ASSERT0(ir);
    MDSSAInfo * mdssainfo = m_usedef_mgr.getMDSSAInfo(ir);
    ASSERT0(mdssainfo);
    xcom::SEGIter * iter = NULL;
    VOpndSet * set = mdssainfo->getVOpndSet();
    for (INT i = set->get_first(&iter); i >= 0; i = set->get_next(i, &iter)) {
        VMD * vopnd = (VMD*)m_usedef_mgr.getVOpnd(i);
        ASSERT0(vopnd && vopnd->is_md());
        MDDef * def = vopnd->getDef();

        if (ir->is_stmt()) {
            ASSERTN(vopnd->version() != 0, ("not yet perform renaming"));
            ASSERTN(def && def->getOcc() == ir, ("IR stmt should have MDDef"));
        }

        if (def != NULL) {
            ASSERT0(def->getResult());
            if (!def->is_phi()) {
                ASSERT0(def->getOcc() && def->getOcc()->is_stmt());
                bool findref = false;
                if (def->getOcc()->getRefMD() != NULL &&
                    MD_id(def->getOcc()->getRefMD()) == vopnd->mdid()) {
                    findref = true;
                }
                if (def->getOcc()->getRefMDSet() != NULL &&
                    def->getOcc()->getRefMDSet()->
                        is_contain_pure(vopnd->mdid())) {
                    findref = true;
                }

                //Do NOT assert if not find reference.
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
                //ASSERT0(findref);
                DUMMYUSE(findref);
            }
        }

        //Check SSA uses.
        //Check if occ of vopnd references the correct MD/MDSet.
        SEGIter * iter2;
        for (INT j = vopnd->getUseSet()->get_first(&iter2);
             j >= 0; j = vopnd->getUseSet()->get_next(j, &iter2)) {
            IR const* occ = (IR*)m_rg->getIR(j);
            ASSERT0(occ);

            bool findref = false;
            if (occ->getRefMD() != NULL &&
                MD_id(occ->getRefMD()) == vopnd->mdid()) {
                findref = true;
            }
            if (occ->getRefMDSet() != NULL &&
                occ->getRefMDSet()->is_contain_pure(vopnd->mdid())) {
                findref = true;
            }

            //Do NOT assert if not find reference MD at USE point which
            //should correspond to vopnd->md().
            //Some transformation, such as IR Refinement, may change
            //the occ's MDSet. This might lead to the inaccurate and
            //redundant MDSSA DU Chain. So the MDSSA DU Chain is conservative,
            //and the correctness of MDSSA dependence is garanteed.
            //e.g:
            //  ist:*<4> id:18 //:MD11, MD12, MD14, MD15
            //    lda: *<4> 'r'
            //    ild: i32 //MMD13: MD16
            //      ld: *<4> 'q' //MMD18
            //=> After IR combination: ist(lda) transformed to st
            //  st:*<4> 'r' //MMD12
            //    ild : i32 //MMD13 : MD16
            //      ld : *<4> 'q'    //MMD18
            //ist combined to st. This reduce referenced MDSet to a single MD
            //as well.
            //ASSERT0(findref);
            DUMMYUSE(findref);
        }
    }
}


//The verification check the DU info in SSA form.
//Current IR must be in SSA form.
bool MDSSAMgr::verify()
{
    START_TIMER(tverify, "MDSSA: Verify After Pass");

    //Check version for each vp.
    BBList * bbl = m_rg->getBBList();
    xcom::C<IRBB*> * ct = NULL;
    for (bbl->get_head(&ct); ct != bbl->end(); ct = bbl->get_next(ct)) {
        IRBB * bb = ct->val();
        xcom::C<IR*> * ctir = NULL;
        for (BB_irlist(bb).get_head(&ctir);
             ctir != BB_irlist(bb).end();
             ctir = BB_irlist(bb).get_next(ctir)) {

            //Verify PHI list.
            MDPhiList * philist = m_usedef_mgr.genBBPhiList(BB_id(bb));
            ASSERT0(philist);
            for (xcom::SC<MDPhi*> * sct = philist->get_head();
                 sct != philist->end(); sct = philist->get_next(sct)) {
                MDPhi * phi = sct->val();
                ASSERT0(phi && phi->is_phi() && phi->getResult());

                for (IR const* opnd = phi->getOpndList();
                     opnd != NULL; opnd = opnd->get_next()) {
                    if (!opnd->is_id()) { continue; }

                    MD const* opnd_md = opnd->getRefMD();
                    CHECK_DUMMYUSE(opnd_md);
                    ASSERTN(MD_id(opnd_md) == phi->getResult()->mdid(),
                            ("MD not matched"));
                    verifySSAInfo(opnd);
                }
            }

            IR * ir = ctir->val();
            m_iter.clean();
            for (IR * x = iterInit(ir, m_iter);
                 x != NULL; x = iterNext(m_iter)) {
                if (x->isMemoryRefNotOperatePR()) {
                    verifySSAInfo(x);
                }
            }
        }
    }
    END_TIMER(tverify, "MDSSA: Verify After Pass");
    return true;
}


//DU chain operation.
//Change Def stmt from 'olddef' to 'newdef'.
//'olddef': source stmt.
//'newdef': target stmt.
//e.g: oldef->USE change to newdef->USE.
void MDSSAMgr::changeDef(IR * olddef, IR * newdef)
{
    ASSERT0(olddef && newdef && olddef->is_stmt() && newdef->is_stmt());
    ASSERT0(olddef != newdef);
    MDSSAInfo * mdssainfo = getUseDefMgr()->getMDSSAInfo(olddef);
    ASSERT0(mdssainfo);

    xcom::SEGIter * iter = NULL;
    for (INT i = mdssainfo->getVOpndSet()->get_first(&iter);
         i >= 0; i = mdssainfo->getVOpndSet()->get_next(i, &iter)) {
        VMD * vopnd = (VMD*)getUseDefMgr()->getVOpnd(i);
        ASSERT0(vopnd && vopnd->is_md());
        ASSERT0(vopnd->getDef()->getOcc() == olddef);
        MDDEF_occ(VMD_def(vopnd)) = newdef;
    }
}


//DU chain operation.
//Change Use expression from 'olduse' to 'newuse'.
//'olduse': source expression.
//'newuse': target expression.
//e.g: Change MDSSA DU chain DEF->olduse to DEF->newuse.
void MDSSAMgr::changeUse(IR * olduse, IR * newuse)
{
    ASSERT0(olduse && newuse && olduse->is_exp() && newuse->is_exp());
    ASSERT0(olduse != newuse);
    MDSSAInfo * mdssainfo = getUseDefMgr()->getMDSSAInfo(olduse);
    ASSERT0(mdssainfo);

    xcom::SEGIter * iter = NULL;
    for (INT i = mdssainfo->getVOpndSet()->get_first(&iter);
         i >= 0; i = mdssainfo->getVOpndSet()->get_next(i, &iter)) {
        VMD * vopnd = (VMD*)getUseDefMgr()->getVOpnd(i);
        ASSERT0(vopnd && vopnd->is_md());
        if (vopnd->getUseSet()->is_contain(olduse->id())) {
            vopnd->getUseSet()->diff(olduse->id());
            vopnd->getUseSet()->bunion(newuse->id());
        }
    }
}


//Coalesce version of MD from 'src' to 'tgt'.
//This function replace definition of USE of src to tgt's defintion.
//e.g: p0=...
//     p1=p0
//     ...=p1
//=> after coalescing, p1 is src, p0 is tgt
//     p0=...
//     ------ //removed
//     ...=p0
void MDSSAMgr::coalesceVersion(IR const* src, IR const* tgt)
{
    MDSSAInfo * src_mdssainfo = getUseDefMgr()->getMDSSAInfo(src);
    MDSSAInfo * tgt_mdssainfo = getUseDefMgr()->getMDSSAInfo(tgt);
    ASSERT0(src_mdssainfo && tgt_mdssainfo);
    xcom::SEGIter * iter1 = NULL;
    for (INT i = src_mdssainfo->getVOpndSet()->get_first(&iter1);
         i >= 0; i = src_mdssainfo->getVOpndSet()->get_next(i, &iter1)) {
        VMD * src_vopnd = (VMD*)getUseDefMgr()->getVOpnd(i);
        ASSERT0(src_vopnd && src_vopnd->is_md());

        //Find the MD in tgt's vopnd-set which has same mdid with src's
        //except the distinct version.
        //e.g: src has MD6Vx, find MD6Vy in tgt vopnd set.
        SEGIter * iter2 = NULL;
        VMD * tgt_vopnd = NULL;
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

        //Replace the USE of src to USE of tgt.
        xcom::SEGIter * iter3 = NULL;
        for (INT k = src_vopnd->getUseSet()->get_first(&iter3);
             k >= 0; k = src_vopnd->getUseSet()->get_next(k, &iter3)) {
            IR const* occ = (IR*)m_rg->getIR(k);
            MDSSAInfo * occ_mdssainfo = getUseDefMgr()->getMDSSAInfo(occ);
            ASSERTN(occ_mdssainfo, ("occ miss MDSSAInfo"));

            occ_mdssainfo->getVOpndSet()->remove(src_vopnd, *m_sbs_mgr);
            occ_mdssainfo->getVOpndSet()->append(tgt_vopnd, *m_sbs_mgr);
            tgt_vopnd->getUseSet()->bunion(occ->id());
        }
        src_vopnd->getUseSet()->clean();
    }

}


//Add occurence to each vopnd in mdssainfo.
void MDSSAMgr::addMDSSAOcc(IR * ir, MDSSAInfo * mdssainfo)
{
    ASSERT0(ir && mdssainfo);
    SEGIter * iter = NULL;
    for (INT i = mdssainfo->getVOpndSet()->get_first(&iter);
         i >= 0; i = mdssainfo->getVOpndSet()->get_next(i, &iter)) {
        VMD * vopnd = (VMD*)getUseDefMgr()->getVOpnd(i);
        ASSERT0(vopnd && vopnd->is_md());
        vopnd->getUseSet()->bunion(ir->id());
    }
}


//Remove MD-SSA and PR-SSA use-def chain.
//e.g: ir=...
//    =ir //S1
//If S1 will be deleted, ir should be removed from its useset in MDSSAInfo.
//NOTE: If ir is an IR tree, e.g: ild(x, ld(y)), removing ild(x) means
//ld(y) will be removed as well. Therefore ld(y)'s MDSSAInfo will be
//updated as well.
void MDSSAMgr::removeMDSSAUseRecur(IR * ir)
{
    MDSSAInfo * mdssainfo = getMDSSAInfoIfAny(ir);
    SSAInfo * prssainfo = NULL;
    if (mdssainfo != NULL) {
        mdssainfo->removeUse(ir, getUseDefMgr());
    } else if ((prssainfo = ir->getSSAInfo()) != NULL) {
        //Whole IR tree may be removed via this function recursively.
        //Maintain the SSAInfo of read-pr/write-pr operation.
        prssainfo->removeUse(ir);
    }
    for (UINT i = 0; i < IR_MAX_KID_NUM(ir); i++) {
        for (IR * x = ir->getKid(i); x != NULL; x = x->get_next()) {
            removeMDSSAUseRecur(x);
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
    if (mdssainfo == NULL) { return; }
    xcom::SEGIter * iter = NULL;
    INT next_i = -1;
    for (INT i = mdssainfo->getVOpndSet()->get_first(&iter);
         i >= 0; i = next_i) {
        next_i = mdssainfo->getVOpndSet()->get_next(i, &iter);
        VMD * vopnd = (VMD*)getUseDefMgr()->getVOpnd(i);
        ASSERT0(vopnd && vopnd->is_md());
        if (vopnd->getDef() == NULL) {
            ASSERTN(vopnd->version() == 0, ("Only zero version MD has no DEF"));
            continue;
        }
        if (vopnd->getDef()->getOcc() == stmt) {
            vopnd->getUseSet()->diff(exp->id());
            mdssainfo->getVOpndSet()->remove(vopnd, *m_sbs_mgr);
        }
    }
}


//This function perform SSA destruction via scanning BB in sequential order.
void MDSSAMgr::destruction(OptCtx * oc)
{
    BBList * bblst = m_rg->getBBList();
    if (bblst->get_elem_count() == 0) { return; }
    UINT bbnum = bblst->get_elem_count();
    xcom::C<IRBB*> * bbct;
    for (bblst->get_head(&bbct);
         bbct != bblst->end(); bbct = bblst->get_next(bbct)) {
        IRBB * bb = bbct->val();
        ASSERT0(bb);
        destructBBSSAInfo(bb);
    }
    if (bbnum != bblst->get_elem_count()) {
        oc->set_flag_if_cfg_changed();
        //Each pass maintain CFG by default.
        OC_is_cfg_valid(*oc) = true;
    }
    m_is_ssa_constructed = false;
}


//wl: work list for temporary used.
void MDSSAMgr::prunePhiForBB(List<IRBB*> & wl, IRBB * bb)
{
    ASSERT0(bb);
    MDPhiList * philist = m_usedef_mgr.genBBPhiList(BB_id(bb));
    ASSERT0(philist);
    xcom::SC<MDPhi*> * next;
    for (xcom::SC<MDPhi*> * sct = philist->get_head();
         sct != philist->end(); sct = next) {
        next = philist->get_next(sct);
        MDPhi * phi = sct->val();
        ASSERT0(phi);
        VMD * common_def = NULL;
        if (!isRedundantPHI(phi, &common_def)) { continue; }

        ASSERT0(common_def);
        for (IR * opnd = phi->getOpndList();
             opnd != NULL; opnd = opnd->get_next()) {
            VMD * vopnd = phi->getOpndVMD(opnd, &m_usedef_mgr);
            if (vopnd == NULL) { continue; }

            ASSERT0(vopnd->is_md());
            if (vopnd->getDef()) {
                ASSERT0(vopnd->getDef()->getBB());
                wl.append_tail(vopnd->getDef()->getBB());
            }

            vopnd->getUseSet()->remove(opnd);
        }

        if (common_def != phi->getResult()) {
            ASSERT0(common_def->is_md());

            SEGIter * iter;
            IRSet * useset = phi->getResult()->getUseSet();
            for (INT i = useset->get_first(&iter);
                 i >= 0; i = useset->get_next(i, &iter)) {
                IR * u = m_rg->getIR(i);
                ASSERT0(u && m_usedef_mgr.getMDSSAInfo(u));

                //Change DEF.
                m_usedef_mgr.getMDSSAInfo(u)->
                    getVOpndSet()->remove(phi->getResult(), *m_sbs_mgr);
                m_usedef_mgr.getMDSSAInfo(u)->
                    getVOpndSet()->append(common_def, *m_sbs_mgr);
            }

            common_def->getUseSet()->bunion(*phi->getResult()->getUseSet());
        }

        philist->remove(phi);
    }
}


//Remove redundant phi.
//wl: work list for temporary used.
void MDSSAMgr::prunePhi(List<IRBB*> & wl)
{
    START_TIMER(t, "MDSSA: Prune phi");

    BBList * bblst = m_rg->getBBList();
    xcom::C<IRBB*> * ct;

    wl.clean();
    for (bblst->get_head(&ct); ct != bblst->end(); ct = bblst->get_next(ct)) {
        IRBB * bb = ct->val();
        ASSERT0(bb);
        wl.append_tail(bb);
    }

    IRBB * bb = NULL;
    while ((bb = wl.remove_head()) != NULL) {
        prunePhiForBB(wl, bb);
    }

    END_TIMER(t, "MDSSA: Prune phi");
}


//Clean MDSSAInfo AI of all IR.
void MDSSAMgr::cleanMDSSAInfoAI()
{
    for (INT i = 0; i <= m_rg->getIRVec()->get_last_idx(); i++) {
        IR * ir = m_rg->getIR(i);
        if (ir != NULL && ir->getAI() != NULL) {
            IR_ai(ir)->clean(AI_MD_SSA);
        }
    }
}


//Reinitialize MD SSA manager.
void MDSSAMgr::reinit()
{
    destroy();
    cleanMDSSAInfoAI();
    m_max_version.clean();
    m_usedef_mgr.reinit();
    init();
}


void MDSSAMgr::construction(OptCtx & oc)
{
    START_TIMER(t0, "MDSSA: Construction");
    m_rg->checkValidAndRecompute(&oc, PASS_DOM, PASS_UNDEF);
    ASSERT0(OC_is_ref_valid(oc));
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

    OC_is_du_chain_valid(oc) = false; //DU chain of PR is voilated.
    m_is_ssa_constructed = true;

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

    ASSERT0(verifyPhi(true));

    prunePhi(wl);

    //Clean version stack after renaming.
    cleanMD2Stack();

    if (g_is_dump_after_pass && g_dump_opt.isDumpMDSSAMgr()) {
        START_TIMER(tdump, "MDSSA: Dump After Pass");
        g_indent = 0;
        m_md_sys->dump(false);
        dump();
        dumpDUChain();
        END_TIMER(tdump, "MDSSA: Dump After Pass");
    }

    ASSERT0(verify());
    ASSERT0(verifyIRandBB(m_rg->getBBList(), m_rg));
    ASSERT0(verifyPhi(false) && verifyVMD());

    m_is_ssa_constructed = true;

    return true;
}
//END MDSSAMgr

bool verifyMDSSAInfo(Region * rg)
{
    MDSSAMgr * ssamgr = (MDSSAMgr*)(rg->getPassMgr()->
        queryPass(PASS_MD_SSA_MGR));
    if (ssamgr != NULL && ssamgr->isMDSSAConstructed()) {
        ASSERT0(ssamgr->verify());
        ASSERT0(ssamgr->verifyPhi(false));
    }
    return true;
}

} //namespace xoc
