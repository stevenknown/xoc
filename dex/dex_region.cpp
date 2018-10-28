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
#include "libdex/DexFile.h"
#include "libdex/DexClass.h"
#include "libdex/DexProto.h"
#include "liropcode.h"
#include "lir.h"
#include "drAlloc.h"
#include "d2d_comm.h"
#include "cominc.h"
#include "comopt.h"
#include "cmdline.h"
#include "dex.h"
#include "trycatch_info.h"
#include "gra.h"
#include "dex_hook.h"
#include "dex_util.h"
#include "dex2ir.h"
#include "ir2dex.h"

PassMgr * DexRegion::allocPassMgr()
{
    return new DexPassMgr(this);
}


//Initialize alias analysis.
IR_AA * DexRegion::allocAliasAnalysis()
{
    return new DEX_AA(this);
}


bool DexRegion::MiddleProcess(OptCtx & oc)
{
   return Region::MiddleProcess(oc);
}


bool DexRegion::HighProcess(OptCtx & oc)
{
    CHAR const* ru_name = getRegionName();
    g_indent = 0;
    SimpCtx simp;
    SIMP_if(&simp) = true;
    SIMP_doloop(&simp) = true;
    SIMP_dowhile(&simp) = true;
    SIMP_whiledo(&simp) = true;
    SIMP_switch(&simp) = false;
    SIMP_break(&simp) = true;
    SIMP_continue(&simp) = true;

    setIRList(simplifyStmtList(getIRList(), &simp));

    ASSERT0(verifySimp(getIRList(), simp));
    ASSERT0(verifyIRList(getIRList(), NULL, this));

    constructIRBBlist();

    ASSERT0(verifyIRandBB(getBBList(), this));

    //All IRs have been moved to each IRBB.
    setIRList(NULL);

    HighProcessImpl(oc);
    return true;
}


//All global prs must be mapped.
bool DexRegion::verifyRAresult(RA & ra, Prno2Vreg & prno2v)
{
    GltMgr * gltm = ra.get_gltm();
    Vector<GLT*> * gltv = gltm->get_gltvec();
    for (UINT i = 0; i < gltm->get_num_of_glt(); i++) {
        GLT * g = gltv->get(i);
        if (g == NULL) { continue; }
        ASSERT0(g->has_allocated());
        if (GLT_bbs(g) == NULL) {
            //parameter may be have no occ.
            continue;
        }
        bool find;
        prno2v.get(GLT_prno(g), &find);
        ASSERT0(find);
    }

    BBList * bbl = getBBList();
    for (IRBB * bb = bbl->get_head(); bb != NULL; bb = bbl->get_next()) {
        LTMgr * ltm = gltm->map_bb2ltm(bb);
        if (ltm == NULL) { continue; }
        Vector<LT*> * lvec = ltm->get_lt_vec();
        for (INT i = 0; i <= lvec->get_last_idx(); i++) {
            LT * l = lvec->get(i);
            if (l == NULL) { continue; }
            ASSERT0(l->has_allocated());
            bool find;
            prno2v.get(LT_prno(l), &find);
            ASSERT0(find);
        }
    }
    return true;
}


void DexRegion::updateRAresult(IN RA & ra, OUT Prno2Vreg & prno2v)
{
    prno2v.maxreg = ra.get_maxreg();
    prno2v.paramnum = ra.get_paramnum();
    GltMgr * gltm = ra.get_gltm();
    BBList * bbl = getBBList();
    prno2v.clean();
    for (IRBB * bb = bbl->get_head(); bb != NULL; bb = bbl->get_next()) {
        LTMgr * ltm = gltm->map_bb2ltm(bb);
        if (ltm == NULL) { continue; }
        Vector<LT*> * lvec = ltm->get_lt_vec();
        for (INT i = 0; i <= lvec->get_last_idx(); i++) {
            LT * l = lvec->get(i);
            if (l == NULL) { continue; }
            ASSERT0(l->has_allocated());
            bool find;
            UINT v = prno2v.get(LT_prno(l), &find);
            if (find) {
                //each prno is corresponding to a unqiue vreg.
                ASSERT0(v == LT_phy(l));
            } else {
                prno2v.set(LT_prno(l), LT_phy(l));
            }
        }
    }
    //prno2v.dump();
    ASSERT0(verifyRAresult(ra, prno2v));
}


void DexRegion::processSimply()
{
    LOG("DexRegion::processSimply %s", getRegionName());
    if (getIRList() == NULL) { return ; }

    OptCtx oc;
    OC_show_comp_time(oc) = g_show_comp_time;

    CHAR const* ru_name = getRegionName();

    constructIRBBlist();

    ASSERT0(verifyIRandBB(getBBList(), this));

    //All IRs have been moved to each IRBB.
    setIRList(NULL);

    PassMgr * passmgr = initPassMgr();
    ASSERT0(passmgr);

    ASSERT0(g_cst_bb_list);
    IR_CFG * cfg = (IR_CFG*)passmgr->registerPass(PASS_CFG);
    ASSERT0(cfg);
    cfg->initCfg(oc);
    ASSERT0(g_do_cfg_dom);
    cfg->LoopAnalysis(oc);

    destroyPassMgr();

    //Do not allocate register.
    getPrno2Vreg()->clean();
    getPrno2Vreg()->copy(*getDex2IR()->getPR2Vreg());
    return;
}


//Add catch type string into symbol tabel to speed up string comparation.
static void addCatchTypeName(DexRegion * rg)
{
    Dex2IR * d2ir = rg->getDex2IR();
    SymTab * symtab = rg->getRegionMgr()->getSymTab();
    for (TryInfo * ti = d2ir->getTryInfo(); ti != NULL; ti = ti->next) {
        for (CatchInfo * ci = ti->catch_list; ci != NULL; ci = ci->next) {
            ASSERT0(ci->kindname);
            symtab->add(ci->kindname);
        }
    }
}


//This function outputs Prno2Vreg after Dex register allocation.
bool DexRegion::process(OptCtx * oc)
{
    if (getIRList() == NULL) { return true; }
    OC_show_comp_time(*oc) = g_show_comp_time;

    g_indent = 0;
    if (!g_silence) {
        LOG("DexRegion process %s", getRegionName());
    }
    //note("\n==---- REGION_NAME:%s ----==", getRegionName());
    prescan(getIRList());

    PassMgr * passmgr = initPassMgr();

    HighProcess(*oc);

    MiddleProcess(*oc);

    ASSERT0(getPassMgr());
    PRSSAMgr * ssamgr = (PRSSAMgr*)passmgr->queryPass(PASS_PR_SSA_MGR);
    if (ssamgr != NULL && ssamgr->isSSAConstructed()) {
        ssamgr->destruction();
    }

    if (!g_retain_pass_mgr_for_region) {
        //Destroy PassMgr.
        destroyPassMgr();
    }

    if (!is_function()) { return true; }

    ///////////////////////////////////////
    //DO NOT REQUEST PASS AFTER THIS LINE//
    ///////////////////////////////////////

    BBList * bbl = getBBList();
    if (bbl->get_elem_count() == 0) { return true; }

    ASSERT0(verifyIRandBB(bbl, this));

    RefineCtx rf;
    RC_insert_cvt(rf) = false; //Do not insert cvt for DEX code.
    refineBBlist(bbl, rf);
    ASSERT0(verifyIRandBB(bbl, this));

    if (g_do_dex_ra) {
        Prno2Vreg * original_prno2vreg = getDex2IR()->getPR2Vreg();
        RA ra(this,
              getTypeIndexRep(),
              getParamNum(),
              getOrgVregNum(),
              getDex2IR()->getVreg2PR(),
              original_prno2vreg,
              &m_var2pr);
        LOG("\t\tdo DEX Register Allcation for '%s'", getRegionName());
        ra.perform(*oc);
        updateRAresult(ra, *getPrno2Vreg());
    } else {
        //Do not allocate register.
        getPrno2Vreg()->clean();
        getPrno2Vreg()->copy(*getDex2IR()->getPR2Vreg());
    }

    return true;
}
