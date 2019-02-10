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
#include "cominc.h"
#include "comopt.h"
#include "callg.h"
#include "ipa.h"

namespace xoc {

Region * IPA::findRegion(IR * call, Region * callru)
{
    ASSERT0(call->is_call());
    CallGraph * cg = m_rumgr->getCallGraph();
    ASSERT0(cg);
    CallNode * callercn = cg->mapRegion2CallNode(callru);
    ASSERTN(callercn, ("caller is not on graph"));

    SYM const* callname = CALL_idinfo(call)->get_name();

    //Iterate accessing successors.
    ASSERT0(cg->get_vertex(CN_id(callercn)));
    for (xcom::EdgeC const* ec =
             VERTEX_out_list(cg->get_vertex(CN_id(callercn)));
         ec != NULL; ec = EC_next(ec)) {
        CallNode * calleecn = cg->mapId2CallNode(
            VERTEX_id(EDGE_to(EC_edge(ec))));
        ASSERT0(calleecn);

        Region * callee = CN_ru(calleecn);
        if (callee == NULL || callercn == calleecn) {
            //callee site does not have a region or the same region.
            continue;
        }

        if (callname == callee->getRegionVar()->get_name()) {
            return callee;
        }
    }
    return NULL;
}


//call: call stmt.
//callru: the region that call stmt resident in.
//Generate dummy use only if MD both exist in caller's MayDef and
//callee's MayUse.
void IPA::createCallDummyuse(IR * call, Region * callru)
{
    Region * calleeru = findRegion(call, callru);
    if (calleeru == NULL || CALL_dummyuse(call) != NULL) {
        return;
    }

    MDSet const* mayuse = calleeru->getMayUse();
    if (mayuse == NULL || mayuse->is_empty()) { return; }

    MDSet const* callermaydef = callru->getMayDef();
    if (callermaydef == NULL || callermaydef->is_empty()) { return; }

    SEGIter * iter;
    IR * last = NULL;
    for (INT j = mayuse->get_first(&iter);
         j >= 0; j = mayuse->get_next(j, &iter)) {
        MD const* md = m_mdsys->getMD(j);
        ASSERT0(md);
        if (!md->is_effect() || !callermaydef->is_contain(md)) {
            continue;
        }

        IR * ld = callru->buildLoad(MD_base(md));
        callru->allocRefForLoad(ld);
        xcom::add_next(&CALL_dummyuse(call), &last, ld);
        IR_parent(ld) = call;
    }
}


void IPA::createCallDummyuse(Region * rg)
{
    ASSERT0(rg);
    IR * ir = rg->getIRList();
    if (ir == NULL) {
        BBList * bbl = rg->getBBList();
        if (bbl == NULL) { return; }
        for (IRBB * bb = bbl->get_head(); bb != NULL; bb = bbl->get_next()) {
            for (IR * ir2 = BB_first_ir(bb);
                 ir2 != NULL; ir2 = BB_next_ir(bb)) {
                if (!ir2->is_call()) { continue; }
                //TODO: handle icall.
                createCallDummyuse(ir2, rg);
            }
        }
        return;
    }

    for (; ir != NULL; ir = ir->get_next()) {
        if (!ir->is_call()) { continue; }
        //TODO: handle icall.
        createCallDummyuse(ir, rg);
    }
}


void IPA::computeCallRefForAllRegion()
{
    START_TIMER(t, "Compute CallRef for all regions");
    for (UINT i = 0; i < m_rumgr->getNumOfRegion(); i++) {
        Region * rg = m_rumgr->getRegion(i);
        if (rg == NULL ||
            (rg->getIRList() == NULL &&
             rg->getBBList()->get_elem_count() == 0)) {
            continue;
        }

        rg->initPassMgr();
        IR_AA * aa = (IR_AA*)rg->getPassMgr()->
            registerPass(PASS_AA);
        IR_DU_MGR * dumgr = (IR_DU_MGR*)rg->getPassMgr()->
            registerPass(PASS_DU_MGR);
        ASSERT0(dumgr);
        dumgr->computeCallRef(COMPUTE_PR_DU|COMPUTE_NOPR_DU);
        rg->getPassMgr()->destroyPass(dumgr);
        rg->getPassMgr()->destroyPass(aa);
    }
    END_TIMER(t, "Compute CallRef for all regions");
}


void IPA::createCallDummyuse(OptCtx & oc)
{
    for (UINT i = 0; i < m_rumgr->getNumOfRegion(); i++) {
        Region * rg = m_rumgr->getRegion(i);
        if (rg == NULL) { continue; }
        createCallDummyuse(rg);

        if (g_compute_classic_du_chain) {
            OptCtx loc(oc);
            recomputeDUChain(rg, loc);
            if (!m_is_keep_dumgr && rg->getPassMgr() != NULL) {
                rg->getPassMgr()->destroyPass(PASS_DU_MGR);
            }
        }
    }

    if (g_compute_classic_du_chain) {
        OC_is_du_chain_valid(oc) = true;
        if (m_is_keep_reachdef) {
            OC_is_reach_def_valid(oc) = true;
        }
    }
}


void IPA::recomputeDUChain(Region * rg, OptCtx & oc)
{
    ASSERT0(rg);
    if (rg->getIRList() == NULL &&
        (rg->getBBList() == NULL ||
         rg->getBBList()->get_elem_count() == 0)) {
        return;
    }
    if (rg->getPassMgr() == NULL) {
        rg->initPassMgr();
    }
    if (!OC_is_aa_valid(oc)) {
        //IR_DU_MGR need IR_AA
        rg->getPassMgr()->registerPass(PASS_AA);
    }
    if (g_do_md_ssa) {
        //Build MD SSA du chain.
        if (m_is_recompute_du_ref) {
            rg->checkValidAndRecompute(&oc,
                PASS_DU_REF,
                PASS_CFG,
                PASS_UNDEF);
        }

        //Compute typical PR du chain.
        IR_DU_MGR * dumgr = (IR_DU_MGR*)rg->getPassMgr()->
            registerPass(PASS_DU_MGR);
        ASSERT0(dumgr);
        dumgr->perform(oc, SOL_REACH_DEF|COMPUTE_PR_DU);
        dumgr->computeMDDUChain(oc, false, COMPUTE_PR_DU);

        MDSSAMgr * mdssamgr = (MDSSAMgr*)rg->getPassMgr()->
            registerPass(PASS_MD_SSA_MGR);
        ASSERT0(mdssamgr);
        if (!mdssamgr->isMDSSAConstructed()) {
            mdssamgr->construction(oc);
        }

        return;
    }

    //Build classic du chain.
    if (m_is_recompute_du_ref) {
        if (m_is_keep_reachdef) {
            rg->checkValidAndRecompute(&oc,
                    PASS_REACH_DEF,
                    PASS_DU_REF,
                    PASS_CFG,
                    PASS_DU_CHAIN,
                    PASS_UNDEF);
        } else {
            rg->checkValidAndRecompute(&oc,
                    PASS_DU_REF,
                    PASS_CFG,
                    PASS_DU_CHAIN,
                    PASS_UNDEF);
        }
    } else {
        if (m_is_keep_reachdef) {
            rg->checkValidAndRecompute(&oc,
                    PASS_REACH_DEF,
                    PASS_CFG,
                    PASS_DU_CHAIN,
                    PASS_UNDEF);
        } else {
            rg->checkValidAndRecompute(&oc,
                    PASS_CFG,
                    PASS_DU_CHAIN,
                    PASS_UNDEF);
        }
    }
}


//NOTE: IPA should be performed on program region.
//IPA will create dummy use for each region, and recompute the
//DU chain if need.
bool IPA::perform(OptCtx & oc)
{
    START_TIMER(t, getPassName());
    ASSERT0(OC_is_callg_valid(oc));
    ASSERT0(m_program && m_program->is_program());
    createCallDummyuse(oc);
    END_TIMER(t, getPassName());
    return true;
}

} //namespace xoc
