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

namespace xoc {

Region * IPA::findRegion(IR * call, Region * callrg)
{
    ASSERT0(call->is_call());
    CallGraph * cg = m_program->getCallGraph();
    ASSERTN(cg, ("IPA need call-graph"));
    CallNode * callercn = cg->mapRegion2CallNode(callrg);
    ASSERTN(callercn, ("caller is not on graph"));

    Sym const* callname = CALL_idinfo(call)->get_name();

    //Iterate accessing successors.
    ASSERT0(cg->getVertex(CN_id(callercn)));
    for (xcom::EdgeC const* ec = cg->getVertex(CN_id(callercn))->getOutList();
         ec != nullptr; ec = ec->get_next()) {
        CallNode * calleecn = cg->getCallNode(ec->getToId());
        ASSERT0(calleecn);

        Region * callee = CN_ru(calleecn);
        if (callee == nullptr || callercn == calleecn) {
            //callee site does not have a region or the same region.
            continue;
        }

        if (callname == callee->getRegionVar()->get_name()) {
            return callee;
        }
    }
    return nullptr;
}


//call: call stmt.
//callrg: the region that call stmt resident in.
//Generate dummy use only if MD both exist in caller's MayDef and
//callee's MayUse.
void IPA::createCallDummyuse(IR * call, Region * callrg)
{
    Region * calleeru = findRegion(call, callrg);
    if (calleeru == nullptr || CALL_dummyuse(call) != nullptr) {
        return;
    }

    MDSet const* mayuse = calleeru->getMayUse();
    if (mayuse == nullptr || mayuse->is_empty()) { return; }

    MDSet const* callermaydef = callrg->getMayDef();
    if (callermaydef == nullptr || callermaydef->is_empty()) { return; }

    MDSetIter iter;
    IR * last = nullptr;
    for (BSIdx j = mayuse->get_first(&iter);
         j != BS_UNDEF; j = mayuse->get_next(j, &iter)) {
        MD const* md = m_mdsys->getMD(j);
        ASSERT0(md);
        if (!md->is_effect() || !callermaydef->is_contain(md, callrg)) {
            continue;
        }

        IR * ld = callrg->getIRMgr()->buildLoad(MD_base(md));
        callrg->getMDMgr()->allocRef(ld);
        xcom::add_next(&CALL_dummyuse(call), &last, ld);
        IR_parent(ld) = call;
    }
}


void IPA::createCallDummyuse(Region * rg)
{
    ASSERT0(rg);
    IR * ir = rg->getIRList();
    if (ir == nullptr) {
        BBList * bbl = rg->getBBList();
        if (bbl == nullptr) { return; }
        for (IRBB * bb = bbl->get_head(); bb != nullptr; bb = bbl->get_next()) {
            BBIRListIter it;
            for (IR * ir2 = bb->getIRList().get_head(&it);
                 ir2 != nullptr; ir2 = bb->getIRList().get_next(&it)) {
                if (!ir2->is_call()) { continue; }
                //TODO: handle icall.
                createCallDummyuse(ir2, rg);
            }
        }
        return;
    }

    for (; ir != nullptr; ir = ir->get_next()) {
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
        if (rg == nullptr ||
            (rg->getIRList() == nullptr &&
             rg->getBBList()->get_elem_count() == 0)) {
            continue;
        }
        rg->initPassMgr();
        rg->initDbxMgr();
        rg->initIRMgr();
        rg->initIRBBMgr();
        DUMgr * dumgr = (DUMgr*)rg->getPassMgr()->registerPass(PASS_DU_MGR);
        ASSERT0(dumgr);
        dumgr->computeCallRef(DUOptFlag(DUOPT_COMPUTE_PR_DU|
                                        DUOPT_COMPUTE_NONPR_DU));
        rg->getPassMgr()->destroyRegisteredPass(PASS_DU_MGR);
        rg->getPassMgr()->destroyRegisteredPass(PASS_AA);
    }
    END_TIMER(t, "Compute CallRef for all regions");
}


void IPA::createCallDummyuse(OptCtx & oc)
{
    for (UINT i = 0; i < m_rumgr->getNumOfRegion(); i++) {
        Region * rg = m_rumgr->getRegion(i);
        if (rg == nullptr) { continue; }
        createCallDummyuse(rg);
        if (g_compute_pr_du_chain && g_compute_nonpr_du_chain) {
            OptCtx * loc = m_rumgr->getAndGenOptCtx(rg);
            ASSERT0(loc);
            recomputeDUChain(rg, *loc);
            if (!m_is_keep_dumgr && rg->getPassMgr() != nullptr) {
                rg->getPassMgr()->destroyRegisteredPass(PASS_DU_MGR);
            }
        }
    }
    if (g_compute_pr_du_chain && g_compute_nonpr_du_chain) {
        OC_is_pr_du_chain_valid(oc) = true;
        OC_is_nonpr_du_chain_valid(oc) = true;
        if (m_is_keep_reachdef) {
            OC_is_reach_def_valid(oc) = true;
        }
    }
}


void IPA::recomputeDUChain(Region * rg, OptCtx & oc)
{
    ASSERT0(rg);
    if (rg->getIRList() == nullptr &&
        (rg->getBBList() == nullptr ||
         rg->getBBList()->get_elem_count() == 0)) {
        return;
    }
    if (rg->getPassMgr() == nullptr) {
        rg->initPassMgr();
        rg->initDbxMgr();
        rg->initIRMgr();
        rg->initIRBBMgr();
    }
    if (!oc.is_aa_valid()) {
        //DUMgr requires AliasAnalysis
        rg->getPassMgr()->registerPass(PASS_AA);
    }
    if (g_do_mdssa) {
        //Build MD SSA du chain.
        if (m_is_recompute_du_ref) {
            rg->getPassMgr()->checkValidAndRecompute(&oc, PASS_DU_REF,
                                                     PASS_CFG, PASS_UNDEF);
        }
        DUMgr * dumgr = (DUMgr*)rg->getPassMgr()->registerPass(PASS_DU_MGR);
        ASSERT0(dumgr);
        dumgr->perform(oc, DUOptFlag(DUOPT_SOL_REACH_DEF|DUOPT_COMPUTE_PR_DU));
        dumgr->computeMDDUChain(oc, false, DUOptFlag(DUOPT_COMPUTE_PR_DU));
        bool rmprdu = false;
        bool rmnonprdu = false;
        MDSSAMgr * mdssamgr = (MDSSAMgr*)rg->getPassMgr()->registerPass(
            PASS_MDSSA_MGR);
        ASSERT0(mdssamgr);
        if (!mdssamgr->is_valid()) {
            mdssamgr->construction(oc);
            //If SSA is enabled, disable classic DU Chain.
            //Since we do not maintain both them as some passes.
            //e.g:In RCE, remove PHI's operand will not update the
            //operand DEF's DUSet.
            //CASE:compiler.gr/alias.loop.gr
            oc.setInvalidNonPRDU();
            rmnonprdu = true;
        }
        xoc::removeClassicDUChain(rg, rmprdu, rmnonprdu);
        return;
    }

    //Build classic du chain.
    if (m_is_recompute_du_ref) {
        if (m_is_keep_reachdef) {
            rg->getPassMgr()->checkValidAndRecompute(&oc, PASS_REACH_DEF,
                                                     PASS_DU_REF, PASS_CFG,
                                                     PASS_DU_CHAIN,
                                                     PASS_UNDEF);
        } else {
            rg->getPassMgr()->checkValidAndRecompute(&oc, PASS_DU_REF,
                                                     PASS_CFG, PASS_DU_CHAIN,
                                                     PASS_UNDEF);
        }
    } else {
        if (m_is_keep_reachdef) {
            rg->getPassMgr()->checkValidAndRecompute(&oc, PASS_REACH_DEF,
                                                     PASS_CFG, PASS_DU_CHAIN,
                                                     PASS_UNDEF);
        } else {
            rg->getPassMgr()->checkValidAndRecompute(&oc, PASS_CFG,
                                                     PASS_DU_CHAIN,
                                                     PASS_UNDEF);
        }
    }
}


//NOTE: IPA should be performed on program region.
//IPA will create dummy use for each region, and recompute the
//DU chain if any required.
bool IPA::perform(OptCtx & oc)
{
    START_TIMER(t, getPassName());
    ASSERT0(oc.is_callgraph_valid());
    ASSERT0(m_program && m_program->is_program());
    createCallDummyuse(oc);
    END_TIMER(t, getPassName());
    return true;
}

} //namespace xoc
