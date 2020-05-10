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

//
//START Inliner
//
bool Inliner::is_call_site(IR * call, Region * rg)
{
    ASSERT0(call->isCallStmt());
    CallNode const* cn1 =
        m_call_graph->mapSym2CallNode(CALL_idinfo(call)->get_name(), rg);
    CallNode const* cn2 = m_call_graph->mapRegion2CallNode(rg);
    return cn1 == cn2;
}


//'caller': caller's region.
//'caller_call': call site in caller.
//'new_irs': indicate the duplicated IR list in caller. Note that
//    these IR must be allocated in caller's region.
IR * Inliner::replaceReturnImpl(
        Region * caller,
        IR * caller_call,
        IR * new_irs,
        LabelInfo * el)
{
    IR * next = NULL;
    for (IR * x = new_irs; x != NULL; x = next) {
        next = x->get_next();
        switch (x->getCode()) {
        case IR_DO_WHILE:
        case IR_WHILE_DO:
        case IR_DO_LOOP: //loop with init , boundary , and step info
            LOOP_body(x) =
                replaceReturnImpl(caller, caller_call, LOOP_body(x), el);
            break;
        case IR_IF:
            IF_truebody(x) =
                replaceReturnImpl(caller, caller_call, IF_truebody(x), el);
            IF_falsebody(x) =
                replaceReturnImpl(caller, caller_call, IF_falsebody(x), el);
            break;
        case IR_SWITCH:
            SWITCH_body(x) =
                replaceReturnImpl(caller, caller_call, SWITCH_body(x), el);
            break;
        case IR_RETURN:
            if (!caller_call->hasReturnValue()) {
                if (el != NULL) {
                    IR * go = caller->buildGoto(el);
                    xcom::insertbefore_one(&new_irs, x, go);
                }
                xcom::remove(&new_irs, x);
                caller->freeIRTree(x);
            } else {
                IR * send = RET_exp(x);
                UINT receive = CALL_prno(caller_call);
                IR * mv_lst = NULL;
                if (send != NULL) {
                    IR * mv = caller->buildStorePR(receive,
                        caller_call->getType(), send);
                    xcom::insertbefore_one(&mv_lst, mv_lst, mv);
                }
                RET_exp(x) = NULL;

                if (el != NULL) {
                    IR * go = caller->buildGoto(el);
                    xcom::add_next(&mv_lst, go);
                }

                xcom::insertbefore(&new_irs, x, mv_lst);
                xcom::remove(&new_irs, x);
                ASSERT0(RET_exp(x) == NULL);
                caller->freeIRTree(x);
            }
            break;
        default: break;
        } //end switch
    }
    return new_irs;
}



void Inliner::checkRegion(
        IN Region * rg,
        OUT bool & need_el,
        OUT bool & has_ret) const
{
    need_el = false;
    has_ret = false;
    List<IR const*> lst;
    IR const* irs = rg->getIRList();
    if (irs == NULL) { return; }
    for (IR const* x = irs; x != NULL; x = x->get_next()) {
        switch (x->getCode()) {
        case IR_DO_WHILE:
        case IR_WHILE_DO:
        case IR_DO_LOOP:
            lst.clean();
            for (IR const* k = iterInitC(LOOP_body(x), lst);
                 k != NULL; k = iterNextC(lst)) {
                if (k->is_return()) {
                    need_el = true;
                    has_ret = true;
                    break;
                }
            }
            break;
        case IR_IF:
            lst.clean();
            for (IR const* k = iterInitC(IF_truebody(x), lst);
                 k != NULL; k = iterNextC(lst)) {
                if (k->is_return()) {
                    need_el = true;
                    has_ret = true;
                    break;
                }
            }
            lst.clean();
            for (IR const* k = iterInitC(IF_falsebody(x), lst);
                 k != NULL; k = iterNextC(lst)) {
                if (k->is_return()) {
                    need_el = true;
                    has_ret = true;
                    break;
                }
            }
            break;
        case IR_SWITCH:
            lst.clean();
            for (IR const* k = iterInitC(SWITCH_body(x), lst);
                 k != NULL; k = iterNextC(lst)) {
                if (k->is_return()) {
                    need_el = true;
                    has_ret = true;
                    break;
                }
            }
            break;
        case IR_RETURN: has_ret = true; break;
        default: break;
        } //end switch

        if (need_el) {
            break;
        }
    }
}


IR * Inliner::replaceReturn(
        Region * caller,
        IR * caller_call,
        IR * new_irs,
        InlineInfo * ii)
{
    LabelInfo * el = NULL;
    if (INLINFO_need_el(ii)) {
        el = caller->genIlabel();
    }

    if (INLINFO_has_ret(ii)) {
        new_irs = replaceReturnImpl(caller, caller_call, new_irs, el);
    }

    if (el != NULL) {
        xcom::add_next(&new_irs, caller->buildLabel(el));
    }
    return new_irs;
}


bool Inliner::do_inline_c(Region * caller, Region * callee)
{
    IR * caller_irs = caller->getIRList();
    IR * callee_irs = callee->getIRList();
    if (caller_irs == NULL || callee_irs == NULL) { return false; }
    IR * next = NULL;
    bool change = false;
    IR * head = caller_irs;
    for (; caller_irs != NULL; caller_irs = next) {
        next = caller_irs->get_next();
        if (caller_irs->is_call() &&
            is_call_site(caller_irs, callee)) {
            IR * new_irs_in_caller = caller->dupIRTreeList(callee_irs);

            InlineInfo * ii = mapRegion2InlineInfo(callee, false);
            if (ii == NULL) {
                bool need_el;
                bool has_ret;
                checkRegion(callee, need_el, has_ret);
                ii = mapRegion2InlineInfo(callee, true);
                INLINFO_has_ret(ii) = has_ret;
                INLINFO_need_el(ii) = need_el;
             }

            new_irs_in_caller = replaceReturn(caller, caller_irs, new_irs_in_caller, ii);
            xcom::insertafter(&caller_irs, new_irs_in_caller);
            xcom::remove(&head, caller_irs);
            change = true;
        }
    }

    caller->setIRList(head);
    return change;
}


//'cand': candidate that expected to be inlined.
void Inliner::do_inline(Region * cand)
{
    CallNode * cn = m_call_graph->mapRegion2CallNode(cand);
    ASSERT0(cn);
    xcom::Vertex * v = m_call_graph->getVertex(CN_id(cn));
    ASSERT0(v);
    for (xcom::EdgeC * el = VERTEX_in_list(v);
         el != NULL; el = EC_next(el)) {
        Region * caller = m_call_graph->mapVertex2CallNode(
            el->getFrom())->region();
        if (caller != NULL) {
            do_inline_c(caller, cand);
        }
    }
}


//Evaluate whether rg can be inlining candidate.
bool Inliner::can_be_cand(Region * rg)
{
    IR * ru_irs = rg->getIRList();
    if (REGION_is_expect_inline(rg) &&
        xcom::cnt_list(ru_irs) < g_inline_threshold) {
        return true;
    }
    return false;
}


bool Inliner::perform(OptCtx & oc)
{
    START_TIMER(t, getPassName());
    DUMMYUSE(oc);
    ASSERT0(OC_is_callg_valid(oc));
    ASSERT0(m_program && m_program->is_program());
    IR * irs = m_program->getIRList();
    while (irs != NULL) {
        if (irs->is_region()) {
            Region * rg = REGION_ru(irs);
            if (can_be_cand(rg)) {
                do_inline(rg);
            }
        }
        irs = irs->get_next();
    }
    END_TIMER(t, getPassName());
    return false;
}
//END Inliner

} //namespace xoc
