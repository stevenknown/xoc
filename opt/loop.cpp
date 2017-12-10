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

namespace xoc {

//Find the bb that is the start of the unqiue backedge of loop.
//  BB1: loop start bb
//  BB2: body start bb
//  BB3: goto loop start bb
//
//BB2 is the loop header fallthrough bb.
bool findTwoSuccessorBBOfLoopHeader(
        LI<IRBB> const* li,
        IR_CFG * cfg,
        UINT * succ1,
        UINT * succ2)
{
    ASSERT0(li && cfg && succ1 && succ2);
    IRBB * head = LI_loop_head(li);

    Vertex * headvex = cfg->get_vertex(BB_id(head));
    if (cfg->get_out_degree(headvex) != 2) {
        //Not natural loop.
        return false;
    }

    EdgeC const* ec = VERTEX_out_list(headvex);
    ASSERT0(ec && EC_next(ec));

    *succ1 = VERTEX_id(EDGE_to(EC_edge(ec)));
    *succ2 = VERTEX_id(EDGE_to(EC_edge(EC_next(ec))));
    return true;
}


//Find the bb that is the start of the unqiue backedge of loop.
//  BB1: loop start bb
//  BB2: body
//  BB3: goto loop start bb
//
//BB3 is the backedge start bb.
IRBB * findSingleBackedgeStartBB(LI<IRBB> const* li, IR_CFG * cfg)
{
    ASSERT0(li && cfg);
    IRBB * head = LI_loop_head(li);

    UINT backedgebbid = 0;
    UINT backedgecount = 0;
    EdgeC const* ec = VERTEX_in_list(cfg->get_vertex(BB_id(head)));
    while (ec != NULL) {
        backedgecount++;
        UINT pred = VERTEX_id(EDGE_from(EC_edge(ec)));
        if (li->is_inside_loop(pred)) {
            backedgebbid = pred;
        }
        ec = EC_next(ec);
    }
    ASSERT0(backedgebbid > 0 && cfg->getBB(backedgebbid));
    if (backedgecount > 2) {
        //There are multiple backedges.
        return NULL;
    }
    return cfg->getBB(backedgebbid);
}


//Find preheader BB. If it does not exist, insert one before loop 'li'.
//
//'insert_bb': return true if this function insert a new bb before loop,
//    otherwise return false.
//
//'force': force to insert preheader BB whatever it has exist.
//    Return the new BB if insertion is successful.
//
//Note if we find the preheader, the last IR of it may be call.
//So if you are going to insert IR at the tail of preheader, the best is
//force to insert a new bb.
IRBB * findAndInsertPreheader(
        LI<IRBB> const* li,
        Region * rg,
        OUT bool & insert_bb,
        bool force)
{
    ASSERT0(li && rg);
    insert_bb = false;
    IR_CFG * cfg = rg->getCFG();
    BBList * bblst = rg->getBBList();
    IRBB * head = LI_loop_head(li);

    C<IRBB*> * bbholder = NULL;
    bblst->find(head, &bbholder);
    ASSERT0(bbholder);
    C<IRBB*> * tt = bbholder;
    IRBB * prev = bblst->get_prev(&tt);

    //Find appropriate BB to be prehead.
    bool find_appropriate_prev_bb = false;

    for (EdgeC const* ec = VERTEX_in_list(cfg->get_vertex(BB_id(head)));
         ec != NULL; ec = EC_next(ec)) {
        UINT pred = VERTEX_id(EDGE_from(EC_edge(ec)));
        if (pred == BB_id(prev)) {
            find_appropriate_prev_bb = true;
            break;
        }
    }

    if (BB_last_ir(prev) != NULL &&
        prev->is_down_boundary(BB_last_ir(prev))) {
        //Can not append IR to prev BB.
        find_appropriate_prev_bb = false;
    }

    if (!force && find_appropriate_prev_bb) { return prev; }

    List<IRBB*> preds;
    cfg->get_preds(preds, head);
    insert_bb = true;
    IRBB * newbb = rg->allocBB();
    bblst->insert_before(newbb, bbholder);
    BitSet * loop_body = LI_bb_set(li);
    for (IRBB * p = preds.get_head(); p != NULL; p = preds.get_next()) {
        if (loop_body->is_contain(BB_id(p))) {
            continue;
        }
        cfg->add_bb(newbb);
        cfg->insertVertexBetween(BB_id(p), BB_id(head), BB_id(newbb));
        BB_is_fallthrough(newbb) = true;
    }

    //Move LabelInfo from head to prehead except the LabelInfo which are the
    //target of IR which belongs to loop body.
    List<LabelInfo const*> & lablst = head->getLabelList();
    if (lablst.get_elem_count() <= 1) {
        //The only label is the loop back edge target.
        return newbb;
    }

    //Record if Labels which attached on head are
    //the branch target of IR which inside the loop.
    //The other Label can be moved to preheader.
    TMap<LabelInfo const*, bool> head_labinfo;
    for (LabelInfo const* lab = lablst.get_head();
         lab != NULL; lab = lablst.get_next()) {
        head_labinfo.set(lab, false);
    }

    for (INT i = LI_bb_set(li)->get_first();
         i >= 0; i = LI_bb_set(li)->get_next(i)) {
        IRBB * bb = cfg->getBB(i);
        ASSERT0(bb);
        for (IR const* ir = BB_first_ir(bb); ir != NULL; ir = BB_next_ir(bb)) {
            if (ir->is_switch()) {
                for (IR * c = SWITCH_case_list(ir);
                     c != NULL; c = c->get_next()) {
                    LabelInfo const* lab = c->getLabel();
                    ASSERT0(lab);
                    if (head_labinfo.find(lab)) {
                        head_labinfo.setAlways(lab, true);
                    }
                }
                continue;
            }

            LabelInfo const* lab = ir->getLabel();
            if (lab == NULL) { continue; }

            if (!head_labinfo.find(lab)) { continue; }

            head_labinfo.setAlways(lab, true);
        }
    }

    C<LabelInfo const*> * ct;
    C<LabelInfo const*> * next_ct;
    for (lablst.get_head(&ct); ct != lablst.end(); ct = next_ct) {
        next_ct = lablst.get_next(ct);
        LabelInfo const* lab = ct->val();
        if (head_labinfo.get(lab)) { continue; }
        lablst.remove(ct);
        newbb->addLabel(lab);
        cfg->get_lab2bb_map()->setAlways(lab, newbb);
    }

    return newbb;
}

} //namespace xoc
