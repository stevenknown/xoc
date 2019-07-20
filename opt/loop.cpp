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

    xcom::Vertex * headvex = cfg->get_vertex(BB_id(head));
    if (cfg->get_out_degree(headvex) != 2) {
        //Not natural loop.
        return false;
    }

    xcom::EdgeC const* ec = VERTEX_out_list(headvex);
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
    xcom::EdgeC const* ec = VERTEX_in_list(cfg->get_vertex(BB_id(head)));
    while (ec != NULL) {
        backedgecount++;
        UINT pred = VERTEX_id(EDGE_from(EC_edge(ec)));
        if (li->isInsideLoop(pred)) {
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

    xcom::C<IRBB*> * bbholder = NULL;
    bblst->find(head, &bbholder);
    ASSERT0(bbholder);
    xcom::C<IRBB*> * tt = bbholder;
    IRBB * prev = bblst->get_prev(&tt);

    //Find appropriate BB to be prehead.
    bool find_appropriate_prev_bb = false;

    for (xcom::EdgeC const* ec = VERTEX_in_list(cfg->get_vertex(BB_id(head)));
         ec != NULL; ec = EC_next(ec)) {
        UINT pred = VERTEX_id(EDGE_from(EC_edge(ec)));
        IRBB const* pred_bb = rg->getCFG()->getBB(pred);
        ASSERT0(pred_bb);
        if (pred == BB_id(prev) && !LI_bb_set(li)->is_contain(BB_id(prev))) {
            //Try to find fallthrough prev BB.
            //CASE:prev is not preheader of head. 
            //      BB_prehead----
            //                   |
            //  ............     |
            //                   |
            //  ---->BB_prev     |
            //  |      |         |
            //  |      v         |
            //  |   BB_head<------  
            //  |      |
            //  |      v      
            //  ----BB_end
            find_appropriate_prev_bb = true;
            break;
        }
    }

    if (BB_last_ir(prev) != NULL &&
        prev->isDownBoundary(BB_last_ir(prev))) {
        //prev should fallthrough to current BB.
        //Can not append IR to prev BB.
        find_appropriate_prev_bb = false;
    }

    if (!force) {
        if (find_appropriate_prev_bb) {
            return prev;
        }
        return NULL;
    }    

    List<IRBB*> preds;
    cfg->get_preds(preds, head);
    IRBB * preheader = rg->allocBB();
    bblst->insert_before(preheader, bbholder);
    LabelInfo const* preheader_lab = rg->genIlabel();
    xcom::BitSet * loop_body = LI_bb_set(li);
    for (IRBB * p = preds.get_head(); p != NULL; p = preds.get_next()) {
        if (loop_body->is_contain(BB_id(p))) {
            //p is inside loop.
            continue;
        }
        if (!insert_bb) {
            //Insert preheader in front of head.
            //Note preheader must fallthrough to head.
            cfg->add_bb(preheader);
            cfg->insertVertexBetween(BB_id(p), BB_id(head), BB_id(preheader));
            BB_is_fallthrough(preheader) = true;
            insert_bb = true;
        }

        //CASE1:
        //  BB_p
        //   |
        //   v
        //  BB_header
        //Have to get the last IR of
        //BB and judge if it is conditional branch.
        //if (BB_is_fallthrough(p)) {
        //    //Nothing to do.
        //    continue;
        //}

        //CASE2:
        //  BB_p(goto lab1)
        //   |
        //   v
        //  BB_header(lab1)
        //=>
        //  BB_p(goto lab2)
        //   |
        //   v
        //  BB_preheader(lab2)      
        //   |
        //   v
        //  BB_header(lab2)
        IR * last_ir = BB_last_ir(p);
        ASSERT0(last_ir);
        if ((last_ir->isConditionalBr() || last_ir->isUnconditionalBr()) &&
            head == cfg->findBBbyLabel(last_ir->getLabel())) {
            //Add newlabel to preheader if not exist.
            preheader->addLabel(preheader_lab);

            //Update branch-target of IR that located in predecessor of head.
            last_ir->setLabel(preheader_lab);
        }
    }

    //Move LabelInfos from head to preheader except LabelInfos that
    //are the target of IR that belongs to loop body.
    List<LabelInfo const*> & lablst = head->getLabelList();
    if (lablst.get_elem_count() <= 1) {
        //The only label is the target of loop back-edge.
        return preheader;
    }

    //Record if Labels which attached on head are
    //branch target of IR which inside loop.
    //Other Labels can be moved to preheader.
    TMap<LabelInfo const*, bool> lab_canbe_move_to_preheader;
    for (LabelInfo const* lab = lablst.get_head();
         lab != NULL; lab = lablst.get_next()) {
        lab_canbe_move_to_preheader.set(lab, false);
    }

    //Mark labels that can not move to preheader.
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
                    if (lab_canbe_move_to_preheader.find(lab)) {
                        lab_canbe_move_to_preheader.setAlways(lab, true);
                    }
                }
                continue;
            }

            LabelInfo const* lab = ir->getLabel();
            if (lab == NULL) { continue; }

            if (!lab_canbe_move_to_preheader.find(lab)) { continue; }

            lab_canbe_move_to_preheader.setAlways(lab, true);
        }
    }

    //Move labels to preheader.
    xcom::C<LabelInfo const*> * ct;
    xcom::C<LabelInfo const*> * next_ct;
    for (lablst.get_head(&ct); ct != lablst.end(); ct = next_ct) {
        next_ct = lablst.get_next(ct);
        LabelInfo const* lab = ct->val();
        if (lab_canbe_move_to_preheader.get(lab)) { continue; }
        lablst.remove(ct);
        preheader->addLabel(lab);
        cfg->getLabel2BBMap()->setAlways(lab, preheader);
    }

    return preheader;
}

} //namespace xoc
