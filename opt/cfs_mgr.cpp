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
#include "cfs_mgr.h"

namespace xoc {

//
//START CfsMgr
//
CFS_INFO * CfsMgr::new_cfs_info(IR_TYPE irtype)
{
    CFS_INFO * ci = (CFS_INFO*)xmalloc(sizeof(CFS_INFO));
    CFS_INFO_cfs_type(ci) = irtype;
    switch (irtype) {
    case IR_IF:
        CFS_INFO_true_body(ci) = m_bs_mgr.create();
        CFS_INFO_false_body(ci) = m_bs_mgr.create();
        break;
    case IR_DO_LOOP:
    case IR_WHILE_DO:
    case IR_DO_WHILE:
        CFS_INFO_loop_body(ci) = m_bs_mgr.create();
        break;
    default:
        UNREACH();
    }
    return ci;
}


void CfsMgr::set_map_ir2cfsinfo(IR * ir, CFS_INFO * ci)
{
    m_map_ir2cfsinfo.set(ir->id(), ci);
}


CFS_INFO * CfsMgr::map_ir2cfsinfo(IR * ir)
{
    return m_map_ir2cfsinfo.get(ir->id());
}


//Record IR list into 'irset'.
void CfsMgr::recordStmt(IR * ir_list, BitSet & irset)
{
    while (ir_list != NULL) {
        irset.bunion(IR_id(ir_list));
        ir_list = ir_list->get_next();
    }
}


AbsNode * CfsMgr::new_abs_node(ABS_TYPE ty)
{
    AbsNode * a = (AbsNode*)xmalloc(sizeof(AbsNode));
    ABS_NODE_type(a) = ty;
    return a;
}


void CfsMgr::dump_indent(UINT indent)
{
    while (indent != 0) {
        fprintf(g_tfile, " ");
        indent--;
    }
}


void CfsMgr::dump_abs_tree(AbsNode * an)
{
    fprintf(g_tfile, "\n==---- DUMP AbsNode Tree ----==\n");
    dump_abs_tree(an, 0);
}


void CfsMgr::dump_abs_tree(AbsNode * an, UINT indent)
{
    while (an != NULL) {
        switch (ABS_NODE_type(an)) {
        case ABS_BB:
            fprintf(g_tfile, "\n"); dump_indent(indent);
            fprintf(g_tfile, "BB%d", BB_id(ABS_NODE_bb(an)));
            break;
        case ABS_LOOP:
            fprintf(g_tfile, "\n"); dump_indent(indent);
            fprintf(g_tfile, "LOOP: HEAD=BB%d", BB_id(ABS_NODE_loop_head(an)));
            dump_abs_tree(ABS_NODE_loop_body(an), indent + 4);
            break;
        case ABS_IF:
            fprintf(g_tfile, "\n"); dump_indent(indent);
            fprintf(g_tfile, "IF: HEAD=BB%d", BB_id(ABS_NODE_if_head(an)));
            if (ABS_NODE_true_body(an) != NULL) {
                fprintf(g_tfile, "\n"); dump_indent(indent);
                fprintf(g_tfile, "TRUE_BODY:");
                dump_abs_tree(ABS_NODE_true_body(an), indent + 4);
            }
            if (ABS_NODE_false_body(an) != NULL) {
                fprintf(g_tfile, "\n"); dump_indent(indent);
                fprintf(g_tfile, "FALSE_BODY:");
                dump_abs_tree(ABS_NODE_false_body(an), indent + 4);
            }
            break;
        }
        an = ABS_NODE_next(an);
    }
}


AbsNode * CfsMgr::map_bb2abs(IRBB const* bb)
{
    return m_map_bb2abs.get(BB_id(bb));
}


void CfsMgr::set_map_bb2abs(IRBB const* bb, AbsNode * abs)
{
    m_map_bb2abs.set(BB_id(bb), abs);
}


AbsNode * CfsMgr::constructAbsLoop(
        IN IRBB * entry,
        IN AbsNode * parent,
        IN BitSet * cur_region,
        IN Graph & cur_graph,
        IN OUT BitSet & visited)
{
    DUMMYUSE(cur_region);
    ASSERT0(cur_region == NULL || cur_region->is_contain(BB_id(entry)));
    IR_CFG * cfg = m_ru->getCFG();
    LI<IRBB> * li = cfg->mapBB2LabelInfo(entry);
    ASSERT0(li != NULL && LI_loop_head(li) == entry);

    AbsNode * node = new_abs_node(ABS_LOOP);
    set_map_bb2abs(entry, node);
    ABS_NODE_parent(node) = parent;
    ABS_NODE_loop_head(node) = entry;
    IRBB * body_start;
    cfg->getKidOfLoop(entry, NULL, &body_start);
    ASSERT0(body_start != NULL);

    CFS_INFO * ci = map_ir2cfsinfo(cfg->get_last_xr(entry));
    CHECK_DUMMYUSE(ci);
    ASSERT0(CFS_INFO_head(ci) == entry);

    ASSERT0(CFS_INFO_loop_body(ci)->is_contain(*LI_bb_set(li)));
    BitSet loc_visited;
    ABS_NODE_loop_body(node) = constructAbsTree(body_start, node,
        LI_bb_set(li), cur_graph, loc_visited);
    visited.bunion(loc_visited);
    visited.bunion(BB_id(entry));
    return node;
}


//'cur_region' covered 'entry'.
AbsNode * CfsMgr::constructAbsIf(
        IN IRBB * entry,
        IN AbsNode * parent,
        IN Graph & cur_graph,
        IN OUT BitSet & visited)
{
    AbsNode * node = new_abs_node(ABS_IF);
    set_map_bb2abs(entry, node);
    ABS_NODE_parent(node) = parent;
    ABS_NODE_if_head(node) = entry;

    IRBB * true_body, * false_body;
    IR_CFG * cfg = m_ru->getCFG();
    cfg->getKidOfIF(entry, &true_body, &false_body, NULL);
    CFS_INFO * ci = map_ir2cfsinfo(cfg->get_last_xr(entry));
    ASSERT0(ci != NULL && CFS_INFO_head(ci) == entry);

    BitSet loc_visited;
    ABS_NODE_true_body(node) = constructAbsTree(true_body, node,
        CFS_INFO_true_body(ci), cur_graph, loc_visited);
    visited.bunion(loc_visited);
    loc_visited.clean();
    ABS_NODE_false_body(node) = constructAbsTree(false_body, node,
        CFS_INFO_false_body(ci), cur_graph, loc_visited);
    visited.bunion(loc_visited);
    visited.bunion(BB_id(entry));
    return node;
}


AbsNode * CfsMgr::constructAbsBB(IN IRBB * bb, IN AbsNode * parent)
{
    AbsNode * node = new_abs_node(ABS_BB);
    set_map_bb2abs(bb, node);
    ABS_NODE_parent(node) = parent;
    ABS_NODE_bb(node) = bb;
    return node;
}


AbsNode * CfsMgr::constructAbsTree(
        IN IRBB * entry,
        IN AbsNode * parent,
        IN BitSet * cur_region,
        IN Graph & cur_graph,
        IN OUT BitSet & visited)
{
    IR_CFG * cfg = m_ru->getCFG();
    AbsNode * lst = NULL;
    IRBB * bb = entry;
    Graph g;
    g.clone(cur_graph);
    Vertex * next = NULL;
    Vertex * v;
    if (cur_region != NULL) {
        if (cur_region->get_elem_count() == 0) {
            visited.clean();
            return NULL;
        }
        INT c;
        for (v = g.get_first_vertex(c); v != NULL; v = next) {
            next = g.get_next_vertex(c);
            if (cur_region->is_contain(VERTEX_id(v))) {
                continue;
            }
            g.removeVertex(v);
        }
    }
    BitSet loc_visited;
    while (bb != NULL &&
           (cur_region == NULL ||
            cur_region->is_contain(BB_id(bb)))) {
        AbsNode * node = NULL;
        loc_visited.clean();
        LI<IRBB> * li = cfg->mapBB2LabelInfo(bb);
        if (li != NULL) {
            node = constructAbsLoop(bb, parent, LI_bb_set(li),
                                    g, loc_visited);
        } else {
            IR * last_xr = cfg->get_last_xr(bb);
            if (last_xr != NULL && //'bb' is branching node of IF.
                last_xr->isConditionalBr()) {
                ASSERT0(map_ir2cfsinfo(last_xr) != NULL);

                //There might not exist ipdom.
                //e.g:
                //  if (x) //BB1
                //      return 1;
                //  return 2;
                //
                //  BB1 does not have a ipdom.
                UINT ipdom = ((DGraph*)cfg)->get_ipdom(BB_id(bb));
                DUMMYUSE(ipdom);
                ASSERT(ipdom > 0, ("bb does not have ipdom"));
                node = constructAbsIf(bb, parent, g, loc_visited);
            } else {
                node = constructAbsBB(bb, parent);
                loc_visited.bunion(BB_id(bb));
            }
        }
        insertbefore_one(&lst, lst, node);

        visited.bunion(loc_visited);
        //Remove visited vertex.
        next = NULL;
        INT c;
        for (v = g.get_first_vertex(c); v != NULL; v = next) {
            next = g.get_next_vertex(c);
            if (!loc_visited.is_contain(VERTEX_id(v))) {
                continue;
            }
            g.removeVertex(v);
        }

        IRBB * cand = NULL;
        for (v = g.get_first_vertex(c); v != NULL; v = g.get_next_vertex(c)) {
            if (g.get_in_degree(v) == 0) {
                ASSERT(cand == NULL, ("multiple immediate-post-dominators"));
                cand = cfg->getBB(VERTEX_id(v));
            }
        }

        if (cand == NULL) {
            //Cannot find leading BB, there might be exist cycle in graph.
            bb = cfg->get_ipdom(bb);
        } else {
            bb = cand;
        }

        if (parent != NULL && bb == ABS_NODE_bb(parent)) {
            //Here control-flow is cyclic.
            break;
        }
    }
    lst = reverse_list(lst);
    return lst;
}


//Construct Control Flow Structure.
AbsNode * CfsMgr::constructAbstractControlFlowStruct()
{
    IR_CFG * cfg = m_ru->getCFG();
    ASSERT(cfg->get_entry(), ("CFG should be single-entry"));
    BitSet visited;
    AbsNode * a = constructAbsTree(cfg->get_entry(), NULL,
        NULL, *(Graph*)cfg, visited);
    //dump_abs_tree(a);
    return a;
}
//END CfsMgr

} //namespace xoc
