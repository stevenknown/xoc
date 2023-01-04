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
//START CfsMgr
//
CfsMgr::CfsMgr(Region * rg) : Pass(rg)
{
    m_pool = smpoolCreate(64, MEM_COMM);
    IRCFG * cfg = rg->getCFG();
    if (cfg != nullptr) {
        m_bb2li.createMap(cfg->getLoopInfo());
    }
}


CFS_INFO * CfsMgr::new_cfs_info(IR_CODE ircode)
{
    CFS_INFO * ci = (CFS_INFO*)xmalloc(sizeof(CFS_INFO));
    CFS_INFO_cfs_type(ci) = ircode;
    switch (ircode) {
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
        UNREACHABLE();
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
void CfsMgr::recordStmt(IR * ir_list, xcom::BitSet & irset)
{
    while (ir_list != nullptr) {
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
        prt(getRegion(), " ");
        indent--;
    }
}


void CfsMgr::dump_abs_tree(AbsNode * an)
{
    note(getRegion(), "\n==---- DUMP AbsNode Tree ----==\n");
    dump_abs_tree(an, 0);
}


void CfsMgr::dump_abs_tree(AbsNode * an, UINT indent)
{
    while (an != nullptr) {
        switch (ABS_NODE_type(an)) {
        case ABS_BB:
            note(getRegion(), "\n"); dump_indent(indent);
            prt(getRegion(), "BB%d", ABS_NODE_bb(an)->id());
            break;
        case ABS_LOOP:
            note(getRegion(), "\n"); dump_indent(indent);
            prt(getRegion(), "LOOP: HEAD=BB%d", ABS_NODE_loop_head(an)->id());
            dump_abs_tree(ABS_NODE_loop_body(an), indent + 4);
            break;
        case ABS_IF:
            note(getRegion(), "\n"); dump_indent(indent);
            prt(getRegion(), "IF: HEAD=BB%d", ABS_NODE_if_head(an)->id());
            if (ABS_NODE_true_body(an) != nullptr) {
                note(getRegion(), "\n"); dump_indent(indent);
                prt(getRegion(), "TRUE_BODY:");
                dump_abs_tree(ABS_NODE_true_body(an), indent + 4);
            }
            if (ABS_NODE_false_body(an) != nullptr) {
                note(getRegion(), "\n"); dump_indent(indent);
                prt(getRegion(), "FALSE_BODY:");
                dump_abs_tree(ABS_NODE_false_body(an), indent + 4);
            }
            break;
        }
        an = ABS_NODE_next(an);
    }
}


AbsNode * CfsMgr::map_bb2abs(IRBB const* bb)
{
    return m_map_bb2abs.get(bb->id());
}


void CfsMgr::set_map_bb2abs(IRBB const* bb, AbsNode * abs)
{
    m_map_bb2abs.set(bb->id(), abs);
}


AbsNode * CfsMgr::constructAbsLoop(IN IRBB * entry, IN AbsNode * parent,
                                   IN xcom::BitSet * cur_region,
                                   IN xcom::Graph & cur_graph,
                                   MOD xcom::BitSet & visited)
{
    DUMMYUSE(cur_region);
    ASSERT0(cur_region == nullptr || cur_region->is_contain(entry->id()));
    IRCFG * cfg = m_rg->getCFG();
    LI<IRBB> * li = m_bb2li.get(entry);
    ASSERT0(li != nullptr && li->getLoopHead() == entry);

    AbsNode * node = new_abs_node(ABS_LOOP);
    set_map_bb2abs(entry, node);
    ABS_NODE_parent(node) = parent;
    ABS_NODE_loop_head(node) = entry;
    IRBB * body_start;
    CFGLifting cfglifting(cfg);
    cfglifting.getKidOfLoop(entry, nullptr, &body_start);
    ASSERT0(body_start != nullptr);

    CFS_INFO * ci = map_ir2cfsinfo(cfg->get_last_xr(entry));
    ASSERT0_DUMMYUSE(ci);
    ASSERT0(CFS_INFO_head(ci) == entry);

    ASSERT0(CFS_INFO_loop_body(ci)->is_contain(*li->getBodyBBSet()));
    xcom::BitSet loc_visited;
    ABS_NODE_loop_body(node) = constructAbsTree(body_start, node,
                                                li->getBodyBBSet(), cur_graph,
                                                loc_visited);
    visited.bunion(loc_visited);
    visited.bunion(entry->id());
    return node;
}


//'cur_region' covered 'entry'.
AbsNode * CfsMgr::constructAbsIf(IN IRBB * entry, IN AbsNode * parent,
                                 IN xcom::Graph & cur_graph,
                                 MOD xcom::BitSet & visited)
{
    AbsNode * node = new_abs_node(ABS_IF);
    set_map_bb2abs(entry, node);
    ABS_NODE_parent(node) = parent;
    ABS_NODE_if_head(node) = entry;

    IRBB * true_body, * false_body;
    IRCFG * cfg = m_rg->getCFG();
    CFGLifting cfglifting(cfg);
    cfglifting.getKidOfIF(entry, &true_body, &false_body, nullptr);
    CFS_INFO * ci = map_ir2cfsinfo(cfg->get_last_xr(entry));
    ASSERT0(ci != nullptr && CFS_INFO_head(ci) == entry);

    xcom::BitSet loc_visited;
    ABS_NODE_true_body(node) = constructAbsTree(true_body, node,
        CFS_INFO_true_body(ci), cur_graph, loc_visited);
    visited.bunion(loc_visited);
    loc_visited.clean();
    ABS_NODE_false_body(node) = constructAbsTree(false_body, node,
        CFS_INFO_false_body(ci), cur_graph, loc_visited);
    visited.bunion(loc_visited);
    visited.bunion(entry->id());
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


AbsNode * CfsMgr::constructAbsTree(IN IRBB * entry, IN AbsNode * parent,
                                   IN xcom::BitSet * cur_region,
                                   IN xcom::Graph & cur_graph,
                                   MOD xcom::BitSet & visited)
{
    IRCFG * cfg = m_rg->getCFG();
    AbsNode * lst = nullptr;
    IRBB * bb = entry;
    xcom::Graph g;
    g.clone(cur_graph, false, false);
    xcom::Vertex * next = nullptr;
    xcom::Vertex * v;
    if (cur_region != nullptr) {
        if (cur_region->get_elem_count() == 0) {
            visited.clean();
            return nullptr;
        }
        VertexIter c;
        for (v = g.get_first_vertex(c); v != nullptr; v = next) {
            next = g.get_next_vertex(c);
            if (cur_region->is_contain(v->id())) {
                continue;
            }
            g.removeVertex(v);
        }
    }
    xcom::BitSet loc_visited;
    while (bb != nullptr &&
           (cur_region == nullptr ||
            cur_region->is_contain(bb->id()))) {
        AbsNode * node = nullptr;
        loc_visited.clean();
        LI<IRBB> * li = m_bb2li.get(bb);
        if (li != nullptr) {
            node = constructAbsLoop(bb, parent, li->getBodyBBSet(),
                                    g, loc_visited);
        } else {
            IR * last_xr = cfg->get_last_xr(bb);
            if (last_xr != nullptr && //'bb' is branching node of IF.
                last_xr->isConditionalBr()) {
                ASSERT0(map_ir2cfsinfo(last_xr) != nullptr);

                //There might not exist ipdom.
                //e.g:
                //  if (x) //BB1
                //      return 1;
                //  return 2;
                //
                //  BB1 does not have a ipdom.
                UINT ipdom = ((xcom::DGraph*)cfg)->get_ipdom(bb->id());
                DUMMYUSE(ipdom);
                ASSERTN(ipdom > 0, ("bb does not have ipdom"));
                node = constructAbsIf(bb, parent, g, loc_visited);
            } else {
                node = constructAbsBB(bb, parent);
                loc_visited.bunion(bb->id());
            }
        }
        xcom::insertbefore_one(&lst, lst, node);

        visited.bunion(loc_visited);
        //Remove visited vertex.
        next = nullptr;
        VertexIter c;
        for (v = g.get_first_vertex(c); v != nullptr; v = next) {
            next = g.get_next_vertex(c);
            if (!loc_visited.is_contain(v->id())) {
                continue;
            }
            g.removeVertex(v);
        }

        IRBB * cand = nullptr;
        for (v = g.get_first_vertex(c);
             v != nullptr; v = g.get_next_vertex(c)) {
            if (v->getInDegree() == 0) {
                ASSERTN(cand == nullptr,
                        ("multiple immediate-post-dominators"));
                cand = cfg->getBB(v->id());
            }
        }

        if (cand == nullptr) {
            //Cannot find leading BB, there might be exist cycle in graph.
            bb = cfg->get_ipdom(bb);
        } else {
            bb = cand;
        }

        if (parent != nullptr && bb == ABS_NODE_bb(parent)) {
            //Here control-flow is cyclic.
            break;
        }
    }
    lst = xcom::reverse_list(lst);
    return lst;
}


//Construct Control Flow Structure.
AbsNode * CfsMgr::constructAbstractControlFlowStruct()
{
    IRCFG * cfg = m_rg->getCFG();
    ASSERTN(cfg->getEntry(), ("CFG should be single-entry"));
    xcom::BitSet visited;
    AbsNode * a = constructAbsTree(cfg->getEntry(), nullptr, nullptr,
                                   *(xcom::Graph*)cfg, visited);
    //dump_abs_tree(a);
    return a;
}
//END CfsMgr

} //namespace xoc
