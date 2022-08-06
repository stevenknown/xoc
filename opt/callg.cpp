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
//START CallGraph
//
void CallGraph::computeEntryList(List<CallNode*> & elst)
{
    elst.clean();
    VertexIter c;
    for (xcom::Vertex * v = get_first_vertex(c);
         v != nullptr; v = get_next_vertex(c)) {
        if (v->getInList() == nullptr) {
            CallNode * cn = m_cnid2cn.get(v->id());
            ASSERT0(cn != nullptr);
            elst.append_tail(cn);
        }
    }
}


void CallGraph::computeExitList(List<CallNode*> & elst)
{
    elst.clean();
    VertexIter c;
    for (xcom::Vertex * v = get_first_vertex(c);
         v != nullptr; v = get_next_vertex(c)) {
        if (v->getOutList() == nullptr) {
            CallNode * cn = m_cnid2cn.get(v->id());
            ASSERT0(cn != nullptr);
            elst.append_tail(cn);
        }
    }
}


//name: file name if you want to dump VCG to specified file.
//flag: default is 0xFFFFffff(-1) means doing dumping
//      with completely information.
void CallGraph::dumpVCG(CHAR const* name, INT flag)
{
    if (name == nullptr) {
        name = "graph_call_graph.vcg";
    }
    UNLINK(name);
    FILE * h = fopen(name, "a+");
    ASSERTN(h != nullptr, ("%s create failed!!!",name));
    UINT org = getRegionMgr()->getLogMgr()->getIndent();
    getRegionMgr()->getLogMgr()->decIndent(org);

    bool dump_src_line = HAVE_FLAG(flag, CALLG_DUMP_SRC_LINE);
    bool dump_ir_detail = HAVE_FLAG(flag, CALLG_DUMP_IR);
    bool dump_inner_region = HAVE_FLAG(flag, CALLG_DUMP_INNER_REGION);

    //Print comment
    fprintf(h, "\n/*");
    fprintf(h, "\n*/\n");

    //Print graph structure description.
    fprintf(h,
            "graph: {"
            "title: \"Graph\"\n"
            "shrink:  15\n"
            "stretch: 27\n"
            "layout_downfactor: 1\n"
            "layout_upfactor: 1\n"
            "layout_nearfactor: 1\n"
            "layout_splinefactor: 70\n"
            "spreadlevel: 1\n"
            "treefactor: 0.500000\n"
            "node_alignment: center\n"
            "orientation: top_to_bottom\n"
            "late_edge_labels: no\n"
            "display_edge_labels: yes\n"
            "dirty_edge_labels: no\n"
            "finetuning: no\n"
            "nearedges: no\n"
            "splines: yes\n"
            "ignoresingles: no\n"
            "straight_phase: no\n"
            "priority_phase: no\n"
            "manhatten_edges: no\n"
            "smanhatten_edges: no\n"
            "port_sharing: no\n"
            "crossingphase2: yes\n"
            "crossingoptimization: yes\n"
            "crossingweight: bary\n"
            "arrow_mode: free\n"
            "layoutalgorithm: mindepthslow\n"
            "node.borderwidth: 2\n"
            "node.color: lightcyan\n"
            "node.textcolor: black\n"
            "node.bordercolor: blue\n"
            "edge.color: darkgreen\n");

    //Dump graph vertex.
    getRegionMgr()->getLogMgr()->push(h, name);
    VertexIter itv = VERTEX_UNDEF;
    List<Var const*> formalparamlst;
    for (xcom::Vertex * v = get_first_vertex(itv);
         v != nullptr; v = get_next_vertex(itv)) {
        INT id = v->id();
        CallNode * cn = m_cnid2cn.get(id);
        ASSERT0(cn != nullptr);
        fprintf(h, "\nnode: { title:\"%d\" shape:box color:gold "
                   "fontname:\"courB\" label:\"", id);

        CHAR const* cnname = CN_sym(cn) != nullptr ?
                             SYM_name(CN_sym(cn)) : "IndirectCall";
        if (cn->region() != nullptr) {
            fprintf(h, "CN(%d):Region(%d):%s\n",
                    cn->id(), cn->region()->id(), cnname);
        } else {
            fprintf(h, "CN(%d):%s\n", cn->id(), cnname);
        }

        fprintf(h, "\n");
        if (dump_ir_detail && cn->region() != nullptr) {
            //Dump formal paramters.
            formalparamlst.clean();
            cn->region()->findFormalParam(formalparamlst, true);
            for (Var const* param = formalparamlst.get_head(); param != nullptr;
                 param = formalparamlst.get_next()) {
                param->dump(m_tm);
            }

            IR * irs = cn->region()->getIRList();
            if (irs != nullptr) {
                for (; irs != nullptr; irs = irs->get_next()) {
                    //fprintf(h, "%s\n", dump_ir_buf(ir, buf));
                    //TODO: implement dump_ir_buf();
                    dumpIR(irs, cn->region(), nullptr, IR_DUMP_KID|
                           (dump_src_line ? IR_DUMP_SRC_LINE : 0)|
                           (dump_inner_region ? IR_DUMP_INNER_REGION : 0));
                }
            } else {
                dumpBBList(cn->region()->getBBList(),
                           cn->region(), dump_inner_region);
            }
        }
        fprintf(h, "\"}");
    }

    //Dump graph edge
    EdgeIter ite;
    for (xcom::Edge * e = get_first_edge(ite);
         e != nullptr; e = get_next_edge(ite)) {
        xcom::Vertex * from = EDGE_from(e);
        xcom::Vertex * to = EDGE_to(e);
        fprintf(h, "\nedge: { sourcename:\"%d\" targetname:\"%d\" %s}",
                from->id(), to->id(),  "");
    }
    getRegionMgr()->getLogMgr()->pop();
    getRegionMgr()->getLogMgr()->incIndent(org);
    fprintf(h, "\n}\n");
    fclose(h);
}


//Create a CallNode accroding to caller.
//This CallNode will corresponding to an unqiue Region.
//Ensure CallNode for Region is unique.
//'rg': the region that ir resident in.
CallNode * CallGraph::newCallNode(IR const* ir, Region * rg)
{
    ASSERT0(ir->isCallStmt() && rg);
    if (ir->is_call()) {
        Sym const* name = CALL_idinfo(ir)->get_name();
        CallNode * cn  = mapSym2CallNode(name, rg);
        if (cn != nullptr) { return cn; }

        //ir invoked imported region.
        cn = allocCallNode();
        CN_sym(cn) = name;
        CN_id(cn) = m_cn_count++;
        genSYM2CN(rg->getTopRegion())->set(name, cn);
        return cn;
    }

    CallNode * cn = allocCallNode();
    CN_id(cn) = m_cn_count++;
    return cn;
}


//Create a CallNode for given Region.
//To guarantee CallNode of Region is unique.
CallNode * CallGraph::newCallNode(Region * rg)
{
    ASSERT0(rg);
    ASSERT0(rg->getRegionVar() && rg->getRegionVar()->get_name());
    Sym const* name = rg->getRegionVar()->get_name();
    if (rg->is_program()) {
        CallNode * cn = mapRegion2CallNode(rg);
        if (cn != nullptr) {
            ASSERTN(cn->region() == rg, ("more than 2 rg with same id"));
            return cn;
        }

        cn = allocCallNode();
        CN_sym(cn) = name;
        CN_id(cn) = m_cn_count++;
        CN_ru(cn) = rg;
        m_ruid2cn.set(rg->id(), cn);
        return cn;
    }

    ASSERT0(genSYM2CN(rg->getParent()));

    CallNode * cn = allocCallNode();
    CN_sym(cn) = name;
    CN_id(cn) = m_cn_count++;
    CN_ru(cn) = rg;
    genSYM2CN(rg->getParent())->set(name, cn);
    ASSERT0(m_ruid2cn.get(rg->id()) == nullptr);
    m_ruid2cn.set(rg->id(), cn);
    return cn;
}


//Build call graph.
bool CallGraph::build(RegionMgr * rumgr)
{
    for (UINT i = 0; i < rumgr->getNumOfRegion(); i++) {
        Region * rg = rumgr->getRegion(i);
        if (rg == nullptr) { continue; }
        ASSERT0(rg->is_function() || rg->is_program());

        if (rg->getParent() != nullptr) {
            SYM2CN * sym2cn = genSYM2CN(rg->getParent());
            ASSERT0(sym2cn);

            Sym const* name = rg->getRegionVar()->get_name();
            ASSERT0(name);

            CallNode * cn = sym2cn->get(name);
            if (cn != nullptr) {
                if (cn->region() == nullptr) {
                    CN_ru(cn) = rg;
                    m_ruid2cn.set(rg->id(), cn);
                }

                if (cn->region() != rg) {
                    //more than one regions has the same id.
                    //UNREACHABLE();
                    return false;
                }

                continue;
            }
        }

        addNode(newCallNode(rg));
    }

    for (UINT i = 0; i < rumgr->getNumOfRegion(); i++) {
        Region * rg = rumgr->getRegion(i);
        if (rg == nullptr) { continue; }

        ASSERT0(rg->is_function() || rg->is_program());
        CallNode * caller = mapRegion2CallNode(rg);
        ASSERT0(caller);
        addNode(caller);
        CIRList * call_list = rg->getCallList();
        ASSERT0(call_list);
        if (call_list->get_elem_count() == 0) { continue; }

        xcom::C<IR const*> * ct;
        for (call_list->get_head(&ct);
             ct != nullptr; ct = call_list->get_next(ct)) {
            IR const* ir = ct->val();
            ASSERT0(ir && ir->isCallStmt());
            ASSERT0(!CALL_is_intrinsic(ir));

            if (!shouldAddEdge(ir)) { continue; }

            CallNode * callee = newCallNode(ir, rg);
            if (ir->is_icall()) {
                //Indirect call.
                CN_unknown_callee(callee) = true;
            }
            addNode(callee);
            addEdge(CN_id(caller), CN_id(callee));
        }
    }

    return true;
}
//END CallGraph

} //namespace xoc
