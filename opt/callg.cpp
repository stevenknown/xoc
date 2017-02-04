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
#include "callg.h"

namespace xoc {

//
//START CallGraph
//
void CallGraph::computeEntryList(List<CallNode*> & elst)
{
    elst.clean();
    INT c;
    for (Vertex * v = get_first_vertex(c);
         v != NULL; v = get_next_vertex(c)) {
        if (VERTEX_in_list(v) == NULL) {
            CallNode * cn = m_cnid2cn.get(VERTEX_id(v));
            ASSERT0(cn != NULL);
            elst.append_tail(cn);
        }
    }
}


void CallGraph::computeExitList(List<CallNode*> & elst)
{
    elst.clean();
    INT c;
    for (Vertex * v = get_first_vertex(c);
         v != NULL; v = get_next_vertex(c)) {
        if (VERTEX_out_list(v) == NULL) {
            CallNode * cn = m_cnid2cn.get(VERTEX_id(v));
            ASSERT0(cn != NULL);
            elst.append_tail(cn);
        }
    }
}


//name: file name if you want to dump VCG to specified file.
//flag: default is 0xFFFFffff(-1) means doing dumping
//      with completely information.
void CallGraph::dump_vcg(CHAR const* name, INT flag)
{
    if (name == NULL) {
        name = "graph_call_graph.vcg";
    }
    UNLINK(name);
    FILE * h = fopen(name, "a+");
    ASSERT(h != NULL, ("%s create failed!!!",name));

    bool dump_src_line = HAVE_FLAG(flag, CALLG_DUMP_SRC_LINE);
    bool dump_ir_detail = HAVE_FLAG(flag, CALLG_DUMP_IR);
    bool dump_inner_region = HAVE_FLAG(flag, CALLG_DUMP_INNER_REGION);

    //Print comment
    fprintf(h, "\n/*");
    FILE * old = g_tfile;
    g_tfile = h;
    //....
    g_tfile = old;
    fprintf(h, "\n*/\n");

    //Print graph structure description.
    fprintf(h, "graph: {"
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
    old = g_tfile;
    g_tfile = h;
    INT c;
    List<VAR const*> formalparamlst;
    for (Vertex * v = m_vertices.get_first(c);
         v != NULL; v = m_vertices.get_next(c)) {
        INT id = VERTEX_id(v);
        CallNode * cn = m_cnid2cn.get(id);
        ASSERT0(cn != NULL);
        fprintf(h, "\nnode: { title:\"%d\" shape:box color:gold "
                   "fontname:\"courB\" label:\"", id);

        CHAR const* cnname = CN_sym(cn) != NULL ?
                SYM_name(CN_sym(cn)) : "IndirectCall";
        if (CN_ru(cn) != NULL) {
            fprintf(h, "CN(%d):Region(%d):%s\n",
                    CN_id(cn), REGION_id(CN_ru(cn)), cnname);
        } else {
            fprintf(h, "CN(%d):%s\n", CN_id(cn), cnname);
        }

        fprintf(h, "\n");
        if (dump_ir_detail && CN_ru(cn) != NULL) {
            //Dump formal paramters.
            formalparamlst.clean();
            CN_ru(cn)->findFormalParam(formalparamlst, true);
            for (VAR const* param = formalparamlst.get_head(); param != NULL;
                 param = formalparamlst.get_next()) {
                param->dump(g_tfile, m_tm);
            }

            g_indent = 0;
            IR * irs = CN_ru(cn)->getIRList();
            if (irs != NULL) {
                for (; irs != NULL; irs = irs->get_next()) {
                    //fprintf(h, "%s\n", dump_ir_buf(ir, buf));
                    //TODO: implement dump_ir_buf();
                    dump_ir(irs, m_tm, NULL, true,
                            dump_src_line, false, dump_inner_region);
                }
            } else {
                dumpBBList(CN_ru(cn)->getBBList(),
                           CN_ru(cn), NULL, dump_inner_region);
            }
        }
        fprintf(h, "\"}");
    }

    //Dump graph edge
    for (Edge * e = m_edges.get_first(c); e != NULL; e = m_edges.get_next(c)) {
        Vertex * from = EDGE_from(e);
        Vertex * to = EDGE_to(e);
        fprintf(h, "\nedge: { sourcename:\"%d\" targetname:\"%d\" %s}",
                VERTEX_id(from), VERTEX_id(to),  "");
    }
    g_tfile = old;
    fprintf(h, "\n}\n");
    fclose(h);
}


//Create a CallNode accroding to caller.
//This CallNode will corresponding to an unqiue Region.
//Ensure CallNode for Region is unique.
//'ru': the region that ir resident in.
CallNode * CallGraph::newCallNode(IR const* ir, Region * ru)
{
    ASSERT0(ir->isCallStmt() && ru);
    if (ir->is_call()) {
        SYM const* name = CALL_idinfo(ir)->get_name();
        CallNode * cn  = mapSym2CallNode(name, ru);
        if (cn != NULL) { return cn; }

        //ir invoked imported region.
        cn = allocCallNode();
        CN_sym(cn) = name;
        CN_id(cn) = m_cn_count++;
        genSYM2CN(ru->getTopRegion())->set(name, cn);
        return cn;
    }

    CallNode * cn = allocCallNode();
    CN_id(cn) = m_cn_count++;
    return cn;
}


//Create a CallNode for given Region.
//To guarantee CallNode of Region is unique.
CallNode * CallGraph::newCallNode(Region * ru)
{
    ASSERT0(ru);
    ASSERT0(ru->getRegionVar() && ru->getRegionVar()->get_name());
    SYM const* name = ru->getRegionVar()->get_name();
    if (ru->is_program()) {
        CallNode * cn = mapRegion2CallNode(ru);
        if (cn != NULL) {
            ASSERT(CN_ru(cn) == ru, ("more than 2 ru with same id"));
            return cn;
        }

        cn = allocCallNode();
        CN_sym(cn) = name;
        CN_id(cn) = m_cn_count++;
        CN_ru(cn) = ru;
        m_ruid2cn.set(REGION_id(ru), cn);
        return cn;
    }

    ASSERT0(genSYM2CN(ru->getParent()));

    CallNode * cn = allocCallNode();
    CN_sym(cn) = name;
    CN_id(cn) = m_cn_count++;
    CN_ru(cn) = ru;
    genSYM2CN(ru->getParent())->set(name, cn);
    ASSERT0(m_ruid2cn.get(REGION_id(ru)) == NULL);
    m_ruid2cn.set(REGION_id(ru), cn);
    return cn;
}


//Build call graph.
bool CallGraph::build(RegionMgr * rumgr)
{
    for (UINT i = 0; i < rumgr->getNumOfRegion(); i++) {
        Region * ru = rumgr->get_region(i);
        if (ru == NULL) { continue; }
        ASSERT0(ru->is_function() || ru->is_program());

        if (ru->getParent() != NULL) {
            SYM2CN * sym2cn = genSYM2CN(ru->getParent());
            ASSERT0(sym2cn);

            SYM const* name = ru->getRegionVar()->get_name();
            ASSERT0(name);

            CallNode * cn = sym2cn->get(name);
            if (cn != NULL) {
                if (CN_ru(cn) == NULL) {
                    CN_ru(cn) = ru;
                    m_ruid2cn.set(REGION_id(ru), cn);
                }

                if (CN_ru(cn) != ru) {
                    //more than one regions has the same id.
                    //UNREACH();
                    return false;
                }

                continue;
            }
        }

        add_node(newCallNode(ru));
    }

    for (UINT i = 0; i < rumgr->getNumOfRegion(); i++) {
        Region * ru = rumgr->get_region(i);
        if (ru == NULL) { continue; }

        ASSERT0(ru->is_function() || ru->is_program());
        CallNode * caller = mapRegion2CallNode(ru);
        ASSERT0(caller);
        add_node(caller);
        List<IR const*> * call_list = ru->getCallList();
        ASSERT0(call_list);
        if (call_list->get_elem_count() == 0) { continue; }

        C<IR const*> * ct;
        for (call_list->get_head(&ct);
             ct != NULL; ct = call_list->get_next(ct)) {
            IR const* ir = ct->val();
            ASSERT0(ir && ir->isCallStmt());
            ASSERT0(!CALL_is_intrinsic(ir));

            if (!shouldAddEdge(ir)) { continue; }

            CallNode * callee = newCallNode(ir, ru);
            if (ir->is_icall()) {
                //Indirect call.
                CN_unknown_callee(callee) = true;
            }
            add_node(callee);
            addEdge(CN_id(caller), CN_id(callee));
        }
    }

    return true;
}
//END CallGraph

} //namespace xoc
