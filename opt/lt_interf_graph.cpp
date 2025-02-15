/*@
Copyright (c) 2013-2021, Su Zhenyu steven.known@gmail.com

All rights reserved.

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

THIS SOFTWARE IS PROVIDED "AS IS" AND ANY
EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
DISCLAIMED. IN NO EVENT SHALL THE CONTRIBUTORS BE LIABLE FOR ANY
DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
(INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND
ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
(INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
@*/
#include "cominc.h"
#include "comopt.h"
#include "targinfo_mgr.h"
#include "lifetime.h"
#include "lt_interf_graph.h"
#include "linear_scan.h"
#include "lsra_impl.h"
#include "lsra_scan_in_pos.h"
#include "lt_prio_mgr.h"
#include "lsra_scan_in_prio.h"

namespace xoc {

void LTIG::dumpVertexDesc(
    xcom::Vertex const* v, OUT xcom::DefFixedStrBuf & buf) const
{
    buf.sprint("$%u", v->id());
}


void LTIG::dumpVCG(CHAR const* name) const
{
    if (name == nullptr) {
        name = "zinterf_graph.vcg";
    }
    UNLINK(name);
    FILE * h = fopen(name, "a+");
    ASSERTN(h, ("%s create failed!!!",name));
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

    StrBuf buf(64);
    //Print node
    VertexIter c;
    for (xcom::Vertex const* v = get_first_vertex(c);
         v != nullptr;  v = get_next_vertex(c)) {
        buf.sprint("$%u:", v->id());
        fprintf(h, "\nnode: { title:\"%d\" label:\"%s\" "
                   "shape:circle fontname:\"courB\" color:gold}",
                   v->id(), buf.buf);
    }
    //Print edge
    EdgeIter ite;
    for (xcom::Edge const* e = get_first_edge(ite);
         e != nullptr;  e = get_next_edge(ite)) {
        fprintf(h, "\nedge: { sourcename:\"%d\" targetname:\"%d\" %s}",
            e->from()->id(), e->to()->id(),
            is_direction() ? "" : "arrowstyle:none" );
    }
    fprintf(h, "\n}\n");
    fclose(h);
}


bool LTIG::is_interferred(LifeTime const* lt1, LifeTime const* lt2) const
{
    ASSERT0(lt1->getPrno() != lt2->getPrno());
    return getEdge(lt1->getPrno(), lt2->getPrno()) != nullptr;
}


void LTIG::build()
{
    LTList const& ltlst = m_lt_mgr.getLTList();
    build(ltlst);
}


void LTIG::build(LTList const& ltlst)
{
    erase();
    LTListIter it1;
    for (LifeTime const* lt1 = ltlst.get_head(&it1);
         lt1 != nullptr; lt1 = ltlst.get_next(&it1)) {
        addVertex(lt1->getPrno());
        LTListIter it2 = it1;
        for (LifeTime const* lt2 = ltlst.get_next(&it2);
             lt2 != nullptr; lt2 = ltlst.get_next(&it2)) {
            ASSERTN(lt1->getPrno() != lt2->getPrno(),
                    ("the relation to PRNO is unqiue"));
            if (lt1->is_intersect(lt2)) {
                addEdge(lt1->getPrno(), lt2->getPrno());
            } else {
                addVertex(lt2->getPrno());
            }
        }
    }
}

} //namespace xoc
