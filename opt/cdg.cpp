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

//
//START CDG
//
void CDG::dump()
{
    dumpVCG("graph_cd_tree.vcg");
    if (g_tfile == NULL) { return; }
    note("\n==---- DUMP Control Dependence ----==");
    INT c;
    for (xcom::Vertex * v = get_first_vertex(c);
         v != NULL; v = get_next_vertex(c)) {
        xcom::EdgeC * in = VERTEX_in_list(v);
        if (in == NULL) {
            note("\nBB%d has NO ctrl BB", v->id());
            continue;
        }
        note("\nBB%d ctrl BB is: ", v->id());
        while (in != NULL) {
            xcom::Vertex * pred = in->getFrom();
            prt("%d,", pred->id());
            in = EC_next(in);
        }
    }
    note("\n");
    fflush(g_tfile);
}


void CDG::get_cd_preds(UINT id, OUT List<xcom::Vertex*> & lst)
{
    xcom::Vertex * v = getVertex(id);
    ASSERT0(v != NULL);
    xcom::EdgeC * in = VERTEX_in_list(v);
    while (in != NULL) {
        xcom::Vertex * pred = in->getFrom();
        lst.append_tail(pred);
        in = EC_next(in);
    }
}


//Return true if b is control dependent on a.
bool CDG::is_cd(UINT a, UINT b)
{
    ASSERT0(getVertex(b));
    xcom::Vertex * v = getVertex(a);
    ASSERT0(v != NULL);
    xcom::EdgeC * out = VERTEX_out_list(v);
    while (out != NULL) {
        if (out->getToId() == b) {
            return true;
        }
        out = out->get_next();
    }
    return false;
}


bool CDG::is_only_cd_self(UINT id)
{
    xcom::Vertex * v = getVertex(id);
    ASSERT0(v != NULL);
    xcom::EdgeC * out = VERTEX_out_list(v);
    while (out != NULL) {
        xcom::Vertex * succ = out->getTo();
        if (succ != v) { return false; }
        out = out->get_next();
    }
    return true;
}


void CDG::get_cd_succs(UINT id, OUT List<xcom::Vertex*> & lst)
{
    xcom::Vertex * v = getVertex(id);
    ASSERT0(v != NULL);
    xcom::EdgeC * out = VERTEX_out_list(v);
    while (out != NULL) {
        xcom::Vertex * succ = out->getTo();
        lst.append_tail(succ);
        out = out->get_next();
    }
}


void CDG::rebuild(IN OUT OptCtx & oc, xcom::DGraph & cfg)
{
    erase();
    build(oc, cfg);
}


void CDG::build(IN OUT OptCtx & oc, xcom::DGraph & cfg)
{
    if (cfg.getVertexNum() == 0) { return; }

    START_TIMER(t, "CDG");
    ASSERT0(OC_is_cfg_valid(oc));
    m_rg->checkValidAndRecompute(&oc, PASS_PDOM, PASS_UNDEF);

    xcom::Graph pdom_tree;
    cfg.get_pdom_tree(pdom_tree);
    if (pdom_tree.getVertexNum() == 0) { return; }

    Vector<Vertex*> top_order;
    pdom_tree.sortInTopologOrder(top_order);
    //dumpIntVector(top_order);

    xcom::BitSetMgr bs_mgr;
    Vector<xcom::BitSet*> cd_set;
    for (INT j = 0; j <= top_order.get_last_idx(); j++) {
        UINT ii = top_order.get(j)->id();
        xcom::Vertex * v = cfg.getVertex(ii);
        ASSERT0(v != NULL);
        addVertex(v->id());
        xcom::BitSet * cd_of_v = cd_set.get(v->id());
        if (cd_of_v == NULL) {
            cd_of_v = bs_mgr.create();
            cd_set.set(v->id(), cd_of_v);
        }

        xcom::EdgeC * in = v->getInList();
        while (in != NULL) {
            xcom::Vertex * pred = in->getFrom();
            if (v->id() != ((xcom::DGraph&)cfg).get_ipdom(pred->id())) {
                cd_of_v->bunion(pred->id());
                //if (pred != v)
                {
                    addEdge(pred->id(), v->id());
                }
            }
            in = in->get_next();
        }
        INT c;
        for (xcom::Vertex * z = cfg.get_first_vertex(c);
             z != NULL; z = cfg.get_next_vertex(c)) {
            if (((xcom::DGraph&)cfg).get_ipdom(z->id()) == v->id()) {
                xcom::BitSet * cd = cd_set.get(z->id());
                if (cd == NULL) {
                    cd = bs_mgr.create();
                    cd_set.set(z->id(), cd);
                }
                for (INT i = cd->get_first(); i != -1; i = cd->get_next(i)) {
                    if (v->id() != ((xcom::DGraph&)cfg).get_ipdom(i)) {
                        cd_of_v->bunion(i);
                        //if (i != (INT)v->id())
                        {
                            addEdge(i, v->id());
                        }
                    }
                }
            }
        }
    } //end for

    OC_is_cdg_valid(oc) = true;
    END_TIMER(t, "CDG");
}
//END CDG

} //namespace xoc
