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
    dump_vcg("graph_cd_tree.vcg");
    if (g_tfile == NULL) return;
    fprintf(g_tfile, "\n==---- DUMP Control Dependence ----==");
    INT c;
    for (Vertex * v = get_first_vertex(c);
         v != NULL; v = get_next_vertex(c)) {
        EdgeC * in = VERTEX_in_list(v);
        if (in == NULL) {
            fprintf(g_tfile, "\nBB%d has NO ctrl BB", VERTEX_id(v));
            continue;
        }
        fprintf(g_tfile, "\nBB%d ctrl BB is: ", VERTEX_id(v));
        while (in != NULL) {
            Vertex * pred = EDGE_from(EC_edge(in));
            fprintf(g_tfile, "%d,", VERTEX_id(pred));
            in = EC_next(in);
        }
    }
    fprintf(g_tfile, "\n");
    fflush(g_tfile);
}


void CDG::get_cd_preds(UINT id, OUT List<Vertex*> & lst)
{
    Vertex * v = get_vertex(id);
    ASSERT0(v != NULL);
    EdgeC * in = VERTEX_in_list(v);
    while (in != NULL) {
        Vertex * pred = EDGE_from(EC_edge(in));
        lst.append_tail(pred);
        in = EC_next(in);
    }
}


//Return true if b is control dependent on a.
bool CDG::is_cd(UINT a, UINT b)
{
    ASSERT0(get_vertex(b));
    Vertex * v = get_vertex(a);
    ASSERT0(v != NULL);
    EdgeC * out = VERTEX_out_list(v);
    while (out != NULL) {
        if (VERTEX_id(EDGE_to(EC_edge(out))) == b) {
            return true;
        }
        out = EC_next(out);
    }
    return false;
}


bool CDG::is_only_cd_self(UINT id)
{
    Vertex * v = get_vertex(id);
    ASSERT0(v != NULL);
    EdgeC * out = VERTEX_out_list(v);
    while (out != NULL) {
        Vertex * succ = EDGE_to(EC_edge(out));
        if (succ != v) return false;
        out = EC_next(out);
    }
    return true;
}


void CDG::get_cd_succs(UINT id, OUT List<Vertex*> & lst)
{
    Vertex * v = get_vertex(id);
    ASSERT0(v != NULL);
    EdgeC * out = VERTEX_out_list(v);
    while (out != NULL) {
        Vertex * succ = EDGE_to(EC_edge(out));
        lst.append_tail(succ);
        out = EC_next(out);
    }
}


void CDG::rebuild(IN OUT OptCtx & oc, DGraph & cfg)
{
    erase();
    build(oc, cfg);
}


void CDG::build(IN OUT OptCtx & oc, DGraph & cfg)
{
    if (cfg.get_vertex_num() == 0) { return; }

    START_TIMER(t, "CDG");
    ASSERT0(OC_is_cfg_valid(oc));
    m_ru->checkValidAndRecompute(&oc, PASS_PDOM, PASS_UNDEF);

    Graph pdom_tree;
    cfg.get_pdom_tree(pdom_tree);
    if (pdom_tree.get_vertex_num() == 0) { return; }

    Vector<UINT> top_order;
    pdom_tree.sortInToplogOrder(top_order, false);
    //dumpIntVector(top_order);

    BitSetMgr bs_mgr;
    Vector<BitSet*> cd_set;
    for (INT j = 0; j <= top_order.get_last_idx(); j++) {
        UINT ii = top_order.get(j);
        Vertex * v = cfg.get_vertex(ii);
        ASSERT0(v != NULL);
        addVertex(VERTEX_id(v));
        BitSet * cd_of_v = cd_set.get(VERTEX_id(v));
        if (cd_of_v == NULL) {
            cd_of_v = bs_mgr.create();
            cd_set.set(VERTEX_id(v), cd_of_v);
        }

        EdgeC * in = VERTEX_in_list(v);
        while (in != NULL) {
            Vertex * pred = EDGE_from(EC_edge(in));
            if (VERTEX_id(v) != ((DGraph&)cfg).get_ipdom(VERTEX_id(pred))) {
                cd_of_v->bunion(VERTEX_id(pred));
                //if (pred != v)
                {
                    addEdge(VERTEX_id(pred), VERTEX_id(v));
                }
            }
            in = EC_next(in);
        }
        INT c;
        for (Vertex * z = cfg.get_first_vertex(c);
             z != NULL; z = cfg.get_next_vertex(c)) {
            if (((DGraph&)cfg).get_ipdom(VERTEX_id(z)) == VERTEX_id(v)) {
                BitSet * cd = cd_set.get(VERTEX_id(z));
                if (cd == NULL) {
                    cd = bs_mgr.create();
                    cd_set.set(VERTEX_id(z), cd);
                }
                for (INT i = cd->get_first(); i != -1; i = cd->get_next(i)) {
                    if (VERTEX_id(v) != ((DGraph&)cfg).get_ipdom(i)) {
                        cd_of_v->bunion(i);
                        //if (i != (INT)VERTEX_id(v))
                        {
                            addEdge(i, VERTEX_id(v));
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
