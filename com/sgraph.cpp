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
#include "ltype.h"
#include "comf.h"
#include "smempool.h"
#include "sstl.h"
#include "bs.h"
#include "sbs.h"
#include "sgraph.h"

namespace xcom {

//Expect for unique vertex in graphic depitction
#define ALWAYS_VERTEX_UNIQUE

//
//START EdgeHash
//
Edge * EdgeHash::create(OBJTY v)
{
    Edge * t = (Edge*)v;
    Vertex * from = m_g->get_vertex(VERTEX_id(EDGE_from(t)));
    Vertex * to = m_g->get_vertex(VERTEX_id(EDGE_to(t)));
    ASSERT0(from && to);
    t = m_g->newEdgeImpl(from, to);
    return t;
}
//END EdgeHash


//DONT CALL Constructor directly.
Graph::Graph(UINT edge_hash_size, UINT vex_hash_size) :
    m_edges(edge_hash_size), m_vertices(vex_hash_size)
{
    m_edge_hash_size = edge_hash_size;
    m_vex_hash_size = vex_hash_size;
    m_ec_pool = NULL;
    init();
}


Graph::Graph(Graph const& g) :
    m_edges(g.m_edge_hash_size), m_vertices(g.m_vex_hash_size)
{
    m_edge_hash_size = g.m_edge_hash_size;
    m_vex_hash_size = g.m_vex_hash_size;
    m_ec_pool = NULL;
    init();
    clone(g);
}


void Graph::init()
{
    if (m_ec_pool != NULL) { return; }

    m_ec_pool = smpoolCreate(sizeof(EdgeC), MEM_CONST_SIZE);
    ASSERTN(m_ec_pool, ("create mem pool failed"));

    m_vertex_pool = smpoolCreate(sizeof(Vertex) * 4, MEM_CONST_SIZE);
    ASSERTN(m_vertex_pool, ("create mem pool failed"));

    m_edge_pool = smpoolCreate(sizeof(Edge) * 4, MEM_CONST_SIZE);
    ASSERTN(m_edge_pool, ("create mem pool failed"));

    //If m_edges already initialized, this call will do nothing.
    m_edges.init(this, m_edge_hash_size);

    //If m_vertices already initialized, this call will do nothing.
    m_vertices.init(m_vex_hash_size);
    m_is_unique = true;
    m_is_direction = true;
    m_dense_vertex = NULL;
}


//Reconstruct vertex hash table, and edge hash table with new bucket size.
//'vertex_hash_sz': new vertex table size to be resized.
//'edge_hash_sz': new edge table size to be resized.
//NOTE: mem pool should have been initialized.
void Graph::resize(UINT vertex_hash_sz, UINT edge_hash_sz)
{
    ASSERT0(m_ec_pool);
    ASSERTN(m_vertices.get_elem_count() == 0, ("graph is not empty"));
    ASSERTN(m_edges.get_elem_count() == 0, ("graph is not empty"));

    if (m_vex_hash_size != vertex_hash_sz) {
        m_vertices.destroy();
        m_vertices.init(vertex_hash_sz);
        m_vex_hash_size = vertex_hash_sz;
    }

    if (m_edge_hash_size != edge_hash_sz) {
        m_edges.destroy();
        m_edges.init(this, edge_hash_sz);
        m_edge_hash_size = edge_hash_sz;
    }
}


size_t Graph::count_mem() const
{
    size_t count = 0;
    count += sizeof(BYTE);
    count += sizeof(m_edge_hash_size);
    count += sizeof(m_vex_hash_size);
    count += m_edges.count_mem();
    count += m_vertices.count_mem();
    count += m_e_free_list.count_mem();
    count += m_el_free_list.count_mem();
    count += m_v_free_list.count_mem();
    count += smpoolGetPoolSize(m_ec_pool);
    count += smpoolGetPoolSize(m_vertex_pool);
    count += smpoolGetPoolSize(m_edge_pool);
    if (m_dense_vertex != NULL) {
        count += m_dense_vertex->count_mem();
    }
    return count;
}


void Graph::destroy()
{
    if (m_ec_pool == NULL) return;
    m_edges.destroy();
    m_vertices.destroy();

    //Set if edge and vertex would not be redundantly.
    m_is_unique = false;
    m_is_direction = false; //Set if graph is direction.
    m_e_free_list.clean(); //edge free list
    m_el_free_list.clean(); //edge-list free list
    m_v_free_list.clean(); //vertex free list

    smpoolDelete(m_ec_pool);
    m_ec_pool = NULL;

    smpoolDelete(m_vertex_pool);
    m_vertex_pool = NULL;

    smpoolDelete(m_edge_pool);
    m_edge_pool = NULL;

    if (m_dense_vertex != NULL) {
        delete m_dense_vertex;
        m_dense_vertex = NULL;
    }
}


//Erasing graph, include all nodes and edges,
//except for mempool, freelist.
void Graph::erase()
{
    ASSERTN(m_ec_pool != NULL, ("Graph must be initialized before clone."));
    //Collect edge, vertex data structure into free-list
    //for allocation on demand.
    INT c;
    for (Vertex * v = m_vertices.get_first(c);
         v != NULL; v = m_vertices.get_next(c)) {
         removeVertex(v);
    }
    #ifdef _DEBUG_
    if (m_dense_vertex != NULL) {
        for (INT i = 0; i <= m_dense_vertex->get_last_idx(); i++) {
            //Should be removed in removeVertex().
            ASSERT0(m_dense_vertex->get((UINT)i) == NULL);
        }
    }
    #endif
}


//Sort vertice by rporder, and update rpo of vertex.
//Record sorted vertex into vlst in incremental order of rpo.
//NOTE: rpo start at 1, and 0 means undefined.
void Graph::computeRpoNoRecursive(Vertex * root, OUT List<Vertex const*> & vlst)
{
    ASSERT0(root && is_graph_entry(root));
    BitSet is_visited;
    Stack<Vertex*> stk;
    stk.push(root);
    Vertex * v;
    UINT order = get_vertex_num();
    while ((v = stk.get_top()) != NULL) {
        is_visited.bunion(VERTEX_id(v));
        EdgeC * el = VERTEX_out_list(v);
        bool find = false; //find unvisited kid.
        while (el != NULL) {
            Vertex * succ = EDGE_to(EC_edge(el));
            if (!is_visited.is_contain(VERTEX_id(succ))) {
                stk.push(succ);
                find = true;
                break;
            }
            el = EC_next(el);
        }
        if (!find) {
            stk.pop();
            vlst.append_head(v);
            VERTEX_rpo(v) = order--;
        }
    }
    //If order is not zero, there are some BBs should be eliminated by CFG
    //optimizations.
    ASSERTN(order == 0, ("still have some BBs that are not assigned an order"));
}


bool Graph::clone(Graph const& src)
{
    erase();
    m_is_unique = src.m_is_unique;
    m_is_direction = src.m_is_direction;

    //Clone vertices
    INT c;
    for (Vertex * srcv = src.get_first_vertex(c);
         srcv != NULL; srcv = src.get_next_vertex(c)) {
        Vertex * v = addVertex(VERTEX_id(srcv));

        //Calls inherited class method.
        //Vertex info of memory should allocated by inherited class method
        if (VERTEX_info(srcv) != NULL) {
            VERTEX_info(v) = cloneVertexInfo(srcv);
        }
    }

    //Clone edges
    for (Edge * srce = src.get_first_edge(c);
         srce != NULL; srce = src.get_next_edge(c)) {
        Edge * e = addEdge(VERTEX_id(EDGE_from(srce)),
            VERTEX_id(EDGE_to(srce)));

        //Calls inherited class method.
        //Edge info of memory should allocated by inherited class method
        if (EDGE_info(srce) != NULL) {
            EDGE_info(e) = cloneEdgeInfo(srce);
        }
    }
    return true;
}


//NOTE: Do NOT use 0 as vertex id.
Vertex * Graph::newVertex(UINT vid)
{
    ASSERTN(m_vertex_pool, ("not yet initialized."));
    ASSERTN(vid != 0, ("Do not use 0 as vertex id"));

    Vertex * vex = NULL;

    #ifndef ALWAYS_VERTEX_UNIQUE
    if (m_is_unique)
    #endif
    {
        Vertex * v = m_vertices.find((OBJTY)(size_t)vid);
        if (v != NULL) {
            return v;
        }
    }

    vex = m_v_free_list.get_free_elem();
    if (vex == NULL) {
        vex = newVertex();
    }
    VERTEX_id(vex) = vid;

    if (m_dense_vertex != NULL) {
        ASSERT0(m_dense_vertex->get(vid) == NULL);
        m_dense_vertex->set(vid, vex);
    }
    return vex;
}


Edge * Graph::newEdge(Vertex * from, Vertex * to)
{
    ASSERTN(m_ec_pool != NULL, ("not yet initialized."));
    if (from == NULL || to == NULL) return NULL;
    Edge teste;
    Vertex testfrom, testto;
    if (m_is_unique) {
        VERTEX_id(&testfrom) = VERTEX_id(from);
        VERTEX_id(&testto) = VERTEX_id(to);
        EDGE_from(&teste) = &testfrom;
        EDGE_to(&teste) = &testto;
        if (m_is_direction) {
            return m_edges.append((OBJTY)&teste, NULL);
        }

        Edge * e = NULL;
        if (m_edges.find(&teste, &e)) {
            ASSERT0(e);
            return e;
        }

        //Both check from->to and to->from
        EDGE_from(&teste) = &testto;
        EDGE_to(&teste) = &testfrom;
        return m_edges.append((OBJTY)&teste, NULL);
    }
    return m_edges.append(newEdgeImpl(from, to));
}


//Reverse edge direction
Edge * Graph::reverseEdge(Edge * e)
{
    ASSERTN(m_ec_pool != NULL, ("not yet initialized."));
    ASSERTN(m_is_direction, ("graph is indirection"));
    void * einfo = EDGE_info(e);
    Vertex * from = EDGE_from(e);
    Vertex * to = EDGE_to(e);
    removeEdge(e);
    e = addEdge(VERTEX_id(to), VERTEX_id(from));
    EDGE_info(e) = einfo;
    return e;
}


//Reverse all edge direction
void Graph::reverseEdges()
{
    ASSERTN(m_ec_pool != NULL, ("not yet initialized."));
    ASSERTN(m_is_direction, ("graph is indirection"));
    List<Edge*> list;
    Edge * e;
    INT c;
    for (e = get_first_edge(c); e != NULL; e = get_next_edge(c)) {
        list.append_tail(e);
    }
    for (e = list.get_head(); e != NULL; e = list.get_next()) {
        reverseEdge(e);
    }
}


//Insert 'newv' between 'v1' and 'v2'.
//e.g: given edge v1->v2, the result is v1->newv->v2.
//Return edge v1->newv, newv->v2.
void Graph::insertVertexBetween(
        IN Vertex * v1,
        IN Vertex * v2,
        IN Vertex * newv,
        OUT Edge ** e1,
        OUT Edge ** e2,
        bool sort)
{
    Edge * e = get_edge(v1, v2);

    EdgeC * v2pos_in_list = NULL;
    EdgeC * v1pos_in_list = NULL;
    if (sort) {
        for (EdgeC * ec = VERTEX_out_list(v1); ec != NULL; ec = EC_next(ec)) {
            if (EDGE_to(EC_edge(ec)) == v2) {
                break;
            }
            v2pos_in_list = ec;
        }

        for (EdgeC * ec = VERTEX_in_list(v2); ec != NULL; ec = EC_next(ec)) {
            if (EDGE_from(EC_edge(ec)) == v1) {
                break;
            }
            v1pos_in_list = ec;
        }
    }

    ASSERTN(e, ("no edge in bewteen %d and %d", VERTEX_id(v1), VERTEX_id(v2)));
    removeEdge(e);
    Edge * tmpe1 = addEdge(v1, newv);
    Edge * tmpe2 = addEdge(newv, v2);
    if (e1 != NULL) { *e1 = tmpe1; }
    if (e2 != NULL) { *e2 = tmpe2; }

    if (!sort) { return; }

    EdgeC * tmpe1_ec = NULL;
    for (EdgeC * ec = VERTEX_out_list(v1); ec != NULL; ec = EC_next(ec)) {
        if (EC_edge(ec) == tmpe1) {
            tmpe1_ec = ec;
            break;
        }
    }
    ASSERT0(tmpe1_ec);

    EdgeC * tmpe2_ec = NULL;
    for (EdgeC * ec = VERTEX_in_list(v2); ec != NULL; ec = EC_next(ec)) {
        if (EC_edge(ec) == tmpe2) {
            tmpe2_ec = ec;
            break;
        }
    }
    ASSERT0(tmpe2_ec);

    xcom::remove(&VERTEX_out_list(v1), tmpe1_ec);
    if (v2pos_in_list == NULL) {
        xcom::append_head(&VERTEX_out_list(v1), tmpe1_ec);
    } else {
        xcom::insertafter_one(&v2pos_in_list, tmpe1_ec);
    }

    xcom::remove(&VERTEX_in_list(v2), tmpe2_ec);
    if (v1pos_in_list == NULL) {
        xcom::append_head(&VERTEX_in_list(v2), tmpe2_ec);
    } else {
        xcom::insertafter_one(&v1pos_in_list, tmpe2_ec);
    }
}


//Insert 'newv' between 'v1' and 'v2'.
//e.g: given edge v1->v2, the result is v1->newv->v2.
//Return edge v1->newv, newv->v2.
//
//NOTICE: newv must be node in graph.
void Graph::insertVertexBetween(
        UINT v1,
        UINT v2,
        UINT newv,
        OUT Edge ** e1,
        OUT Edge ** e2,
        bool sort)
{
    Vertex * pv1 = get_vertex(v1);
    Vertex * pv2 = get_vertex(v2);
    Vertex * pnewv = get_vertex(newv);
    ASSERT0(pv1 && pv2 && pnewv);
    insertVertexBetween(pv1, pv2, pnewv, e1, e2, sort);
}


Edge * Graph::removeEdge(Edge * e)
{
    ASSERTN(m_ec_pool != NULL, ("not yet initialized."));
    if (e == NULL) { return NULL; }
    Vertex * from = EDGE_from(e);
    Vertex * to = EDGE_to(e);

    //remove out of out-list of 'from'
    EdgeC * el = VERTEX_out_list(from);
    while (el != NULL) {
        if (EC_edge(el) == e) { break; }
        el = EC_next(el);
    }
    ASSERTN(el != NULL, ("can not find out-edge, it is illegal graph"));
    xcom::remove(&VERTEX_out_list(from), el);
    m_el_free_list.add_free_elem(el);

    //remove out of in-list of 'to'
    el = VERTEX_in_list(to);
    while (el != NULL) {
        if (EC_edge(el) == e) { break; }
        el = EC_next(el);
    }
    ASSERTN(el != NULL, ("can not find in-edge, it is illegal graph"));
    xcom::remove(&VERTEX_in_list(to), el);
    m_el_free_list.add_free_elem(el);

    //remove edge out of edge-hash
    e = m_edges.removed(e);
    m_e_free_list.add_free_elem(e);
    return e;
}


Vertex * Graph::removeVertex(Vertex * vex)
{
    ASSERTN(m_ec_pool != NULL, ("not yet initialized."));
    if (vex == NULL) return NULL;
    EdgeC * el = VERTEX_out_list(vex);
    //remove all out-edge
    while (el != NULL) {
        EdgeC * tmp = el;
        el = EC_next(el);
        removeEdge(EC_edge(tmp));
    }

    //remove all in-edge
    el = VERTEX_in_list(vex);
    while (el != NULL) {
        EdgeC * tmp = el;
        el = EC_next(el);
        removeEdge(EC_edge(tmp));
    }
    vex = m_vertices.removed(vex);
    m_v_free_list.add_free_elem(vex);
    if (m_dense_vertex != NULL) {
        m_dense_vertex->set(VERTEX_id(vex), NULL);
    }
    return vex;
}


//Return all neighbors of 'vid' on graph.
//Return false if 'vid' is not on graph.
//
//'ni_list': record the neighbours of 'vid'.
//    Note that this function ensure each neighbours in ni_list is unique.
bool Graph::getNeighborList(OUT List<UINT> & ni_list, UINT vid) const
{
    ASSERTN(m_ec_pool != NULL, ("not yet initialized."));

    //Ensure VertexHash::find is readonly.
    Graph * pthis = const_cast<Graph*>(this);
    Vertex * vex  = pthis->get_vertex(vid);
    if (vex == NULL) { return false; }

    EdgeC * el = VERTEX_in_list(vex);
    while (el != NULL) {
        UINT v = VERTEX_id(EDGE_from(EC_edge(el)));
        if (!ni_list.find(v)) {
            ni_list.append_tail(v);
        }
        el = EC_next(el);
    }

    el = VERTEX_out_list(vex);
    while (el != NULL) {
        UINT v = VERTEX_id(EDGE_to(EC_edge(el)));
        if (!ni_list.find(v)) {
            ni_list.append_tail(v);
        }
        el = EC_next(el);
    }
    return true;
}


//Return all neighbors of 'vid' on graph.
//Return false if 'vid' is not on graph.
//'niset': record the neighbours of 'vid'.
//    Note that this function ensure each neighbours in niset is unique.
//    Using sparse bitset is faster than list in most cases.
bool Graph::get_neighbor_set(OUT DefSBitSet & niset, UINT vid) const
{
    ASSERTN(m_ec_pool != NULL, ("not yet initialized."));
    //Ensure VertexHash::find is readonly.
    Graph * pthis = const_cast<Graph*>(this);
    Vertex * vex  = pthis->get_vertex(vid);
    if (vex == NULL) { return false; }

    EdgeC * el = VERTEX_in_list(vex);
    while (el != NULL) {
        UINT v = VERTEX_id(EDGE_from(EC_edge(el)));
        niset.bunion(v);
        el = EC_next(el);
    }

    el = VERTEX_out_list(vex);
    while (el != NULL) {
        UINT v = VERTEX_id(EDGE_to(EC_edge(el)));
        niset.bunion(v);
        el = EC_next(el);
    }
    return true;
}


UINT Graph::get_degree(Vertex const* vex) const
{
    ASSERTN(m_ec_pool != NULL, ("not yet initialized."));
    if (vex == NULL) return 0;
    return get_in_degree(vex) + get_out_degree(vex);
}


UINT Graph::get_in_degree(Vertex const* vex) const
{
    ASSERTN(m_ec_pool != NULL, ("not yet initialized."));
    if (vex == NULL) { return 0; }
    UINT degree = 0;
    EdgeC * el = VERTEX_in_list(vex);
    while (el != NULL) {
        degree++;
        el = EC_next(el);
    }
    return degree;
}


UINT Graph::get_out_degree(Vertex const* vex) const
{
    ASSERTN(m_ec_pool != NULL, ("not yet initialized."));
    if (vex == NULL) { return 0; }

    UINT degree = 0;
    EdgeC * el = VERTEX_out_list(vex);
    while (el != NULL) {
        degree++;
        el = EC_next(el);
    }
    return degree;
}


Edge * Graph::get_edge(Vertex const* from, Vertex const* to) const
{
    ASSERTN(m_ec_pool != NULL, ("not yet initialized."));
    if (from == NULL || to == NULL) { return NULL; }

    EdgeC * el = VERTEX_out_list(from);
    while (el != NULL) {
        Edge * e = EC_edge(el);
        if (EDGE_from(e) == from && EDGE_to(e) == to) {
            return e;
        }
        if (!m_is_direction &&
            (EDGE_from(e) == to && EDGE_to(e) == from)) {
            return e;
        }
        el = EC_next(el);
    }

    if (!m_is_direction) {
        EdgeC * el2 = VERTEX_out_list(to);
        while (el2 != NULL) {
            Edge * e = EC_edge(el2);
            if (EDGE_from(e) == to && EDGE_to(e) == from) {
                return e;
            }
            el2 = EC_next(el2);
        }
    }
    return NULL;
}


Edge * Graph::get_edge(UINT from, UINT to) const
{
    ASSERTN(m_ec_pool != NULL, ("not yet initialized."));
    Vertex * fp = get_vertex(from);
    Vertex * tp = get_vertex(to);
    return get_edge(fp, tp);
}


bool Graph::is_equal(Graph & g) const
{
    if (get_vertex_num() != g.get_vertex_num() ||
        get_edge_num() != g.get_edge_num()) {
        return false;
    }

    BitSet vs;
    INT c;
    for (Vertex * v1 = get_first_vertex(c);
         v1 != NULL; v1 = get_next_vertex(c)) {
        Vertex * v2 = g.get_vertex(VERTEX_id(v1));
        if (v2 == NULL) {
            return false;
        }

        vs.clean();
        EdgeC * el = VERTEX_out_list(v1);
        Edge * e = NULL;
        UINT v1_succ_n = 0;
        if (el == NULL) {
            if (VERTEX_out_list(v2) != NULL) {
                return false;
            }
            continue;
        }

        for (e = EC_edge(el); e != NULL; el = EC_next(el),
             e = el ? EC_edge(el) : NULL) {
            vs.bunion(VERTEX_id(EDGE_to(e)));
            v1_succ_n++;
        }

        UINT v2_succ_n = 0;
        el = VERTEX_out_list(v2);
        for (e = EC_edge(el); e != NULL; el = EC_next(el),
             e = el ? EC_edge(el) : NULL) {
            v2_succ_n++;
            if (!vs.is_contain(VERTEX_id(EDGE_to(e)))) {
                return false;
            }
        }

        if (v1_succ_n != v2_succ_n) {
            return false;
        }
    }
    return true;
}


//Is there exist a path connect 'from' and 'to'.
bool Graph::is_reachable(Vertex * from, Vertex * to) const
{
    ASSERTN(m_ec_pool != NULL, ("not yet initialized."));
    ASSERTN(from != NULL && to != NULL, ("parameters cannot be NULL"));
    EdgeC * el = VERTEX_out_list(from);
    Edge * e = NULL;
    if (el == NULL) { return false; }

    //Walk through each succ of 'from'
    for (e = EC_edge(el); e != NULL; el = EC_next(el),
         e = el ? EC_edge(el):NULL) {
        Vertex * succ = EDGE_to(e);
        if (VERTEX_id(succ) == VERTEX_id(to)) {
            return true;
        } else {
            if (is_reachable(succ, to)) {
                return true;
            }
        }
    } //end for
    return false;
}


//Sort graph vertices in topological order.
//'vex_vec': record nodes with topological sort.
//NOTE: current graph will be empty at function return.
//    If one need to keep the graph unchanged, clone graph
//    as a tmpgraph and operate on the tmpgraph.
//    e.g: Graph org;
//        And org must be unchanged,
//        Graph tmp(org);
//        tmp.sortInToplogOrder(...)
bool Graph::sortInToplogOrder(OUT Vector<UINT> & vex_vec, bool is_topdown)
{
    ASSERTN(m_ec_pool != NULL, ("Graph still not yet initialize."));
    if (get_vertex_num() == 0) {
        return true;
    }
    List<Vertex*> vlst;
    UINT pos = 0;
    vex_vec.clean();
    vex_vec.grow(get_vertex_num());
    while (this->get_vertex_num() != 0) {
        vlst.clean();
        Vertex * v;
        INT c;
        for (v = this->get_first_vertex(c);
             v != NULL; v = this->get_next_vertex(c)) {
            if (is_topdown) {
                if (VERTEX_in_list(v) == NULL) {
                    vlst.append_tail(v);
                }
            } else if (VERTEX_out_list(v) == NULL) {
                vlst.append_tail(v);
            }
        }
        if (vlst.get_elem_count() == 0 && this->get_vertex_num() != 0) {
            ASSERTN(0, ("exist cycle in graph"));
            return false;
        }
        for (v = vlst.get_head(); v != NULL; v = vlst.get_next()) {
            vex_vec.set(pos, VERTEX_id(v));
            pos++;
            this->removeVertex(v);
        }
    }
    return true;
}


//Remove all edges between v1 and v2.
void Graph::removeEdgeBetween(Vertex * v1, Vertex * v2)
{
    EdgeC * ec = VERTEX_out_list(v1);
    while (ec != NULL) {
        EdgeC * next = EC_next(ec);
        Edge * e = EC_edge(ec);
        if ((EDGE_from(e) == v1 && EDGE_to(e) == v2) ||
            (EDGE_from(e) == v2 && EDGE_to(e) == v1)) {
            removeEdge(e);
        }
        ec = next;
    }

    ec = VERTEX_in_list(v1);
    while (ec != NULL) {
        EdgeC * next = EC_next(ec);
        Edge * e = EC_edge(ec);
        if ((EDGE_from(e) == v1 && EDGE_to(e) == v2) ||
            (EDGE_from(e) == v2 && EDGE_to(e) == v1)) {
            removeEdge(e);
        }
        ec = next;
    }
}


//Remove transitive edge.
//e.g: Given edges of G, there are v1->v2->v3, v1->v3, then v1->v3 named
//transitive edge.
//
//Algo:
//    INPUT: Graph with N edges.
//    1. Sort vertices in topological order.
//    2. Associate each edges with indicator respective,
//       and recording them in one matrix(N*N)
//        e.g: e1:v0->v2, e2:v1->v2, e3:v0->v1
//              0   1    2
//            0 --  e3   e1
//            1 --  --   e2
//            2 --  --   --
//
//    3. Scan vertices according to toplogical order,
//       remove all edges which the target-node has been
//       marked at else rows.
//        e.g: There are dependence edges: v0->v1, v0->v2.
//         If v1->v2 has been marked, we said v0->v2 is removable,
//         and the same goes for the rest of edges.
void Graph::removeTransitiveEdge()
{
    BitSetMgr bs_mgr;
    Vector<UINT> vex_vec;
    sortInToplogOrder(vex_vec, true);
    Vector<UINT> vid2pos_in_bitset_map; //Map from Vertex-ID to BitSet.

    //Mapping vertex id to its position in 'vex_vec'.
    for (INT i = 0; i <= vex_vec.get_last_idx(); i++) {
        vid2pos_in_bitset_map.set(vex_vec.get((UINT)i), (UINT)i);
    }

    //Associate each edges with indicator respective.
    Vector<BitSet*> edge_indicator; //container of bitset.
    INT c;
    for (Edge * e = m_edges.get_first(c); e != NULL; e = m_edges.get_next(c)) {
        UINT from = VERTEX_id(EDGE_from(e));
        UINT to = VERTEX_id(EDGE_to(e));

        UINT frompos = vid2pos_in_bitset_map.get(from);
        BitSet * bs = edge_indicator.get(frompos);
        if (bs == NULL) {
            bs = bs_mgr.create();
            edge_indicator.set(frompos, bs);
        }

        //Each from-vertex is associated with
        //a bitset to record all to-vertices.
        bs->bunion(vid2pos_in_bitset_map.get(to));
    } //end for each of edge

    //Scanning vertices in topological order.
    for (INT i = 0; i < vex_vec.get_last_idx(); i++) {
        //Get the successor vector.
        BitSet * bs = edge_indicator.get((UINT)i);
        if (bs != NULL && bs->get_elem_count() >= 2) {
            //Do NOT remove the first edge. Position in bitset
            //has been sorted in topological order.
            for (INT pos_i = bs->get_first(); pos_i >= 0;
                 pos_i = bs->get_next((UINT)pos_i)) {
                UINT kid_from_vid = vex_vec.get((UINT)pos_i);
                UINT kid_from_pos = vid2pos_in_bitset_map.get(kid_from_vid);

                //Get bitset that 'pos_i' associated.
                BitSet * kid_from_bs = edge_indicator.get(kid_from_pos);
                if (kid_from_bs != NULL) {
                    for (INT pos_j = bs->get_next((UINT)pos_i); pos_j >= 0;
                         pos_j = bs->get_next((UINT)pos_j)) {
                        if (kid_from_bs->is_contain((UINT)pos_j)) {
                            //The edge 'i->pos_j' is redundant.
                            UINT to_vid = vex_vec.get((UINT)pos_j);
                            UINT src_vid = vex_vec.get((UINT)i);
                            removeEdge(get_edge(src_vid, to_vid));
                            bs->diff((UINT)pos_j);
                        }
                    }
                } //end if
            } //end for
        } //end if
    } //end for each vertex
}


void Graph::dump_dot(CHAR const* name) const
{
    if (name == NULL) {
        name = "graph.dot";
    }
    UNLINK(name);
    FILE * h = fopen(name, "a+");
    ASSERTN(h, ("%s create failed!!!", name));

    fprintf(h, "digraph G {\n");
    //Print node
    INT c;
    for (Vertex const* v = m_vertices.get_first(c);
         v != NULL; v = m_vertices.get_next(c)) {
        fprintf(h, "\nnode%d [shape = Mrecord, label=\"{V%d}\"];",
                VERTEX_id(v), VERTEX_id(v));
    }

    //Print edge
    for (Edge const* e = m_edges.get_first(c);
         e != NULL; e = m_edges.get_next(c)) {
        fprintf(h, "\nnode%d->node%d[label=\"%s\"]",
                    VERTEX_id(EDGE_from(e)),
                    VERTEX_id(EDGE_to(e)),
                    "");
    }
    fprintf(h, "\n}\n");
    fclose(h);
}


void Graph::dump_vcg(CHAR const* name) const
{
    ASSERTN(m_ec_pool != NULL, ("not yet initialized."));
    if (name == NULL) {
        name = "graph.vcg";
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

    //Print node
    INT c;
    for (Vertex const* v = m_vertices.get_first(c);
         v != NULL; v = m_vertices.get_next(c)) {
        fprintf(h, "\nnode: { title:\"%d\" label:\"%d\" "
                "shape:circle fontname:\"courB\" color:gold}",
                VERTEX_id(v), VERTEX_id(v));
    }

    //Print edge
    for (Edge const* e = m_edges.get_first(c);
         e != NULL; e = m_edges.get_next(c)) {
        fprintf(h, "\nedge: { sourcename:\"%d\" targetname:\"%d\" %s}",
                VERTEX_id(EDGE_from(e)),
                VERTEX_id(EDGE_to(e)),
                m_is_direction ? "" : "arrowstyle:none" );
    }
    fprintf(h, "\n}\n");
    fclose(h);
}


//
//START DGraph
//
DGraph::DGraph(UINT edge_hash_size, UINT vex_hash_size) :
    Graph(edge_hash_size, vex_hash_size)
{
    m_bs_mgr = NULL;
}


DGraph::DGraph(DGraph const& g) : Graph(g)
{
    m_bs_mgr = g.m_bs_mgr;
    if (m_bs_mgr != NULL) {
        cloneDomAndPdom(g);
    }
}


bool DGraph::cloneDomAndPdom(DGraph const& src)
{
    ASSERT0(m_bs_mgr != NULL);
    INT c;
    for (Vertex * srcv = src.m_vertices.get_first(c);
         srcv != NULL; srcv = src.m_vertices.get_next(c)) {
        UINT src_vid = VERTEX_id(srcv);
        Vertex * tgtv = get_vertex(src_vid);
        ASSERT0(tgtv != NULL);

        BitSet const* set = src.read_dom_set(VERTEX_id(srcv));
        if (set != NULL) {
            get_dom_set(tgtv)->copy(*set);
        }

        set = src.read_pdom_set(VERTEX_id(srcv));
        if (set != NULL) {
            get_pdom_set(tgtv)->copy(*set);
        }
    } //end for each vertices
    m_idom_set.copy(src.m_idom_set);
    m_ipdom_set.copy(src.m_ipdom_set);
    return true;
}


size_t DGraph::count_mem() const
{
    UINT count = 0;
    count += m_dom_set.count_mem();
    count += m_pdom_set.count_mem(); //record post-dominator-set of each vertex.
    count += m_idom_set.count_mem(); //immediate dominator.
    count += m_ipdom_set.count_mem(); //immediate post dominator.
    count += sizeof(m_bs_mgr); //Do NOT count up the bitset in BS_MGR.
    return count;
}


//Vertices should have been sorted in topological order.
//And we access them by reverse-topological order.
//'vlst': compute dominator for vertices in vlst if it
//    is not empty or else compute all graph.
//'uni': universe.
bool DGraph::computeDom(List<Vertex const*> const* vlst, BitSet const* uni)
{
    List<Vertex const*> tmpvlst;
    List<Vertex const*> * pvlst = &tmpvlst;
    if (vlst != NULL) {
        //Here one must guarantee pvlst would not be modified.
        pvlst = const_cast<List<Vertex const*>*>(vlst);
    } else {
        INT c;
        for (Vertex const* u = get_first_vertex(c);
             u != NULL; u = get_next_vertex(c)) {
            pvlst->append_tail(u);
        }
    }

    BitSet const* luni = NULL;
    if (uni != NULL) {
        luni = uni;
    } else {
        BitSet * x = new BitSet();
        C<Vertex const*> * ct = NULL;
        for (pvlst->get_head(&ct);
             ct != pvlst->end(); ct = pvlst->get_next(ct)) {
            Vertex const* u = ct->val();
            ASSERT0(u);
            x->bunion(VERTEX_id(u));
        }
        luni = x;
    }

    //Initialize dom-set for each BB.
    C<Vertex const*> * ct;
    for (pvlst->get_head(&ct); ct != pvlst->end(); ct = pvlst->get_next(ct)) {
        Vertex const* v = ct->val();
        ASSERT0(v);
        if (is_graph_entry(v)) {
            BitSet * dom = get_dom_set(v);
            dom->clean();
            dom->bunion(VERTEX_id(v));
        } else {
            get_dom_set(v)->copy(*luni);
        }
    }

    //DOM[entry] = {entry}
    //DOM[n] = {n} กศ { กษ(DOM[pred] of predecessor of 'n') }
    bool change = true;
    BitSet tmp;
    UINT count = 0;
    while (change && count < 10) {
        count++;
        change = false;
        C<Vertex const*> * ct2;
        for (pvlst->get_head(&ct2);
             ct2 != pvlst->end(); ct2 = pvlst->get_next(ct2)) {
            Vertex const* v = ct2->val();
            ASSERT0(v);
            UINT vid = VERTEX_id(v);
            if (is_graph_entry(v)) {
                continue;
            } else {
                //Access each preds
                EdgeC * ec = VERTEX_in_list(v);
                while (ec != NULL) {
                    Vertex * pred = EDGE_from(EC_edge(ec));
                    if (ec == VERTEX_in_list(v)) {
                        tmp.copy(*m_dom_set.get(VERTEX_id(pred)));
                    } else {
                        tmp.intersect(*m_dom_set.get(VERTEX_id(pred)));
                    }
                    ec = EC_next(ec);
                }
                tmp.bunion(vid);

                BitSet * dom = m_dom_set.get(VERTEX_id(v));
                if (!dom->is_equal(tmp)) {
                    dom->copy(tmp);
                    change = true;
                }
            } //end else
        } //end for
    }//end while
    ASSERT0(!change);

    if (uni == NULL && luni != NULL) {
        delete luni;
    }
    return true;
}


//Vertices should have been sorted in topological order.
//And we access them by reverse-topological order.
//'vlst': compute dominator for vertices in vlst if it
//    is not empty or else compute all graph.
//'uni': universe.
bool DGraph::computeDom3(List<Vertex const*> const* vlst, BitSet const* uni)
{
    DUMMYUSE(uni);
    List<Vertex const*> tmpvlst;
    List<Vertex const*> * pvlst = &tmpvlst;
    if (vlst != NULL) {
        //Here one must guarantee pvlst would not be modified.
        pvlst = const_cast<List<Vertex const*>*>(vlst);
    } else {
        INT c;
        for (Vertex const* u = get_first_vertex(c);
             u != NULL; u = get_next_vertex(c)) {
            pvlst->append_tail(u);
        }
    }

    //Initialize dom-set for each BB.
    C<Vertex const*> * ct;
    for (pvlst->get_head(&ct); ct != pvlst->end(); ct = pvlst->get_next(ct)) {
        Vertex const* v = ct->val();
        ASSERT0(v);
        if (is_graph_entry(v)) {
            BitSet * dom = get_dom_set(v);
            dom->clean();
            dom->bunion(VERTEX_id(v));
        } else {
            get_dom_set(v)->clean();
        }
    }

    //DOM[entry] = {entry}
    //DOM[n] = {n} กศ { กษ(DOM[pred] of predecessor of 'n') }
    bool change = true;
    BitSet tmp;
    UINT count = 0;
    while (change && count < 10) {
        count++;
        change = false;
        C<Vertex const*> * ct2;
        for (pvlst->get_head(&ct2);
             ct2 != pvlst->end(); ct2 = pvlst->get_next(ct2)) {
            Vertex const* v = ct2->val();
            ASSERT0(v);
            UINT vid = VERTEX_id(v);
            if (is_graph_entry(v)) {
                continue;
            } else {
                //Access each preds
                EdgeC * ec = VERTEX_in_list(v);
                UINT meet = 0;
                while (ec != NULL) {
                    Vertex * pred = EDGE_from(EC_edge(ec));
                    BitSet * ds = m_dom_set.get(VERTEX_id(pred));
                    if (ds->is_empty()) {
                        ec = EC_next(ec);
                        continue;
                    }

                    if (meet == 0) {
                        tmp.copy(*ds);
                    } else {
                        tmp.intersect(*ds);
                    }
                    meet++;
                    ec = EC_next(ec);
                }

                if (meet == 0) { tmp.clean(); }
                tmp.bunion(vid);

                BitSet * dom = m_dom_set.get(VERTEX_id(v));
                if (!dom->is_equal(tmp)) {
                    dom->copy(tmp);
                    change = true;
                }
            } //end else
        } //end for
    } //end while
    ASSERT0(!change);
    return true;
}


//Compute post-dominator according to rpo.
//root: root node of graph.
//uni: universe.
//Note you should use this function carefully, it may be expensive, because that
//the function does not check if RPO is available, namely, it will always
//compute the RPO.
bool DGraph::computePdomByRpo(Vertex * root, BitSet const* uni)
{
    List<Vertex const*> vlst;
    computeRpoNoRecursive(root, vlst);
    vlst.reverse();

    bool res = false;
    if (uni == NULL) {
        res = computePdom(&vlst);
    } else {
        res = computePdom(&vlst, uni);
    }
    CHECK_DUMMYUSE(res);
    return true;
}


//Vertices should have been sorted in topological order.
//We access them by reverse-topological order.
bool DGraph::computePdom(List<Vertex const*> const* vlst)
{
    ASSERT0(vlst);
    BitSet uni;
    C<Vertex const*> * ct;
    for (vlst->get_head(&ct); ct != vlst->end(); ct = vlst->get_next(ct)) {
        Vertex const* u = ct->val();
        ASSERT0(u);
        uni.bunion(VERTEX_id(u));
    }
    return computePdom(vlst, &uni);
}


//Vertices should have been sorted in topological order.
//And we access them by reverse-topological order.
//vlst: vertex list.
//uni: universe.
bool DGraph::computePdom(List<Vertex const*> const* vlst, BitSet const* uni)
{
    ASSERT0(vlst && uni);

    //Initialize pdom for each bb
    C<Vertex const*> * ct;
    for (vlst->get_head(&ct); ct != vlst->end(); ct = vlst->get_next(ct)) {
        Vertex const* v = ct->val();
        ASSERT0(v);
        if (is_graph_exit(v)) {
            BitSet * pdom = get_pdom_set(v);
            pdom->clean();
            pdom->bunion(VERTEX_id(v));
        } else {
            get_pdom_set(v)->copy(*uni);
        }
    }

    //PDOM[exit] = {exit}
    //PDOM[n] = {n} U {กษ(PDOM[succ] of each succ of n)}
    bool change = true;
    BitSet tmp;
    UINT count = 0;
    while (change && count < 10) {
        count++;
        change = false;
        C<Vertex const*> * ct2;
        for (vlst->get_head(&ct2);
             ct2 != vlst->end(); ct2 = vlst->get_next(ct2)) {
            Vertex const* v = ct2->val();
            ASSERT0(v);
            UINT vid = VERTEX_id(v);
            if (is_graph_exit(v)) {
                continue;
            } else {
                tmp.clean();
                //Access each succs
                EdgeC * ec = VERTEX_out_list(v);
                while (ec != NULL) {
                    Vertex * succ = EDGE_to(EC_edge(ec));
                    if (ec == VERTEX_out_list(v)) {
                        tmp.copy(*m_pdom_set.get(VERTEX_id(succ)));
                    } else {
                        tmp.intersect(*m_pdom_set.get(VERTEX_id(succ)));
                    }
                    ec = EC_next(ec);
                }
                tmp.bunion(vid);

                BitSet * pdom = m_pdom_set.get(VERTEX_id(v));
                if (!pdom->is_equal(tmp)) {
                    pdom->copy(tmp);
                    change = true;
                }
            }
        } //end for
    } // end while

    ASSERT0(!change);
    return true;
}


//This function need idom to be avaiable.
//NOTE: set does NOT include node itself.
bool DGraph::computeDom2(List<Vertex const*> const& vlst)
{
    C<Vertex const*> * ct;
    BitSet avail;
    for (vlst.get_head(&ct); ct != vlst.end(); ct = vlst.get_next(ct)) {
        Vertex const* v = ct->val();
        ASSERT0(v);
        BitSet * doms = get_dom_set(VERTEX_id(v));
        doms->clean();
        ASSERT0(doms);
        for (UINT idom = get_idom(VERTEX_id(v));
             idom != 0; idom = get_idom(idom)) {
            if (avail.is_contain(idom)) {
                BitSet const* idom_doms = get_dom_set(idom);
                doms->copy(*idom_doms);
                doms->bunion(idom);
                break;
            }
            doms->bunion(idom);
        }
        avail.bunion(VERTEX_id(v));
    }
    return true;
}


//Vertices should have been sorted in rpo.
//'vlst': a list of vertex which sort in rpo order.
//
//NOTE:
//    1. The root node has better to be the first one in 'vlst'.
//    2. Do not use '0' as vertex id, it is used as Undefined.
//    3. Entry does not have idom.
bool DGraph::computeIdom2(List<Vertex const*> const& vlst)
{
    bool change = true;

    //Initialize idom-set for each BB.
    m_idom_set.clean();
    UINT nentry = 0;
    while (change) {
        change = false;
        //Access with topological order.
        C<Vertex const*> * ct;
        for (vlst.get_head(&ct); ct != vlst.end(); ct = vlst.get_next(ct)) {
            Vertex const* v = ct->val();
            ASSERT0(v);
            if (is_graph_entry(v)) {
                m_idom_set.set(VERTEX_id(v), (INT)VERTEX_id(v));
                nentry++;
                continue;
            }

            //Access each preds
            EdgeC const* ec = VERTEX_in_list(v);
            Vertex const* idom = NULL;
            while (ec != NULL) {
                Vertex const* pred = EDGE_from(EC_edge(ec));
                UINT pid = VERTEX_id(pred);

                if (m_idom_set.get(pid) == 0) {
                    ec = EC_next(ec);
                    continue;
                }

                if (idom == NULL) {
                    idom = pred;
                    ec = EC_next(ec);
                    continue;
                }

                Vertex const* j = pred;
                Vertex const* k = idom;
                while (j != k) {
                    while (VERTEX_rpo(j) > VERTEX_rpo(k)) {
                        j = get_vertex((UINT)m_idom_set.get(VERTEX_id(j)));
                        ASSERT0(j);
                        if (is_graph_entry(j)) {
                            break;
                        }
                    }
                    while (VERTEX_rpo(j) < VERTEX_rpo(k)) {
                        k = get_vertex((UINT)m_idom_set.get(VERTEX_id(k)));
                        ASSERT0(k);
                        if (is_graph_entry(k)) {
                            break;
                        }
                    }
                    if (is_graph_entry(j) && is_graph_entry(k)) {
                        break;
                    }
                 }

                if (j != k) {
                    //Multi entries.
                    ASSERT0(is_graph_entry(j) && is_graph_entry(k));
                    idom = NULL;
                    break;
                }

                idom = j;
                ec = EC_next(ec);
            }

            if (idom == NULL) {
                if (m_idom_set.get(VERTEX_id(v)) != 0) {
                    m_idom_set.set(VERTEX_id(v), 0);
                    change = true;
                }
            } else if ((UINT)m_idom_set.get(VERTEX_id(v)) != VERTEX_id(idom)) {
                m_idom_set.set(VERTEX_id(v), (INT)VERTEX_id(idom));
                change = true;
            }
        } //end for
    }

    C<Vertex const*> * ct;
    for (vlst.get_head(&ct); ct != vlst.end(); ct = vlst.get_next(ct)) {
        Vertex const* v = ct->val();
        ASSERT0(v);
        if (is_graph_entry(v)) {
            m_idom_set.set(VERTEX_id(v), 0);
            nentry--;
            if (nentry == 0) { break; }
        }
    }
    return true;
}


//NOTE: Entry does not have idom.
bool DGraph::computeIdom()
{
    //Initialize idom-set for each BB.
    m_idom_set.clean();

    //Access with topological order.
    INT c;
    for (Vertex * v = m_vertices.get_first(c);
         v != NULL; v = m_vertices.get_next(c)) {
        UINT cur_id = VERTEX_id(v);
        if (is_graph_entry(v)) {
            continue;
        } else if (m_dom_set.get(cur_id)->get_elem_count() >= 2) {
            BitSet * p = m_dom_set.get(cur_id);
            ASSERTN(p != NULL, ("should compute dom first"));
            if (p->get_elem_count() == 1) {
                //There is no idom if 'dom' set only contain itself.
                ASSERT0(m_idom_set.get(cur_id) == 0);
                continue;
            }
            p->diff(cur_id);

            #ifdef MAGIC_METHOD
            INT i;
            for (i = p->get_first(); i != -1; i = p->get_next((UINT)i)) {
                if (m_dom_set.get((UINT)i)->is_equal(*p)) {
                    ASSERT0(m_idom_set.get(cur_id) == 0);
                    m_idom_set.set(cur_id, i);
                    break;
                }
            }
            ASSERTN(i != -1, ("not find idom?"));
            #else
            INT i;
            for (i = tmp.get_first(); i != -1; i = tmp.get_next(i)) {
                for (INT j = tmp.get_first(); j != -1; j = tmp.get_next(j)) {
                    if (i == j) {
                        continue;
                    }
                    if (m_dom_set.get(j)->is_contain(i)) {
                        tmp.diff(i);
                        //Search 'tmp' over again.
                        i = tmp.get_first();
                        j = i;
                    }
                }
            }
            i = tmp.get_first();
            ASSERTN(i != -1, ("cannot find idom of BB:%d", cur_id));
            ASSERTN(m_idom_set.get(cur_id) == 0, ("recompute idom for BB:%d", cur_id));
            m_idom_set.set(cur_id, i);
            #endif
            p->bunion(cur_id);
        } //end else if
    } //end for
    return true;
}


//NOTE: Exit does not have idom.
bool DGraph::computeIpdom()
{
    //Initialize ipdom-set for each BB.
    m_ipdom_set.clean();

    //Processing in reverse-topological order.
    INT c;
    for (Vertex const* v = m_vertices.get_last(c);
         v != NULL; v = m_vertices.get_prev(c)) {
        UINT cur_id = VERTEX_id(v);
        if (is_graph_exit(v)) {
            continue;
        } else if (m_pdom_set.get(cur_id)->get_elem_count() > 1) {
            BitSet * p = m_pdom_set.get(cur_id);
            ASSERTN(p != NULL, ("should compute pdom first"));
            if (p->get_elem_count() == 1) {
                //There is no ipdom if 'pdom' set only contain itself.
                ASSERT0(m_ipdom_set.get(cur_id) == 0);
                continue;
            }

            p->diff(cur_id);
            INT i;
            for (i = p->get_first(); i != -1; i = p->get_next((UINT)i)) {
                if (m_pdom_set.get((UINT)i)->is_equal(*p)) {
                    ASSERT0(m_ipdom_set.get(cur_id) == 0);
                    m_ipdom_set.set(cur_id, i);
                    break;
                }
            }
            //ASSERTN(i != -1, ("not find ipdom")); //Not find.
            p->bunion(cur_id);
        } //end else if
    } //end for
    return true;
}


//'dom': output dominator tree.
void DGraph::get_dom_tree(OUT Graph & dom)
{
    INT c;
    for (Vertex * v = m_vertices.get_first(c);
         v != NULL; v = m_vertices.get_next(c)) {
        UINT vid = VERTEX_id(v);
        dom.addVertex(vid);
        if (m_idom_set.get(vid) != 0) {
            dom.addEdge((UINT)m_idom_set.get(vid), vid);
        }
    }
}


//'pdom': output post-dominator tree.
void DGraph::get_pdom_tree(OUT Graph & pdom)
{
    INT c;
    for (Vertex * v = m_vertices.get_first(c);
         v != NULL; v = m_vertices.get_next(c)) {
        UINT vid = VERTEX_id(v);
        pdom.addVertex(vid);
        if (m_ipdom_set.get(vid) != 0) { //id of bb starting at 1.
            pdom.addEdge((UINT)m_ipdom_set.get(vid), vid);
        }
    }
}


//Dump dom set, pdom set, idom, ipdom.
//'dump_dom_tree': set to be true to dump dominate
//  tree, and post dominate Tree.
void DGraph::dump_dom(FILE * h, bool dump_dom_tree)
{
    if (h == NULL) { return; }
    fprintf(h, "\n==---- DUMP DOM/PDOM/IDOM/IPDOM ----==");
    INT c;
    for (Vertex * v = m_vertices.get_first(c);
         v != NULL; v = m_vertices.get_next(c)) {
        UINT vid = VERTEX_id(v);
        fprintf(h, "\nVERTEX(%d) dom: ", vid);

        BitSet * bs;
        if ((bs = m_dom_set.get(vid)) != NULL) {
            for (INT id = bs->get_first();
                 id != -1 ; id = bs->get_next((UINT)id)) {
                if ((UINT)id != vid) {
                    fprintf(h, "%d ", id);
                }
            }
        }

        fprintf(h, "\n     pdom: ");

        if ((bs = m_pdom_set.get(vid)) != NULL) {
            for (INT id = bs->get_first();
                 id != -1; id = bs->get_next((UINT)id)) {
                if ((UINT)id != vid) {
                    fprintf(h, "%d ", id);
                }
            }
        }

        if (m_idom_set.get(vid) != 0) {
            fprintf(h, "\n     idom: %d", m_idom_set.get(vid));
        } else {
            fprintf(h, "\n");
        }

        if (m_ipdom_set.get(vid) != 0) {
            fprintf(h, "\n     ipdom: %d", m_ipdom_set.get(vid));
        } else {
            fprintf(h, "\n");
        }
    } //end for each vertices

    fprintf(h, "\n");
    fflush(h);

    if (dump_dom_tree) {
        Graph dom;
        get_dom_tree(dom);
        dom.dump_vcg("graph_dom_tree.vcg");
        dom.erase();
        get_pdom_tree(dom);
        dom.dump_vcg("graph_pdom_tree.vcg");
    }
}


//Sort node in dominator-tree in preorder.
void DGraph::sortDomTreeInPreorder(IN Vertex * root, OUT List<Vertex*> & lst)
{
    ASSERT0(root);
    BitSet is_visited;
    is_visited.bunion(VERTEX_id(root));
    lst.append_tail(root);

    Vertex * v;
    Stack<Vertex*> stk;
    stk.push(root);
    while ((v = stk.pop()) != NULL) {
        if (!is_visited.is_contain(VERTEX_id(v))) {
            is_visited.bunion(VERTEX_id(v));
            stk.push(v);
            //The only place to process vertex.
            lst.append_tail(v);
        }

        //Visit children.
        EdgeC * el = VERTEX_out_list(v);
        Vertex * succ;
        while (el != NULL) {
            succ = EDGE_to(EC_edge(el));
            if (!is_visited.is_contain(VERTEX_id(succ))) {
                stk.push(v);
                stk.push(succ);
                break;
            }
            el = EC_next(el);
        }
    }
}


//Sort node on graph in bfs-order.
//'order_buf': record the bfs-order for each vertex.
//NOTE: BFS does NOT keep the sequence if you are going to
//access vertex in lexicographic order.
void DGraph::sortInBfsOrder(
        Vector<UINT> & order_buf,
        Vertex * root,
        BitSet & visit)
{
    List<Vertex*> worklst;
    worklst.append_tail(root);
    UINT order = 1;
    while (worklst.get_elem_count() > 0) {
        Vertex * sv = worklst.remove_head();
        order_buf.set(VERTEX_id(sv), order);
        order++;
        visit.bunion(VERTEX_id(sv));
        EdgeC * el = VERTEX_out_list(sv);
        while (el != NULL) {
            Vertex * to = EDGE_to(EC_edge(el));
            if (visit.is_contain(VERTEX_id(to))) {
                el = EC_next(el);
                continue;
            }
            worklst.append_tail(to);
            el = EC_next(el);
        }
    }
}


void DGraph::sortDomTreeInPostrder(IN Vertex * root, OUT List<Vertex*> & lst)
{
    ASSERT0(root);
    BitSet is_visited;

    //Find the leaf node.
    Vertex * v;
    Stack<Vertex*> stk;
    stk.push(root);
    while ((v = stk.pop()) != NULL) {
        //Visit children first.
        EdgeC * el = VERTEX_out_list(v);
        bool find = false; //find unvisited kid.
        Vertex * succ;
        while (el != NULL) {
            succ = EDGE_to(EC_edge(el));
            if (!is_visited.is_contain(VERTEX_id(succ))) {
                stk.push(v);
                stk.push(succ);
                find = true;
                break;
            }
            el = EC_next(el);
        }
        if (!find) {
            is_visited.bunion(VERTEX_id(v));
            //The only place to process vertex.
            lst.append_tail(v);
        }
    }
}


void DGraph::_removeUnreachNode(UINT id, BitSet & visited)
{
    visited.bunion(id);
    Vertex * vex = get_vertex(id);
    EdgeC * el = VERTEX_out_list(vex);
    while (el != NULL) {
        UINT succ = VERTEX_id(EDGE_to(EC_edge(el)));
        if (!visited.is_contain(succ)) {
            _removeUnreachNode(succ, visited);
        }
        el = EC_next(el);
    }
}


//Perform DFS to seek for unreachable node.
//Return true if some nodes removed.
bool DGraph::removeUnreachNode(UINT entry_id)
{
    if (get_vertex_num() == 0) return false;
    bool removed = false;
    BitSet visited;
    _removeUnreachNode(entry_id, visited);
    INT c;
    for (Vertex * v = get_first_vertex(c);
         v != NULL; v = get_next_vertex(c)) {
        if (!visited.is_contain(VERTEX_id(v))) {
            removeVertex(v);
            removed = true;
        }
    }
    return removed;
}
//END DGraph

} //namespace xcom
