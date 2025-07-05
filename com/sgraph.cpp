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
#include "xcominc.h"

namespace xcom {

//Expect for unique vertex in graphic depitction
#define ALWAYS_VERTEX_UNIQUE

//
//START VexTab
//
void VexTab::dump(FILE * h) const
{
    ASSERT0(h);
    fprintf(h, "VexTab:");
    VexTabIter it;
    bool first = true;
    for (VexIdx t = get_first(it);
         t != VERTEX_UNDEF; t = get_next(it)) {
        if (!first) { fprintf(h, ","); }
        first = false;
        fprintf(h, "V%u", t);
    }
    if (first) { fprintf(h, "--"); }
    fflush(h);
}
//END VexTab


//
//START Vertex
//
//Return the vertex that is the from-vertex of Nth in-edge.
//n: the index of vertex, start from 0.
Vertex * Vertex::getNthInVertex(UINT n) const
{
    UINT i = 0;
    EdgeC const* ec = getInList();
    for (; i < n && ec != nullptr; ec = ec->get_next(), i++) {}
    return ec != nullptr ? ec->getFrom() : nullptr;
}


Vertex * Vertex::getNextFromVex(VexIter * it) const
{
    ASSERT0(it && xcom::in_list(getInList(), *it));
    if (*it == nullptr) { return nullptr; }
    *it = (*it)->get_next();
    return (*it) != nullptr ? (*it)->getFrom() : nullptr;
}


Vertex * Vertex::getFirstToVex(OUT VexIter * it) const
{
    ASSERT0(it);
    *it = getOutList();
    return (*it) != nullptr ? (*it)->getTo() : nullptr;
}


Vertex * Vertex::getFirstFromVex(OUT VexIter * it) const
{
    ASSERT0(it);
    *it = getInList();
    return (*it) != nullptr ? (*it)->getFrom() : nullptr;
}


Vertex * Vertex::getNextToVex(OUT VexIter * it) const
{
    ASSERT0(it && xcom::in_list(getOutList(), *it));
    if (*it == nullptr) { return nullptr; }
    *it = (*it)->get_next();
    return (*it) != nullptr ? (*it)->getTo() : nullptr;
}


//Return the vertex that is the from-vertex of Nth out-edge.
//n: the index of vertex, start from 0.
Vertex * Vertex::getNthOutVertex(UINT n) const
{
    UINT i = 0;
    EdgeC const* ec = getOutList();
    for (; i < n && ec != nullptr; ec = ec->get_next(), i++) {}
    return ec != nullptr ? ec->getTo() : nullptr;
}
//END Vertex


//
//START CompareEdgeFunc
//
Edge * CompareEdgeFunc::createKey(Edge const* ref)
{
    ASSERT0(m_g);
    EdgeC * outec;
    return m_g->allocEdgeImpl(ref->from(), ref->to(), &m_inec, &outec);
}
//END CompareEdgeFunc


//DONT CALL Constructor directly.
Graph::Graph(UINT vex_hash_size) : m_edgetab(self())
{
    m_vex_hash_size = vex_hash_size;
    m_ec_pool = nullptr;
    m_dense_vertex = nullptr; //default vertex layout is sparse.
    m_sparse_vertex = nullptr;
    init();
}


Graph::Graph(Graph const& g) : m_edgetab(self())
{
    m_vex_hash_size = g.m_vex_hash_size;
    m_ec_pool = nullptr;
    m_dense_vertex = nullptr; //default vertex layout is sparse.
    m_sparse_vertex = nullptr;
    init();
    clone(g, true, true);
}


void Graph::init()
{
    if (m_ec_pool != nullptr) { return; }
    m_ec_pool = smpoolCreate(sizeof(EdgeC), MEM_CONST_SIZE);
    ASSERTN(m_ec_pool, ("create mem pool failed"));

    m_vertex_pool = smpoolCreate(sizeof(Vertex) * 4, MEM_CONST_SIZE);
    ASSERTN(m_vertex_pool, ("create mem pool failed"));

    m_edge_pool = smpoolCreate(sizeof(Edge) * 4, MEM_CONST_SIZE);
    ASSERTN(m_edge_pool, ("create mem pool failed"));

    //If m_edges already initialized, this call will do nothing.
    m_edgetab.init(this);

    if (is_dense()) {
        ASSERT0(m_dense_vertex == nullptr);
        ASSERTN(sizeof(VexIdx) <= sizeof(VecIdx), ("maybe out of boundary"));
        m_dense_vertex = new Vector<Vertex*>(m_vex_hash_size);
    } else {
        ASSERT0(m_sparse_vertex == nullptr);
        m_sparse_vertex = new VertexHash(m_vex_hash_size);
    }
    m_is_unique = true;
    m_is_direction = true;
    m_dense_vex_num = 0;
}


//Reconstruct vertex hash table, and edge hash table with new bucket size.
//vertex_hash_sz: new vertex table size to be resized.
//NOTE:
//  1. mem pool should have been initialized
//  2. Graph should be erased before resize.
void Graph::resize(UINT vertex_hash_sz)
{
    ASSERT0(m_ec_pool);
    if (is_dense()) {
        //Dense vertex vector does not need resize.
        return;
    }
    ASSERTN(getVertexNum() == 0, ("should erase graph before resize"));
    if (m_vex_hash_size == vertex_hash_sz) { return; }
    m_sparse_vertex->destroy();
    m_sparse_vertex->init(vertex_hash_sz);
    m_vex_hash_size = vertex_hash_sz;
}


size_t Graph::count_mem() const
{
    size_t count = sizeof(Graph);
    count -= sizeof(FreeList<Edge>); //has been counted in Graph.
    count -= sizeof(FreeList<EdgeC>); //has been counted in Graph.
    count -= sizeof(FreeList<Vertex>); //has been counted in Graph.
    count += m_edgetab.count_mem();
    count += m_e_free_list.count_mem();
    count += m_el_free_list.count_mem();
    count += m_v_free_list.count_mem();
    count += smpoolGetPoolSize(m_ec_pool);
    count += smpoolGetPoolSize(m_vertex_pool);
    count += smpoolGetPoolSize(m_edge_pool);
    if (is_dense()) {
        count += m_dense_vertex->count_mem();
    } else {
        count += m_sparse_vertex->count_mem();
    }
    return count;
}


void Graph::destroy()
{
    if (m_ec_pool == nullptr) { return; }
    m_edgetab.destroy();

    //Set if edge and vertex would not be redundantly.
    m_is_unique = false;
    m_is_direction = false; //Set if graph is direction.
    m_e_free_list.clean(); //edge free list
    m_el_free_list.clean(); //edge-list free list
    m_v_free_list.clean(); //vertex free list

    smpoolDelete(m_ec_pool);
    m_ec_pool = nullptr;

    smpoolDelete(m_vertex_pool);
    m_vertex_pool = nullptr;

    smpoolDelete(m_edge_pool);
    m_edge_pool = nullptr;

    if (is_dense()) {
        delete m_dense_vertex;
        m_dense_vertex = nullptr;
        m_dense_vex_num = 0;
    } else {
        delete m_sparse_vertex;
        m_sparse_vertex = nullptr;
    }
}


//Erasing graph, include all nodes and edges,
//except for EdgeInfo and VertexInfo.
void Graph::erase()
{
    ASSERTN(m_ec_pool != nullptr, ("Graph must be initialized before clone."));
    if (is_dense()) {
        m_dense_vertex->destroy();
        m_dense_vertex->init();
        m_dense_vex_num = 0;
    } else {
        m_sparse_vertex->destroy();
        //If m_sparse_vertex already initialized, this call will do nothing.
        m_sparse_vertex->init(m_vex_hash_size);
    }

    m_e_free_list.clean(); //edge free list
    m_el_free_list.clean(); //edge-list free list
    m_v_free_list.clean(); //vertex free list

    smpoolDelete(m_ec_pool);
    smpoolDelete(m_vertex_pool);
    smpoolDelete(m_edge_pool);
    m_ec_pool = smpoolCreate(sizeof(EdgeC), MEM_CONST_SIZE);
    ASSERTN(m_ec_pool, ("create mem pool failed"));
    m_vertex_pool = smpoolCreate(sizeof(Vertex) * 4, MEM_CONST_SIZE);
    ASSERTN(m_vertex_pool, ("create mem pool failed"));
    m_edge_pool = smpoolCreate(sizeof(Edge) * 4, MEM_CONST_SIZE);
    ASSERTN(m_edge_pool, ("create mem pool failed"));

    m_edgetab.destroy();
    m_edgetab.init(this);
}


void Graph::clone(Graph const& src, bool clone_edge_info, bool clone_vex_info)
{
    erase();
    m_is_unique = src.m_is_unique;
    m_is_direction = src.m_is_direction;
    set_dense(src.is_dense());

    //Clone vertices
    VertexIter itv = VERTEX_UNDEF;
    for (Vertex * srcv = src.get_first_vertex(itv);
         srcv != nullptr; srcv = src.get_next_vertex(itv)) {
        Vertex * v = addVertex(VERTEX_id(srcv));
        v->clone(srcv);

        //Calls inherited class method.
        //Vertex info of memory should allocated by inherited class method
        if (VERTEX_info(srcv) != nullptr && clone_vex_info) {
            VERTEX_info(v) = cloneVertexInfo(srcv);
        }
    }

    //Clone edges
    EdgeIter ite;
    for (Edge * srce = src.get_first_edge(ite);
         srce != nullptr; srce = src.get_next_edge(ite)) {
        Edge * e = addEdge(VERTEX_id(EDGE_from(srce)),
                           VERTEX_id(EDGE_to(srce)));

        //Calls inherited class method.
        //Edge info of memory should allocated by inherited class method
        if (EDGE_info(srce) != nullptr && clone_edge_info) {
            EDGE_info(e) = cloneEdgeInfo(srce);
        }
    }
}


//NOTE: Do NOT use 0 as vertex id.
Vertex * Graph::allocVertex(VexIdx vid)
{
    ASSERTN(m_vertex_pool, ("not yet initialized."));
    ASSERTN(vid != VERTEX_UNDEF, ("Use undefined vertex id"));
    Vertex * vex = m_v_free_list.get_free_elem();
    if (vex == nullptr) {
        vex = allocVertex();
    } else {
        vex->init();
    }
    VERTEX_id(vex) = vid;
    return vex;
}


//Replace orginal in-vertex with a list of new vertex.
//Return the position of 'from' that is in the in-vertex list of 'to'.
//newin: record a list of new vertex id.
//from: original in-vertex id
//to: original out-vertex id
VexIdx Graph::replaceInVertex(VexIdx from, VexIdx to, List<VexIdx> const& newin)
{
    Edge * e = getEdge(from, to);
    ASSERTN(e, ("not found given edge"));
    Vertex * tp = getVertex(to);
    ASSERT0(tp);
    UINT from_pos = 0;
    removeEdge(e, nullptr, &from_pos);
    addEdgeAtPos(newin, tp, from_pos);
    return from_pos;
}


//Add edge from->to, whereas 'from' is the nth predecessor of 'to'.
//pos: the position of 'from' in predecessors of 'to', start at 0.
//     For any new edge, the default position is the last of in/out list,
//     thus the pos should not greater than the number of predecessors + 1.
//     e.g: there are 2 predecessors of 'to', pos can not greater than 2.
void Graph::addEdgeAtPos(List<VexIdx> const& fromlist, Vertex * to, UINT pos)
{
    bool swap = true;
    List<VexIdx>::Iter it;
    EdgeC * marker = nullptr;
    for (fromlist.get_head(&it); it != nullptr; fromlist.get_next(&it)) {
        VexIdx from = it->val();
        ASSERT0(from != VERTEX_UNDEF);
        Edge * newedge = addEdge(addVertex(from), to);
        if (!m_edgetab.getCompareKeyObject()->isNewElem()) {
            continue;
        }
        ASSERTN_DUMMYUSE(m_edgetab.getCompareKeyObject()->getInEC()->
                         getEdge() == newedge, ("unmatch EdgeC"));
        if (marker == nullptr) {
            EdgeC * p = to->getInList();
            for (UINT i = 0; p != nullptr; i++, p = p->get_next()) {
                if (i == pos) {
                    marker = p;
                    break;
                }
            }
            if (marker->get_next() == nullptr) {
                //marker is the last position, namely all new element will be
                //append at the last of in-list.
                swap = false;
            }
        }

        //Note if marker is nullptr, the 'from' already be the source
        //vertex of 'to'.
        ASSERT0(marker);
        if (!swap) { continue; }

        //The insert position is in the middle of original source list.
        EdgeC * newedgec = m_edgetab.getCompareKeyObject()->getInEC();
        ASSERT0(newedgec);
        xcom::remove(&VERTEX_in_list(to), newedgec);
        xcom::insertbefore(&VERTEX_in_list(to), marker, newedgec);
    }
}


Edge * Graph::allocEdge(Vertex * from, Vertex * to)
{
    ASSERTN(m_ec_pool != nullptr, ("not yet initialized."));
    if (from == nullptr || to == nullptr) return nullptr;
    if (m_is_unique) {
        Edge placeholder;
        EDGE_from(&placeholder) = from;
        EDGE_to(&placeholder) = to;
        if (m_is_direction) {
            m_edgetab.getCompareKeyObject()->clean();
            return m_edgetab.append(&placeholder);
        }

        Edge * e = m_edgetab.get(&placeholder);
        if (e != nullptr) {
            return e;
        }

        //Both check from->to and to->from
        EDGE_from(&placeholder) = to;
        EDGE_to(&placeholder) = from;
        m_edgetab.getCompareKeyObject()->clean();
        return m_edgetab.append(&placeholder);
    }
    EdgeC * inec;
    EdgeC * outec;
    m_edgetab.getCompareKeyObject()->clean();
    return m_edgetab.append(allocEdgeImpl(from, to, &inec, &outec));
}


//Reverse edge direction
Edge * Graph::reverseEdge(Edge * e)
{
    ASSERTN(m_ec_pool != nullptr, ("not yet initialized."));
    ASSERTN(m_is_direction, ("graph is indirection"));
    EdgeInfoType einfo = EDGE_info(e);
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
    ASSERTN(m_ec_pool != nullptr, ("not yet initialized."));
    ASSERTN(m_is_direction, ("graph is indirection"));
    List<Edge*> list;
    Edge * e;
    EdgeIter c;
    for (e = get_first_edge(c); e != nullptr; e = get_next_edge(c)) {
        list.append_tail(e);
    }
    for (e = list.get_head(); e != nullptr; e = list.get_next()) {
        reverseEdge(e);
    }
}


//Insert 'newv' between 'v1' and 'v2'.
//e.g: given edge v1->v2, the result is v1->newv->v2.
//Return edge v1->newv, newv->v2.
void Graph::insertVertexBetween(
    IN Vertex * v1, IN Vertex * v2, IN Vertex * newv, OUT Edge ** e1,
    OUT Edge ** e2, bool sort)
{
    ASSERT0(v1 && v2 && newv);
    Edge * e = getEdge(v1, v2);
    EdgeC * v2pos_in_list = nullptr;
    EdgeC * v1pos_in_list = nullptr;
    if (sort) {
        for (EdgeC * ec = v1->getOutList();
             ec != nullptr; ec = ec->get_next()) {
            if (ec->getTo() == v2) {
                break;
            }
            v2pos_in_list = ec;
        }
        for (EdgeC * ec = v2->getInList();
             ec != nullptr; ec = ec->get_next()) {
            if (ec->getFrom() == v1) {
                break;
            }
            v1pos_in_list = ec;
        }
    }

    ASSERTN(e, ("no edge in bewteen %d and %d", v1->id(), v2->id()));
    removeEdge(e);
    Edge * tmpe1 = addEdge(v1, newv);
    Edge * tmpe2 = addEdge(newv, v2);
    if (e1 != nullptr) { *e1 = tmpe1; }
    if (e2 != nullptr) { *e2 = tmpe2; }
    if (!sort) { return; }

    EdgeC * tmpe1_ec = nullptr;
    for (EdgeC * ec = v1->getOutList(); ec != nullptr; ec = EC_next(ec)) {
        if (EC_edge(ec) == tmpe1) {
            tmpe1_ec = ec;
            break;
        }
    }
    ASSERT0(tmpe1_ec);

    EdgeC * tmpe2_ec = nullptr;
    for (EdgeC * ec = v2->getInList(); ec != nullptr; ec = EC_next(ec)) {
        if (EC_edge(ec) == tmpe2) {
            tmpe2_ec = ec;
            break;
        }
    }
    ASSERT0(tmpe2_ec);

    xcom::remove(&VERTEX_out_list(v1), tmpe1_ec);
    if (v2pos_in_list == nullptr) {
        xcom::append_head(&VERTEX_out_list(v1), tmpe1_ec);
    } else {
        xcom::insertafter_one(&v2pos_in_list, tmpe1_ec);
    }

    xcom::remove(&VERTEX_in_list(v2), tmpe2_ec);
    if (v1pos_in_list == nullptr) {
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
void Graph::insertVertexBetween(VexIdx v1, VexIdx v2, VexIdx newv,
                                OUT Edge ** e1, OUT Edge ** e2, bool sort)
{
    Vertex * pv1 = getVertex(v1);
    Vertex * pv2 = getVertex(v2);
    Vertex * pnewv = addVertex(newv);
    ASSERT0(pv1 && pv2 && pnewv);
    insertVertexBetween(pv1, pv2, pnewv, e1, e2, sort);
}


//pos_in_outlist: optional, record the position in outlist of 'from' of 'e'
//pos_in_inlist: optional, record the position in inlist of 'to' of 'e'
Edge * Graph::removeEdge(Edge * e, OUT UINT * pos_in_outlist,
                         OUT UINT * pos_in_inlist)
{
    ASSERTN(m_ec_pool != nullptr, ("not yet initialized."));
    if (e == nullptr) { return nullptr; }
    Vertex * from = EDGE_from(e);
    Vertex * to = EDGE_to(e);
    //remove out of out-list of 'from'
    EdgeC * el;
    UINT j = 0;
    for (el = from->getOutList(); el != nullptr; j++) {
        if (el->getEdge() == e) {
            if (pos_in_outlist != nullptr) {
                *pos_in_outlist = j;
            }
            break;
        }
        el = el->get_next();
    }
    ASSERTN(el, ("can not find out-edge, it is illegal graph"));
    xcom::remove(&VERTEX_out_list(from), el);
    m_el_free_list.add_free_elem(el);

    //remove out of in-list of 'to'
    UINT i = 0;
    for (el = VERTEX_in_list(to); el != nullptr; i++) {
        if (el->getEdge() == e) {
            if (pos_in_inlist != nullptr) {
                *pos_in_inlist = i;
            }
            break;
        }
        el = el->get_next();
    }
    ASSERTN(el, ("can not find in-edge, it is illegal graph"));
    xcom::remove(&VERTEX_in_list(to), el);
    m_el_free_list.add_free_elem(el);

    //remove edge out of edge-tab
    m_edgetab.remove(e);
    m_e_free_list.add_free_elem(e);
    return e;
}


Vertex * Graph::removeVertex(Vertex * vex)
{
    ASSERTN(m_ec_pool != nullptr, ("not yet initialized."));
    if (vex == nullptr) { return nullptr; }
    for (EdgeC * el = vex->getOutList(); el != nullptr;) {
        EdgeC * tmp = el;
        el = el->get_next();
        removeEdge(tmp->getEdge());
    }
    for (EdgeC * el = vex->getInList(); el != nullptr;) {
        EdgeC * tmp = el;
        el = el->get_next();
        removeEdge(tmp->getEdge());
    }
    if (is_dense()) {
        m_dense_vertex->set((VecIdx)vex->id(), nullptr);
        m_dense_vex_num--;
    } else {
        vex = m_sparse_vertex->remove(vex);
    }
    m_v_free_list.add_free_elem(vex);
    return vex;
}


//Return all neighbors of 'vid' on graph.
//Return false if 'vid' is not on graph.
//
//'ni_list': record the neighbours of 'vid'.
//    Note that this function ensure each neighbours in ni_list is unique.
bool Graph::getNeighborList(OUT List<VexIdx> & ni_list, VexIdx vid) const
{
    ASSERTN(m_ec_pool != nullptr, ("not yet initialized."));

    //Ensure VertexHash::find is readonly.
    Graph * pthis = const_cast<Graph*>(this);
    Vertex * vex  = pthis->getVertex(vid);
    if (vex == nullptr) { return false; }
    AdjVertexIter iti;
    for (Vertex const* in = Graph::get_first_in_vertex(vex, iti);
         in != nullptr; in = Graph::get_next_in_vertex(iti)) {
        if (!ni_list.find(in->id())) {
            ni_list.append_tail(in->id());
        }
    }
    AdjVertexIter ito;
    for (Vertex const* out = Graph::get_first_out_vertex(vex, ito);
         out != nullptr; out = Graph::get_next_out_vertex(ito)) {
        if (!ni_list.find(out->id())) {
            ni_list.append_tail(out->id());
        }
    }
    return true;
}


//Return all neighbors of 'vid' on graph.
//Return false if 'vid' is not on graph.
//niset: record the neighbours of 'vid'.
//       Note that this function ensure each neighbours in niset is unique.
//       Using sparse bitset is faster than list in most cases.
bool Graph::getNeighborSet(OUT DefSBitSet & niset, VexIdx vid) const
{
    ASSERTN(m_ec_pool != nullptr, ("not yet initialized."));
    //Ensure VertexHash::find is readonly.
    Graph * pthis = const_cast<Graph*>(this);
    Vertex * vex  = pthis->getVertex(vid);
    if (vex == nullptr) { return false; }

    EdgeC * el = vex->getInList();
    while (el != nullptr) {
        VexIdx v = el->getFromId();
        niset.bunion((BSIdx)v);
        el = EC_next(el);
    }

    el = vex->getOutList();
    while (el != nullptr) {
        niset.bunion((BSIdx)el->getToId());
        el = EC_next(el);
    }
    return true;
}


UINT Graph::getDegree(Vertex const* vex) const
{
    ASSERTN(m_ec_pool != nullptr, ("not yet initialized."));
    if (vex == nullptr) { return 0; }
    return vex->getInDegree() + vex->getOutDegree();
}


bool Graph::isInDegreeMoreThan(Vertex const* vex, UINT num) const
{
    ASSERTN(m_ec_pool != nullptr, ("not yet initialized."));
    if (vex == nullptr) { return 0; }
    UINT degree = 0;
    for (EdgeC * el = vex->getInList(); el != nullptr; el = EC_next(el)) {
        degree++;
        if (degree > num) { return true; }
    }
    return degree > num; //Both degree and num may be 0.
}


bool Graph::isOutDegreeMoreThan(Vertex const* vex, UINT num) const
{
    ASSERTN(m_ec_pool != nullptr, ("not yet initialized."));
    if (vex == nullptr) { return 0; }
    UINT degree = 0;
    for (EdgeC * el = vex->getOutList(); el != nullptr; el = EC_next(el)) {
        degree++;
        if (degree > num) { return true; }
    }
    return degree > num; //Both degree and num may be 0.
}


bool Graph::isInDegreeEqualTo(Vertex const* vex, UINT num) const
{
    ASSERTN(m_ec_pool != nullptr, ("not yet initialized."));
    if (vex == nullptr) { return 0; }
    UINT degree = 0;
    for (EdgeC * el = vex->getInList(); el != nullptr; el = EC_next(el)) {
        degree++;
        if (degree == num) { return true; }
    }
    return degree == num; //Both degree and num may be 0.
}


bool Graph::isOutDegreeEqualTo(Vertex const* vex, UINT num) const
{
    ASSERTN(m_ec_pool != nullptr, ("not yet initialized."));
    if (vex == nullptr) { return 0; }
    UINT degree = 0;
    for (EdgeC * el = vex->getOutList(); el != nullptr; el = EC_next(el)) {
        degree++;
        if (degree == num) { return true; }
    }
    return degree == num; //Both degree and num may be 0.
}


Edge * Graph::getEdge(Vertex const* from, Vertex const* to) const
{
    ASSERTN(m_ec_pool != nullptr, ("not yet initialized."));
    if (from == nullptr || to == nullptr) { return nullptr; }

    EdgeC * el = from->getOutList();
    while (el != nullptr) {
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
        EdgeC * el2 = to->getOutList();
        while (el2 != nullptr) {
            Edge * e = EC_edge(el2);
            if (EDGE_from(e) == to && EDGE_to(e) == from) {
                return e;
            }
            el2 = EC_next(el2);
        }
    }
    return nullptr;
}


Edge * Graph::getEdge(VexIdx from, VexIdx to) const
{
    ASSERTN(m_ec_pool != nullptr, ("not yet initialized."));
    Vertex * fp = getVertex(from);
    Vertex * tp = getVertex(to);
    return getEdge(fp, tp);
}


bool Graph::is_equal(Graph & g) const
{
    if (getVertexNum() != g.getVertexNum() ||
        getEdgeNum() != g.getEdgeNum()) {
        return false;
    }

    BitSet vs;
    VertexIter c = VERTEX_UNDEF;
    for (Vertex * v1 = get_first_vertex(c);
         v1 != nullptr; v1 = get_next_vertex(c)) {
        Vertex * v2 = g.getVertex(VERTEX_id(v1));
        if (v2 == nullptr) {
            return false;
        }

        vs.clean();
        EdgeC * el = v1->getOutList();
        Edge * e = nullptr;
        UINT v1_succ_n = 0;
        if (el == nullptr) {
            if (v2->getOutList() != nullptr) {
                return false;
            }
            continue;
        }

        for (e = EC_edge(el); e != nullptr; el = EC_next(el),
             e = el ? EC_edge(el) : nullptr) {
            vs.bunion((BSIdx)VERTEX_id(EDGE_to(e)));
            v1_succ_n++;
        }

        UINT v2_succ_n = 0;
        el = v2->getOutList();
        for (e = EC_edge(el); e != nullptr; el = EC_next(el),
             e = el ? EC_edge(el) : nullptr) {
            v2_succ_n++;
            if (!vs.is_contain((BSIdx)VERTEX_id(EDGE_to(e)))) {
                return false;
            }
        }

        if (v1_succ_n != v2_succ_n) {
            return false;
        }
    }
    return true;
}


bool Graph::is_livein_from(Vertex const* v, Vertex const* pred,
                           Vertex const* start) const
{
    class Visited {
    public:
        BitSet * m_bs;
        TTab<VexIdx> * m_tab;
    public:
        Visited(bool is_dense)
        {
            m_bs = nullptr;
            m_tab = nullptr;
            if (is_dense) { m_bs = new BitSet(); }
            else { m_tab = new TTab<VexIdx>(); }
        }
        ~Visited()
        {
            if (m_bs != nullptr) { delete m_bs; }
            if (m_tab != nullptr) { delete m_tab; }
        }
        void add(VexIdx v)
        {
            if (m_bs != nullptr) { m_bs->bunion((BSIdx)v); }
            else { m_tab->append(v); }
        }
        bool is_visited(VexIdx v) const
        {
            if (m_bs != nullptr) { return m_bs->is_contain((BSIdx)v); }
            else { return m_tab->find(v); }
        }
    };
    Visited visit(is_dense());
    ASSERT0(is_pred(v, pred));
    List<Vertex const*> wl;
    wl.append_tail(pred);
    visit.add(v->id());
    Vertex const* t = nullptr;
    while ((t = wl.remove_head()) != nullptr) {
        if (t == start) { return true; }
        visit.add(t->id());
        AdjVertexIter it;
        for (Vertex const* in = get_first_in_vertex(t, it);
             in != nullptr; in = get_next_in_vertex(it)) {
            if (!visit.is_visited(in->id())) {
                wl.append_tail(in);
            }
        }
    }
    return false;
}


bool Graph::sortInTopologOrder(OUT Vector<Vertex*> & vex_vec)
{
    ASSERTN(m_ec_pool != nullptr, ("Graph still not yet initialize."));
    if (getVertexNum() == 0) {
        return true;
    }
    List<Vertex*> ready_list;
    DefMiscBitSetMgr sm;
    DefSBitSet is_removed(sm.getSegMgr());
    VertexIter c = VERTEX_UNDEF;
    for (xcom::Vertex * vex = get_first_vertex(c);
         vex != nullptr; vex = get_next_vertex(c)) {
        if (vex->getInDegree() == 0) {
            ready_list.append_tail(vex);
        }
    }
    vex_vec.set(getVertexNum() - 1, nullptr);
    UINT pos = 0;
    for (; ready_list.get_elem_count() != 0;) {
        Vertex * ready = ready_list.remove_head();
        is_removed.bunion((BSIdx)ready->id());
        vex_vec.set(pos, ready);
        pos++;
        AdjVertexIter ito;
        for (Vertex * out = get_first_out_vertex(ready, ito);
             out != nullptr; out = get_next_out_vertex(ito)) {
            //Determine if in-degree is not equal to 0.
            UINT in_degree = 0;
            AdjVertexIter it;
            for (Vertex const* in = get_first_in_vertex(out, it);
                 in != nullptr; in = get_next_in_vertex(it)) {
                if (is_removed.is_contain((BSIdx)in->id())) {
                    continue;
                }
                in_degree++;
            }
            if (in_degree == 0) {
                ready_list.append_tail(out);
            }
        }
    }
    return pos == getVertexNum();
}


//Remove all edges between v1 and v2.
void Graph::removeEdgeBetween(Vertex * v1, Vertex * v2)
{
    EdgeC * ec = v1->getOutList();
    while (ec != nullptr) {
        EdgeC * next = EC_next(ec);
        Edge * e = EC_edge(ec);
        if ((EDGE_from(e) == v1 && EDGE_to(e) == v2) ||
            (EDGE_from(e) == v2 && EDGE_to(e) == v1)) {
            removeEdge(e);
        }
        ec = next;
    }

    ec = VERTEX_in_list(v1);
    while (ec != nullptr) {
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
//Return false if the function failed to remove transitive edge.
//Note the function handle only acyclic graph.
//e.g: Given edges of G, there are v0->v1->v2->v3, v0->v3, then v0->v3 named
//transitive edge.
//ALGO:
//  INPUT: Graph with N vertices.
//  1. Sort vertices in topological order.
//  2. Associate each edges with indicator respective,
//     and recording them in one matrix(N*N)
//     e.g: e1:v0->v2, e2:v1->v2, e3:v0->v1
//            0   1    2
//          0 --  e3   e1
//          1 --  --   e2
//          2 --  --   --
//  3. Scan vertices according to toplogical order,
//     remove all edges which the target-node has been
//     marked at else rows.
//     e.g: There are dependence edges: v0->v1, v0->v2.
//     If v1->v2 has been marked, we said v0->v2 is removable,
//     and the same goes for the rest of edges.
//     e.g: E->D, A->D are transitive edges.
//         E   A
//       __|   |_
//      |   \ /  |
//      |    V   |
//      |    B   |
//      |    |   |
//      |    |   |
//      |    V   |
//      |    C   |
//      |___ | __|
//          \|/
//           V
//           D
bool Graph::removeTransitiveEdge()
{
    Vector<Vertex*> vex_vec;
    bool succ = sortInTopologOrder(vex_vec);
    if (!succ) { return false; }
    DefMiscBitSetMgr bs_mgr;
    Vector<DefSBitSetCore*> reachset_vec;
    TMap<VexIdx, DefSBitSetCore*> reachset_map;
    BitSet is_visited;

    //Scanning vertices in topological order.
    for (VecIdx i = 0; i <= vex_vec.get_last_idx(); i++) {
        Vertex const* fromvex = vex_vec.get(i);
        //Note the vertices have sorted and stored in dense mode in vector.
        //Thus there is no node can be skipped.
        ASSERT0(fromvex);
        if (is_dense()) {
            removeTransitiveEdgeHelper(
                fromvex, &reachset_vec, is_visited, bs_mgr);
            continue;
        }
        removeTransitiveEdgeHelper(fromvex,
            (Vector<DefSBitSetCore*>*)&reachset_map, is_visited, bs_mgr);
    }
    if (is_dense()) {
        for (VecIdx i = 0; i <= reachset_vec.get_last_idx(); i++) {
            DefSBitSetCore * bs = reachset_vec.get(i);
            if (bs != nullptr) {
                bs->clean(bs_mgr);
            }
        }
        return true;
    }
    TMapIter<VexIdx, DefSBitSetCore*> iter;
    DefSBitSetCore * bs = nullptr;
    for (reachset_map.get_first(iter, &bs);
         bs != nullptr; reachset_map.get_next(iter, &bs)) {
        bs->clean(bs_mgr);
    }
    return true;
}


void Graph::removeTransitiveEdgeHelper(
    Vertex const* fromvex, Vector<DefSBitSetCore*> * reachset,
    BitSet & is_visited, DefMiscBitSetMgr & bs_mgr)
{
    ASSERT0(reachset);
    if (is_visited.is_contain((BSIdx)fromvex->id())) { return; }
    is_visited.bunion((BSIdx)fromvex->id());

    ASSERT0(fromvex);
    if (fromvex->getOutList() == nullptr) { return; }

    //Reachset defines the set of vertex that fromvex is able to reach.
    DefSBitSetCore * from_reachset = is_dense() ?
        reachset->get(fromvex->id()) :
        ((TMap<VexIdx, DefSBitSetCore*>*)reachset)->get(fromvex->id());
    if (from_reachset == nullptr) {
        from_reachset = bs_mgr.allocSBitSetCore();
        if (is_dense()) {
            reachset->set(fromvex->id(), from_reachset);
        } else {
            ((TMap<VexIdx, DefSBitSetCore*>*)reachset)->set(fromvex->id(),
                                                          from_reachset);
        }
    }

    //Position in bitset has been sorted in topological order.
    EdgeC const* next_ec = nullptr;
    for (EdgeC const* ec = fromvex->getOutList(); ec != nullptr; ec = next_ec) {
        next_ec = ec->get_next();
        Vertex const* tovex = ec->getTo();

        from_reachset->bunion(tovex->id(), bs_mgr);
        removeTransitiveEdgeHelper(tovex, reachset, is_visited, bs_mgr);

        //Reachset defines the set of vertex that tovex is able to reach.
        DefSBitSetCore * to_reachset = is_dense() ?
            reachset->get(tovex->id()) :
            ((TMap<VexIdx, DefSBitSetCore*>*)reachset)->get(tovex->id());
        if (to_reachset == nullptr) { continue; }
        from_reachset->bunion(*to_reachset, bs_mgr);

        //Get successor set correspond to to_dense.
        if (tovex->getOutList() == nullptr) { continue; }

        //Iterate other successors except 'to'.
        EdgeC const* next_ec2 = nullptr;
        for (EdgeC const* ec2 = fromvex->getOutList();
             ec2 != nullptr; ec2 = next_ec2) {
            next_ec2 = ec2->get_next();
            Vertex const* othervex = ec2->getTo();
            if (othervex == tovex || !to_reachset->is_contain(othervex->id())) {
                continue;
            }
            if (next_ec == ec2) {
                next_ec = ec->get_next();
            }
            removeEdge(ec2->getEdge());
        }
    }
}


void Graph::dumpVexVector(Vector<Vertex*> const& vec, FILE * h)
{
    if (h == nullptr) { return; }
    fprintf(h, "\n");
    for (VecIdx i = 0; i <= vec.get_last_idx(); i++) {
        Vertex const* x = vec.get(i);
        if (x != nullptr) {
            if (i != 0) { fprintf(h, ","); }
            fprintf(h, "V%d", x->id());
        }
    }
    fflush(h);
}


void Graph::dumpHeight(FILE * h) const
{
    if (h == nullptr) { return; }
    fprintf(h, "\nHEIGHT:");
    VertexIter itv = VERTEX_UNDEF;
    bool first = true;
    for (Vertex const* v = get_first_vertex(itv);
         v != nullptr; v = get_next_vertex(itv)) {
        if (!first) { fprintf(h, ","); }
        first = false;
        fprintf(h, "V%d:%u", v->id(), v->getHeight());
    }
    fprintf(h, "\n");
    fflush(h);
}


void Graph::dumpVertexAux(Vertex const* v, OUT DefFixedStrBuf & buf) const
{
    //NOTE:DOT file use '\l' as newline charactor.
    //Target Dependent Code.

    //If you dump in multiple-line, the last \l is very important to display
    //DOT file in a fine manner.
    //Print carriage-return for DOT file, use \l instead of \n to
    //avoid DOT complaint.
    //fprintf(h, "\\l");
}


void Graph::dumpVertexDesc(
    xcom::Vertex const* v, OUT DefFixedStrBuf & buf) const
{
    //Just dump the vertex index.
    buf.strcat("V%d", v->id());
}


void Graph::dumpEdgeDesc(
    xcom::Edge const* e, OUT DefFixedStrBuf & buf) const
{
    if (e->info() == nullptr) { return; }
    buf.strcat("%u", e->info());
}


CHAR const* Graph::dumpVertex(Vertex const* v, OUT DefFixedStrBuf & buf) const
{
    buf.strcat("\nnode%d [shape = Mrecord, label=\"", v->id());
    DefFixedStrBuf tbuf;
    dumpVertexDesc(v, tbuf);
    buf.strcat("%s", tbuf.getBuf());
    dumpVertexAux(v, buf);

    //The end char of properties.
    buf.strcat("\"];");
    return buf.getBuf();
}


void Graph::dumpVertex(FILE * h, Vertex const* v) const
{
    DefFixedStrBuf buf;
    fprintf(h, "%s", dumpVertex(v, buf));
}


CHAR const* Graph::dumpEdge(Edge const* e, OUT DefFixedStrBuf & buf) const
{
    buf.strcat("\nnode%d->node%d", e->from()->id(), e->to()->id());
    buf.strcat("[");
    bool prt_comma = false;
    if (!is_direction()) {
        if (prt_comma) { buf.strcat(","); }
        buf.strcat("dir=none");
        prt_comma = true;
    }
    {
        //Print label.
        if (prt_comma) { buf.strcat(","); }
        DefFixedStrBuf descbuf;
        dumpEdgeDesc(e, descbuf);
        buf.strcat("label=\"%s\"", descbuf.getBuf());
        prt_comma = true;
    }
    buf.strcat("]");
    return buf.getBuf();
}


void Graph::dumpEdge(FILE * h, Edge const* e) const
{
    DefFixedStrBuf buf;
    fprintf(h, "%s", dumpEdge(e, buf));
}


//Print node
CHAR const* Graph::dumpAllVertices(OUT DefFixedStrBuf & buf) const
{
    VertexIter itv = VERTEX_UNDEF;
    for (Vertex const* v = get_first_vertex(itv);
         v != nullptr; v = get_next_vertex(itv)) {
        dumpVertex(v, buf);
    }
    return buf.getBuf();
}


void Graph::dumpAllVertices(FILE * h) const
{
    DefFixedStrBuf buf;
    fprintf(h, "%s", dumpAllVertices(buf));
}


//Print edge
CHAR const* Graph::dumpAllEdges(OUT DefFixedStrBuf & buf) const
{
    EdgeIter ite;
    for (Edge const* e = get_first_edge(ite);
         e != nullptr; e = get_next_edge(ite)) {
        dumpEdge(e, buf);
    }
    return buf.getBuf();
}


void Graph::dumpAllEdges(FILE * h) const
{
    DefFixedStrBuf buf;
    fprintf(h, "%s", dumpAllEdges(buf));
}


void Graph::dumpDOT(CHAR const* name) const
{
    if (name == nullptr) {
        name = "graph.dot";
    }
    FileObj fo(name, true, false);
    FILE * h = fo.getFileHandler();
    ASSERTN(h, ("%s create failed!!!", name));
    fprintf(h, "digraph G {\n");
    dumpAllVertices(h);
    dumpAllEdges(h);
    fprintf(h, "\n}\n");
}


void Graph::dumpVCG(CHAR const* name) const
{
    ASSERTN(m_ec_pool != nullptr, ("not yet initialized."));
    if (name == nullptr) {
        name = "graph.vcg";
    }
    FileObj fo(name, true, false);
    FILE * h = fo.getFileHandler();
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
    VertexIter itv = VERTEX_UNDEF;
    for (Vertex const* v = get_first_vertex(itv);
         v != nullptr; v = get_next_vertex(itv)) {
        fprintf(h, "\nnode: { title:\"%d\" label:\"%d\" "
                "shape:circle fontname:\"courB\" color:gold}",
                v->id(), v->id());
    }

    //Print edge
    EdgeIter ite;
    for (Edge const* e = get_first_edge(ite);
         e != nullptr; e = get_next_edge(ite)) {
        fprintf(h, "\nedge: { sourcename:\"%d\" targetname:\"%d\" %s}",
                e->from()->id(), e->to()->id(),
                is_direction() ? "" : "arrowstyle:none" );
    }
    fprintf(h, "\n}\n");
}


bool Graph::isReachExit(Vertex const* vex, UINT try_limit,
                        OUT bool & try_failed)
{
    if (is_graph_exit(vex)) { return true; }
    UINT count = 0;
    GraphIterOut iterout(*this, vex);
    for (Vertex const* t = iterout.get_first();
         t != nullptr; t = iterout.get_next(t)) {
        count++;
        if (is_graph_exit(t)) { return true; }
        if (count >= try_limit) {
            try_failed = true;
            return false;
        }
    }
    return false;
}


bool Graph::isReachIn(
    Vertex const* vex, Vertex const* reachin, UINT try_limit,
    OUT bool & try_failed, VexTab const* terminate_vex_tab)
{
    ASSERT0(vex && reachin);
    try_failed = false;
    BitSet visited;
    List<Vertex const*> wl;
    AdjVertexIter it;
    for (Vertex const* in = Graph::get_first_in_vertex(vex, it);
         in != nullptr; in = Graph::get_next_in_vertex(it)) {
        if (terminate_vex_tab != nullptr && terminate_vex_tab->find(in->id())) {
            continue;
        }
       visited.bunion(in->id());
        wl.append_tail(in);
    }
    xcom::Vertex const* v = nullptr;
    UINT count = 0;
    while ((v = wl.remove_head()) != nullptr) {
        if (v == reachin) { return true; }
        count++;
        AdjVertexIter it;
        for (Vertex const* in = Graph::get_first_in_vertex(v, it);
             in != nullptr; in = Graph::get_next_in_vertex(it)) {
            if (terminate_vex_tab != nullptr &&
                terminate_vex_tab->find(in->id())) {
                continue;
            }
            if (in == reachin) {
                return true;
            }
            if (count >= try_limit) {
                try_failed = true;
                return false;
            }
            if (!visited.is_contain(in->id())) {
                wl.append_tail(in);
                visited.bunion(in->id());
            }
        }
    }
    return false;
}


UINT Graph::WhichPred(VexIdx pred_vex_id, Vertex const* vex,
                      OUT bool & is_pred)
{
    ASSERT0(vex);
    UINT n = 0;
    AdjVertexIter it;
    is_pred = false;
    for (Vertex const* in = get_first_in_vertex(vex, it); in != nullptr;
         in = get_next_in_vertex(it)) {
        if (in->id() == pred_vex_id) {
            is_pred = true;
            return n;
        }
        n++;
    }
    //CASE:User may require the function by a vertex which is not the
    //predecessor of 'vex'. Thus set 'is_pred' to false to faciliate user
    //usage.
    //UNREACHABLE();
    return VERTEX_UNDEF;
}


Vertex * Graph::get_first_in_vertex(Vertex const* vex, AdjVertexIter & it)
{
    ASSERT0(vex);
    it = vex->getInList();
    if (it == nullptr) { return nullptr; }
    return it->getFrom();
}


Vertex * Graph::get_next_in_vertex(AdjVertexIter & it)
{
    if (it == nullptr) { return nullptr; }
    it = it->get_next();
    return it == nullptr ? nullptr : it->getFrom();
}


Vertex * Graph::get_first_out_vertex(Vertex const* vex, AdjVertexIter & it)
{
    ASSERT0(vex);
    it = vex->getOutList();
    if (it == nullptr) { return nullptr; }
    return it->getTo();
}


Vertex * Graph::get_next_out_vertex(AdjVertexIter & it)
{
    if (it == nullptr) { return nullptr; }
    it = it->get_next();
    return it == nullptr ? nullptr : it->getTo();
}


Edge * Graph::get_first_out_edge(Vertex const* vex, AdjEdgeIter & it)
{
    ASSERT0(vex);
    it = vex->getOutList();
    if (it == nullptr) { return nullptr; }
    return it->getEdge();
}


Edge * Graph::get_next_out_edge(AdjEdgeIter & it)
{
    if (it == nullptr) { return nullptr; }
    it = it->get_next();
    return it == nullptr ? nullptr : it->getEdge();
}


Edge * Graph::get_first_in_edge(Vertex const* vex, AdjEdgeIter & it)
{
    ASSERT0(vex);
    it = vex->getInList();
    if (it == nullptr) { return nullptr; }
    return it->getEdge();
}


Edge * Graph::get_next_in_edge(AdjEdgeIter & it)
{
    if (it == nullptr) { return nullptr; }
    it = it->get_next();
    return it == nullptr ? nullptr : it->getEdge();
}
//END Graph


//
//START DGraph
//
DGraph::DGraph(UINT vex_hash_size) : Graph(vex_hash_size)
{
    set_dense(true);
    m_bs_mgr = nullptr;
}


DGraph::DGraph(DGraph const& g) : Graph(g)
{
    ASSERTN(g.is_dense(), ("Dominate Graph have to be dense graph"));
    m_bs_mgr = g.m_bs_mgr;
    if (m_bs_mgr != nullptr) {
        cloneDomAndPdom(g);
    }
}


void DGraph::cloneDomAndPdom(DGraph const& src)
{
    ASSERT0(m_bs_mgr != nullptr);
    ASSERTN(src.is_dense(), ("Dominate Graph have to be dense graph"));
    VertexIter c = VERTEX_UNDEF;
    for (Vertex * srcv = src.get_first_vertex(c);
         srcv != nullptr; srcv = src.get_next_vertex(c)) {
        VexIdx src_vid = VERTEX_id(srcv);
        Vertex * tgtv = getVertex(src_vid);
        ASSERT0(tgtv != nullptr);

        DomSet const* set = src.get_dom_set(VERTEX_id(srcv));
        if (set != nullptr) {
            gen_dom_set(tgtv)->copy(*set);
        }

        set = src.get_pdom_set(VERTEX_id(srcv));
        if (set != nullptr) {
            gen_pdom_set(tgtv)->copy(*set);
        }
    }
    m_idom_set.copy(src.m_idom_set);
    m_ipdom_set.copy(src.m_ipdom_set);
}


size_t DGraph::count_mem() const
{
    size_t count = m_dom_set.count_mem();
    count += m_pdom_set.count_mem(); //record post-dominator-set of each vertex.
    count += m_idom_set.count_mem(); //immediate dominator.
    count += m_ipdom_set.count_mem(); //immediate post dominator.
    count += sizeof(m_bs_mgr); //Do NOT count up the bitset in BS_MGR.
    return count;
}


bool DGraph::computeDom(List<Vertex const*> const* vlst, DomSet const* uni)
{
    List<Vertex const*> tmpvlst;
    List<Vertex const*> * pvlst = &tmpvlst;
    if (vlst != nullptr) {
        //Here one must guarantee pvlst would not be modified.
        pvlst = const_cast<List<Vertex const*>*>(vlst);
    } else {
        VertexIter c = VERTEX_UNDEF;
        for (Vertex const* u = get_first_vertex(c);
             u != nullptr; u = get_next_vertex(c)) {
            pvlst->append_tail(u);
        }
    }

    DomSet const* luni = nullptr;
    if (uni != nullptr) {
        luni = uni;
    } else {
        DomSet * x = new DomSet();
        C<Vertex const*> * ct = nullptr;
        for (pvlst->get_head(&ct);
             ct != pvlst->end(); ct = pvlst->get_next(ct)) {
            Vertex const* u = ct->val();
            ASSERT0(u);
            x->bunion(VERTEX_id(u));
        }
        luni = x;
    }

    //Initialize dom-set.
    C<Vertex const*> * ct;
    for (pvlst->get_head(&ct); ct != pvlst->end(); ct = pvlst->get_next(ct)) {
        Vertex const* v = ct->val();
        ASSERT0(v);
        if (is_graph_entry(v)) {
            DomSet * dom = gen_dom_set(v);
            dom->clean();
            dom->bunion(v->id());
        } else {
            gen_dom_set(v)->copy(*luni);
        }
    }

    //DOM[entry] = {entry}
    //DOM[n] = {n} กศ { กษ(DOM[pred] of predecessor of 'n') }
    bool change = true;
    DomSet tmp;
    UINT count = 0;
    while (change && count < 10) {
        count++;
        change = false;
        C<Vertex const*> * ct2;
        for (pvlst->get_head(&ct2); ct2 != pvlst->end();
             ct2 = pvlst->get_next(ct2)) {
            Vertex const* v = ct2->val();
            ASSERT0(v);
            VexIdx vid = VERTEX_id(v);
            if (is_graph_entry(v)) {
                continue;
            }

            //Access each preds
            EdgeC * ec = v->getInList();
            while (ec != nullptr) {
                Vertex * pred = ec->getFrom();
                if (ec == v->getInList()) {
                    tmp.copy(*m_dom_set.get(VERTEX_id(pred)));
                } else {
                    tmp.intersect(*m_dom_set.get(VERTEX_id(pred)));
                }
                ec = EC_next(ec);
            }
            tmp.bunion(vid);

            DomSet * dom = m_dom_set.get(VERTEX_id(v));
            if (!dom->is_equal(tmp)) {
                dom->copy(tmp);
                change = true;
            }
        } //end for
    } //end while
    ASSERT0(!change);

    if (uni == nullptr && luni != nullptr) {
        delete luni;
    }
    return true;
}


bool DGraph::computeDom3(List<Vertex const*> const* vlst, DomSet const* uni)
{
    DUMMYUSE(uni);
    List<Vertex const*> tmpvlst;
    List<Vertex const*> * pvlst = &tmpvlst;
    if (vlst != nullptr) {
        //Here one must guarantee pvlst would not be modified.
        pvlst = const_cast<List<Vertex const*>*>(vlst);
    } else {
        VertexIter c = VERTEX_UNDEF;
        for (Vertex const* u = get_first_vertex(c);
             u != nullptr; u = get_next_vertex(c)) {
            pvlst->append_tail(u);
        }
    }

    //Initialize dom-set.
    C<Vertex const*> * ct;
    for (pvlst->get_head(&ct); ct != pvlst->end(); ct = pvlst->get_next(ct)) {
        Vertex const* v = ct->val();
        ASSERT0(v);
        if (is_graph_entry(v)) {
            DomSet * dom = gen_dom_set(v);
            dom->clean();
            dom->bunion(VERTEX_id(v));
        } else {
            gen_dom_set(v)->clean();
        }
    }

    //DOM[entry] = {entry}
    //DOM[n] = {n} กศ { กษ(DOM[pred] of predecessor of 'n') }
    bool change = true;
    DomSet tmp;
    UINT count = 0;
    while (change && count < 10) {
        count++;
        change = false;
        C<Vertex const*> * ct2;
        for (pvlst->get_head(&ct2);
             ct2 != pvlst->end(); ct2 = pvlst->get_next(ct2)) {
            Vertex const* v = ct2->val();
            ASSERT0(v);
            if (is_graph_entry(v)) {
                continue;
            }

            //Access each preds
            UINT meet = 0;
            AdjVertexIter it;
            for (Vertex const* in = Graph::get_first_in_vertex(v, it);
                 in != nullptr; in = Graph::get_next_in_vertex(it)) {
                DomSet * ds = m_dom_set.get(in->id());
                if (ds->is_empty()) {
                    continue;
                }
                if (meet == 0) {
                    tmp.copy(*ds);
                } else {
                    tmp.intersect(*ds);
                }
                meet++;
            }

            if (meet == 0) { tmp.clean(); }
            tmp.bunion(v->id());

            DomSet * dom = m_dom_set.get(v->id());
            if (!dom->is_equal(tmp)) {
                dom->copy(tmp);
                change = true;
            }
        }
    }
    ASSERT0(!change);
    return true;
}


bool DGraph::computePdomByRPO(Vertex * root, DomSet const* uni)
{
    RPOVexList vlst;
    m_rpomgr.computeRPO(*this, root, vlst);
    vlst.reverse();

    bool res = false;
    if (uni == nullptr) {
        res = computePdom(vlst);
    } else {
        res = computePdom(vlst, uni);
    }
    ASSERT0_DUMMYUSE(res);
    return true;
}


bool DGraph::computePdom(List<Vertex const*> const& vlst)
{
    DomSet uni;
    C<Vertex const*> * ct;
    for (vlst.get_head(&ct); ct != vlst.end(); ct = vlst.get_next(ct)) {
        Vertex const* u = ct->val();
        ASSERT0(u);
        uni.bunion(VERTEX_id(u));
    }
    return computePdom(vlst, &uni);
}


void DGraph::revisePdomByIpdom()
{
    VertexIter it = VERTEX_UNDEF;
    for (Vertex const* v = get_first_vertex(it);
         v != nullptr; v = get_next_vertex(it)) {
        if (get_ipdom(v->id()) == VERTEX_UNDEF) {
            freePdomSet(v->id());
        }
    }
}


bool DGraph::computePdom(List<Vertex const*> const& vlst, DomSet const* uni)
{
    ASSERT0(uni);

    //Initialize pdom.
    C<Vertex const*> * ct;
    bool find_exit = false;
    for (vlst.get_head(&ct); ct != vlst.end(); ct = vlst.get_next(ct)) {
        Vertex const* v = ct->val();
        ASSERT0(v);
        gen_pdom_set(v)->clean();
        if (is_graph_exit(v)) {
            find_exit = true;
            break;
        }
    }
    if (!find_exit) {
        //Note the graph may NOT have an exit vertex.
        return true;
    }
    for (vlst.get_head(&ct); ct != vlst.end(); ct = vlst.get_next(ct)) {
        Vertex const* v = ct->val();
        ASSERT0(v);
        if (is_graph_exit(v)) {
            DomSet * pdom = gen_pdom_set(v);
            pdom->clean();
            pdom->bunion(VERTEX_id(v));
        } else {
            gen_pdom_set(v)->copy(*uni);
        }
    }

    //PDOM[exit] = {exit}
    //PDOM[n] = {n} U {กษ(PDOM[succ] of each succ of n)}
    bool change = true;
    DomSet tmp;
    UINT count = 0;
    //The previous max count limit is 10, and change to 13 due to some cases.
    while (change && count < 13) {
        count++;
        change = false;
        C<Vertex const*> * ct2;
        for (vlst.get_head(&ct2); ct2 != vlst.end();
             ct2 = vlst.get_next(ct2)) {
            Vertex const* v = ct2->val();
            ASSERT0(v);
            VexIdx vid = VERTEX_id(v);
            if (is_graph_exit(v)) {
                //Note the graph may NOT have an exit vertex.
                continue;
            }

            tmp.clean();
            //Access each succs
            EdgeC * ec = v->getOutList();
            while (ec != nullptr) {
                Vertex * succ = ec->getTo();
                if (ec == v->getOutList()) {
                    tmp.copy(*m_pdom_set.get(VERTEX_id(succ)));
                } else {
                    tmp.intersect(*m_pdom_set.get(VERTEX_id(succ)));
                }
                ec = EC_next(ec);
            }
            tmp.bunion(vid);

            DomSet * pdom = m_pdom_set.get(VERTEX_id(v));
            if (!pdom->is_equal(tmp)) {
                pdom->copy(tmp);
                change = true;
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
    DomSet avail;
    for (vlst.get_head(&ct); ct != vlst.end(); ct = vlst.get_next(ct)) {
        Vertex const* v = ct->val();
        ASSERT0(v);
        DomSet * doms = gen_dom_set(VERTEX_id(v));
        doms->clean();
        ASSERT0(doms);
        for (VexIdx idom = get_idom(VERTEX_id(v));
             idom != VERTEX_UNDEF; idom = get_idom(idom)) {
            doms->bunion(idom);
            if (avail.is_contain(idom)) {
                DomSet const* idom_doms = gen_dom_set(idom);
                doms->bunion(*idom_doms);
                break;
            }
        }
        avail.bunion(VERTEX_id(v));
    }
    return true;
}


//NOTE: the function needs RPO to compute DomInfo.
//vextab: the vertex set of subgraph.
static Vertex const* computeIdomViaOnePred(
    Vertex const* entry, Vertex const* v, Vertex const* pred,
    Vertex const* idom_cand, DGraph const& g, Vector<VexIdx> const& idomset,
    TTab<VexIdx> const& vextab)
{
    ASSERT0(pred && idom_cand);
    Vertex const* j = pred;
    Vertex const* k = idom_cand;
    while (j != k) {
        ASSERTN(j->rpo() != RPO_UNDEF, ("miss RPO info"));
        ASSERTN(k->rpo() != RPO_UNDEF, ("miss RPO info"));
        while (j->rpo() > k->rpo()) {
            VexIdx j_idom = idomset.get(j->id());
            if (j_idom == VERTEX_UNDEF) {
                //CASE:the vertex 'j' does not have an idom because it may
                //NOT reach by 'entry'. This may happen during the
                //processing of graph modification.
                //e.g:V11 is entry, V6 is pred.
                //    V11-->V4
                //          ^
                //         /
                //    V6---
                //Thus skip current searching-path.
                return nullptr;
            }
            if (!vextab.find(j_idom)) {
                //j's idom is not belong to subgraph, thus re-choose a path
                //to traverl.
                return nullptr;
            }
            j = g.getVertex(j_idom);
            ASSERT0(j);
            if (j == entry) {
                break;
            }
        }
        while (j->rpo() < k->rpo()) {
            VexIdx k_idom = idomset.get(k->id());
            if (k_idom == VERTEX_UNDEF) {
                //CASE:the vertex 'k' does not have an idom because it may
                //NOT reach by 'entry'. This may happen during the
                //processing of graph modification.
                //e.g:V11 is entry, V6 is pred.
                //    V11-->V4
                //          ^
                //         /
                //    V6---
                //Thus skip current searching-path.
                return nullptr;
            }
            if (!vextab.find(k_idom)) {
                //k's idom is not belong to subgraph, thus re-choose a path
                //to traverl.
                return nullptr;
            }
            k = g.getVertex(k_idom);
            ASSERT0(k);
            if (k == entry) {
                break;
            }
        }
        if (j == entry && k == entry) {
            break;
        }
    }
    ASSERT0(j == k);
    return j;
}


//The function compute the idom by retrive predecessor of 'v'.
//NOTE: the function will not compute idom outside the 'vextab'.
//NOTE: the function needs RPO to compute DomInfo.
//vextab: a set of vertex need to process.
//        Note the vertex set may be subset of vertex of graph g.
static Vertex const* computeIdomViaPred(
    Vertex const* entry, Vertex const* v, DGraph const& g,
    Vector<VexIdx> const& idomset, TTab<VexIdx> const& vextab)
{
    //Access each preds
    Vertex const* idom_cand = nullptr;
    AdjVertexIter it;
    for (Vertex const* in = Graph::get_first_in_vertex(v, it);
         in != nullptr; in = Graph::get_next_in_vertex(it)) {
        if (!vextab.find(in->id())) { continue; }
        if (idomset.get(in->id()) == VERTEX_UNDEF) { continue; }
        //if (idomset.get(in->id()) == VERTEX_UNDEF) {
        //    //CASE:the vertex 'in' does not have an idom because it may NOT
        //    //reach by 'entry'. This may happen during the processing of
        //    //graph modification.
        //    //e.g:V11 is entry, V6 is pred, where V6 can not be reached from
        //    //the root of graph.
        //    //ROOT-->V11-->V4
        //    //             ^
        //    //            /
        //    //       V6---
        //    //Thus skip 'in'.
        //    continue;
        //}
        if (idom_cand == nullptr) {
            idom_cand = in;
            continue;
        }
        Vertex const* next_idom_cand = computeIdomViaOnePred(
            entry, v, in, idom_cand, g, idomset, vextab);
        if (next_idom_cand == nullptr) {
            //Thus skip current searching-path.
            continue;
        }
        if (next_idom_cand != idom_cand) {
            idom_cand = next_idom_cand;
        }
    }
    return idom_cand;
}


static bool verifyAllVexHaveIDom(List<Vertex const*> const& vlst,
                                 DGraph const& g,
                                 Vector<VexIdx> const& idom_set)
{
    List<Vertex const*>::Iter it;
    for (vlst.get_head(&it); it != vlst.end(); it = vlst.get_next(it)) {
        Vertex const* v = it->val();
        if (g.is_graph_entry(v)) { continue; }
        ASSERT0(idom_set.get(v->id()) != VERTEX_UNDEF);
    }
    return true;
}


//NOTE: the function needs RPO to compute DomInfo.
static bool computeIdomForVertexInSubGraph(
    Vertex const* entry, List<Vertex const*> const& vlst,
    DGraph const& g, TTab<VexIdx> const& vextab, OUT Vector<VexIdx> & idomset)
{
    bool changed = false;
    //Access vertex in RPO.
    List<Vertex const*>::Iter it;
    for (vlst.get_head(&it); it != vlst.end(); it = vlst.get_next(it)) {
        Vertex const* v = it->val();
        ASSERT0(v);
        if (g.is_graph_entry(v)) {
            //Note 'entry' is the entry of subgraph, it may not be the full
            //graph.
            idomset.set(v->id(), v->id());
            continue;
        }
        if (v == entry) { continue; }
        Vertex const* idom = computeIdomViaPred(
            entry, v, g, idomset, vextab);
        if (idom == nullptr) {
            if (idomset.get(v->id()) != VERTEX_UNDEF) {
                idomset.set(v->id(), VERTEX_UNDEF);
                changed = true;
            }
            continue;
        }
        if (idomset.get(v->id()) == idom->id()) { continue; }
        idomset.set(v->id(), idom->id());
        changed = true;
    }
    return changed;
}


//NOTE: the function needs RPO to compute DomInfo.
void DGraph::computeIdomForSubGraph(Vertex const* entry,
                                    List<Vertex const*> const& vlst)
{
    TTab<VexIdx> vextab;
    //Initialize idom-set for vertex in 'vlst' before recompute their idom.
    List<Vertex const*>::Iter ct;
    for (vlst.get_head(&ct); ct != vlst.end(); ct = vlst.get_next(ct)) {
        Vertex const* v = ct->val();
        vextab.append(v->id());
        if (v == entry) {
            //No need to clean entry's idom.
            continue;
        }
        m_idom_set.set(v->id(), VERTEX_UNDEF);
    }
    bool changed = true;
    UINT count = 0;
    while (changed) {
        changed = computeIdomForVertexInSubGraph(
            entry, vlst, *this, vextab, m_idom_set);
        count++;
    }
    if (is_graph_entry(entry)) {
        //Entry vertex does not have idom.
        m_idom_set.set(entry->id(), VERTEX_UNDEF);
    }
    ASSERT0_DUMMYUSE(verifyAllVexHaveIDom(vlst, *this, m_idom_set));
}


bool DGraph::computeIdomForFullGraph(List<Vertex const*> const& vlst)
{
    bool changed = false;
    //Access with topological order.
    List<Vertex const*>::Iter ct;
    for (Vertex const* v = vlst.get_head(&ct);
         ct != vlst.end(); v = vlst.get_next(&ct)) {
        if (is_graph_entry(v)) {
            m_idom_set.set(v->id(), v->id());
            continue;
        }
        //Access each predecessors.
        Vertex const* idom = nullptr;
        AdjVertexIter it;
        for (Vertex const* in = Graph::get_first_in_vertex(v, it);
             in != nullptr; in = Graph::get_next_in_vertex(it)) {
            if (m_idom_set.get(in->id()) == VERTEX_UNDEF) {
                continue;
            }
            if (idom == nullptr) {
                idom = in;
                continue;
            }
            Vertex const* j = in;
            Vertex const* k = idom;
            while (j != k) {
                ASSERTN(j->rpo() != RPO_UNDEF, ("miss RPO info"));
                ASSERTN(k->rpo() != RPO_UNDEF, ("miss RPO info"));
                while (j->rpo() > k->rpo()) {
                    VexIdx j_idom = m_idom_set.get(j->id());
                    ASSERT0(j_idom != VERTEX_UNDEF);
                    j = getVertex(j_idom);
                    ASSERT0(j);
                    if (is_graph_entry(j)) {
                        break;
                    }
                }
                while (j->rpo() < k->rpo()) {
                    VexIdx k_idom = m_idom_set.get(k->id());
                    ASSERT0(k_idom != VERTEX_UNDEF);
                    k = getVertex(k_idom);
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
                idom = nullptr;
                break;
            }
            idom = j;
        }
        if (idom == nullptr) {
            if (m_idom_set.get(v->id()) != VERTEX_UNDEF) {
                m_idom_set.set(v->id(), VERTEX_UNDEF);
                changed = true;
            }
            continue;
        }
        if (m_idom_set.get(v->id()) != idom->id()) {
            m_idom_set.set(v->id(), idom->id());
            changed = true;
        }
    }
    return changed;
}


void DGraph::computeEntryList(OUT List<Vertex const*> & lst) const
{
    VertexIter it;
    for (Vertex * v = get_first_vertex(it);
         v != nullptr; v = get_next_vertex(it)) {
        if (is_graph_entry(v)) {
            lst.append_tail(v);
        }
    }
}


bool DGraph::computeIdom2(List<Vertex const*> const& vlst)
{
    //Initialize idom-set.
    m_idom_set.clean();
    bool changed = true;
    while (changed) {
        changed = computeIdomForFullGraph(vlst);
    }
    List<Vertex const*> elst;
    computeEntryList(elst);
    List<Vertex const*>::Iter ct;
    for (Vertex const* v = elst.get_head(&ct);
         ct != elst.end(); v = elst.get_next(&ct)) {
        m_idom_set.set(v->id(), VERTEX_UNDEF);
    }
    return true;
}


bool DGraph::verifyPdom(DGraph & g, RPOVexList const& rpovlst) const
{
    RPOVexList vlst;
    C<Vertex const*> * ct;
    for (Vertex const* v = rpovlst.get_tail(&ct); v != nullptr;
         v = rpovlst.get_prev(&ct)) {
        ASSERT0(v->id() != VERTEX_UNDEF);
        vlst.append_tail(v);
    }
    bool f1 = g.computePdom(vlst);
    ASSERT0_DUMMYUSE(f1);
    bool f2 = g.computeIpdom();
    ASSERT0_DUMMYUSE(f2);
    g.revisePdomByIpdom();
    ASSERT0(g.m_ipdom_set.get_elem_count() == m_ipdom_set.get_elem_count());
    for (VecIdx i = 0; i <= m_ipdom_set.get_last_idx(); i++) {
        VexIdx cur = m_ipdom_set.get(i);
        VexIdx anti = g.m_ipdom_set.get(i);
        ASSERTN_DUMMYUSE(cur == anti, ("unmatch ipdom"));
    }
    return true;
}


bool DGraph::verifyDom(DGraph & g, RPOVexList const& rpovlst) const
{
    g.computeIdom2(rpovlst);
    VecIdx max = MAX(m_idom_set.get_last_idx(), g.m_idom_set.get_last_idx());
    for (VecIdx i = 0; i <= max; i++) {
        VexIdx cur = m_idom_set.get(i);
        VexIdx anti = g.m_idom_set.get(i);
        ASSERTN_DUMMYUSE(cur == anti, ("unmatch idom"));
    }
    return true;
}


bool DGraph::verifyDom() const
{
    DGraph g;
    g.clone(*this, false, false);
    Vertex * entry = nullptr;
    VertexIter it = VERTEX_UNDEF;
    for (Vertex * v = g.get_first_vertex(it); v != nullptr;
         v = g.get_next_vertex(it)) {
        if (g.is_graph_entry(v)) {
            entry = v;
            break;
        }
    }
    if (entry == nullptr) { return true; }
    RPOVexList rpovlst;
    RPOMgr rpomgr;
    rpomgr.computeRPO(*this, entry, rpovlst);
    verifyDom(g, rpovlst);
    return true;
}


bool DGraph::verifyPdom() const
{
    DGraph g;
    g.clone(*this, false, false);
    Vertex * entry = nullptr;
    VertexIter it = VERTEX_UNDEF;
    for (Vertex * v = g.get_first_vertex(it); v != nullptr;
         v = g.get_next_vertex(it)) {
        if (g.is_graph_entry(v)) {
            entry = v;
            break;
        }
    }
    if (entry == nullptr) { return true; }
    RPOVexList rpovlst;
    RPOMgr rpomgr;
    rpomgr.computeRPO(*this, entry, rpovlst);
    verifyPdom(g, rpovlst);
    return true;
}


bool DGraph::verifyDomAndPdom() const
{
    DGraph g;
    g.clone(*this, false, false);
    Vertex * entry = nullptr;
    VertexIter it = VERTEX_UNDEF;
    for (Vertex * v = g.get_first_vertex(it); v != nullptr;
         v = g.get_next_vertex(it)) {
        if (g.is_graph_entry(v)) {
            entry = v;
            break;
        }
    }
    if (entry == nullptr) { return true; }
    RPOVexList rpovlst;
    RPOMgr rpomgr;
    rpomgr.computeRPO(*this, entry, rpovlst);
    verifyDom(g, rpovlst);
    verifyPdom(g, rpovlst);
    return true;
}


//NOTE: Entry does not have idom.
bool DGraph::computeIdom()
{
    //Initialize idom-set.
    m_idom_set.clean();

    //Access with topological order.
    VertexIter c = VERTEX_UNDEF;
    for (Vertex * v = get_first_vertex(c);
         v != nullptr; v = get_next_vertex(c)) {
        VexIdx cur_id = VERTEX_id(v);
        if (is_graph_entry(v)) {
            continue;
        }
        if (m_dom_set.get(cur_id)->get_elem_count() >= 2) {
            BitSet * p = m_dom_set.get(cur_id);
            ASSERTN(p != nullptr, ("should compute dom first"));
            if (p->get_elem_count() == 1) {
                //There is no idom if 'dom' set only contain itself.
                ASSERT0(m_idom_set.get(cur_id) == VERTEX_UNDEF);
                continue;
            }
            p->diff(cur_id);

            #ifdef MAGIC_METHOD
            BSIdx i;
            for (i = p->get_first(); i != BS_UNDEF;
                 i = p->get_next((VexIdx)i)) {
                if (m_dom_set.get((VexIdx)i)->is_equal(*p)) {
                    ASSERT0(m_idom_set.get(cur_id) == VERTEX_UNDEF);
                    m_idom_set.set(cur_id, i);
                    break;
                }
            }
            ASSERTN(i != BS_UNDEF, ("not find idom?"));
            #else
            BSIdx i;
            for (i = tmp.get_first(); i != BS_UNDEF; i = tmp.get_next(i)) {
                for (BSIdx j = tmp.get_first(); j != BS_UNDEF;
                     j = tmp.get_next(j)) {
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
            ASSERTN(i != BS_UNDEF, ("cannot find idom of Vex:%d", cur_id));
            ASSERTN(m_idom_set.get(cur_id) == VERTEX_UNDEF,
                    ("recompute idom for Vex:%d", cur_id));
            m_idom_set.set(cur_id, i);
            #endif
            p->bunion(cur_id);
        } //end else if
    } //end for
    return true;
}


Vertex * Graph::get_last_vertex(VertexIter & it) const
{
    ASSERTN(m_ec_pool != nullptr, ("not yet initialized."));
    if (is_dense()) {
        for (VecIdx i = m_dense_vertex->get_last_idx(); !IS_VECUNDEF(i); i--) {
            Vertex * vex = m_dense_vertex->get(i);
            if (vex != nullptr) {
                it = i;
                return vex;
            }
        }
        return nullptr;
    }
    return m_sparse_vertex->get_last(it);
}


Vertex * Graph::get_prev_vertex(VertexIter & it) const
{
    ASSERTN(m_ec_pool != nullptr, ("not yet initialized."));
    if (is_dense()) {
        for (VecIdx i = it - 1; !IS_VECUNDEF(i); i--) {
            Vertex * vex = m_dense_vertex->get(i);
            if (vex != nullptr) {
                it = i;
                return vex;
            }
        }
        return nullptr;
    }
    return m_sparse_vertex->get_prev(it);
}


Vertex * Graph::get_first_vertex(VertexIter & it) const
{
    ASSERTN(m_ec_pool != nullptr, ("not yet initialized."));
    if (is_dense()) {
        for (VecIdx i = VERTEX_UNDEF + 1;
             i <= m_dense_vertex->get_last_idx(); i++) {
            Vertex * vex = m_dense_vertex->get(i);
            if (vex != nullptr) {
                it = i;
                return vex;
            }
        }
        return nullptr;
    }
    return m_sparse_vertex->get_first(it);
}


Vertex * Graph::get_next_vertex(VertexIter & it) const
{
    ASSERTN(m_ec_pool != nullptr, ("not yet initialized."));
    if (is_dense()) {
        for (VecIdx i = it + 1; i <= m_dense_vertex->get_last_idx(); i++) {
            Vertex * vex = m_dense_vertex->get(i);
            if (vex != nullptr) {
                it = i;
                return vex;
            }
        }
        return nullptr;
    }
    return m_sparse_vertex->get_next(it);
}


//NOTE: graph exit vertex does not have ipdom.
bool DGraph::computeIpdom()
{
    //Initialize ipdom-set.
    m_ipdom_set.clean();

    //Processing in reverse-topological order.
    VertexIter c = VERTEX_UNDEF;
    for (Vertex const* v = get_last_vertex(c);
         v != nullptr; v = get_prev_vertex(c)) {
        VexIdx cur_id = VERTEX_id(v);
        if (is_graph_exit(v) ||
            m_pdom_set.get(cur_id)->get_elem_count() <= 1) {
            continue;
        }

        BitSet * p = m_pdom_set.get(cur_id);
        ASSERTN(p != nullptr, ("should compute pdom first"));
        if (p->get_elem_count() == 1) {
            //There is no ipdom if 'pdom' set only contain itself.
            ASSERT0(m_ipdom_set.get(cur_id) == VERTEX_UNDEF);
            continue;
        }

        p->diff(cur_id);
        BSIdx i;
        for (i = p->get_first(); i != BS_UNDEF; i = p->get_next(i)) {
            if (m_pdom_set.get(i)->is_equal(*p)) {
                ASSERT0(m_ipdom_set.get(cur_id) == VERTEX_UNDEF);
                m_ipdom_set.set(cur_id, i);
                break;
            }
        }
        //ASSERTN(i != BS_UNDEF, ("not find ipdom")); //Not find.
        p->bunion(cur_id);
    }
    return true;
}


void DGraph::genDomTreeForSubGraph(Vertex const* root, OUT DomTree & dt,
                                   OUT UINT & iter_times) const
{
    ASSERT0(root && isVertex(root));
    dt.erase();
    dt.set_dense(is_dense());
    VexIdx rootid = root->id();
    Vertex * dtv = dt.addVertex(rootid);
    dt.setRoot(dtv);
    GraphIterOut iterout(*this, root);
    iter_times = 0;
    for (Vertex const* t = iterout.get_first();
         t != nullptr; t = iterout.get_next(t)) {
        if (!is_dom(rootid, t->id())) { continue; }
        VexIdx idom = m_idom_set.get(t->id());
        ASSERT0(idom != VERTEX_UNDEF);
        dt.addEdge(idom, t->id());
        iter_times++;
    }
}


//Collect the vertex that dominated by 'root'.
static void collectDomTreeCoveredVertex(
    Vertex const* root, DGraph const& g, VexTab const* modset,
    OUT RPOVexList & affectlst, OUT TMap<Vertex const*, VexIdx> & vex2idom,
    OUT UINT & iter_times)
{
    DUMMYUSE(collectDomTreeCoveredVertex);
    DomTree dt;
    //Generate DomTree according to old DomInfo.
    g.genDomTreeForSubGraph(root, dt, iter_times);
    DomTreeIter dtit(dt);
    for (Vertex const* dtv = dtit.get_first();
         dtv != nullptr; dtv = dtit.get_next(dtv)) {
        Vertex const* gv = g.getVertex(dtv->id());
        ASSERT0(gv);
        affectlst.append_tail(gv);
        if (modset != nullptr) {
            vex2idom.set(gv, g.get_idom(gv->id()));
        }
    }
}


//Collect the vertex by DFS whereas dominated by 'root'.
static void collectDomTreeCoveredReachableVertex(
    Vertex const* root, DGraph const& g, VexTab const* modset,
    OUT RPOVexList & affectlst, OUT TMap<Vertex const*, VexIdx> & vex2idom,
    OUT UINT & iter_times)
{
    GraphIterOut iterout(g, root);
    affectlst.append_tail(root);
    VexIdx rootid = root->id();
    for (Vertex const* t = iterout.get_first();
         t != nullptr; t = iterout.get_next(t)) {
        iter_times++;
        if (t == root) { continue; }
        if (!g.is_dom(rootid, t->id())) { continue; }
        affectlst.append_tail(t);
        if (modset != nullptr) {
            vex2idom.set(t, g.get_idom(t->id()));
        }
    }
}


//dt: generate dominator tree and record in it.
void DGraph::genDomTree(OUT DomTree & dt) const
{
    dt.erase();
    dt.set_dense(is_dense());
    VertexIter c = VERTEX_UNDEF;
    for (Vertex * v = get_first_vertex(c);
         v != nullptr; v = get_next_vertex(c)) {
        VexIdx vid = VERTEX_id(v);
        dt.addVertex(vid);
        VexIdx idom = m_idom_set.get(vid);
        if (idom != VERTEX_UNDEF) {
            ASSERTN(isVertex(idom), ("idom set is out of date"));
            dt.addEdge(idom, vid);
        }
    }
    VertexIter it;
    Vertex * dtv = dt.get_first_vertex(it);
    if (dtv == nullptr) { return; }
    Vertex * p = nullptr;
    for (; (p = dt.getParent(dtv)) != nullptr; dtv = p) {}
    dt.setRoot(dtv);
}


//pdom: generate post-dominator tree and record in it.
void DGraph::genPDomTree(OUT DomTree & pdt) const
{
    VertexIter c = VERTEX_UNDEF;
    pdt.set_dense(is_dense());
    for (Vertex * v = get_first_vertex(c);
         v != nullptr; v = get_next_vertex(c)) {
        VexIdx vid = v->id();
        pdt.addVertex(vid);
        VexIdx ipdom = m_ipdom_set.get(vid);
        if (ipdom != VERTEX_UNDEF) {
            ASSERTN(isVertex(ipdom), ("ipdom set is out of date"));
            pdt.addEdge(ipdom, vid);
        }
    }
}


void DGraph::dumpDomAndPdom(
    CHAR const* name, bool dump_dom_tree, bool dump_pdom_tree) const
{
    if (name == nullptr) {
        name = "graph_dom.txt";
    }
    FileObj fo(name, true, false);
    FILE * h = fo.getFileHandler();
    ASSERTN(h, ("%s create failed!!!",name));
    dumpDomAndPdom(h, dump_dom_tree, dump_pdom_tree);
}


void DGraph::dumpDomAndPdom(
    FILE * h, bool dump_dom_tree, bool dump_pdom_tree) const
{
    if (h == nullptr) { return; }
    StrBuf buf(32);
    fprintf(h, "%s\n", dumpDomAndPdom(buf));
    fflush(h);
    if (dump_dom_tree) {
        DomTree dom;
        genDomTree(dom);
        dom.dumpDOT("graph_dom_tree.dot");
    }
    if (dump_pdom_tree) {
        DomTree dom;
        genPDomTree(dom);
        dom.dumpDOT("graph_pdom_tree.dot");
    }
}


//Dump dom set, pdom set, idom, ipdom.
CHAR const* DGraph::dumpDomAndPdom(OUT StrBuf & buf) const
{
    buf.strcat("\n==---- DUMP DOM/PDOM/IDOM/IPDOM ----==");
    VertexIter c = VERTEX_UNDEF;
    for (Vertex * v = get_first_vertex(c);
         v != nullptr; v = get_next_vertex(c)) {
        VexIdx vid = VERTEX_id(v);
        BitSet * bs;
        buf.strcat("\nVERTEX(%d)", vid);
        buf.strcat("\n  domset:");
        if ((bs = m_dom_set.get(vid)) != nullptr) {
            for (BSIdx id = bs->get_first();
                 id != BS_UNDEF ; id = bs->get_next((VexIdx)id)) {
                if ((VexIdx)id != vid) {
                    buf.strcat("%u ", id);
                }
            }
        }
        buf.strcat("\n  pdomset:");
        if ((bs = m_pdom_set.get(vid)) != nullptr) {
            for (BSIdx id = bs->get_first();
                 id != BS_UNDEF; id = bs->get_next((VexIdx)id)) {
                if ((VexIdx)id != vid) {
                    buf.strcat("%u ", id);
                }
            }
        }
        if (m_idom_set.get(vid) != VERTEX_UNDEF) {
            buf.strcat("\n  idom:%u", m_idom_set.get(vid));
        } else {
            buf.strcat("\n");
        }
        if (m_ipdom_set.get(vid) != VERTEX_UNDEF) {
            buf.strcat("\n  ipdom:%u", m_ipdom_set.get(vid));
        } else {
            buf.strcat("\n");
        }
    }
    return buf.getBuf();
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
    while ((v = stk.pop()) != nullptr) {
        if (!is_visited.is_contain(VERTEX_id(v))) {
            is_visited.bunion(VERTEX_id(v));
            stk.push(v);
            //The only place to process vertex.
            lst.append_tail(v);
        }

        //Visit children.
        EdgeC * el = v->getOutList();
        Vertex * succ;
        while (el != nullptr) {
            succ = el->getTo();
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
void DGraph::sortInBfsOrder(Vector<VexIdx> & order_buf, Vertex * root,
                            BitSet & visit)
{
    List<Vertex*> worklst;
    worklst.append_tail(root);
    VexIdx order = 1;
    while (worklst.get_elem_count() > 0) {
        Vertex * sv = worklst.remove_head();
        order_buf.set(VERTEX_id(sv), order);
        order++;
        visit.bunion(VERTEX_id(sv));
        EdgeC * el = sv->getOutList();
        while (el != nullptr) {
            Vertex * to = el->getTo();
            if (visit.is_contain(VERTEX_id(to))) {
                el = EC_next(el);
                continue;
            }
            worklst.append_tail(to);
            el = EC_next(el);
        }
    }
}


//Sort in-edge of vex in given order.
//order: record the given order of each predecessor. Note the number
//       of elements have to equal to the number of predecessor of vex.
void DGraph::sortPred(MOD Vertex * vex, Vector<VexIdx> const& order)
{
    UINT pos = 0;
    EdgeC * next_ec = nullptr;
    for (EdgeC * ec = vex->getInList(); ec != nullptr; ec = next_ec, pos++) {
        next_ec = ec->get_next();
        ASSERT0(ec->getToId() == vex->id());
        VexIdx anti_pred = order.get(pos);
        if (ec->getFromId() == anti_pred) { continue; }

        //Find the anticipated pred.
        EdgeC * ec2 = next_ec;
        for (; ec2 != nullptr; ec2 = ec2->get_next()) {
            if (ec2->getFromId() == anti_pred) {
                break;
            }
        }
        ASSERTN(ec2, ("not found anti-pred"));
        xcom::swap(&VERTEX_in_list(vex), ec, ec2);
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
    while ((v = stk.pop()) != nullptr) {
        //Visit children first.
        EdgeC * el = v->getOutList();
        bool find = false; //find unvisited kid.
        Vertex * succ;
        while (el != nullptr) {
            succ = el->getTo();
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


void DGraph::removeUnreachNodeRecur(VexIdx id, BitSet & visited)
{
    visited.bunion(id);
    Vertex * vex = getVertex(id);
    AdjVertexIter it;
    for (Vertex const* out = Graph::get_first_out_vertex(vex, it);
         out != nullptr; out = Graph::get_next_out_vertex(it)) {
        if (!visited.is_contain(out->id())) {
            removeUnreachNodeRecur(out->id(), visited);
        }
    }
}


void DGraph::freePdomSet(VexIdx vid)
{
    DomSet * pdomset = m_pdom_set.get(vid);
    if (pdomset != nullptr) {
        m_bs_mgr->free(pdomset);
        m_pdom_set.set(vid, nullptr);
    }
}


void DGraph::freeDomSet(VexIdx vid)
{
    DomSet * domset = m_dom_set.get(vid);
    if (domset != nullptr) {
        m_bs_mgr->free(domset);
        m_dom_set.set(vid, nullptr);
    }
}


//Add vertex to domset and pdomset for both livein and liveout paths.
static void removeVexFromDomAndPdomSet(DGraph * g, Vertex const* marker)
{
    GraphIterIn iterin(*g, marker);
    VexIdx vexid = marker->id();
    for (Vertex const* t = iterin.get_first();
         t != nullptr; t = iterin.get_next(t)) {
        if (t->id() == vexid) { continue; }
        g->gen_dom_set(t->id())->diff(vexid);
        g->gen_pdom_set(t->id())->diff(vexid);
    }

    GraphIterOut iterout(*g, marker);
    for (Vertex const* t = iterout.get_first();
         t != nullptr; t = iterout.get_next(t)) {
        if (t->id() == vexid) { continue; }
        g->gen_dom_set(t->id())->diff(vexid);
        g->gen_pdom_set(t->id())->diff(vexid);
    }
}


//The function try to change the DOM info when given vertex has been bypassed.
//vex: the vertex id that will be bypassed.
//e.g:
//  pred->vex->succ
//  where vex's idom is pred, vex's ipdom is succ, succ's idom is vex.
//after bypassing,
//  pred->vex->succ
//   \         ^
//    \_______/
//  succ's dom become pred. vex is neither DOM nor PDOM.
bool DGraph::changeDomInfoByAddBypassEdge(VexIdx pred, VexIdx vex, VexIdx succ)
{
    ASSERT0(getEdge(getVertex(pred), getVertex(vex)));
    ASSERT0(getEdge(getVertex(vex), getVertex(succ)));
    ASSERT0(getEdge(getVertex(pred), getVertex(succ)));
    ASSERT0(getVertex(pred)->getOutDegree() == 2);
    ASSERT0(getVertex(succ)->getInDegree() == 2);
    ASSERT0(getVertex(vex)->getOutDegree() == 1);
    ASSERT0(getVertex(vex)->getInDegree() == 1);
    VexIdx ipdom_pred = get_ipdom(pred);
    if (ipdom_pred != VERTEX_UNDEF && ipdom_pred != vex) { return false; }
    VexIdx idom_succ = get_idom(succ);
    if (idom_succ != VERTEX_UNDEF && idom_succ != vex) { return false; }
    if (ipdom_pred != VERTEX_UNDEF) {
        //PDOM may not yet be computed by PassMgr.
        set_ipdom(pred, succ);
    }
    if (idom_succ != VERTEX_UNDEF) {
        //DOM may not yet be computed by PassMgr.
        set_idom(succ, pred);
    }
    removeVexFromDomAndPdomSet(this, getVertex(vex));
    return true;
}


//Add vertex newid to domset and pdomset for both livein and liveout paths
//of marker.
//is_dom: true if 'newid' is the new Dom of 'marker', false means 'newid' is
//        the new PDom of 'marker'.
//CASE:
//    Ventry
//  ___| |__
// |        |
// v        v
// V5       V6
// |        |
// ----   ---
//     || ___________
//     vvv           |
//     V3            |
//      | \         V8
//      |  \________^
//      v
//    Vexit
//after adding IDom V13 of V3
//    Ventry
//  ___| |__
// |        |
// v        v
// V5       V6
// |        |
// ----  ---
//     ||
//     vv
//     V13
//      |
//      |
//      | ___________
//      vv           |
//     V3            |
//      | \         V8
//      |  \________^
//      v
//    Vexit
//CASE2:
//          Ventry
//           |
//           v
//    -----> V3
//   |      / |
//   |     v  |
//    --- V8  |
//          \ |
//           vv
//           Vexit
//after adding IPDom V13 of V3
//          Ventry
//           |
//           v
//    -----> V3
//   |       |
//   |       v
//   |       V13
//   |      / |
//   |     v  |
//    --- V8  |
//          \ |
//           vv
//           Vexit
static void addVexToDomAndPdomSet(
    DGraph * g, Vertex const* marker, VexIdx newid, bool is_dom)
{
    VexIdx markerid = marker->id();
    DomSet visited;
    List<Vertex const*> wl;
    wl.append_tail(marker);
    for (Vertex const* v = wl.remove_head(); v != nullptr;
         v = wl.remove_head()) {
        visited.bunion(v->id());
        if (v->id() != markerid && v->id() != newid) {
            DomSet * domset = g->gen_dom_set(v->id());
            DomSet * pdomset = g->gen_pdom_set(v->id());
            if (is_dom) {
                //newid is the Dom of marker.
                if (domset->is_contain(markerid)) {
                    domset->bunion(newid);
                } else if (g->is_ipdom(markerid, v->id())) {
                    g->set_ipdom(v->id(), newid);
                    pdomset->bunion(newid);
                }
            } else {
                //newid is the PDom of marker.
                if (pdomset->is_contain(markerid)) {
                    pdomset->bunion(newid);
                } else if (g->is_idom(markerid, v->id())) {
                    g->set_idom(v->id(), newid);
                    domset->bunion(newid);
                }
            }
        }
        AdjVertexIter it;
        for (Vertex const* in = Graph::get_first_in_vertex(v, it);
             in != nullptr; in = Graph::get_next_in_vertex(it)) {
            if (!visited.is_contain(in->id())) {
                visited.bunion(in->id());
                wl.append_tail(in);
            }
        }
    }

    //If the succ of 'marker' is also the pred, then it has been mark visited.
    //Unmark the succ in order to handle more subsequent successors.
    visited.clean();
    wl.append_tail(marker);
    for (Vertex const* v = wl.remove_head(); v != nullptr;
         v = wl.remove_head()) {
        visited.bunion(v->id());
        if (v->id() != markerid && v->id() != newid) {
            DomSet * domset = g->gen_dom_set(v->id());
            DomSet * pdomset = g->gen_pdom_set(v->id());
            if (is_dom) {
                //newid is the Dom of marker.
                if (domset->is_contain(markerid)) {
                    domset->bunion(newid);
                } else if (g->is_ipdom(markerid, v->id())) {
                    g->set_ipdom(v->id(), newid);
                    pdomset->bunion(newid);
                }
            } else {
                //newid is the PDom of marker.
                if (pdomset->is_contain(markerid)) {
                    pdomset->bunion(newid);
                } else if (g->is_idom(markerid, v->id())) {
                    g->set_idom(v->id(), newid);
                    domset->bunion(newid);
                }
            }
        }
        AdjVertexIter it;
        for (Vertex const* out = Graph::get_first_out_vertex(v, it);
             out != nullptr; out = Graph::get_next_out_vertex(it)) {
            if (visited.is_contain(out->id())) { continue; }
            visited.bunion(out->id());
            wl.append_tail(out);
        }
    }
}


//NOTE: the function needs RPO to compute DomInfo.
bool DGraph::recomputeDomInfoForSubGraph(
    Vertex const* root, OUT VexTab * modset, OUT UINT & iter_times)
{
    ASSERT0(root);
    RPOVexList affectlst;
    TMap<Vertex const*, VexIdx> vex2idom;
    collectDomTreeCoveredReachableVertex(
        root, *this, modset, affectlst, vex2idom, iter_times);
    computeIdomForSubGraph(root, affectlst);
    if (modset != nullptr) {
        //Record all vertices that dominator info changed.
        RPOVexListIter it;
        for (Vertex const* v = affectlst.get_head(&it);
             v != nullptr; v = affectlst.get_next(&it)) {
            VexIdx newidom = get_idom(v->id());
            ASSERT0(newidom != VERTEX_UNDEF || is_graph_entry(v));
            if (vex2idom.get(v) == newidom) { continue; }
            if (is_graph_entry(v)) {
                //CASE:if idom of graph entry changed, that means the entry is
                //not original entry of graph, it become an entry just
                //because the in-edge is removed.
                modset->add(v);
                continue;
            }
            Vertex const* idomv = getVertex(newidom);
            ASSERT0(idomv);
            modset->add(idomv);
        }
    }
    bool f = computeDom2(affectlst);
    ASSERT0_DUMMYUSE(f);
    return true;
}


//NOTE: the function needs RPO to compute DomInfo.
void DGraph::reviseDomInfoAfterAddOrRemoveEdge(
    Vertex const* from, Vertex const* to, OUT VexTab * modset,
    OUT Vertex const*& root, OUT UINT & iter_times)

{
    VexIdx idom;
    if (is_dom(from->id(), to->id())) {
        root = from;
    } else if (is_dom(to->id(), from->id())) {
        root = to;
    } else if ((idom = get_idom(from->id())) == get_idom(to->id())) {
        root = getVertex(idom);
    } else {
        //CAUTION:LCA query is costly.
        DomTree ldt;
        genDomTree(ldt);
        ldt.computeHeight();
        xcom::NaiveLCA lca(&ldt);
        idom = lca.query(from->id(), to->id());
        ASSERT0(idom != VERTEX_UNDEF);
        root = getVertex(idom);
    }
    ASSERT0(root);
    bool succ = recomputeDomInfoForSubGraph(root, modset, iter_times);
    ASSERTN_DUMMYUSE(succ, ("graph is legal to recompute DOM info"));
}


static void reviseIDomAndIPdom(
    DGraph & g, Vertex const* marker, Vertex const* newsucc,
    Vertex const* oldsucc)
{
    if (g.get_ipdom(marker->id()) != VERTEX_UNDEF) {
        //ipdom is meanlingless if there is no exit in graph.
        g.set_ipdom(newsucc->id(), oldsucc->id());
        if (g.get_ipdom(marker->id()) == oldsucc->id()) {
            g.set_ipdom(marker->id(), newsucc->id());
        }
    }
    //There is no need to consider the IDom of 'marker'.
    //idom is meanlingless if there is no entry in graph.
    g.set_idom(newsucc->id(), marker->id());
    if (g.get_idom(oldsucc->id()) == marker->id()) {
        g.set_idom(oldsucc->id(), newsucc->id());
    }
}


void DGraph::try_union_dom_set(VexIdx vex, VexIdx newdomvex)
{
    ASSERT0(vex != VERTEX_UNDEF);
    ASSERT0(newdomvex != VERTEX_UNDEF);
    gen_dom_set(vex)->bunion(newdomvex);
}


void DGraph::try_union_dom_set(VexIdx vex, BitSet const* newset)
{
    if (newset == nullptr) { return; }
    gen_dom_set(vex)->bunion(*newset);
}


void DGraph::try_union_pdom_set(VexIdx vex, VexIdx newpdomvex)
{
    ASSERT0(vex != VERTEX_UNDEF);
    ASSERT0(newpdomvex != VERTEX_UNDEF);
    gen_pdom_set(vex)->bunion(newpdomvex);
}


void DGraph::try_union_pdom_set(VexIdx vex, BitSet const* newset)
{
    ASSERT0(vex != VERTEX_UNDEF);
    if (newset == nullptr) { return; }
    gen_pdom_set(vex)->bunion(*newset);
}


static void reviseDomAndPdomSet(
    DGraph & g, Vertex const* marker, Vertex const* newsucc,
    Vertex const* oldsucc, bool marker_is_idom_of_oldsucc)
{
    //Copy Dom Set from marker to newsucc.
    g.try_union_dom_set(newsucc->id(), g.get_dom_set(marker->id()));
    g.try_union_dom_set(newsucc->id(), marker->id());

    //Set PDom Set of marker.
    g.try_union_pdom_set(marker->id(), newsucc->id());

    //Copy PDom Set from oldsucc to newsucc.
    g.try_union_pdom_set(newsucc->id(), g.get_pdom_set(oldsucc->id()));
    g.try_union_pdom_set(newsucc->id(), oldsucc->id());
    if (marker_is_idom_of_oldsucc) {
        //Maintain domset, pdomset of each vertex that are in the domset
        //and pdomset of 'marker'.
        addVexToDomAndPdomSet(&g, oldsucc, newsucc->id(), true);
    }
}


void DGraph::addDomInfoToImmediateSucc(
    Vertex const* marker, Vertex const* newsucc, Vertex const* oldsucc)
{
    ASSERT0(marker && newsucc && oldsucc);
    ASSERTN(get_idom(newsucc->id()) == VERTEX_UNDEF &&
            get_ipdom(newsucc->id()) == VERTEX_UNDEF, ("not new vertex"));
    ASSERTN(get_dom_set(newsucc->id()) == nullptr &&
            get_pdom_set(newsucc->id()) == nullptr, ("not new vertex"));
    ASSERT0(newsucc->getInDegree() == 1 && newsucc->getOutDegree() == 1);
    ASSERT0(is_unique_succ(marker, newsucc));

    //CONFIRMED: No need to care about the in-degree of oldsucc, because once
    //user guaranteed marker is idom of oldsucc, there is only one CASE to
    //handle.
    //TBD:Do we have to restrict the in-degree of oldsucc? User has guaranteed
    //that marker is the idom of oldsucc, and there is only ONE path from
    //marker to oldsucc.
    //e.g:licm.gr, LICM insert BB11 between BB10->BB2, BB11 is the fallthrough
    //BB of BB10, and BB2 has more than one predecessors. The case is legal.
    //ASSERT0(oldsucc->getInDegree() == 1);

    bool marker_is_idom_of_oldsucc = is_idom(marker->id(), oldsucc->id());
    reviseIDomAndIPdom(*this, marker, newsucc, oldsucc);
    reviseDomAndPdomSet(*this, marker, newsucc, oldsucc,
                        marker_is_idom_of_oldsucc);
}


void DGraph::addDomToNewSingleInOutBB(
    Vertex const* marker, Vertex const* newsucc, Vertex const* oldsucc)
{
    ASSERT0(marker && newsucc && oldsucc);
    ASSERTN(get_idom(newsucc->id()) == VERTEX_UNDEF &&
            get_ipdom(newsucc->id()) == VERTEX_UNDEF, ("not new vertex"));
    ASSERT0(newsucc->getInDegree() == 1 && newsucc->getOutDegree() == 1);

    //Set idom info.
    set_idom(newsucc->id(), marker->id());
    if (oldsucc->getInDegree() == 1 &&
        get_idom(oldsucc->id()) == marker->id()) {
        set_idom(oldsucc->id(), newsucc->id());
    }

    if (get_dom_set(newsucc->id()) == nullptr) {
        //Copy Dom info from marker to newsucc.
        DomSet const* ds = get_dom_set(marker->id());
        ASSERT0(ds);
        if (ds != nullptr) {
            gen_dom_set(newsucc->id())->bunion(*ds);
        }
        gen_dom_set(newsucc->id())->bunion(marker->id());
    }

    //Update the Dom info of oldsucc.
    if (oldsucc->getInDegree() == 1) {
        if (get_dom_set(oldsucc->id()) == nullptr) {
            gen_dom_set(oldsucc->id())->bunion(newsucc->id());
            return;
        }
        const_cast<DomSet*>(get_dom_set(oldsucc->id()))->bunion(newsucc->id());
        return;
    }

    //Sometimes, extra BBs are generated to connect with oldsucc.
    AdjVertexIter it = nullptr;
    for (Vertex const* in = Graph::get_first_in_vertex(oldsucc, it);
         in != nullptr; in = Graph::get_next_out_vertex(it)) {
        if (in->getInDegree() != 1 || in->getOutDegree() != 1) { continue; }

        if (get_dom_set(in->id()) != nullptr) { continue; }
        AdjVertexIter it2 = nullptr;
        Vertex const* parent = Graph::get_first_in_vertex(in, it2);
        set_idom(in->id(), parent->id());
    }
}


//The function adds Dom, Pdom, IDom, IPDom information for newipdom, whereas
//update the related info for 'marker'.
//marker: a marker vertex.
//newipdom: the vertex that must be ipdom of 'marker'.
void DGraph::addDomInfoToNewIPDom(Vertex const* marker, Vertex const* newipdom)
{
    ASSERTN(get_idom(newipdom->id()) == VERTEX_UNDEF &&
            get_ipdom(newipdom->id()) == VERTEX_UNDEF, ("not new vertex"));
    VexIdx markerid = marker->id();
    VexIdx newipdomid = newipdom->id();
    VexIdx ipdom = get_ipdom(markerid);
    set_ipdom(markerid, newipdomid);
    if (ipdom != VERTEX_UNDEF) {
        set_ipdom(newipdomid, ipdom);
        VexIdx idomipdom = get_idom(ipdom);
        if (idomipdom == markerid) {
            set_idom(ipdom, newipdomid);
            set_idom(newipdomid, markerid);
        }
    }
    if (get_idom(markerid) != VERTEX_UNDEF) {
        //idom is meanlingless if there is no entry in graph.
        set_idom(newipdomid, markerid);
    }
    if (get_dom_set(newipdomid) == nullptr) {
        //Copy Dom, PDom info from marker to newidom.
        DomSet const* ds = get_dom_set(markerid);
        ASSERT0(ds);
        gen_dom_set(newipdomid)->bunion(*ds);
        DomSet const* pds = get_pdom_set(markerid);
        if (pds != nullptr && !pds->is_empty()) {
            gen_pdom_set(newipdomid)->bunion(*pds);
        }
    }
    //Set domset, pdomset.
    addVexToDomAndPdomSet(this, marker, newipdomid, false);
    gen_pdom_set(markerid)->bunion(newipdomid);
    gen_dom_set(newipdomid)->bunion(markerid);
}


void DGraph::addDomInfoToNewIDom(Vertex const* marker, Vertex const* newidom,
                                 bool & add_pdom_failed)
{
    ASSERTN(get_idom(newidom->id()) == VERTEX_UNDEF &&
            get_ipdom(newidom->id()) == VERTEX_UNDEF, ("not new vertex"));
    add_pdom_failed = false;
    VexIdx markerid = marker->id();
    VexIdx newidomid = newidom->id();
    VexIdx idom = get_idom(markerid);
    set_idom(markerid, newidomid);
    if (idom != VERTEX_UNDEF) {
        set_idom(newidomid, idom);
        VexIdx ipdomidom = get_ipdom(idom);
        if (ipdomidom == markerid) {
            set_ipdom(idom, newidomid);
            set_ipdom(newidomid, markerid);
        }
    }
    bool try_failed = false;
    if (get_ipdom(markerid) != VERTEX_UNDEF ||
        isReachExit(marker, 200, try_failed)) {
        //There is no ipdom is meanlingless if there is no exit
        //vertex in graph.
        set_ipdom(newidomid, markerid);
    }
    if (try_failed) {
        add_pdom_failed = true;
    }
    if (get_dom_set(newidomid) == nullptr) {
        //Copy Dom, PDom info from marker to newidom.
        DomSet const* ds = get_dom_set(markerid);
        ASSERT0(ds);
        gen_dom_set(newidomid)->bunion(*ds);
        DomSet const* pds = get_pdom_set(markerid);
        if (pds != nullptr && !pds->is_empty()) {
            gen_pdom_set(newidomid)->bunion(*pds);
        }
    }
    //Set domset, pdomset.
    addVexToDomAndPdomSet(this, marker, newidomid, true);
    gen_dom_set(markerid)->bunion(newidomid);
    gen_pdom_set(newidomid)->bunion(markerid);
}


void DGraph::removeDomInfo(Vertex const* marker, bool iter_pred_succ,
                           OUT UINT & iter_time)
{
    iter_time = 0;
    ASSERT0(marker && marker->id() != VERTEX_UNDEF);
    VexIdx markerid = marker->id();
    freeDomSet(markerid);
    freePdomSet(markerid);
    VexIdx markeridom = get_idom(markerid);
    VexIdx vexipdom = get_ipdom(markerid);
    set_idom(markerid, VERTEX_UNDEF);
    set_ipdom(markerid, VERTEX_UNDEF);
    if (!iter_pred_succ) { return; }
    GraphIterIn iterin(*this, marker);
    for (Vertex const* t = iterin.get_first();
         t != nullptr; t = iterin.get_next(t)) {
        gen_dom_set(t->id())->diff(markerid);
        gen_pdom_set(t->id())->diff(markerid);
        if (get_idom(t->id()) == markerid) {
            set_idom(t->id(), markeridom);
        }
        if (get_ipdom(t->id()) == markerid) {
            set_ipdom(t->id(), vexipdom);
        }
        iter_time++;
    }
    GraphIterOut iterout(*this, marker);
    for (Vertex const* t = iterout.get_first();
         t != nullptr; t = iterout.get_next(t)) {
        gen_dom_set(t->id())->diff(markerid);
        gen_pdom_set(t->id())->diff(markerid);
        if (get_idom(t->id()) == markerid) {
            set_idom(t->id(), markeridom);
        }
        if (get_ipdom(t->id()) == markerid) {
            set_ipdom(t->id(), vexipdom);
        }
        iter_time++;
    }
}


//Perform DFS to seek for unreachable node.
//Return true if some nodes removed.
bool DGraph::removeUnreachNode(VexIdx entry_id)
{
    if (getVertexNum() == 0) { return false; }
    bool removed = false;
    BitSet visited;
    removeUnreachNodeRecur(entry_id, visited);
    VertexIter c = VERTEX_UNDEF;
    for (Vertex * v = get_first_vertex(c);
         v != nullptr; v = get_next_vertex(c)) {
        if (!visited.is_contain(VERTEX_id(v))) {
            removeVertex(v);
            removed = true;
        }
    }
    return removed;
}
//END DGraph


//
//START GraphIterIn
//
GraphIterIn::GraphIterIn(Graph const& g, Vertex const* start,
                         Vertex const* stop) : m_g(g), m_stop(stop)
{
    ASSERT0(start);
    ASSERTN(g.getVertex(start->id()) == start,
            ("start is not vertex of graph"));
    if (start == stop) { return; }
    AdjVertexIter it;
    for (Vertex * pred = Graph::get_first_in_vertex(start, it);
         pred != nullptr; pred = Graph::get_next_in_vertex(it)) {
        m_visited.append(pred->id());
        m_wl.append_tail(pred);
    }
}


void GraphIterIn::clean()
{
    m_wl.clean();
    m_visited.clean();
}


Vertex * GraphIterIn::get_first()
{
    return m_wl.remove_head();
}


Vertex * GraphIterIn::get_next(Vertex const* t)
{
    ASSERT0(t);
    if (t != m_stop) {
        AdjVertexIter it;
        for (Vertex * predv = Graph::get_first_in_vertex(t, it);
             predv != nullptr; predv = Graph::get_next_in_vertex(it)) {
            if (m_visited.find(predv->id())) { continue; }
            m_visited.append(predv->id());
            m_wl.append_tail(predv);
        }
    }
    return m_wl.remove_head();
}
//END GraphIterIn


//
//START GraphIterOut
//
GraphIterOut::GraphIterOut(Graph const& g, Vertex const* start) : m_g(g)
{
    ASSERTN(g.getVertex(start->id()) == start,
            ("start is not vertex of graph"));
    AdjVertexIter it;
    for (Vertex * succ = Graph::get_first_out_vertex(start, it);
         succ != nullptr; succ = Graph::get_next_out_vertex(it)) {
        m_visited.append(succ->id());
        m_wl.append_tail(succ);
    }
}


void GraphIterOut::clean()
{
    m_wl.clean();
    m_visited.clean();
}


Vertex * GraphIterOut::get_first()
{
    return m_wl.remove_head();
}


Vertex * GraphIterOut::get_next(Vertex const* t)
{
    ASSERT0(t);
    AdjVertexIter it;
    for (Vertex * succv = Graph::get_first_out_vertex(t, it);
         succv != nullptr; succv = Graph::get_next_out_vertex(it)) {
        if (m_visited.find(succv->id())) { continue; }
        m_visited.append(succv->id());
        m_wl.append_tail(succv);
    }
    return m_wl.remove_head();
}
//END GraphIterOut


//
//START GraphIterOutEdge
//
GraphIterOutEdge::GraphIterOutEdge(Graph const& g, Vertex const* start) : m_g(g)
{
    ASSERT0(start);
    ASSERTN(g.getVertex(start->id()) == start,
            ("start is not vertex of graph"));
    AdjEdgeIter it;
    for (Edge * e = Graph::get_first_out_edge(start, it);
         e != nullptr; e = Graph::get_next_out_edge(it)) {
        ASSERT0(e->from() == start);
        m_visited.append(e->to()->id());
        m_wl.append_tail(e);
    }
}


Edge * GraphIterOutEdge::get_first()
{
    return m_wl.remove_head();
}


Edge * GraphIterOutEdge::get_next(Edge const* e)
{
    ASSERT0(e);
    AdjEdgeIter it;
    for (Edge * oute = Graph::get_first_out_edge(e->to(), it);
         oute != nullptr; oute = Graph::get_next_out_edge(it)) {
        if (m_visited.find(oute->to()->id())) { continue; }
        m_visited.append(oute->to()->id());
        m_wl.append_tail(oute);
    }
    return m_wl.remove_head();
}
//END GraphIterOutEdge


//
//START MST
//
void MSTPath::dump(FILE * h, MST const& mst) const
{
    ASSERT0(h);
    MSTPathIter it;
    bool first = true;
    fprintf(h, "\n");
    for (Edge const* e = get_head(&it); e != nullptr; e = get_next(&it)) {
        if (!first) { fprintf(h, ","); }
        first = false;
        fprintf(h, "%u->%u(%u)",
                e->from()->id(), e->to()->id(), mst.getWeight(e));
    }
    fflush(h);
}


void MSTPath::dumpDOT(MST const& mst, CHAR const* name) const
{
    Graph g;
    g.set_direction(false);
    MSTPathIter it;
    for (Edge const* e = get_head(&it); e != nullptr; e = get_next(&it)) {
        Edge * newe = g.addEdge(e->from()->id(), e->to()->id());
        EDGE_info(newe) = (EdgeInfoType)(ULONGLONG)mst.getWeight(e);
    }
    g.dumpDOT(name);
}


Edge const* MST::findMinWeightEdge(
    Vertex const* v, TTab<Vertex const*> const& visited,
    OUT MSTWeightType & minw, OUT Vertex const** newvex)
{
    ASSERT0(v);
    MSTWeightType curw = getMaxWeight();
    Edge const* min = nullptr;
    AdjEdgeIter it;
    for (Edge const* out = Graph::get_first_out_edge(v, it);
         out != nullptr; out = Graph::get_next_out_edge(it)) {
        Vertex const* succ = out->to();
        if (visited.find(succ)) { continue; }
        MSTWeightType outew = getWeight(out);
        ASSERT0(outew != MST_WEIGHT_UNDEF);
        if (LessOrEqualThan(curw, outew)) { continue; }
        curw = outew;
        minw = outew;
        min = out;
        *newvex = succ;
    }
    for (Edge const* in = Graph::get_first_in_edge(v, it);
         in != nullptr; in = Graph::get_next_in_edge(it)) {
        Vertex const* pred = in->from();
        if (visited.find(pred)) { continue; }
        MSTWeightType inw = getWeight(in);
        ASSERT0(inw != MST_WEIGHT_UNDEF);
        if (LessOrEqualThan(curw, inw)) { continue; }
        curw = inw;
        minw = inw;
        min = in;
        *newvex = pred;
    }
    return min;
}


Edge const* MST::findMinWeightEdgeForVisited(
    MOD List<Vertex const*> & wl, TTab<Vertex const*> const& visited,
    OUT MSTWeightType & minw, OUT Vertex const** newvex)
{
    Edge const* cur_mine = nullptr;
    MSTWeightType cur_minw = getMaxWeight();
    List<Vertex const*>::Iter it;
    List<Vertex const*>::Iter nextit;
    for (wl.get_head(&it); it != nullptr; it = nextit) {
        nextit = wl.get_next(it);
        Vertex const* visited_vex = it->val();
        MSTWeightType lminw;
        Vertex const* lnewvex = nullptr;
        Edge const* lmine = findMinWeightEdge(
            visited_vex, visited, lminw, &lnewvex);
        if (lmine == nullptr) {
            //Meet exit vertex or all predecessors and successors
            //have been visited.
            wl.remove(it);
            continue;
        }
        if (LessOrEqualThan(cur_minw, lminw)) { continue; }
        cur_mine = lmine;
        cur_minw = lminw;
        ASSERT0(lnewvex);
        *newvex = lnewvex;
    }
    minw = cur_minw;
    return cur_mine;
}


MSTWeightType MST::build(Vertex const* root, OUT MSTPath & path)
{
    if (root == nullptr) { return MST_WEIGHT_UNDEF; }
    TTab<Vertex const*> visited;
    List<Vertex const*> wl;
    visited.append(root);
    wl.append_tail(root);
    path.clean();
    MSTWeightType tot_minw = MST_WEIGHT_UNDEF;
    for (;;) {
        MSTWeightType minw;
        Vertex const* newvex = nullptr;
        Edge const* mine = findMinWeightEdgeForVisited(
            wl, visited, minw, &newvex);
        if (mine == nullptr) {
            //Meet the exit of graph.
            break;
        }
        ASSERT0(newvex);
        ASSERT0(newvex == mine->from() || newvex == mine->to());
        ASSERT0(!visited.find(newvex));
        ASSERT0(!wl.find(newvex));
        wl.append_tail(newvex);
        visited.append(newvex);
        path.append_tail(mine);
        tot_minw += minw;
    }
    ASSERT0(tot_minw != MST_WEIGHT_UNDEF);
    return tot_minw;
}
//END MST

} //namespace xcom
