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
#ifndef __GRAPH_H_
#define __GRAPH_H_

namespace xcom {

#define MAGIC_METHOD
#define VERTEX_UNDEF 0
#define RPO_INTERVAL 5
#define RPO_INIT_VAL 0
#define RPO_UNDEF -1

class Vertex;
class Edge;
class EdgeC;
class Graph;
class BMat;

#define EDGE_next(e) ((e)->next)
#define EDGE_prev(e) ((e)->prev)
#define EDGE_from(e) ((e)->_from)
#define EDGE_to(e) ((e)->_to)
#define EDGE_info(e) ((e)->_info)
class Edge {
public:
    Edge * prev; //used by FreeList
    Edge * next; //used by FreeList
    Vertex * _from;
    Vertex * _to;
    void * _info;
public:
    void copyEdgeInfo(Edge const* src) { EDGE_info(this) = src->info(); }

    void init()
    {
        prev = nullptr;
        next = nullptr;
        _from = nullptr;
        _to = nullptr;
        _info = nullptr;
    }
    void * info() const { return EDGE_info(this); }

    Vertex * from() const { return EDGE_from(this); }
    Vertex * to() const { return EDGE_to(this); }
};


#define VERTEX_next(v) ((v)->next)
#define VERTEX_prev(v) ((v)->prev)
#define VERTEX_id(v) ((v)->_id)
#define VERTEX_rpo(v) ((v)->_rpo)
#define VERTEX_in_list(v) ((v)->in_list)
#define VERTEX_out_list(v) ((v)->out_list)
#define VERTEX_info(v) ((v)->_info)
class Vertex {
public:
    Vertex * prev; //used by FreeList and HASH
    Vertex * next; //used by FreeList and HASH
    EdgeC * in_list; //incoming edge list
    EdgeC * out_list;//outgoing edge list
    UINT _id;
    INT _rpo;
    void * _info;
public:
    void init()
    { prev = nullptr, next = nullptr, in_list = nullptr, out_list = nullptr,
      _id = VERTEX_UNDEF, _rpo = RPO_UNDEF, _info = nullptr; }
    UINT id() const { return VERTEX_id(this); }
    void * info() const { return VERTEX_info(this); }

    UINT getInDegree() const;
    UINT getOutDegree() const;
    EdgeC * getOutList() const { return VERTEX_out_list(this); }
    EdgeC * getInList() const { return VERTEX_in_list(this); }

    //Return the vertex that is the source-vertex of Nth in-edge.
    Vertex const* getNthInVertex(UINT n) const;

    //Return the vertex that is the sink-vertex of Nth out-edge.
    Vertex const* getNthOutVertex(UINT n) const;

    INT rpo() const { return VERTEX_rpo(this); }
};


//The container of EDEG.
#define EC_next(el) ((el)->next)
#define EC_prev(el) ((el)->prev)
#define EC_edge(el) ((el)->edge)
class EdgeC {
public:
    EdgeC * next;
    EdgeC * prev;
    Edge * edge;
public:
    void init() { next = nullptr, prev = nullptr, edge = nullptr; }

    Vertex * getTo() const { return EDGE_to(EC_edge(this)); }
    UINT getToId() const { return VERTEX_id(EDGE_to(EC_edge(this))); }
    Vertex * getFrom() const { return EDGE_from(EC_edge(this)); }
    UINT getFromId() const { return VERTEX_id(EDGE_from(EC_edge(this))); }
    EdgeC * get_next() const { return EC_next(this); }
    EdgeC * get_prev() const { return EC_prev(this); }
    Edge * getEdge() const { return EC_edge(this); }
};


#define MAKE_VALUE(from, to) (((from)<<16)|(to))
class EdgeHashFunc {
public:
    UINT get_hash_value(Edge * e, UINT bs) const
    {
        //Note this function does not guarantee hash-value is unique.
        //This might cause much hash conflict accroding to hash
        //element value.
        ASSERT0(isPowerOf2(bs));
        return hash32bit(MAKE_VALUE(VERTEX_id(EDGE_from(e)),
                                    VERTEX_id(EDGE_to(e)))) & (bs - 1);
    }

    UINT get_hash_value(OBJTY val, UINT bs) const
    { return get_hash_value((Edge*)val, bs); }

    bool compare(Edge * e1, Edge * e2) const
    {
        return (VERTEX_id(EDGE_from(e1)) == VERTEX_id(EDGE_from(e2))) &&
               (VERTEX_id(EDGE_to(e1)) == VERTEX_id(EDGE_to(e2)));
    }

    bool compare(Edge * t1, OBJTY val) const
    {
        Edge * t2 = (Edge*)val;
        return VERTEX_id(EDGE_from(t1)) == VERTEX_id(EDGE_from(t2)) &&
               VERTEX_id(EDGE_to(t1)) == VERTEX_id(EDGE_to(t2));
    }
};


class CompareEdgeFunc {
public:
    Graph * m_g;
    EdgeC * m_inec; //only need in-ec for now.
public:
    //Return true if EdgeTab add a new Edge.
    bool isNewElem() const { return getInEC() != nullptr; }

    bool is_less(Edge const* e1, Edge const* e2) const
    {
        return (e1->from()->id() < e2->from()->id()) ||
               ((e1->from()->id() == e2->from()->id()) &&
                (e1->to()->id() < e2->to()->id()));
    }

    bool is_equ(Edge const* e1, Edge const* e2) const
    {
        return e1->from()->id() == e2->from()->id() &&
               e1->to()->id() == e2->to()->id();
    }

    void clean() { m_inec = nullptr; }
    Edge * createKey(Edge const* ref);
    EdgeC * getInEC() const { return m_inec; }
};


typedef TTabIter<Edge*> EdgeTabIter;

class EdgeTab : public TTab<Edge*, CompareEdgeFunc> {
    Graph * m_g;
public:
    EdgeTab(Graph * g) { init(g); }

    void init(Graph * g)
    {
        m_g = g;
        getCompareKeyObject()->m_g = g;
        TTab<Edge*, CompareEdgeFunc>::init();
    }

    void destroy()
    {
        m_g = nullptr;
        TTab<Edge*, CompareEdgeFunc>::destroy();
    }

    size_t count_mem() const
    { return sizeof(m_g) + TTab<Edge*, CompareEdgeFunc>::count_mem(); }
};


class VertexHashFunc {
public:
    UINT get_hash_value(OBJTY val, UINT bs) const
    {
        ASSERT0(isPowerOf2(bs));
        return hash32bit((UINT)(size_t)val) & (bs - 1);
    }

    UINT get_hash_value(Vertex const* vex, UINT bs) const
    {
        ASSERT0(isPowerOf2(bs));
        return hash32bit(VERTEX_id(vex)) & (bs - 1);
    }

    bool compare(Vertex * v1, Vertex * v2) const
    { return (VERTEX_id(v1) == VERTEX_id(v2)); }

    bool compare(Vertex * v1, OBJTY val) const
    { return (VERTEX_id(v1) == (UINT)(size_t)val); }
};


class VertexHash : public Hash<Vertex*, VertexHashFunc> {
    COPY_CONSTRUCTOR(VertexHash);
protected:
    SMemPool * m_ec_pool;
public:
    VertexHash(UINT bsize = 64) : Hash<Vertex*, VertexHashFunc>(bsize)
    { m_ec_pool = smpoolCreate(sizeof(Vertex) * 4, MEM_CONST_SIZE); }
    virtual ~VertexHash() { smpoolDelete(m_ec_pool); }

    virtual Vertex * create(OBJTY v)
    {
        Vertex * vex = (Vertex*)smpoolMallocConstSize(sizeof(Vertex),
                                                      m_ec_pool);
        ASSERT0(vex);
        ::memset(vex, 0, sizeof(Vertex));
        VERTEX_id(vex) = (UINT)(size_t)v;
        return vex;
    }
    size_t count_mem() const
    {
        return sizeof(m_ec_pool) + Hash<Vertex*, VertexHashFunc>::count_mem() +
               smpoolGetPoolSize(m_ec_pool);
    }
};

typedef EdgeTabIter EdgeIter;
typedef INT VertexIter;
typedef C<Vertex const*> * RPOVexListIter;
class RPOVexList : public List<Vertex const*> {
};

//A graph G = (V, E), consists of a set of vertices, V, and a set of edges, E.
//Each edge is a pair (v,w), where v,w belong to V. Edges are sometimes
//referred to as arcs. If the pair is ordered, then the graph is directed.
//Directed graphs are sometimes referred to as digraphs. Vertex w is adjacent
//to v if and only if (v,w) belong to E. In an undirected graph with edge (v,w),
//and hence (w,v), w is adjacent to v and v is adjacent to w.
//Sometimes an edge has a third component, known as either a weight or a cost.
//
//NOTICE: for accelerating perform operation of each vertex, e.g
//compute dominator, please try best to add vertex with
//topological order.
class Graph {
    friend class CompareEdgeFunc;
    friend class VertexHash;
protected:
    //it is true if the number of edges between any two
    //vertices are not more than one.
    BYTE m_is_unique:1;
    BYTE m_is_direction:1; //true if graph is direction.
    UINT m_edge_hash_size;
    UINT m_vex_hash_size;
    UINT m_dense_vex_num; //record the number of vertex in dense mode.
    //record vertex if vertex id is densen distribution.
    //map vertex id to vertex.
    Vector<Vertex*> * m_dense_vertex;
    VertexHash * m_sparse_vertex; //record all vertices.
    SMemPool * m_vertex_pool;
    SMemPool * m_edge_pool;
    SMemPool * m_ec_pool;
    EdgeTab m_edgetab; //record all edges.
    FreeList<Edge> m_e_free_list; //record freed Edge for reuse.
    FreeList<EdgeC> m_el_free_list; //record freed EdgeC for reuse.
    FreeList<Vertex> m_v_free_list; //record freed Vertex for reuse.

protected:
    //Add 'e' into out-edges of 'vex'
    inline EdgeC * addOutList(Vertex * vex, Edge * e)
    {
        ASSERTN(m_ec_pool != nullptr, ("not yet initialized."));
        ASSERT0(vex && e);
        EdgeC * last = nullptr;
        for (EdgeC * el = VERTEX_out_list(vex); el != nullptr; el = EC_next(el)) {
            last = el;
            if (EC_edge(el) == e) { return el; }
        }
        ASSERT0(last == nullptr || EC_next(last) == nullptr);
        EdgeC * ec = newEdgeC(e);
        xcom::add_next(&VERTEX_out_list(vex), &last, ec);
        return ec;
    }

    //Add 'e' into in-edges of 'vex'
    inline EdgeC * addInList(Vertex * vex, Edge * e)
    {
        ASSERTN(m_ec_pool, ("not yet initialized."));
        ASSERT0(vex && e);
        EdgeC * last = nullptr;
        for (EdgeC * el = VERTEX_in_list(vex); el != nullptr; el = EC_next(el)) {
            last = el;
            if (EC_edge(el) == e) { return el; }
        }
        ASSERT0(last == nullptr || EC_next(last) == nullptr);
        EdgeC * ec = newEdgeC(e);
        xcom::add_next(&VERTEX_in_list(vex), &last, ec);
        return ec;
    }

    virtual void * cloneEdgeInfo(Edge*)
    { ASSERTN(0, ("Target Dependent Code")); return nullptr; }

    virtual void * cloneVertexInfo(Vertex*)
    { ASSERTN(0, ("Target Dependent Code")); return nullptr; }

    inline Vertex * newVertex()
    {
        Vertex * vex = (Vertex*)smpoolMallocConstSize(sizeof(Vertex),
                                                      m_vertex_pool);
        ASSERT0(vex);
        ::memset(vex, 0, sizeof(Vertex));
        vex->init();
        return vex;
    }

    inline Edge * newEdge(UINT from, UINT to)
    {
        ASSERTN(m_ec_pool != nullptr, ("not yet initialized."));
        Vertex * fp = addVertex(from);
        Vertex * tp = addVertex(to);
        return newEdge(fp, tp);
    }
    Edge * newEdge(Vertex * from, Vertex * to);
    Vertex * newVertex(UINT vid);

    inline Edge * newEdgeImpl(Vertex * from, Vertex * to,
                              OUT EdgeC ** inec, OUT EdgeC ** outec)
    {
        Edge * e = m_e_free_list.get_free_elem();
        if (e == nullptr) {
            e = (Edge*)smpoolMallocConstSize(sizeof(Edge), m_edge_pool);
            ::memset(e, 0, sizeof(Edge));
        }
        e->init();
        EDGE_from(e) = from;
        EDGE_to(e) = to;
        ASSERT0(inec && outec);
        *inec = addInList(to, e);
        *outec = addOutList(from, e);
        return e;
    }

    inline EdgeC * newEdgeC(Edge * e)
    {
        ASSERTN(m_ec_pool != nullptr, ("not yet initialized."));
        if (e == nullptr) { return nullptr; }

        EdgeC * el = m_el_free_list.get_free_elem();
        if (el == nullptr) {
            el = (EdgeC*)smpoolMallocConstSize(sizeof(EdgeC), m_ec_pool);
            ::memset(el, 0, sizeof(EdgeC));
        }
        el->init();
        EC_edge(el) = e;
        return el;
    }

    void removeTransitiveEdgeHelper(Vertex const* fromvex,
                                    Vector<DefSBitSetCore*> * reachset_vec,
                                    BitSet & is_visited,
                                    DefMiscBitSetMgr & bs_mgr);
    Graph * self() { return this; }
public:
    Graph(UINT edge_hash_size = 64, UINT vex_hash_size = 64);
    Graph(Graph const& g);
    Graph const& operator = (Graph const&);
    virtual ~Graph() { destroy(); }

    void init();
    void destroy();
    inline Edge * addEdge(UINT from, UINT to)
    {
        ASSERTN(m_ec_pool != nullptr, ("not yet initialized."));
        return newEdge(from, to);
    }
    inline Edge * addEdge(Vertex * from, Vertex * to)
    {
        ASSERTN(m_ec_pool != nullptr, ("not yet initialized."));
        return newEdge(from, to);
    }
    //Add edge from->to, whereas 'from' is the nth predecessor of 'to'.
    //pos: the position of 'from' in predecessors of 'to', start at 0.
    //     For any new edge, the default position is the last of in/out list,
    //     thus the pos should not greater than the number of predecessors + 1.
    //     e.g: there are 2 predecessors of 'to', pos can not greater than 2.
    void addEdgeAtPos(List<UINT> const& fromlist, Vertex * to, UINT pos);
    inline Vertex * addVertex(UINT vid)
    {
        ASSERTN(m_ec_pool != nullptr, ("not yet initialized."));
        if (is_dense()) {
            Vertex * vex = m_dense_vertex->get(vid);
            if (vex != nullptr) {
                return vex;
            }
            vex = newVertex(vid);
            m_dense_vertex->set(vid, vex);
            m_dense_vex_num++;
            return vex;
        }

        Vertex * vex = m_sparse_vertex->find((OBJTY)(size_t)vid);
        if (vex != nullptr) {
            return vex;
        }
        return m_sparse_vertex->append(newVertex(vid));
    }

    //Sort vertice by RPO order, and update rpo of vertex.
    //Record sorted vertex into vlst in incremental order of RPO.
    //NOTE: rpo start at RPO_INIT_VAL.
    void computeRPONoRecursive(MOD Vertex * root, OUT RPOVexList & vlst) const;
    bool clone(Graph const& src, bool clone_edge_info, bool clone_vex_info);
    //Count memory usage for current object.
    size_t count_mem() const;

    virtual void dumpVertex(FILE * h, Vertex const* v) const;
    virtual void dumpEdge(FILE * h, Edge const* e) const;
    void dumpDOT(CHAR const* name = nullptr) const;
    void dumpVCG(CHAR const* name = nullptr) const;
    void dumpVexVector(Vector<Vertex*> const& vec, FILE * h);

    //Return true if graph vertex id is dense.
    bool is_dense() const { return m_dense_vertex != nullptr; }
    //Return true if 'succ' is successor of 'v'.
    bool is_succ(Vertex * v, Vertex * succ) const
    {
        for (EdgeC * e = VERTEX_out_list(v); e != nullptr; e = EC_next(e)) {
            if (EDGE_to(EC_edge(e)) == succ) {
                return true;
            }
        }
        return false;
    }

    //Return true if 'pred' is predecessor of 'v'.
    bool is_pred(Vertex const* v, Vertex const* pred) const
    {
        for (EdgeC const* e = VERTEX_in_list(v); e != nullptr; e = EC_next(e)) {
            if (e->getFrom() == pred) {
                return true;
            }
        }
        return false;
    }

    bool is_equal(Graph & g) const;
    bool is_unique() const { return m_is_unique; }
    bool is_direction() const { return m_is_direction; }

    //Return true if exist a path start from 'start' to 'v', which livein
    //from 'pred' vertex.
    //pred: the predecessor of v.
    bool is_livein_from(UINT v, UINT pred, UINT start) const
    {
        ASSERTN(m_ec_pool != nullptr, ("not yet initialized."));
        return is_livein_from(getVertex(v), getVertex(pred), getVertex(start));
    }
    bool is_livein_from(Vertex const* v, Vertex const* pred,
                        Vertex const* start) const;
    void insertVertexBetween(IN Vertex * v1,
                             IN Vertex * v2,
                             IN Vertex * newv,
                             OUT Edge ** e1 = nullptr,
                             OUT Edge ** e2 = nullptr,
                             bool sort = true);
    void insertVertexBetween(UINT v1,
                             UINT v2,
                             UINT newv,
                             OUT Edge ** e1 = nullptr,
                             OUT Edge ** e2 = nullptr,
                             bool sort = true);
    bool is_graph_entry(Vertex const* v) const
    { return VERTEX_in_list(v) == nullptr; }

    //Return true if vertex is exit node of graph.
    bool is_graph_exit(Vertex const* v) const
    { return VERTEX_out_list(v) == nullptr; }

    //Return true if In-Degree of 'vex' equal to 'num'.
    bool isInDegreeEqualTo(Vertex const* vex, UINT num) const;

    //Return true if Out-Degree of 'vex' equal to 'num'.
    bool isOutDegreeEqualTo(Vertex const* vex, UINT num) const;

    //Return true if vid indicates the graph vertex.
    bool isVertex(UINT vid) const { return getVertex(vid) != nullptr; }

    //Return true if vid indicates the graph vertex.
    bool isEdge(UINT from, UINT to) const
    { return getEdge(from, to) != nullptr; }

    //Return true if rpo is available to assign to a new vertex.
    //And the rpo will not repeat with other vertex.
    static bool isUsableRPO(INT rpo)
    { return rpo >= 0 && ((rpo % RPO_INTERVAL) != 0); }

    //Erasing graph, include all nodes and edges,
    //except for EdgeInfo and VertexInfo.
    void erase();

    bool getNeighborList(MOD List<UINT> & ni_list, UINT vid) const;
    bool getNeighborSet(OUT DefSBitSet & niset, UINT vid) const;
    UINT getDegree(UINT vid) const
    {
        ASSERTN(m_ec_pool != nullptr, ("not yet initialized."));
        return getDegree(getVertex(vid));
    }
    UINT getDegree(Vertex const* vex) const;
    UINT getVertexNum() const
    {
        ASSERTN(m_ec_pool != nullptr, ("not yet initialized."));
        if (is_dense()) {
            return m_dense_vex_num;
        }
        return m_sparse_vertex->get_elem_count();
    }
    UINT getEdgeNum() const
    {
        ASSERTN(m_ec_pool != nullptr, ("not yet initialized."));
        return m_edgetab.get_elem_count();
    }
    inline Vertex * getVertex(UINT vid) const
    {
        ASSERTN(m_ec_pool != nullptr, ("not yet initialized."));
        if (is_dense()) {
            return m_dense_vertex->get(vid);
        }
        return (Vertex*)m_sparse_vertex->find((OBJTY)(size_t)vid);
    }
    Edge * getEdge(UINT from, UINT to) const;
    Edge * getEdge(Vertex const* from, Vertex const* to) const;
    Edge * get_first_edge(EdgeIter & it) const
    {
        ASSERTN(m_ec_pool != nullptr, ("not yet initialized."));
        return m_edgetab.get_first(it);
    }
    Edge * get_next_edge(EdgeIter & it) const
    {
        ASSERTN(m_ec_pool != nullptr, ("not yet initialized."));
        return m_edgetab.get_next(it);
    }
    Vertex * get_first_vertex(VertexIter & cur) const;
    Vertex * get_next_vertex(VertexIter & cur) const;
    Vertex * get_last_vertex(VertexIter & cur) const;
    Vertex * get_prev_vertex(VertexIter & cur) const;

    void resize(UINT vertex_hash_sz, UINT edge_hash_sz);
    Edge * reverseEdge(Edge * e); //Reverse edge direction.(e.g: a->b => b->a)
    void reverseEdges(); //Reverse all edges.
    //pos_in_outlist: optional, record the position in outlist of 'from' of 'e'
    //pos_in_inlist: optional, record the position in inlist of 'to' of 'e'
    Edge * removeEdge(Edge * e, OUT UINT * pos_in_outlist = nullptr,
                      OUT UINT * pos_in_inlist = nullptr);
    void removeEdgeBetween(Vertex * v1, Vertex * v2);
    Vertex * removeVertex(Vertex * vex);
    Vertex * removeVertex(UINT vid)
    {
        ASSERTN(m_ec_pool != nullptr, ("not yet initialized."));
        return removeVertex(getVertex(vid));
    }
    void removeTransitiveEdge();

    //Replace orginal predecessor with a list of new source vertex.
    //Return the position of 'from' that is in the predecessor list of 'to'.
    //newfrom: record a list of new source vertex id.
    //from: original source vertex id
    //to: original target vertex id
    UINT replaceSource(UINT from, UINT to, List<UINT> const& newfrom);

    //Sort graph vertices in topological order.
    //vex_vec: record vertics in topological order.
    //Return true if sorting success, otherwise there exist cycles in graph.
    //Note you should NOT retrieve vertex in 'vex_vec' via vertex's index because
    //they are stored in dense manner.
    bool sortInTopologOrder(OUT Vector<Vertex*> & vex_vec);
    void set_unique(bool is_unique)
    {
        ASSERTN(m_ec_pool != nullptr, ("not yet initialized."));
        m_is_unique = (BYTE)is_unique;
    }
    void set_direction(bool has_direction)
    {
        ASSERTN(m_ec_pool != nullptr, ("not yet initialized."));
        m_is_direction = (BYTE)has_direction;
    }
    void set_dense(bool dense)
    {
        ASSERTN(m_ec_pool != nullptr, ("not yet initialized."));
        ASSERT0(getVertexNum() == 0);
        ASSERT0(m_dense_vertex || m_sparse_vertex);
        if (dense) {
            //Set to dense.
            if (m_dense_vertex == nullptr) {
                m_dense_vertex = new Vector<Vertex*>();
            }
            if (m_sparse_vertex != nullptr) {
                delete m_sparse_vertex;
                m_sparse_vertex = nullptr;
            }
            return;
        }

        //Set to sparse.
        if (m_sparse_vertex == nullptr) {
            m_sparse_vertex = new VertexHash(m_vex_hash_size);
        }
        if (m_dense_vertex != nullptr) {
            delete m_dense_vertex;
            m_dense_vertex = nullptr;
        }
    }

    //Return true if find an order of RPO for 'v'
    //that less than order of 'ref'.
    static bool tryFindLessRPO(Vertex * v, Vertex const* ref);

    //Compute which predecessor is pred_vex_id to 'vex'.
    //e.g: If pred_vex_id is the first predecessor, return 0.
    //pred_vex_id: vertex id of predecessor.
    //Note 'pred_vex_id' must be one of predecessors of 'vex'.
    static UINT WhichPred(UINT pred_vex_id, Vertex const* vex)
    {
        UINT n = 0;
        for (xcom::EdgeC const* c = vex->getInList();
             c != nullptr; c = c->get_next()) {
            xcom::Vertex const* in = c->getFrom();
            if (in->id() == pred_vex_id) {
                return n;
            }
            n++;
        }
        UNREACHABLE(); //pred_vex_id should be a predecessor of vex.
        return 0;
    }
};


//This class indicate a Dominator Tree.
class DomTree : public Graph {
public:
    DomTree() {}
    Vertex const* getParent(Vertex const* v) const
    {
        ASSERT0(getVertex(v->id()) == v);
        return v->getNthInVertex(0);
    }
};

typedef BitSet DomSet;

//
//Graph with Dominator info.
//
class DGraph : public Graph {
protected:
    Vector<BitSet*> m_dom_set; //record dominator-set of each vertex.
    Vector<BitSet*> m_pdom_set; //record post-dominator-set of each vertex.
    Vector<INT> m_idom_set; //immediate dominator.
    Vector<INT> m_ipdom_set; //immediate post dominator.
    BitSetMgr * m_bs_mgr;
    void _removeUnreachNode(UINT id, BitSet & visited);
    void freeDomPdomSet(UINT vid);
    bool verifyPdom(DGraph & g, List<Vertex const*> const& rpovlst) const;
    bool verifyDom(DGraph & g, List<Vertex const*> const& rpovlst) const;
public:
    DGraph(UINT edge_hash_size = 64, UINT vex_hash_size = 64);
    DGraph(DGraph const& g);
    DGraph const& operator = (DGraph const&);

    //The function adds Dom, Pdom, IDom, IPDom information for newidom, whereas
    //update the related info for 'vex'.
    //vex: a marker vertex.
    //newidom: the vertex that must be idom of 'vex'.
    void addDomInfoByNewIDom(Vertex const* vex, Vertex const* newidom);
    void addDomInfoByNewIDom(Vertex const* vex, UINT newidom)
    { addDomInfoByNewIDom(vex, addVertex(newidom)); }
    void addDomInfoByNewIDom(UINT vex, UINT newidom)
    { addDomInfoByNewIDom(getVertex(vex), addVertex(newidom)); }
    void addDomInfoByNewIPDom(Vertex const* vex, Vertex const* newipdom);
    void addDomInfoByNewIPDom(UINT vex, UINT newipdom)
    { addDomInfoByNewIPDom(getVertex(vex), addVertex(newipdom)); }

    //The function clones graph by given graph.
    //clone_edge_info: true to clone EdgeInfo of g.
    //clone_vex_info: true to clone VertexInfo of g.
    bool clone(DGraph const& g, bool clone_edge_info, bool clone_vex_info)
    {
        m_bs_mgr = g.m_bs_mgr;
        return Graph::clone(g, clone_edge_info, clone_vex_info);
    }

    //The function clones Dom and PDom info by given graph.
    bool cloneDomAndPdom(DGraph const& src);
    bool computeDom3(List<Vertex const*> const* vlst, DomSet const* uni);
    bool computeDom2(List<Vertex const*> const& vlst);
    bool computeDom(List<Vertex const*> const* vlst = nullptr,
                    DomSet const* uni = nullptr);
    bool computePdomByRPO(Vertex * root, DomSet const* uni);

    //Compute dominate vertex.
    bool computePdom(List<Vertex const*> const& vlst);

    //Compute dominate vertex.
    bool computePdom(List<Vertex const*> const& vlst, DomSet const* uni);

    //Compute immediate dominate vertex.
    bool computeIdom();

    //Compute immediate dominate vertex.
    bool computeIdom2(List<Vertex const*> const& vlst);

    //Compute immediate post dominate vertex.
    //NOTE: graph exit vertex does not have idom.
    bool computeIpdom();

    //Count memory usage for current object.
    size_t count_mem() const;

    //The function try to change the DOM info when given vertex has been
    //bypassed.
    //vex: the vertex id that will be bypassed.
    //e.g:
    //  pred->vex->succ
    //  where vex's idom is pred, vex's ipdom is succ, succ's idom is vex.
    //after bypassing,
    //  pred->vex->succ
    //   \         ^
    //    \_______/
    //  succ's dom become pred.
    bool changeDomInfoByAddBypassEdge(UINT vex);

    void dumpDom(FILE * h, bool dump_dom_tree = true) const;
    void dumpDom(CHAR const* name, bool dump_dom_tree = true) const;

    //Note graph entry node does not have idom.
    //id: vertex id.
    UINT get_idom(UINT id) const { return (UINT)m_idom_set.get(id); }

    //Note graph exit node does not have ipdom.
    //id: vertex id.
    UINT get_ipdom(UINT id) const { return (UINT)m_ipdom_set.get(id); }

    //dt: generate dominator tree and record in it.
    void genDomTree(OUT DomTree & dt) const;
    //pdt: generate post-dominator tree and record in it.
    void genPDomTree(OUT DomTree & pdt) const;

    DomSet const* read_dom_set(UINT id) const { return m_dom_set.get(id); }

    //Get vertices who dominate vertex 'id'.
    //NOTE: set does NOT include 'v' itself.
    inline DomSet * get_dom_set(UINT id)
    {
        ASSERT0(m_bs_mgr != nullptr);
        DomSet * set = m_dom_set.get(id);
        if (set == nullptr) {
            set = m_bs_mgr->create();
            m_dom_set.set(id, set);
        }
        return set;
    }

    //Get vertices who dominate vertex 'v'.
    //NOTE: set does NOT include 'v' itself.
    DomSet * get_dom_set(Vertex const* v)
    {
        ASSERT0(v != nullptr);
        return get_dom_set(VERTEX_id(v));
    }

    DomSet const* read_pdom_set(UINT id) const { return m_pdom_set.get(id); }

    //Get vertices who post dominated by vertex 'id'.
    //NOTE: set does NOT include 'v' itself.
    inline DomSet * get_pdom_set(UINT id)
    {
        ASSERT0(m_bs_mgr != nullptr);
        DomSet * set = m_pdom_set.get(id);
        if (set == nullptr) {
            set = m_bs_mgr->create();
            m_pdom_set.set(id, set);
        }
        return set;
    }

    //Get vertices who post dominated by vertex 'v'.
    //NOTE: set does NOT include 'v' itself.
    DomSet * get_pdom_set(Vertex const* v)
    {
        ASSERT0(v != nullptr);
        return get_pdom_set(VERTEX_id(v));
    }

    //Return true if 'v1' dominate 'v2'.
    bool is_dom(UINT v1, UINT v2) const
    {
        ASSERTN(read_dom_set(v2), ("no DOM info about vertex%d", v2));
        return read_dom_set(v2)->is_contain(v1);
    }

    //Return true if 'v1' post dominate 'v2'.
    bool is_pdom(UINT v1, UINT v2) const
    {
        ASSERTN(read_pdom_set(v2), ("no PDOM info about vertex%d", v2));
        return read_pdom_set(v2)->is_contain(v1);
    }

    //Sort node on graph in bfs-order.
    void sortInBfsOrder(Vector<UINT> & order_buf, Vertex * root,
                        BitSet & visit);

    //Sort node in dominator-tree in preorder.
    void sortDomTreeInPreorder(IN Vertex * root, OUT List<Vertex*> & lst);
    void sortDomTreeInPostrder(IN Vertex * root, OUT List<Vertex*> & lst);

    //Sort in-edge of vex in given order.
    //order: record the given order of each predecessor. Note the number
    //       of elements have to equal to the number of predecessor of vex.
    void sortPred(MOD Vertex * vex, Vector<UINT> const& order);
    void setBitSetMgr(BitSetMgr * bs_mgr) { m_bs_mgr = bs_mgr; }
    void set_idom(UINT vid, UINT idom) { m_idom_set.set(vid, idom); }
    void set_ipdom(UINT vid, UINT ipdom) { m_ipdom_set.set(vid, ipdom); }

    bool removeUnreachNode(UINT entry_id);
    void removeDomInfo(Vertex const* vex);
    void removeDomInfo(UINT vex) { removeDomInfo(getVertex(vex)); }

    bool verifyDom() const;
    bool verifyPdom() const;
    bool verifyDomAndPdom() const;
};

} //namespace xcom
#endif
