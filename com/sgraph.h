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
    Edge * prev; //used by FreeList and EdgeHash
    Edge * next; //used by FreeList and EdgeHash
    Vertex * _from;
    Vertex * _to;
    void * _info;
public:
    void init()
    { prev = NULL, next = NULL, _from = NULL, _to = NULL, _info = NULL; }
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
    UINT _rpo;
    void * _info;
public:
    void init()
    { prev = NULL, next = NULL, in_list = NULL, out_list = NULL,
      _id = VERTEX_UNDEF, _rpo = 0, _info = NULL; }
    UINT id() const { return VERTEX_id(this); }    

    EdgeC * getOutList() const { return VERTEX_out_list(this); }
    EdgeC * getInList() const { return VERTEX_in_list(this); }

    UINT rpo() const { return VERTEX_rpo(this); }
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
    void init() { next = NULL, prev = NULL, edge = NULL; }

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


class EdgeHash : public Hash<Edge*, EdgeHashFunc> {
    Graph * m_g;
public:
    EdgeHash(UINT bsize = 64) : Hash<Edge*, EdgeHashFunc>(bsize) {}
    virtual ~EdgeHash() {}

    void init(Graph * g, UINT bsize)
    {
        m_g = g;
        Hash<Edge*, EdgeHashFunc>::init(bsize);
    }

    void destroy()
    {
        m_g = NULL;
        Hash<Edge*, EdgeHashFunc>::destroy();
    }

    virtual Edge * create(OBJTY v);
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
        Vertex * vex =
            (Vertex*)smpoolMallocConstSize(sizeof(Vertex), m_ec_pool);
        ASSERT0(vex);
        ::memset(vex, 0, sizeof(Vertex));
        VERTEX_id(vex) = (UINT)(size_t)v;
        return vex;
    }
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
    friend class EdgeHash;
    friend class VertexHash;
protected:
    //it is true if the number of edges between any two
    //vertices are not more than one.
    BYTE m_is_unique:1;
    BYTE m_is_direction:1; //true if graph is direction.
    UINT m_edge_hash_size;
    UINT m_vex_hash_size;
    EdgeHash m_edges; //record all edges.
    VertexHash m_vertices; //record all vertices.
    FreeList<Edge> m_e_free_list; //record freed Edge for reuse.
    FreeList<EdgeC> m_el_free_list; //record freed EdgeC for reuse.
    FreeList<Vertex> m_v_free_list; //record freed Vertex for reuse.
    SMemPool * m_vertex_pool;
    SMemPool * m_edge_pool;
    SMemPool * m_ec_pool;

    //record vertex if vertex id is densen distribution.
    //map vertex id to vertex.
    Vector<Vertex*> * m_dense_vertex;

protected:
    //Add 'e' into out-edges of 'vex'
    inline void addOutList(Vertex * vex, Edge * e)
    {
        ASSERTN(m_ec_pool != NULL, ("not yet initialized."));
        ASSERT0(vex && e);
        EdgeC * last = NULL;
        for (EdgeC * el = VERTEX_out_list(vex); el != NULL; el = EC_next(el)) {
            last = el;
            if (EC_edge(el) == e) return;
        }
        ASSERT0(last == NULL || EC_next(last) == NULL);
        xcom::add_next(&VERTEX_out_list(vex), &last, newEdgeC(e));
    }

    //Add 'e' into in-edges of 'vex'
    inline void addInList(Vertex * vex, Edge * e)
    {
        ASSERTN(m_ec_pool, ("not yet initialized."));
        ASSERT0(vex && e);
        EdgeC * last = NULL;
        for (EdgeC * el = VERTEX_in_list(vex); el != NULL; el = EC_next(el)) {
            last = el;
            if (EC_edge(el) == e) { return; }
        }
        ASSERT0(last == NULL || EC_next(last) == NULL);
        xcom::add_next(&VERTEX_in_list(vex), &last, newEdgeC(e));
    }

    virtual void * cloneEdgeInfo(Edge*)
    { ASSERTN(0, ("Target Dependent Code")); return NULL; }

    virtual void * cloneVertexInfo(Vertex*)
    { ASSERTN(0, ("Target Dependent Code")); return NULL; }

    inline Vertex * newVertex()
    {
        Vertex * vex = (Vertex*)smpoolMallocConstSize(
            sizeof(Vertex), m_vertex_pool);
        ASSERT0(vex);
        ::memset(vex, 0, sizeof(Vertex));
        vex->init();
        return vex;
    }

    inline Edge * newEdge(UINT from, UINT to)
    {
        ASSERTN(m_ec_pool != NULL, ("not yet initialized."));
        Vertex * fp = addVertex(from);
        Vertex * tp = addVertex(to);
        return newEdge(fp, tp);
    }
    Edge * newEdge(Vertex * from, Vertex * to);
    Vertex * newVertex(UINT vid);

    inline Edge * newEdgeImpl(Vertex * from, Vertex * to)
    {
        Edge * e = m_e_free_list.get_free_elem();
        if (e == NULL) {
            e = (Edge*)smpoolMallocConstSize(sizeof(Edge), m_edge_pool);
            ::memset(e, 0, sizeof(Edge));
        }
        e->init();
        EDGE_from(e) = from;
        EDGE_to(e) = to;
        addInList(to, e);
        addOutList(from, e);
        return e;
    }

    inline EdgeC * newEdgeC(Edge * e)
    {
        ASSERTN(m_ec_pool != NULL, ("not yet initialized."));
        if (e == NULL) { return NULL; }

        EdgeC * el = m_el_free_list.get_free_elem();
        if (el == NULL) {
            el = (EdgeC*)smpoolMallocConstSize(sizeof(EdgeC), m_ec_pool);
            ::memset(el, 0, sizeof(EdgeC));
        }
        el->init();
        EC_edge(el) = e;
        return el;
    }
public:
    Graph(UINT edge_hash_size = 64, UINT vex_hash_size = 64);
    Graph(Graph const& g);
    Graph const& operator = (Graph const&);
    virtual ~Graph() { destroy(); }

    void init();
    void destroy();
    inline Edge * addEdge(UINT from, UINT to)
    {
        ASSERTN(m_ec_pool != NULL, ("not yet initialized."));
        return newEdge(from, to);
    }
    inline Edge * addEdge(Vertex * from, Vertex * to)
    {
        ASSERTN(m_ec_pool != NULL, ("not yet initialized."));
        return newEdge(from, to);
    }
    inline Vertex * addVertex(UINT vid)
    {
        ASSERTN(m_ec_pool != NULL, ("not yet initialized."));
        return m_vertices.append(newVertex(vid));
    }

    void computeRpoNoRecursive(IN OUT Vertex * root,
                               OUT List<Vertex const*> & vlst);
    bool clone(Graph const& src, bool clone_edge_info, bool clone_vex_info);
    //Count memory usage for current object.
    size_t count_mem() const;

    void dumpDOT(CHAR const* name = NULL) const;
    void dumpVCG(CHAR const* name = NULL) const;

    //Return true if graph vertex id is dense.
    bool is_dense() const { return m_dense_vertex != NULL; }
    //Return true if 'succ' is successor of 'v'.
    bool is_succ(Vertex * v, Vertex * succ) const
    {
        for (EdgeC * e = VERTEX_out_list(v); e != NULL; e = EC_next(e)) {
            if (EDGE_to(EC_edge(e)) == succ) {
                return true;
            }
        }
        return false;
    }

    //Return true if 'pred' is predecessor of 'v'.
    bool is_pred(Vertex * v, Vertex * pred) const
    {
        for (EdgeC * e = VERTEX_in_list(v); e != NULL; e = EC_next(e)) {
            if (e->getFrom() == pred) {
                return true;
            }
        }
        return false;
    }

    bool is_equal(Graph & g) const;
    bool is_unique() const { return m_is_unique; }
    bool is_direction() const { return m_is_direction; }

    //Is there exist a path connect 'from' and 'to'.
    bool is_reachable(UINT from, UINT to) const
    {
        ASSERTN(m_ec_pool != NULL, ("not yet initialized."));
        return is_reachable(getVertex(from), getVertex(to));
    }
    bool is_reachable(Vertex * from, Vertex * to) const;
    void insertVertexBetween(IN Vertex * v1,
                             IN Vertex * v2,
                             IN Vertex * newv,
                             OUT Edge ** e1 = NULL,
                             OUT Edge ** e2 = NULL,
                             bool sort = true);
    void insertVertexBetween(UINT v1,
                             UINT v2,
                             UINT newv,
                             OUT Edge ** e1 = NULL,
                             OUT Edge ** e2 = NULL,
                             bool sort = true);
    bool is_graph_entry(Vertex const* v) const
    { return VERTEX_in_list(v) == NULL; }

    //Return true if vertex is exit node of graph.
    bool is_graph_exit(Vertex const* v) const
    { return VERTEX_out_list(v) == NULL; }
    //Return true if In-Degree of 'vex' equal to 'num'.
    bool isInDegreeEqualTo(Vertex const* vex, UINT num) const;
    //Return true if Out-Degree of 'vex' equal to 'num'.
    bool isOutDegreeEqualTo(Vertex const* vex, UINT num) const;

    void erase();

    bool getNeighborList(IN OUT List<UINT> & ni_list, UINT vid) const;
    bool getNeighborSet(OUT DefSBitSet & niset, UINT vid) const;
    UINT getDegree(UINT vid) const
    {
        ASSERTN(m_ec_pool != NULL, ("not yet initialized."));
        return getDegree(getVertex(vid));
    }
    UINT getDegree(Vertex const* vex) const;
    UINT getInDegree(Vertex const* vex) const;
    UINT getOutDegree(Vertex const* vex) const;
    UINT getVertexNum() const
    {
        ASSERTN(m_ec_pool != NULL, ("not yet initialized."));
        return m_vertices.get_elem_count();
    }
    UINT getEdgeNum() const
    {
        ASSERTN(m_ec_pool != NULL, ("not yet initialized."));
        return m_edges.get_elem_count();
    }
    inline Vertex * getVertex(UINT vid) const
    {
        ASSERTN(m_ec_pool != NULL, ("not yet initialized."));
        if (m_dense_vertex != NULL) {
            return m_dense_vertex->get(vid);
        }
        return (Vertex*)m_vertices.find((OBJTY)(size_t)vid);
    }
    Edge * getEdge(UINT from, UINT to) const;
    Edge * getEdge(Vertex const* from, Vertex const* to) const;
    Edge * get_first_edge(INT & cur) const
    {
        ASSERTN(m_ec_pool != NULL, ("not yet initialized."));
        return m_edges.get_first(cur);
    }
    Edge * get_next_edge(INT & cur) const
    {
        ASSERTN(m_ec_pool != NULL, ("not yet initialized."));
        return m_edges.get_next(cur);
    }
    Vertex * get_first_vertex(INT & cur) const
    {
        ASSERTN(m_ec_pool != NULL, ("not yet initialized."));
        return m_vertices.get_first(cur);
    }
    Vertex * get_next_vertex(INT & cur) const
    {
        ASSERTN(m_ec_pool != NULL, ("not yet initialized."));
        return m_vertices.get_next(cur);
    }

    void resize(UINT vertex_hash_sz, UINT edge_hash_sz);
    Edge * reverseEdge(Edge * e); //Reverse edge direction.(e.g: a->b => b->a)
    void reverseEdges(); //Reverse all edges.
    Edge * removeEdge(Edge * e);
    void removeEdgeBetween(Vertex * v1, Vertex * v2);
    Vertex * removeVertex(Vertex * vex);
    Vertex * removeVertex(UINT vid)
    {
        ASSERTN(m_ec_pool != NULL, ("not yet initialized."));
        return removeVertex(getVertex(vid));
    }
    void removeTransitiveEdge();
    void removeTransitiveEdgeHelper(Vertex const* fromvex,
                                    Vector<DefSBitSetCore*> * reachset_vec,
                                    BitSet & is_visited,
                                    DefMiscBitSetMgr & bs_mgr);

    bool sortInTopologOrder(OUT Vector<Vertex*> & vex_vec);
    void set_unique(bool is_unique)
    {
        ASSERTN(m_ec_pool != NULL, ("not yet initialized."));
        m_is_unique = (BYTE)is_unique;
    }
    void set_direction(bool has_direction)
    {
        ASSERTN(m_ec_pool != NULL, ("not yet initialized."));
        m_is_direction = (BYTE)has_direction;
    }
    void set_dense(bool is_dense)
    {
        if (is_dense) {
            if (m_dense_vertex == NULL) {
                m_dense_vertex = new Vector<Vertex*>();
            }
            return;
        }
        if (m_dense_vertex == NULL) {
            delete m_dense_vertex;
            m_dense_vertex = NULL;
        }
    }
};


//This class indicate a Dominator Tree.
class DomTree : public Graph {
public:
    DomTree() { set_dense(true); }
};


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
public:
    DGraph(UINT edge_hash_size = 64, UINT vex_hash_size = 64);
    DGraph(DGraph const& g);
    DGraph const& operator = (DGraph const&);

    bool clone(DGraph const& g, bool clone_edge_info, bool clone_vex_info)
    {
        m_bs_mgr = g.m_bs_mgr;
        return Graph::clone(g, clone_edge_info, clone_vex_info);
    }
    bool cloneDomAndPdom(DGraph const& src);
    bool computeDom3(List<Vertex const*> const* vlst, BitSet const* uni);
    bool computeDom2(List<Vertex const*> const& vlst);
    bool computeDom(List<Vertex const*> const* vlst = NULL,
                    BitSet const* uni = NULL);
    bool computePdomByRpo(Vertex * root, BitSet const* uni);
    bool computePdom(List<Vertex const*> const* vlst);
    bool computePdom(List<Vertex const*> const* vlst, BitSet const* uni);
    bool computeIdom();
    bool computeIdom2(List<Vertex const*> const& vlst);
    bool computeIpdom();
    //Count memory usage for current object.
    size_t count_mem() const;

    void dump_dom(FILE * h, bool dump_dom_tree = true);

    //idom must be positive
    //NOTE: Entry does not have idom.
    //'id': vertex id.
    UINT get_idom(UINT id) const { return (UINT)m_idom_set.get(id); }

    //ipdom must be positive
    //NOTE: Exit does not have idom.
    //'id': vertex id.
    UINT get_ipdom(UINT id) const { return (UINT)m_ipdom_set.get(id); }

    void get_dom_tree(OUT Graph & dom);
    void get_pdom_tree(OUT Graph & pdom);

    BitSet const* read_dom_set(UINT id) const { return m_dom_set.get(id); }

    //Get vertices who dominate vertex 'id'.
    //NOTE: set does NOT include 'v' itself.
    inline BitSet * get_dom_set(UINT id)
    {
        ASSERT0(m_bs_mgr != NULL);
        BitSet * set = m_dom_set.get(id);
        if (set == NULL) {
            set = m_bs_mgr->create();
            m_dom_set.set(id, set);
        }
        return set;
    }

    //Get vertices who dominate vertex 'v'.
    //NOTE: set does NOT include 'v' itself.
    BitSet * get_dom_set(Vertex const* v)
    {
        ASSERT0(v != NULL);
        return get_dom_set(VERTEX_id(v));
    }

    BitSet const* read_pdom_set(UINT id) const { return m_pdom_set.get(id); }

    //Get vertices who post dominated by vertex 'id'.
    //NOTE: set does NOT include 'v' itself.
    inline BitSet * get_pdom_set(UINT id)
    {
        ASSERT0(m_bs_mgr != NULL);
        BitSet * set = m_pdom_set.get(id);
        if (set == NULL) {
            set = m_bs_mgr->create();
            m_pdom_set.set(id, set);
        }
        return set;
    }

    //Get vertices who post dominated by vertex 'v'.
    //NOTE: set does NOT include 'v' itself.
    BitSet * get_pdom_set(Vertex const* v)
    {
        ASSERT0(v != NULL);
        return get_pdom_set(VERTEX_id(v));
    }

    //Return true if 'v1' dominate 'v2'.
    bool is_dom(UINT v1, UINT v2) const
    {
        ASSERT0(read_dom_set(v2));
        return read_dom_set(v2)->is_contain(v1);
    }

    //Return true if 'v1' post dominate 'v2'.
    bool is_pdom(UINT v1, UINT v2) const
    {
        ASSERT0(read_pdom_set(v2));
        return read_pdom_set(v2)->is_contain(v1);
    }

    //Sort node on graph in bfs-order.
    void sortInBfsOrder(Vector<UINT> & order_buf,
                        Vertex * root,
                        BitSet & visit);
    //Sort node in dominator-tree in preorder.
    void sortDomTreeInPreorder(IN Vertex * root, OUT List<Vertex*> & lst);
    void sortDomTreeInPostrder(IN Vertex * root, OUT List<Vertex*> & lst);
    void setBitSetMgr(BitSetMgr * bs_mgr) { m_bs_mgr = bs_mgr; }
    bool removeUnreachNode(UINT entry_id);
};

} //namespace xcom
#endif
