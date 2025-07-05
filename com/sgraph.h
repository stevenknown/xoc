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

#define HEIGHT_UNDEF ((VexIdx)-1)
//The height of root is 0.
#define HEIGHT_INIT_VAL 0

#define GRAPH_REACHIN_MAX_TRY_LIMIT ((UINT)-1)

class Vertex;
class Edge;
class EdgeC;
class Graph;
class BMat;
class DomTree;
class MST;

//Adjacent Vertex Iterator.
typedef EdgeC const* AdjVertexIter;
typedef EdgeC const* AdjVertexIter;

//Adjacent Vertex Iterator.
typedef EdgeC const* AdjEdgeIter;
typedef EdgeC const* AdjEdgeIter;
typedef void* EdgeInfoType;
typedef void* VertexInfoType;

#define EDGE_next(e) ((e)->next)
#define EDGE_prev(e) ((e)->prev)
#define EDGE_from(e) ((e)->m_from)
#define EDGE_to(e) ((e)->m_to)
#define EDGE_info(e) ((e)->m_info)
class Edge {
public:
    Edge * prev; //Only used by FreeList
    Edge * next; //Only used by FreeList
    Vertex * m_from; //Describes the 'from' vertex of the edge.
    Vertex * m_to; //Describes the 'to' vertex of the edge.
    EdgeInfoType m_info; //Records the auxiliary info that attached by user.
public:
    void copyEdgeInfo(Edge const* src) { EDGE_info(this) = src->info(); }

    Edge * get_next() const { return EDGE_next(this); }
    Edge * get_prev() const { return EDGE_prev(this); }

    void init()
    {
        prev = nullptr;
        next = nullptr;
        m_from = nullptr;
        m_to = nullptr;
        m_info = nullptr;
    }
    void * info() const { return EDGE_info(this); }

    Vertex * from() const { return EDGE_from(this); }
    Vertex * to() const { return EDGE_to(this); }
};

typedef UINT VexIdx;
typedef EdgeC const* VexIter;

#define VERTEX_next(v) ((v)->next)
#define VERTEX_prev(v) ((v)->prev)
#define VERTEX_id(v) ((v)->m_id)
#define VERTEX_rpo(v) ((v)->m_rpo)
#define VERTEX_height(v) ((v)->m_height)
#define VERTEX_in_list(v) ((v)->m_in_list)
#define VERTEX_out_list(v) ((v)->m_out_list)
#define VERTEX_info(v) ((v)->m_info)
class Vertex {
public:
    VexIdx m_id; //Vertex id must be unique.
    VexIdx m_height; //Only useful if current vertex is a tree node.
    RPOVal m_rpo; //Describes RPO of the vertex.
    Vertex * prev; //Only can be used by FreeList and HASH.
    Vertex * next; //Only can be used by FreeList and HASH.
    EdgeC * m_in_list; //Describes incoming edge list.
    EdgeC * m_out_list; //Describes outgoing edge list.
    VertexInfoType m_info; //Records the auxiliary info that attached by user.
public:
    void clone(Vertex const* src)
    { m_height = src->m_height; m_rpo = src->m_rpo; }

    void init()
    {
        prev = nullptr;
        next = nullptr;
        m_in_list = nullptr;
        m_out_list = nullptr;
        m_id = VERTEX_UNDEF;
        VERTEX_rpo(this) = RPO_UNDEF;
        VERTEX_height(this) = HEIGHT_UNDEF;
        m_info = nullptr;
    }
    VexIdx id() const { return VERTEX_id(this); }
    void * info() const { return VERTEX_info(this); }

    UINT getInDegree() const { return xcom::cnt_list(VERTEX_in_list(this)); }
    UINT getOutDegree() const { return xcom::cnt_list(VERTEX_out_list(this)); }
    EdgeC * getOutList() const { return VERTEX_out_list(this); }
    EdgeC * getInList() const { return VERTEX_in_list(this); }
    Vertex * getFirstToVex(OUT VexIter * it) const;
    Vertex * getFirstFromVex(OUT VexIter * it) const;
    Vertex * getNextToVex(OUT VexIter * it) const;
    Vertex * getNextFromVex(OUT VexIter * it) const;

    //Return the vertex that is the source-vertex of Nth in-edge.
    Vertex * getNthInVertex(UINT n) const;

    //Return the vertex that is the sink-vertex of Nth out-edge.
    Vertex * getNthOutVertex(UINT n) const;

    //Return the height if current vertex is regarded as tree node.
    VexIdx getHeight() const { return VERTEX_height(this); }

    //Return RPO.
    RPOVal rpo() const { return VERTEX_rpo(this); }
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
    VexIdx getToId() const { return VERTEX_id(EDGE_to(EC_edge(this))); }
    Vertex * getFrom() const { return EDGE_from(EC_edge(this)); }
    VexIdx getFromId() const { return VERTEX_id(EDGE_from(EC_edge(this))); }
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
        //This might cause much hash conflict according to hash
        //element value.
        ASSERT0(isPowerOf2(bs));
        return hash32bit((UINT32)MAKE_VALUE(e->from()->id(), e->to()->id()))
               & (bs - 1);
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
        return hash32bit((UINT32)vex->id()) & (bs - 1);
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
        Vertex * vex = (Vertex*)smpoolMallocConstSize(
            sizeof(Vertex), m_ec_pool);
        ASSERT0(vex);
        ::memset((void*)vex, 0, sizeof(Vertex));
        VERTEX_id(vex) = (VexIdx)(size_t)v;
        return vex;
    }
    size_t count_mem() const
    {
        return sizeof(m_ec_pool) + Hash<Vertex*, VertexHashFunc>::count_mem() +
               smpoolGetPoolSize(m_ec_pool);
    }
};

typedef EdgeTabIter EdgeIter;
typedef VecIdx VertexIter;

typedef TTabIter<VexIdx> VexTabIter;
class VexTab : public TTab<VexIdx> {
public:
    void add(Vertex const* v) { append(v->id()); }
    void add(VexTab const& src)
    {
        VexTabIter it;
        for (VexIdx t = src.get_first(it);
             t != VERTEX_UNDEF; t = src.get_next(it)) {
            append(t);
        }
    }
    void dump(FILE * h) const;
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
    //It is true if the number of edges between any two vertices are
    //not more than one.
    bool m_is_unique;
    bool m_is_direction; //true if graph is direction.
    UINT m_vex_hash_size;
    UINT m_dense_vex_num; //record the number of vertex in dense mode.

    //Record vertex if vertex id is densen distribution.
    //Map vertex id to vertex.
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
        for (EdgeC * el = vex->getOutList(); el != nullptr; el = EC_next(el)) {
            last = el;
            if (EC_edge(el) == e) { return el; }
        }
        ASSERT0(last == nullptr || EC_next(last) == nullptr);
        EdgeC * ec = allocEdgeC(e);
        xcom::add_next(&VERTEX_out_list(vex), &last, ec);
        return ec;
    }

    //Add 'e' into in-edges of 'vex'
    inline EdgeC * addInList(Vertex * vex, Edge * e)
    {
        ASSERTN(m_ec_pool, ("not yet initialized."));
        ASSERT0(vex && e);
        EdgeC * last = nullptr;
        for (EdgeC * el = vex->getInList(); el != nullptr; el = EC_next(el)) {
            last = el;
            if (EC_edge(el) == e) { return el; }
        }
        ASSERT0(last == nullptr || EC_next(last) == nullptr);
        EdgeC * ec = allocEdgeC(e);
        xcom::add_next(&VERTEX_in_list(vex), &last, ec);
        return ec;
    }

    //Allocate a vertex and assign 'vid' to it.
    //Note the vertex id must be unqiue.
    Vertex * allocVertex(VexIdx vid);
    Vertex * allocVertex()
    {
        Vertex * vex = (Vertex*)smpoolMallocConstSize(
            sizeof(Vertex), m_vertex_pool);
        ASSERT0(vex);
        ::memset((void*)vex, 0, sizeof(Vertex));
        vex->init();
        return vex;
    }
    Edge * allocEdgeImpl(Vertex * from, Vertex * to,
                          OUT EdgeC ** inec, OUT EdgeC ** outec)
    {
        Edge * e = m_e_free_list.get_free_elem();
        if (e == nullptr) {
            e = (Edge*)smpoolMallocConstSize(sizeof(Edge), m_edge_pool);
            ::memset((void*)e, 0, sizeof(Edge));
        }
        e->init();
        EDGE_from(e) = from;
        EDGE_to(e) = to;
        ASSERT0(inec && outec);
        *inec = addInList(to, e);
        *outec = addOutList(from, e);
        return e;
    }
    Edge * allocEdge(VexIdx from, VexIdx to)
    {
        ASSERTN(m_ec_pool != nullptr, ("not yet initialized."));
        Vertex * fp = addVertex(from);
        Vertex * tp = addVertex(to);
        return allocEdge(fp, tp);
    }
    Edge * allocEdge(Vertex * from, Vertex * to);
    inline EdgeC * allocEdgeC(Edge * e)
    {
        ASSERTN(m_ec_pool != nullptr, ("not yet initialized."));
        if (e == nullptr) { return nullptr; }

        EdgeC * el = m_el_free_list.get_free_elem();
        if (el == nullptr) {
            el = (EdgeC*)smpoolMallocConstSize(sizeof(EdgeC), m_ec_pool);
            ::memset((void*)el, 0, sizeof(EdgeC));
        }
        el->init();
        EC_edge(el) = e;
        return el;
    }

    virtual void * cloneEdgeInfo(Edge*)
    { ASSERTN(0, ("Target Dependent Code")); return nullptr; }

    virtual void * cloneVertexInfo(Vertex*)
    { ASSERTN(0, ("Target Dependent Code")); return nullptr; }

    void removeTransitiveEdgeHelper(
        Vertex const* fromvex, Vector<DefSBitSetCore*> * reachset_vec,
        BitSet & is_visited, DefMiscBitSetMgr & bs_mgr);

    Graph * self() { return this; }
public:
    Graph(UINT vex_hash_size = 64);
    Graph(Graph const& g);
    Graph const& operator = (Graph const&);
    virtual ~Graph() { destroy(); }

    void init();
    void destroy();
    inline Edge * addEdge(VexIdx from, VexIdx to)
    {
        ASSERTN(m_ec_pool != nullptr, ("not yet initialized."));
        return allocEdge(from, to);
    }
    inline Edge * addEdge(Vertex * from, Vertex * to)
    {
        ASSERTN(m_ec_pool != nullptr, ("not yet initialized."));
        return allocEdge(from, to);
    }
    //Add edge from->to, whereas 'from' is the nth predecessor of 'to'.
    //pos: the position of 'from' in predecessors of 'to', start at 0.
    //     For any new edge, the default position is the last of in/out list,
    //     thus the pos should not greater than the number of predecessors + 1.
    //     e.g: there are 2 predecessors of 'to', pos can not greater than 2.
    void addEdgeAtPos(List<VexIdx> const& fromlist, Vertex * to, UINT pos);
    inline Vertex * addVertex(VexIdx vid)
    {
        ASSERTN(m_ec_pool != nullptr, ("not yet initialized."));
        if (is_dense()) {
            Vertex * vex = m_dense_vertex->get((VecIdx)vid);
            if (vex != nullptr) {
                return vex;
            }
            vex = allocVertex(vid);
            m_dense_vertex->set((VecIdx)vid, vex);
            m_dense_vex_num++;
            return vex;
        }

        Vertex * vex = m_sparse_vertex->find((OBJTY)(size_t)vid);
        if (vex != nullptr) {
            return vex;
        }
        return m_sparse_vertex->append(allocVertex(vid));
    }

    void clone(Graph const& src, bool clone_edge_info, bool clone_vex_info);

    //Count memory usage for current object.
    size_t count_mem() const;

    //Dump height for all vertices.
    //NOTE: the graph must be regarded as a tree.
    void dumpHeight(FILE * h) const;

    //Dump all vertices into given buffer.
    CHAR const* dumpAllVertices(OUT DefFixedStrBuf & buf) const;

    //Dump all vertices into given file handler.
    void dumpAllVertices(FILE * h) const;

    //Dump all edges into given buffer.
    CHAR const* dumpAllEdges(OUT DefFixedStrBuf & buf) const;

    //Dump all edges into given file handler.
    void dumpAllEdges(FILE * h) const;

    //Dump vertex auxiliaries of vertex.
    virtual void dumpVertexAux(Vertex const* v, OUT DefFixedStrBuf & buf) const;

    //User interface.
    //Dump the description of given vertex.
    //e.g: a vertex's description could just be the vertex's id.
    virtual void dumpVertexDesc(
        Vertex const* v, OUT DefFixedStrBuf & buf) const;

    //User interface.
    //Dump the description of given edge.
    //e.g: a edge's description could just be the edge's info.
    virtual void dumpEdgeDesc(
        xcom::Edge const* e, OUT DefFixedStrBuf & buf) const;

    //Dump vertex into buffer.
    CHAR const* dumpVertex(Vertex const* v, OUT DefFixedStrBuf & buf) const;

    //Dump a vector of vertex into file handler.
    void dumpVexVector(Vector<Vertex*> const& vec, FILE * h);

    //Dump vertex into file handler.
    virtual void dumpVertex(FILE * h, Vertex const* v) const;

    //Dump edge into buffer.
    CHAR const* dumpEdge(Edge const* e, OUT DefFixedStrBuf & buf) const;

    //Dump edge into file handler.
    virtual void dumpEdge(FILE * h, Edge const* e) const;

    //Dump graph to PDF file scripts.
    void dumpDOT(CHAR const* name = nullptr) const;

    //Dump graph to VCG file scripts.
    void dumpVCG(CHAR const* name = nullptr) const;

    //Return true if graph vertex id is dense.
    bool is_dense() const { return m_dense_vertex != nullptr; }

    //Return true if 'succ' is successor of 'v'.
    bool is_succ(Vertex const* v, Vertex const* succ) const
    {
        ASSERT0(v && succ);
        for (EdgeC const* e = v->getOutList();
             e != nullptr; e = e->get_next()) {
            if (e->getTo() == succ) {
                return true;
            }
        }
        return false;
    }

    //Return true if 'succ' is the unqiue successor of 'v'.
    bool is_unique_succ(Vertex const* v, Vertex const* succ) const
    {
        ASSERT0(v && succ);
        EdgeC const* e = v->getOutList();
        if (e == nullptr || e->getTo() != succ || e->get_next() != nullptr) {
            return false;
        }
        return true;
    }

    //Return true if 'pred' is predecessor of 'v'.
    bool is_pred(Vertex const* v, Vertex const* pred) const
    {
        ASSERT0(v && pred);
        for (EdgeC const* e = v->getInList();
             e != nullptr; e = e->get_next()) {
            if (e->getFrom() == pred) {
                return true;
            }
        }
        return false;
    }

    //Return true if 'pred' is the unqiue predecessor of 'v'.
    bool is_unique_pred(Vertex const* v, Vertex const* pred) const
    {
        ASSERT0(v && pred);
        EdgeC const* e = v->getInList();
        if (e == nullptr || e->getFrom() != pred || e->get_next() != nullptr) {
            return false;
        }
        return true;
    }
    bool is_equal(Graph & g) const;
    bool is_unique() const { return m_is_unique; }
    bool is_direction() const { return m_is_direction; }

    //Return true if exist a path start from 'start' to 'v', which livein
    //from 'pred' vertex.
    //pred: the predecessor of v.
    bool is_livein_from(VexIdx v, VexIdx pred, VexIdx start) const
    {
        ASSERTN(m_ec_pool != nullptr, ("not yet initialized."));
        return is_livein_from(getVertex(v), getVertex(pred), getVertex(start));
    }
    bool is_livein_from(Vertex const* v, Vertex const* pred,
                        Vertex const* start) const;

    //The function inserts vertex 'newv' between edge 'v1'<->'v2'.
    //NOTE: the newv should have been added to current graph.
    void insertVertexBetween(
        IN Vertex * v1, IN Vertex * v2, IN Vertex * newv,
        OUT Edge ** e1 = nullptr, OUT Edge ** e2 = nullptr, bool sort = true);

    //The function inserts vertex 'newv' between edge 'v1'<->'v2'.
    //The function will add vertex if there is no 'newv' before.
    void insertVertexBetween(
        VexIdx v1, VexIdx v2, VexIdx newv, OUT Edge ** e1 = nullptr,
        OUT Edge ** e2 = nullptr, bool sort = true);

    //Return true if 'v' is the entry of current graph.
    bool is_graph_entry(Vertex const* v) const
    { ASSERT0(v); return v->getInList() == nullptr; }

    //Return true if vertex is the exit of current graph.
    bool is_graph_exit(Vertex const* v) const
    { ASSERT0(v); return v->getOutList() == nullptr; }

    //Return true if In-Degree of 'vex' equal to 'num'.
    bool isInDegreeEqualTo(Vertex const* vex, UINT num) const;

    //Return true if Out-Degree of 'vex' equal to 'num'.
    bool isOutDegreeEqualTo(Vertex const* vex, UINT num) const;

    //Return true if In-Degree of 'vex' is more than 'num'.
    //e.g: given in-degree of vex is 2, if num is 0,1,2 return false,
    //otherwise return true.
    bool isInDegreeMoreThan(Vertex const* vex, UINT num) const;

    //Return true if Out-Degree of 'vex' is more than 'num'.
    //e.g: given out-degree of vex is 2, if num is 0,1,2 return false,
    //otherwise return true.
    bool isOutDegreeMoreThan(Vertex const* vex, UINT num) const;

    //Return true if vid indicates the graph vertex.
    bool isVertex(VexIdx vid) const { return getVertex(vid) != nullptr; }

    //Return true if v is the vertex that allocated in current graph.
    bool isVertex(Vertex const* v) const { return getVertex(v->id()) == v; }

    //Return true if there is at least an edge that from 'from' to 'to'.
    bool isEdge(VexIdx from, VexIdx to) const
    { return getEdge(from, to) != nullptr; }

    //Return true if there is at least an edge that from 'from' to 'to'.
    bool isEdge(Vertex const* from, Vertex const* to) const
    { return isEdge(from->id(), to->id()); }

    //The function try to answer whether 'reachin' can reach 'vex' from one of
    //in-vertexs of 'vex'. Return false if it is not or unknown.
    //e.g:a      b->c
    //    |__   _|
    //       | |
    //       V V
    //        d
    //Suppose 'vex' is d, if 'reachin' is a OR b, the function return true.
    //and if 'reachin' is c, the function return false.
    //
    //try_limit: the maximum time to try.
    //try_failed: return true if the function running exceed the try_limit.
    //terminate_Vex_tab: optional, if it is not NULL, it contains the
    //  boundary vertex, that the function will stop processing when meeting
    //  the boundary vertex.
    //  e.g:a->b->c
    //            |
    //       -----
    //      |
    //      V
    //      d---->e
    //  if 'vex' is d, 'reachin' is a, terminate_vex_tab contains c, there is
    //  nothing to do in the function, and the function return false.
    static bool isReachIn(
        Vertex const* vex, Vertex const* reachin, UINT try_limit,
        OUT bool & try_failed, VexTab const* terminate_vex_tab = nullptr);

    //Return true if vex can reach graph exit.
    //Note the function is costly and use with caution.
    //try_limit: the maximum time to try.
    //try_failed: return true if the function running exceed the try_limit.
    bool isReachExit(Vertex const* vex, UINT try_limit, OUT bool & try_failed);

    //Erasing graph, include all nodes and edges,
    //except for EdgeInfo and VertexInfo.
    void erase();

    bool getNeighborList(MOD List<VexIdx> & ni_list, VexIdx vid) const;
    bool getNeighborSet(OUT DefSBitSet & niset, VexIdx vid) const;
    UINT getDegree(VexIdx vid) const
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
    inline Vertex * getVertex(VexIdx vid) const
    {
        ASSERTN(m_ec_pool != nullptr, ("not yet initialized."));
        ASSERT0(vid != VERTEX_UNDEF);
        if (is_dense()) {
            return m_dense_vertex->get((VecIdx)vid);
        }
        return (Vertex*)m_sparse_vertex->find((OBJTY)(size_t)vid);
    }
    Edge * getEdge(VexIdx from, VexIdx to) const;
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
    Vertex * get_first_vertex(VertexIter & it) const;
    Vertex * get_next_vertex(VertexIter & it) const;
    Vertex * get_last_vertex(VertexIter & it) const;
    Vertex * get_prev_vertex(VertexIter & it) const;

    //The function will iterate all predecessors vertex of 'vex'.
    static Vertex * get_first_in_vertex(Vertex const* vex, AdjVertexIter & it);
    static Vertex * get_next_in_vertex(AdjVertexIter & it);

    //The function will iterate all successors vertex of 'vex'.
    static Vertex * get_first_out_vertex(Vertex const* vex, AdjVertexIter & it);
    static Vertex * get_next_out_vertex(AdjVertexIter & it);

    //The function will iterate all out-edge of successors of 'vex'.
    static Edge * get_first_out_edge(Vertex const* vex, AdjEdgeIter & it);
    static Edge * get_next_out_edge(AdjEdgeIter & it);

    //The function will iterate all in-edge of predecessors of 'vex'.
    static Edge * get_first_in_edge(Vertex const* vex, AdjEdgeIter & it);
    static Edge * get_next_in_edge(AdjEdgeIter & it);

    //Reconstruct vertex hash table, and edge hash table with new bucket size.
    //vertex_hash_sz: new vertex table size to be resized.
    //NOTE:
    //  1. mem pool should have been initialized
    //  2. Graph should be erased before resize.
    void resize(UINT vertex_hash_sz);
    Edge * reverseEdge(Edge * e); //Reverse edge direction.(e.g: a->b => b->a)
    void reverseEdges(); //Reverse all edges.
    //pos_in_outlist: optional, record the position in outlist of 'from' of 'e'
    //pos_in_inlist: optional, record the position in inlist of 'to' of 'e'
    Edge * removeEdge(Edge * e, OUT UINT * pos_in_outlist = nullptr,
                      OUT UINT * pos_in_inlist = nullptr);
    void removeEdgeBetween(Vertex * v1, Vertex * v2);
    Vertex * removeVertex(Vertex * vex);
    Vertex * removeVertex(VexIdx vid)
    {
        ASSERTN(m_ec_pool != nullptr, ("not yet initialized."));
        return removeVertex(getVertex(vid));
    }
    bool removeTransitiveEdge();

    //Replace orginal in-vertex with a list of new vertex.
    //Return the position of 'from' that is in the in-vertex list of 'to'.
    //newin: record a list of new vertex id.
    //from: original in-vertex id
    //to: original out-vertex id
    VexIdx replaceInVertex(VexIdx from, VexIdx to, List<VexIdx> const& newin);

    //Sort graph vertices in topological order.
    //vex_vec: record vertics in topological order.
    //Return true if sorting success, otherwise return false which means there
    //exist cycles in current graph.
    //Note you should NOT retrieve vertex in 'vex_vec' via vertex's index
    //because they are stored in dense manner.
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

    //Find which predecessor is pred_vex_id to 'vex'.
    //Return the position of 'vex', position start at 0.
    //e.g: If pred_vex_id is the first predecessor, return 0.
    //pred_vex_id: vertex id of predecessor.
    //is_pred: set to true if pred_vex_id is one of predecessor of 'vex',
    //         otherwise set false. Note user may input a vertex id which is
    //         not the predecessors of 'vex'.
    static UINT WhichPred(VexIdx pred_vex_id, Vertex const* vex,
                          OUT bool & is_pred);
};


typedef BitSet DomSet;

//
//Graph with Dominator info.
//
class DGraph : public Graph {
protected:
    BitSetMgr * m_bs_mgr;
    Vector<BitSet*> m_dom_set; //record dominator-set of each vertex.
    Vector<BitSet*> m_pdom_set; //record post-dominator-set of each vertex.
    Vector<VexIdx> m_idom_set; //immediate dominator.
    Vector<VexIdx> m_ipdom_set; //immediate post dominator.
    RPOMgr m_rpomgr;
protected:
    //The function will compute idom for subgraph that rooted by 'entry'.
    //NOTE: the function needs RPO to compute DomInfo.
    //entry: the entry vertex of subgraph. Note there can be only ONE entry.
    //vlst: the Vertex list that belong to the subgraph.
    void computeIdomForSubGraph(Vertex const* entry,
                                List<Vertex const*> const& vlst);

    //Return true if idom-set status changed.
    bool computeIdomForFullGraph(List<Vertex const*> const& vlst);
    void freeDomSet(VexIdx vid);
    void freePdomSet(VexIdx vid);
    void removeUnreachNodeRecur(VexIdx id, BitSet & visited);
    bool verifyPdom(DGraph & g, RPOVexList const& rpovlst) const;
    bool verifyDom(DGraph & g, RPOVexList const& rpovlst) const;
public:
    DGraph(UINT vex_hash_size = 64);
    DGraph(DGraph const& g);
    DGraph const& operator = (DGraph const&);

    //The function adds Dom, Pdom, IDom, IPDom information for newsucc, whereas
    //update the related info for 'marker'.
    //NOTE: the function maintains the Dom and PDom info after graph changed.
    //marker: a marker vertex.
    //newsucc: the vertex that must be immediate successor of 'marker'.
    //         The function will check the relation.
    //oldsucc: the vertex that must be immediate successor of 'marker' before
    //         graph changed.
    //         NOTE: user has to guarrantee the relation of 'marker' and
    //         'oldsucc' because the function could not check the relation.
    //e.g: given edge marker->oldsucc, after insert newsucc, the graph will be:
    //     marker->newsucc->oldsucc, where there is only ONE predecessor to
    //     newsucc.
    void addDomInfoToImmediateSucc(Vertex const* marker, Vertex const* newsucc,
                                   Vertex const* oldsucc);

    //The function adds Dom and IDom information for newsucc, whereas
    //update the related info for 'oldsucc'. The newsucc is a new insert BB,
    //and has one input edge and one output edge.
    //NOTE: the function maintains the Dom and PDom info after graph changed.
    //marker: a marker vertex.
    //newsucc: the vertex that must be immediate successor of 'marker'.
    //e.g: marker->oldsucc, after insert newsucc, the graph will be:
    //     marker->newsucc->oldsucc, where there is only ONE edge between
    //     marker->newsucc, and newsucc->oldsucc.
    void addDomToNewSingleInOutBB(Vertex const* marker, Vertex const* newsucc,
                                  Vertex const* oldsucc);

    //The function adds Dom, Pdom, IDom, IPDom information for newidom, whereas
    //update the related info for 'marker'.
    //NOTE: the function maintains the Dom and PDom info after graph changed.
    //marker: a marker vertex.
    //newidom: the vertex that must be idom of 'marker'.
    //add_pdom_failed: return true if the function add PDomInfo failed.
    void addDomInfoToNewIDom(Vertex const* marker, Vertex const* newidom,
                             OUT bool & add_pdom_failed);

    //The function adds Dom, Pdom, IDom, IPDom information for newidom, whereas
    //update the related info for 'marker'.
    //NOTE: the function maintains the Dom and PDom info after graph changed.
    //marker: a marker vertex.
    //newidom: the vertex that must be idom of 'marker'.
    //add_pdom_failed: return true if the function add PDomInfo failed.
    void addDomInfoToNewIDom(Vertex const* marker, VexIdx newidom,
                             OUT bool & add_pdom_failed)
    { addDomInfoToNewIDom(marker, addVertex(newidom), add_pdom_failed); }

    //The function adds Dom, Pdom, IDom, IPDom information for newidom, whereas
    //update the related info for 'marker'.
    //NOTE: the function maintains the Dom and PDom info after graph changed.
    //marker: a marker vertex.
    //newidom: the vertex that must be idom of 'marker'.
    //add_pdom_failed: return true if the function add PDomInfo failed.
    void addDomInfoToNewIDom(VexIdx marker, VexIdx newidom,
                             OUT bool & add_pdom_failed)
    {
        addDomInfoToNewIDom(getVertex(marker), addVertex(newidom),
                            add_pdom_failed);
    }

    //The function adds Dom, Pdom, IDom, IPDom information for newipdom,
    //whereas update the related info for 'marker'.
    //NOTE: the function maintains the Dom and PDom info after graph changed.
    //marker: a marker vertex.
    //newipdom: the vertex that must be idom of 'marker'.
    void addDomInfoToNewIPDom(Vertex const* marker, Vertex const* newipdom);

    //The function adds Dom, Pdom, IDom, IPDom information for newipdom,
    //whereas update the related info for 'marker'.
    //NOTE: the function maintains the Dom and PDom info after graph changed.
    //marker: a marker vertex.
    //newipdom: the vertex that must be idom of 'marker'.
    void addDomInfoToNewIPDom(VexIdx marker, VexIdx newipdom)
    { addDomInfoToNewIPDom(getVertex(marker), addVertex(newipdom)); }

    //The function clones graph by given graph.
    //clone_edge_info: true to clone EdgeInfo of g.
    //clone_vex_info: true to clone VertexInfo of g.
    void clone(DGraph const& g, bool clone_edge_info, bool clone_vex_info)
    {
        Graph::clone(g, clone_edge_info, clone_vex_info);
        m_bs_mgr = g.m_bs_mgr;
        if (m_bs_mgr != nullptr) {
            cloneDomAndPdom(g);
        }
    }

    //The function compute the entry vertex.
    void computeEntryList(OUT List<Vertex const*> & lst) const;

    //The function clones Dom and PDom info by given graph.
    void cloneDomAndPdom(DGraph const& src);

    //Vertices should have been sorted in topological order.
    //And we access them by reverse-topological order.
    //vlst: compute dominator for vertices in vlst if it
    //      is not empty or else compute all vertices of graph.
    //uni: universe.
    bool computeDom3(List<Vertex const*> const* vlst, DomSet const* uni);
    bool computeDom2(List<Vertex const*> const& vlst);

    //Vertices should have been sorted in topological order.
    //And we access them by reverse-topological order.
    //vlst: compute dominator for vertices in vlst if it is not empty or else
    //      compute all vertices of graph.
    //uni: universe.
    bool computeDom(List<Vertex const*> const* vlst = nullptr,
                    DomSet const* uni = nullptr);

    //Compute post-dominator according to rpo.
    //root: root node of graph.
    //uni: universe.
    //Note you should use this function carefully, it may be expensive, because
    //that the function does not check if RPO is available, namely, it will
    //always compute the RPO.
    bool computePdomByRPO(Vertex * root, DomSet const* uni);

    //Compute post dominate vertex.
    //Vertices should have been sorted in topological order.
    //And we access them by reverse-topological order.
    //vlst: vertex list.
    //uni: universe.
    //Note the graph may NOT have an exit vertex. If a vertex does not have an
    //ipdom, its pdom-set is meaningless and should be clean.
    //e.g: A cycle in a graph that will not reach to the exit vertex.
    bool computePdom(List<Vertex const*> const& vlst);
    bool computePdom(List<Vertex const*> const& vlst, DomSet const* uni);

    //Compute immediate dominate vertex.
    bool computeIdom();

    //Compute immediate dominate vertex.
    //Vertices should have been sorted in rpo.
    //vlst: a list of vertex which sort in rpo order.
    //NOTE:
    //  1. The root node has better to be the first one in 'vlst'.
    //  2. Entry vertex does not have idom.
    bool computeIdom2(List<Vertex const*> const& vlst);

    //Compute immediate post dominate vertex.
    //NOTE: graph exit vertex does not have idom.
    bool computeIpdom();

    //Count memory usage for current object.
    size_t count_mem() const;

    //The function try to change the DOM info when given vertex has been
    //bypassed.
    //pred: the predecessor of vex.
    //vex: the vertex id that will be bypassed.
    //succ: the successor of vex.
    //e.g:
    //  pred->vex->succ
    //  where vex's idom is pred, vex's ipdom is succ, succ's idom is vex.
    //after bypassing,
    //  pred->vex->succ
    //   \         ^
    //    \_______/
    //  succ's dom become pred. vex is neither DOM nor PDOM.
    bool changeDomInfoByAddBypassEdge(VexIdx pred, VexIdx vex, VexIdx succ);

    //Dump dom set, pdom set, idom, ipdom.
    //dump_dom_tree: set to be true to dump dominate
    //               tree, and post dominate Tree.
    void dumpDomAndPdom(
        FILE * h, bool dump_dom_tree = true, bool dump_pdom_tree = true) const;
    void dumpDomAndPdom(
        CHAR const* name = nullptr, bool dump_dom_tree = true,
        bool dump_pdom_tree = true) const;
    CHAR const* dumpDomAndPdom(OUT StrBuf & buf) const;

    //Note graph entry node does not have idom.
    //id: vertex id.
    VexIdx get_idom(VexIdx id) const { return m_idom_set.get((VecIdx)id); }

    //Note graph exit node does not have ipdom.
    //id: vertex id.
    VexIdx get_ipdom(VexIdx id) const { return m_ipdom_set.get((VecIdx)id); }

    //The function generate DomTree for whole graph.
    //dt: generate dominator tree and record in it.
    void genDomTree(OUT DomTree & dt) const;

    //The function generate DomTree for subgraph that all vertexs in subgraph
    //are at least dominated by 'root'.
    //dt: generate dominator tree and record in it.
    void genDomTreeForSubGraph(Vertex const* root, OUT DomTree & dt,
                               OUT UINT & iter_time) const;

    //pdt: generate post-dominator tree and record in it.
    void genPDomTree(OUT DomTree & pdt) const;

    DomSet const* get_dom_set(VexIdx id) const
    { return m_dom_set.get((VecIdx)id); }

    RPOMgr & getRPOMgr() { return m_rpomgr; }

    //Get vertices who dominate vertex 'id'.
    //NOTE: set does NOT include 'v' itself.
    inline DomSet * gen_dom_set(VexIdx id)
    {
        ASSERT0(m_bs_mgr != nullptr);
        DomSet * set = m_dom_set.get((VecIdx)id);
        if (set == nullptr) {
            set = m_bs_mgr->create();
            m_dom_set.set((VecIdx)id, set);
        }
        return set;
    }

    //Get vertices who dominate vertex 'v'.
    //NOTE: set does NOT include 'v' itself.
    DomSet * gen_dom_set(Vertex const* v)
    {
        ASSERT0(v != nullptr);
        return gen_dom_set(VERTEX_id(v));
    }

    DomSet const* get_pdom_set(VexIdx id) const
    { return m_pdom_set.get((VecIdx)id); }

    //Get vertices who post dominated by vertex 'id'.
    //NOTE: set does NOT include 'v' itself.
    inline DomSet * gen_pdom_set(VexIdx id)
    {
        ASSERT0(m_bs_mgr != nullptr);
        DomSet * set = m_pdom_set.get((VecIdx)id);
        if (set == nullptr) {
            set = m_bs_mgr->create();
            m_pdom_set.set((VecIdx)id, set);
        }
        return set;
    }

    //Get vertices who post dominated by vertex 'v'.
    //NOTE: set does NOT include 'v' itself.
    DomSet * gen_pdom_set(Vertex const* v)
    {
        ASSERT0(v != nullptr);
        return gen_pdom_set(VERTEX_id(v));
    }

    //Return true if 'v1' dominate 'v2'.
    bool is_dom(VexIdx v1, VexIdx v2) const
    {
        ASSERTN(get_dom_set(v2), ("no DOM info about vertex%d", v2));
        return get_dom_set(v2)->is_contain((BSIdx)v1);
    }

    //Return true if 'v1' immediate-dominate 'v2'.
    bool is_idom(VexIdx v1, VexIdx v2) const
    {
        ASSERT0(v1 != VERTEX_UNDEF);
        return get_idom(v2) == ((BSIdx)v1);
    }

    //Return true if 'v1' post dominate 'v2'.
    bool is_pdom(VexIdx v1, VexIdx v2) const
    {
        ASSERTN(get_pdom_set(v2), ("no PDOM info about vertex%d", v2));
        return get_pdom_set(v2)->is_contain((BSIdx)v1);
    }

    //Return true if 'v1' immediate-post-dominate 'v2'.
    bool is_ipdom(VexIdx v1, VexIdx v2) const
    {
        ASSERT0(v1 != VERTEX_UNDEF);
        return get_ipdom(v2) == ((BSIdx)v1);
    }

    //Sort node on graph in bfs-order.
    void sortInBfsOrder(Vector<VexIdx> & order_buf, Vertex * root,
                        BitSet & visit);

    //Sort node in dominator-tree in preorder.
    void sortDomTreeInPreorder(IN Vertex * root, OUT List<Vertex*> & lst);
    void sortDomTreeInPostrder(IN Vertex * root, OUT List<Vertex*> & lst);

    //Sort in-edge of vex in given order.
    //order: record the given order of each predecessor. Note the number
    //       of elements have to equal to the number of predecessor of vex.
    void sortPred(MOD Vertex * vex, Vector<VexIdx> const& order);
    void setBitSetMgr(BitSetMgr * bs_mgr) { m_bs_mgr = bs_mgr; }
    void set_idom(VexIdx vid, VexIdx idom)
    { m_idom_set.set((VecIdx)vid, idom); }
    void set_ipdom(VexIdx vid, VexIdx ipdom)
    { m_ipdom_set.set((VecIdx)vid, ipdom); }

    //The function revise DomInfo after graph changed.
    //NOTE: the function needs RPO to compute DomInfo.
    //Note the RPO must be available.
    //modset: if it is not null, means user asked to collect vertex that idom
    //        changed by the function.
    //root: record the root vertex the of subgraph that affected by adding
    //      or removing edge.
    void reviseDomInfoAfterAddOrRemoveEdge(
        Vertex const* from, Vertex const* to, OUT VexTab * modset,
        OUT Vertex const*& root, OUT UINT & iter_time);

    //The function recompute DomInfo after subgraph changed.
    //NOTE: the function needs RPO to compute DomInfo.
    //Return true if all vertex rooted by 'root' has idom.
    //Note the RPO must be available.
    //modset: if it is not null, means user asked to collect vertex that idom
    //        changed by the function.
    bool recomputeDomInfoForSubGraph(
        Vertex const* root, OUT VexTab * modset, OUT UINT & iter_time);
    bool removeUnreachNode(VexIdx entry_id);
    void revisePdomByIpdom();

    //The function removes all Dom, Pdom, IDom, IPDom information about vex.
    //Note the function may have too much cost because it will iterate vertex
    //till meeting the entry and exit of graph.
    //iter_pred_succ: true to remove dominfo by iterate vex's predecessors
    //and sucessors til entry and exit.
    //iter_time: the number of times that the function iterates graph vertex.
    void removeDomInfo(Vertex const* vex, bool iter_pred_succ,
                       OUT UINT & iter_time);
    void removeDomInfo(VexIdx vex, bool iter_pred_succ, OUT UINT & iter_time)
    { removeDomInfo(getVertex(vex), iter_pred_succ, iter_time); }

    void try_union_dom_set(VexIdx vex, VexIdx newdomvex);
    void try_union_dom_set(VexIdx vex, BitSet const* newset);
    void try_union_pdom_set(VexIdx vex, VexIdx newpdomvex);
    void try_union_pdom_set(VexIdx vex, BitSet const* newset);

    bool verifyDom() const;
    bool verifyPdom() const;
    bool verifyDomAndPdom() const;
};


//The class provides helper functions to convenient iterate vertex on graph.
//If 'start' vertex is given, the class will visit all predecessors start from
//'start' until reach one of entries of graph.
class GraphIterIn {
    COPY_CONSTRUCTOR(GraphIterIn);
    Graph const& m_g;
    Vertex const* m_stop;
    List<Vertex*> m_wl;
    TTab<VexIdx> m_visited;
public:
    //stop: if stop vertex is given, the iterator will not iterate
    //      stop's in-vertex.
    GraphIterIn(Graph const& g, Vertex const* start,
                Vertex const* stop = nullptr);
    void clean();
    Vertex * get_first();
    Vertex * get_next(Vertex const* t);
};


//The class provides helper functions to convenient iterate vertex on graph.
//If 'start' vertex is given, the class will visit all successors start from
//'start' until reach one of exit of graph.
class GraphIterOut {
    COPY_CONSTRUCTOR(GraphIterOut);
    List<Vertex*> m_wl;
    TTab<VexIdx> m_visited;
protected:
    Graph const& m_g;
public:
    GraphIterOut(Graph const& g, Vertex const* start);
    void clean();
    Vertex * get_first();
    Vertex * get_next(Vertex const* t);
};


//The class provides helper functions to convenient iterate edge on graph.
//If 'start' vertex is given, the class will visit all out-edge of vertex
//start from 'start' until reach one of exit of graph.
class GraphIterOutEdge {
    COPY_CONSTRUCTOR(GraphIterOutEdge);
    List<Edge*> m_wl;
    TTab<VexIdx> m_visited;
protected:
    Graph const& m_g;
public:
    GraphIterOutEdge(Graph const& g, Vertex const* start);
    Edge * get_first();
    Edge * get_next(Edge const* t);
};


//The class represents the computation of Minimum Spanning Tree.
//Note that the edge weight can NOT be negative.
#define MST_WEIGHT_UNDEF 0
typedef UINT MSTWeightType;
typedef TMap<Edge const*, MSTWeightType> Edge2Weight;

typedef List<Edge const*>::Iter MSTPathIter;
class MSTPath : public List<Edge const*> {
public:
    void dump(FILE * h, MST const& mst) const;
    void dumpDOT(MST const& mst, CHAR const* name = nullptr) const;
};

class MST {
    COPY_CONSTRUCTOR(MST);
    friend class MSTPath;
protected:
    Graph const& m_g;
    Edge2Weight & m_e2w;
protected:
    Edge const* findMinWeightEdge(
        Vertex const* v, TTab<Vertex const*> const& visited,
        OUT MSTWeightType & minw, OUT Vertex const** newvex);
    Edge const* findMinWeightEdgeForVisited(
        MOD List<Vertex const*> & wl, TTab<Vertex const*> const& visited,
        OUT MSTWeightType & minw, OUT Vertex const** newvex);

    MSTWeightType getMaxWeight() const { return (MSTWeightType)-1; }
    MSTWeightType getWeight(Edge const* e) const { return m_e2w.get(e); }

    bool LessOrEqualThan(MSTWeightType curw, MSTWeightType outew) const
    { return curw <= outew; }
public:
    MST(Graph const& g, Edge2Weight & e2w) : m_g(g), m_e2w(e2w) {}

    //Return the minimum weight of MST.
    MSTWeightType build(Vertex const* root, OUT MSTPath & path);
    MSTWeightType build(OUT MSTPath & path)
    {
        VertexIter it;
        return build(m_g.get_first_vertex(it), path);
    }
};

} //namespace xcom
#endif
