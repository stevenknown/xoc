#include "../../header_for_xgen.h"

void testReachin()
{
    {
    Graph g;
    g.addEdge(3, 5);
    Vertex const* v3 = g.getVertex(3);
    Vertex const* v5 = g.getVertex(5);
    VexTab vtab;
    bool try_failed;
    bool succ = Graph::isReachIn(
        v5, v5, GRAPH_REACHIN_MAX_TRY_LIMIT, try_failed, &vtab);
    ASSERT0(!succ);
    ASSERT0(!try_failed);
    }

    {
    Graph g;
    g.addEdge(3, 5);
    Vertex const* v3 = g.getVertex(3);
    Vertex const* v5 = g.getVertex(5);
    VexTab vtab;
    bool try_failed;
    g.addEdge(5, 5);
    bool succ = Graph::isReachIn(
        v5, v5, GRAPH_REACHIN_MAX_TRY_LIMIT, try_failed, &vtab);
    ASSERT0(succ);
    ASSERT0(!try_failed);
    }

    {
    Graph g;
    g.addEdge(3, 5);
    Vertex const* v3 = g.getVertex(3);
    Vertex const* v5 = g.getVertex(5);
    VexTab vtab;
    vtab.append(3);
    bool try_failed;
    g.addEdge(5, 5);
    bool succ = Graph::isReachIn(
        v5, v5, GRAPH_REACHIN_MAX_TRY_LIMIT, try_failed, &vtab);
    ASSERT0(succ);
    ASSERT0(!try_failed);
    }

    {
    Graph g;
    g.addEdge(3, 5);
    Vertex const* v3 = g.getVertex(3);
    Vertex const* v5 = g.getVertex(5);
    VexTab vtab;
    vtab.append(5);
    bool try_failed;
    g.addEdge(5, 5);
    bool succ = Graph::isReachIn(
        v5, v5, GRAPH_REACHIN_MAX_TRY_LIMIT, try_failed, &vtab);
    ASSERT0(!succ);
    ASSERT0(!try_failed);
    }

    {
    Graph g;
    g.addEdge(3, 5);
    g.addEdge(4, 5);
    g.addEdge(6, 4);
    g.addEdge(5, 6);
    Vertex const* v3 = g.getVertex(3);
    Vertex const* v5 = g.getVertex(5);
    VexTab vtab;
    bool try_failed;
    bool succ = Graph::isReachIn(
        v5, v5, GRAPH_REACHIN_MAX_TRY_LIMIT, try_failed, &vtab);
    ASSERT0(succ);
    ASSERT0(!try_failed);
    }

    {
    Graph g;
    g.addEdge(3, 5);
    g.addEdge(4, 5);
    g.addEdge(6, 4);
    g.addEdge(5, 6);
    Vertex const* v3 = g.getVertex(3);
    Vertex const* v5 = g.getVertex(5);
    VexTab vtab;
    vtab.append(5);
    bool try_failed;
    bool succ = Graph::isReachIn(
        v5, v5, GRAPH_REACHIN_MAX_TRY_LIMIT, try_failed, &vtab);
    ASSERT0(!succ);
    ASSERT0(!try_failed);
    }

    {
    Graph g;
    g.addEdge(2, 1);
    g.addEdge(4, 2);
    g.addEdge(3, 4);
    g.addEdge(2, 3);
    g.addEdge(3, 5);
    g.addEdge(5, 6);
    g.addEdge(6, 2);
    g.addEdge(5, 7);
    g.addEdge(7, 1);
    Vertex const* v3 = g.getVertex(3);
    Vertex const* v4 = g.getVertex(4);
    Vertex const* v5 = g.getVertex(5);
    VexTab vtab;
    vtab.append(3);
    bool try_failed;
    bool succ = Graph::isReachIn(
        v5, v4, GRAPH_REACHIN_MAX_TRY_LIMIT, try_failed, &vtab);
    ASSERT0(!succ);
    ASSERT0(!try_failed);

    vtab.clean();
    bool succ2 = Graph::isReachIn(
        v5, v4, GRAPH_REACHIN_MAX_TRY_LIMIT, try_failed, &vtab);
    ASSERT0(succ2);
    ASSERT0(!try_failed);
 
    }
}

int testAddDom1()
{
    xcom::DGraph g;
    g.addEdge(1, 2);
    Vertex * v1 = g.getVertex(1);
    Vertex * v2 = g.getVertex(2);
    xcom::RPOVexList vlst;
    g.getRPOMgr().computeRPO(g, v1, vlst);
    bool f = g.computeIdom2(vlst);
    ASSERT0(f);
    f = g.computeDom2(vlst);
    ASSERT0(f);
    f = g.computePdom(vlst);
    ASSERT0(f);
    f = g.computeIpdom();
    ASSERT0(f);
    Vertex * v3 = g.addVertex(3);
    g.insertVertexBetween(v1, v2, v3);
    g.addDomInfoToImmediateSucc(v1, v3, v2);
    g.dumpDomAndPdom(stdout);
    return 0;
}

int testAddDom2()
{
    xcom::DGraph g;
    g.addEdge(1, 2);
    g.addEdge(10, 11);
    g.addEdge(10, 12);
    g.addEdge(10, 1);
    g.addEdge(2, 20);
    g.addEdge(20, 21);
    Vertex * v1 = g.getVertex(1);
    Vertex * v2 = g.getVertex(2);
    Vertex * v10 = g.getVertex(10);
    xcom::RPOVexList vlst;
    g.getRPOMgr().computeRPO(g, v10, vlst);
    bool f = g.computeIdom2(vlst);
    ASSERT0(f);
    f = g.computeDom2(vlst);
    ASSERT0(f);
    f = g.computePdom(vlst);
    ASSERT0(f);
    f = g.computeIpdom();
    ASSERT0(f);
    //  v1->v2
    //after
    //        ->v6--
    //       |      v
    //  v1->v3------------->v4->v2
    //       |      ^
    //        ->v5--
    g.addEdge(1, 3);
    g.addEdge(3, 6);
    g.addEdge(3, 5);
    g.addEdge(6, 4);
    g.addEdge(5, 4);
    g.addEdge(4, 2);
    g.removeEdge(1, 2);
    g. addDomInfoToImmediateSuccDiamondRegion(
        g.getVertex(1),
        g.getVertex(3),
        g.getVertex(6),
        g.getVertex(5),
        g.getVertex(4),
        g.getVertex(2));
    g.dumpDomAndPdom(stdout);
    return 0;
}

void testIterIn()
{
    xcom::DGraph g;
    g.addEdge(1,2);
    g.addEdge(2,3);
    g.addEdge(2,4);
    g.addEdge(3,5);
    g.addEdge(4,5);
    g.addEdge(5,6);
    g.addEdge(4,6);
    g.addEdge(6,7);
    g.addEdge(7,6);
    g.addEdge(7,8);

    //Compute DomInfo.
    xcom::RPOVexList vlst;
    Vertex * v1 = g.getVertex(1);
    g.getRPOMgr().computeRPO(g, v1, vlst);
    bool f = g.computeIdom2(vlst);
    ASSERT0(f);
    f = g.computeDom2(vlst);
    ASSERT0(f);
    f = g.computePdom(vlst);
    ASSERT0(f);
    f = g.computeIpdom();
    ASSERT0(f);

    Vertex const* startvex = g.getVertex(6);
    xcom::GraphIterIn<> iterin(g, startvex, nullptr);
    VexIdx meetupid = 2;
    //iterin.getCompareFuncObject().init(g, meetupid);
    printf("\ntestIterIn():");
    Vector<VexIdx> order;
    for (Vertex const* t = iterin.get_first();
         t != nullptr; t = iterin.get_next(t)) {
        if (!g.is_dom(meetupid, t->id())) { continue; }
        if (t->id() == meetupid) { break; }
        if (t->id() == startvex->id()) { continue; }
        if (g.is_dom(startvex->id(), t->id())) { continue; }
        order.append(t->id());
    }
    ASSERT0(order.get_elem_count() == 3);
    ASSERT0(order.get(0) == 5 &&
            order.get(1) == 4 &&
            order.get(2) == 3);
}

void testBFSGraphIter()
{
    xcom::Graph g;
    g.addEdge(1, 2);
    g.addEdge(2, 5);
    g.addEdge(2, 3);
    g.addEdge(3, 4);
    g.addEdge(4, 3);
    g.addEdge(3, 5);
    g.addEdge(2, 6);
    g.addEdge(2, 7);
    g.addEdge(6, 8);
    g.addEdge(8, 9);
    g.addEdge(9, 5);
    g.addEdge(7, 10);
    g.addEdge(10, 5);
    Vertex * entry = g.getVertex(1);
    GraphIterOutDFS it(entry);
    printf("\n");
    for (Vertex const* v = it.get_first(); v != nullptr;
         v = it.get_next()) {
        printf("v%u,", v->id());
    }
    printf("\n");


    class MyVis : public xcom::VisitGraphFuncBase {
    public:
        void visitWhenAllKidHaveBeenVisited(
            Vertex const* v, MOD Stack<Vertex const*> & stk)
        {
            printf("\nSecond Meet and All Kid Visited:v%u", v->id());
        }

        //The function is a callback interface.
        //The function will be invoked when first accessing the vertex v.
        //Return true to process the kid vertex on tree.
        //v: the vertex on Tree.
        //stk: the visiting stack of vertex. Usually, user does not need to
        //     manipulate the element in stk.
        bool visitWhenFirstMeet(Vertex const* v, MOD Stack<Vertex const*> & stk)
        {
            printf("\nFirst Meet:v%u", v->id());
            return true;
        }
    };
    MyVis vf;
    xcom::VisitGraphDFS<MyVis> visg(g, vf);
    visg.visit(entry);
    printf("\n");
}

class XXX {
    COPY_CONSTRUCTOR(XXX);
    friend class DFNVisitFunc;
protected:
    class DFNVisitFunc : public xcom::VisitGraphFuncBase {
    public:
        UINT m_cnt;
        XXX * m_dfn;
    public:
        DFNVisitFunc(XXX * dfn) : m_dfn(dfn) { m_cnt = 1; }
        void visitWhenAllKidHaveBeenVisited(
            Vertex const* v, MOD Stack<Vertex const*> & stk)
        {
            m_dfn->set_out(v, m_cnt);
            m_cnt++;
        }

        //The function is a callback interface.
        //The function will be invoked when first accessing the vertex v.
        //Return true to process the kid vertex on tree.
        //v: the vertex on Tree.
        //stk: the visiting stack of vertex. Usually, user does not need to
        //     manipulate the element in stk.
        bool visitWhenFirstMeet(Vertex const* v, MOD Stack<Vertex const*> & stk)
        {
            m_dfn->set_in(v, m_cnt);
            m_cnt++;
            return true;
        }
    };
protected:
    Graph const& m_g;
    Vector<UINT> m_in;
    Vector<UINT> m_out;
public:
    XXX(Graph const& g) : m_g(g) {}
    void compute(Vertex const* start);
    void dump(FILE * h) const;
    UINT get_in(Vertex const* v) const { return m_in.get(v->id()); }
    UINT get_out(Vertex const* v) const { return m_out.get(v->id()); }
    void set_in(Vertex const* v, UINT cnt) { m_in.set(v->id(), cnt); }
    void set_out(Vertex const* v, UINT cnt) { return m_out.set(v->id(), cnt); }
};

void XXX::dump(FILE * h) const
{
    xcom::VertexIter it;
    for (Vertex const* v = m_g.get_first_vertex(it);
         v != nullptr; v = m_g.get_next_vertex(it)) {
        fprintf(h, "\nv%u:in(%u),out(%u)", v->id(), get_in(v), get_out(v));
    }
    fprintf(h, "\n");
}

void XXX::compute(Vertex const* start)
{
    XXX::DFNVisitFunc vf(this);
    xcom::VisitGraphDFS<XXX::DFNVisitFunc> visgraph(m_g, vf, true);
    visgraph.visit(start);
}

void testXXX()
{
    xcom::Graph g;
    g.addEdge(1, 2);
    g.addEdge(2, 5);
    g.addEdge(2, 3);
    g.addEdge(3, 4);
    g.addEdge(4, 3);
    g.addEdge(3, 5);
    g.addEdge(2, 6);
    g.addEdge(2, 7);
    g.addEdge(6, 8);
    g.addEdge(8, 9);
    g.addEdge(9, 5);
    g.addEdge(7, 10);
    g.addEdge(10, 5);
    Vertex * start = g.getVertex(1);
    XXX dd(g);
    dd.compute(start);
    dd.dump(stdout);
}

void testXXX2()
{
    xcom::DGraph g;
    g.addEdge(1, 2);
    g.addEdge(1, 3);
    g.addEdge(2, 4);
    g.addEdge(3, 4);
    g.addEdge(4, 5);

    //Compute DomInfo.
    xcom::RPOVexList vlst;
    Vertex * v1 = g.getVertex(1);
    g.getRPOMgr().computeRPO(g, v1, vlst);
    bool f = g.computeIdom2(vlst);
    ASSERT0(f);
    f = g.computeDom2(vlst);
    ASSERT0(f);
    f = g.computePdom(vlst);
    ASSERT0(f);
    f = g.computeIpdom();
    ASSERT0(f);

    //
    DomTree dt;
    g.genDomTree(dt);
    dt.dumpDOT();
    Vertex * start = dt.getVertex(1);
    XXX dd(dt);
    dd.compute(start);
    dd.dump(stdout);
}



int main()
{
    testAddDom1();
    testAddDom2();
    testReachin();
    testIterIn();
    testBFSGraphIter();
    testXXX();
    testXXX2();
    return 0;
}
