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
DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE
FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,
OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE
USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
@*/
#include "xcominc.h"

namespace xcom {

static void prtIndent(FILE * h, UINT indent)
{
    ASSERT0(h);
    for (UINT i = 0; i < indent; i++) { fprintf(h, " "); }
}

//
//START RPOVexList
//
bool RPOVexList::isEqual(RPOVexList const& src) const
{
    if (get_elem_count() != src.get_elem_count()) { return false; }
    RPOVexListIter srcit;
    Vertex const* srcv = src.get_head(&srcit);
    RPOVexListIter it;
    Vertex const* v = get_head(&it);
    for (; v != nullptr && srcv != nullptr;
         v = get_next(&it), srcv = get_next(&srcit)) {
        if (v != srcv) {
            return false;
        }
    }
    return v == srcv;
}


void RPOVexList::dump(FILE * h, UINT indent) const
{
    ASSERT0(h);
    RPOVexListIter it;
    for (xcom::Vertex const* v = get_head(&it);
         v != nullptr; v = get_next(&it)) {
        fprintf(h, "\n");
        prtIndent(h, indent);
        fprintf(h, "v%u(rpo:%d)", v->id(), v->rpo());
    }
    fflush(h);
}


void RPOVexList::dump(CHAR const* filename) const
{
    ASSERT0(filename);
    FileObj fo(filename);
    ASSERT0(fo.getFileHandler());
    dump(fo.getFileHandler(), 0);
}
//END RPOVexList


class VertexQuickSort : public QuickSort<Vertex const*> {
protected:
    virtual Vertex const* _max(Vertex const* a, Vertex const* b) const
    { return a->rpo() > b->rpo() ? a : b; }

    virtual Vertex const* _min(Vertex const* a, Vertex const* b) const
    { return a->rpo() < b->rpo() ? a : b; }

    virtual bool GreatThan(Vertex const* a, Vertex const* b) const
    { return a->rpo() > b->rpo(); }

    virtual bool LessThan(Vertex const* a, Vertex const* b) const
    { return a->rpo() < b->rpo(); }
};

void RPOMgr::collectRPOVexList(Graph const& g, OUT RPOVexList & vlst)
{
    Vector<Vertex const*> vec(g.getVertexNum());
    VertexIter it;
    for (Vertex const* v = g.get_first_vertex(it);
         v != nullptr; v = g.get_next_vertex(it)) {
        ASSERT0(v->rpo() != RPO_UNDEF);
        vec.append(v);
    }
    VertexQuickSort qs;
    qs.sort(vec);
    vlst.clean();
    for (VecIdx i = 0; i < (VecIdx)vec.get_elem_count(); i++) {
        Vertex const* v = vec.get(i);
        ASSERT0(v);
        vlst.append_tail(v);
    }
}


//Sort vertice by RPO order, and update rpo of vertex.
//Record sorted vertex into vlst in incremental order of RPO.
//NOTE: rpo start at RPO_INIT_VAL.
void RPOMgr::computeRPO(Graph const& g, MOD Vertex * root,
                        OUT RPOVexList & vlst)
{
    ASSERT0(root && g.is_graph_entry(root));
    m_used_rpo.clean();
    BitSet is_visited;
    Stack<Vertex*> stk;
    stk.push(root);
    Vertex * v;
    RPOUVal order = RPO_INIT_VAL + g.getVertexNum() * RPO_INTERVAL;
    vlst.clean();
    while ((v = stk.get_top()) != nullptr) {
        is_visited.bunion((BSIdx)VERTEX_id(v));
        bool find = false; //find unvisited kid.
        AdjVertexIter vit;
        for (Vertex * succ = Graph::get_first_out_vertex(v, vit);
             succ != nullptr; succ = Graph::get_next_out_vertex(vit)) {
            if (!is_visited.is_contain((BSIdx)succ->id())) {
                stk.push(succ);
                find = true;
                break;
            }
        }
        if (!find) {
            stk.pop();
            vlst.append_head(v);
            order -= RPO_INTERVAL;
            VERTEX_rpo(v) = order;
        }
    }

    //If order of BB is not zero, there must have some BBs should be
    //eliminated by CFG optimizations.
    ASSERTN(order == RPO_INIT_VAL,
            ("some vertex does not have RPO, exist unreachable one"));
}


//Try to find an usable RPO that is between 'begin' and 'end'.
//Note the algorithm has assigned positive integers as RPO to each vertex
//by every RPO_INTERVAL numbers. These assigned integers are regarded as
//unusable integer.
//'begin' and 'end' have to be within same INTERVAL.
RPOVal RPOMgr::tryFindUsableRPO(RPOVal begin, RPOVal end)
{
    ASSERT0(begin > RPO_UNDEF && end > RPO_UNDEF && begin <= end);
    RPOVal dis = end - begin;
    if (dis > RPO_INTERVAL) {
        //STRATEGY:Choose the larger RPO distance as the searching range.
        //RPO range: |---begin----------|-------|----------end----|
        //           30  34<----dis1-->40       60<--dis2->68
        RPOVal dis1start = begin;
        RPOVal dis1end = computeNearestGreaterUnUsableRPO(begin) - 1;
        RPOVal dis1 = dis1end - dis1start;
        RPOVal dis2start = computeNearestLessUnUsableRPO(end) + 1;
        RPOVal dis2end = end;
        RPOVal dis2 = dis2end - dis2start;
        ASSERT0(dis1 >= 0 && dis2 >= 0);
        if (dis1 > dis2) {
            begin = dis1start;
            end = dis1end;
        } else {
            begin = dis2start;
            end = dis2end;
        }
        dis = end - begin;
    }
    ASSERT0(dis <= RPO_INTERVAL);
    RPOVal lower = MIN(computeNearestLessUnUsableRPO(begin),
                       computeNearestLessUnUsableRPO(end));
    ASSERTN_DUMMYUSE(computeNearestGreaterUnUsableRPO(begin) <=
                     lower + RPO_INTERVAL,
                     ("cross interval"));
    ASSERTN_DUMMYUSE(computeNearestGreaterUnUsableRPO(end) <=
                     lower + RPO_INTERVAL,
                     ("cross interval"));
    //Note if dis is 0, there is only one candidiate.
    for (RPOVal div = 2; div <= RPO_INTERVAL; div++) {
        RPOVal newrpo = begin + dis / div;
        if (isUsableRPO(newrpo)) {
            add(newrpo);
            return newrpo;
        }
    }
    return RPO_UNDEF;
}


//Return true if find an order of RPO for 'v' that less than order of 'ref'.
bool RPOMgr::tryFindLessRPO(Vertex * v, Vertex const* ref)
{
    ASSERT0(v && ref);
    RPOVal rpo = ref->rpo() - 1;
    ASSERT0(rpo >= RPO_INIT_VAL);
    if (isUsableRPO(rpo)) {
        VERTEX_rpo(v) = rpo;
        add(rpo);
        return true;
    }
    return false;
}


//Note newvex should be the previous vertex to marker.
static RPOVal compRPOIfVexPriorMarker(
    Vertex const* newvex, Vertex const* marker, RPOMgr * rpomgr)
{
    ASSERT0(newvex && marker && marker->rpo() != RPO_UNDEF);
    //'newvex' is prior to 'marker'.
    //Collect the maxmimum RPO of predecessors of marker.
    RPOVal maxpredrpo = MIN_HOST_INT_VALUE;
    xcom::AdjVertexIter it;
    for (xcom::Vertex const* pred = Graph::get_first_in_vertex(marker, it);
         pred != nullptr; pred = Graph::get_next_in_vertex(it)) {
        if (pred->id() == marker->id()) { continue; }
        if (pred->id() == newvex->id()) {
            //CAUTION: If newvex has already be predecessor of marker, that
            //means user is going to compute RPO after graph has been changed.
            //Thus the function may be unable to find a correct RPO for
            //'newvex'.
            return RPO_UNDEF;
        }
        if (pred->rpo() == RPO_UNDEF) {
            //Exist invalid rpo, recompute them first.
            return RPO_UNDEF;
        }
        if (pred->rpo() >= marker->rpo()) {
            //Do NOT ignore the backward predecessor vertex.
            //TBD: Should we ignore the backward vertex to participate in the
            //computation of maxpredrpo?
            //If newvex is acutally lexicograical after marker, such as the
            //from-vertex in backedge of a natural loop, we can not infer
            //an usable RPO for 'newvex' easily.
            //continue;
        }
        maxpredrpo = MAX(pred->rpo(), maxpredrpo);
    }
    RPOVal rpo = RPO_UNDEF;
    #ifdef UPDATE_RPO_JUST_BY_SINGLE_STEP
    rpo = marker->rpo() - 1;
    if (rpo <= maxpredrpo) {
        rpo = RPO_UNDEF;
    }
    #else
    RPOVal begin = maxpredrpo == MIN_HOST_INT_VALUE ?
        RPOMgr::computeNearestGreaterUnUsableRPO(marker->rpo()) -
        RPO_INTERVAL : maxpredrpo + 1;
    RPOVal end = marker->rpo() - 1;
    if (begin > end) {
        //Can not find usable RPO.
        //CASE:compile.gr/guard.gr
        // newvex is V14, marker is V2
        // V13 rpo:19
        //  |
        //  v v````````
        // V2 rpo:20   |
        //  |          |
        //  v          |
        // V4 rpo:30---
        return RPO_UNDEF;
    }
    rpo = rpomgr->tryFindUsableRPO(begin, end);
    #endif
    return rpo;
}


//CASE1: The marker has a unique successor next and next's RPO is less than
//the marker's RPO.
//
//  newvex is V6, marker is V5, next is V2.
//
//         V1 rpo:10                             V1 rpo:10
//         |                                     |
//         v                                     v
//         V2 rpo:20  <------                    V2 rpo:20  <------
//    _____|_____            |              _____|_____            |
//   |           |           |             |           |           |
//   v           v        V6 rpo: ? =>     v           v         V6 rpo: 40
//   V3 rpo:25   V4 rpo:30   ^             V3 rpo:25   V4 rpo:30   ^
//   |___________|           |             |___________|           |
//         |                 |                   |                 |
//         v                 |                   v                 |
//         V5 rpo:40  -------                    V5 rpo:35  -------
//
//Find a possible RPO before the marker and set it as the marker's RPO,
//the original marker's RPO as the newvex's RPO.
static RPOVal compRPOIfMarkerPriorVexCase1(
    Vertex const* newvex, Vertex const* marker, RPOMgr * rpomgr)
{
    ASSERT0(marker->getOutDegree() == 1);
    ASSERT0(marker->getNthOutVertex(0)->rpo() < marker->rpo());

    RPOVal rpo = compRPOIfVexPriorMarker(newvex, marker, rpomgr);

    if (rpo == RPO_UNDEF) { return RPO_UNDEF; }

    RPOVal new_rpo = VERTEX_rpo(marker);
    VERTEX_rpo(const_cast<Vertex*>(marker)) = rpo;

    return new_rpo;
}


//Note newvex should be the next vertex to marker.
static RPOVal compRPOIfMarkerPriorVex(
    Vertex const* newvex, Vertex const* marker, RPOMgr * rpomgr)
{
    ASSERT0(newvex && marker);
    //newvex is after marker.
    //Collect the minimal RPO of successors of marker.
    RPOVal minsuccrpo = MAX_HOST_INT_VALUE;
    xcom::AdjVertexIter it;
    for (xcom::Vertex const* succ = Graph::get_first_out_vertex(marker, it);
         succ != nullptr; succ = Graph::get_next_out_vertex(it)) {
        if (succ->id() != marker->id()) {
            if (succ->id() == newvex->id()) {
                //CAUTION: If newvex has already be successor of marker, that
                //means user is going to compute RPO after graph has been
                //changed.
                //Thus the function may be unable to find a correct RPO for
                //'newvex'.
                return RPO_UNDEF;
            }
            if (succ->rpo() == RPO_UNDEF) {
                //Exist invalid rpo, recompute them first.
                return RPO_UNDEF;
            }
            if (succ->rpo() <= marker->rpo()) {
                //Do NOT ignore the backward predecessor vertex.
                //TBD: Should we ignore the backward vertex to participate
                //in the computation of minpredrpo?
                //If newvex is acutally lexicograical before marker, such as
                //the from-vertex in backedge of a natural loop, we can not
                //infer an usable RPO for 'newvex' easily.
                //continue;
            }
            minsuccrpo = MIN(succ->rpo(), minsuccrpo);
        }
    }
    RPOVal rpo = RPO_UNDEF;
    #ifdef UPDATE_RPO_JUST_BY_SINGLE_STEP
    rpo = marker->rpo() + 1;
    if (rpo >= minsuccrpo) {
        rpo = RPO_UNDEF;
    }
    #else
    RPOVal begin = marker->rpo() + 1;
    RPOVal end = minsuccrpo == MAX_HOST_INT_VALUE ?
        RPOMgr::computeNearestLessUnUsableRPO(marker->rpo()) + RPO_INTERVAL :
        minsuccrpo - 1;
    if (begin > end) {
        //Can not find usable RPO.
        //CASE:compile.gr/guard.gr
        // newvex is V14, marker is V2
        // V13 rpo:19
        //  |
        //  v v````````
        // V2 rpo:20   |
        //  |          |
        //  v          |
        // V4 rpo:30---

        if (marker->getOutDegree() == 1) {
            return compRPOIfMarkerPriorVexCase1(newvex, marker, rpomgr);
        }
        return RPO_UNDEF;
    }
    rpo = rpomgr->tryFindUsableRPO(begin, end);
    #endif
    return rpo;
}


//Ensure the RPO in CFG conforms to the expected RPO result.
//Collect RPO from current CFG into a orgrpovexlst.
bool RPOMgr::verifyRPOVexList(Graph const& g, Vertex const* root,
                              RPOVexList const& vlst)
{
    //CASE:alias.loop.c
    //The RPO is strongly dependent on the visiting-order of kids of
    //each vertex.
    //e.g:in given example, g is:
    //        _______
    //       |       v
    //  v12->v1->v7->v6
    //            ^__|
    //v7 can be previous_vex of v6, vice-verse, v6 can be previous_vex of
    //v7 too. It depends on which kids of v1 is visited first.
    //However the verify method can not enumerate all possible cases.
    //Thus we would not use the method to verify RPOVexList in usual.
    return true;

    //Collect RPO from current CFG into a orgrpovexlst.
    RPOVexList tvlst;
    Graph tg;
    tg.clone(g, false, false);
    Vertex * troot = tg.getVertex(root->id());
    ASSERT0(troot);
    RPOMgr mgr;
    mgr.computeRPO(tg, troot, tvlst);
    ASSERT0(vlst.get_elem_count() == tvlst.get_elem_count());
    TMap<VexIdx, DefSBitSet*> v2bs;
    DefMiscBitSetMgr bsmgr;
    BitSet previous_vex;
    RPOVexListIter rpovit;
    for (Vertex const* tv = tvlst.get_head(&rpovit);
         tv != nullptr; tv = tvlst.get_next(&rpovit)) {
        DefSBitSet * pbs = v2bs.get(tv->id());
        if (pbs == nullptr) {
            pbs = bsmgr.allocSBitSet();
            v2bs.set(tv->id(), pbs);
        }
        AdjVertexIter ait;
        for (Vertex const* in = tg.get_first_in_vertex(tv, ait);
             in != nullptr; in = tg.get_next_in_vertex(ait)) {
            if (!previous_vex.is_contain((BSIdx)in->id())) {
                continue;
            }
            //Only collect the predecessors that placed previous to 'tv'.
            pbs->bunion(in->id());
        }
        previous_vex.bunion(tv->id());
    }
    previous_vex.clean();
    for (Vertex const* v = vlst.get_head(&rpovit);
         v != nullptr; v = vlst.get_next(&rpovit)) {
        DefSBitSet const* pbs = v2bs.get(v->id());
        ASSERT0(pbs);
        DefSBitSetIter it;
        for (BSIdx i = pbs->get_first(&it);
             i != BS_UNDEF; i = pbs->get_next(i, &it)) {
            ASSERT0(previous_vex.is_contain(i));
        }
        previous_vex.bunion(v->id());
    }
    return true;
}


bool RPOMgr::tryUpdateRPO(MOD Vertex * newvex, Vertex const* marker,
                          MOD RPOVexList * rpovexlst, bool newvex_prior_marker)
{
    ASSERT0(newvex != marker);
    ASSERT0(newvex->rpo() == RPO_UNDEF && marker->rpo() != RPO_UNDEF);
    RPOVal rpo = RPO_UNDEF;
    if (newvex_prior_marker) {
        rpo = compRPOIfVexPriorMarker(newvex, marker, this);
    } else {
        rpo = compRPOIfMarkerPriorVex(newvex, marker, this);
    }
    //Try to update RPO
    if (rpo == RPO_UNDEF) {
        VERTEX_rpo(newvex) = RPO_UNDEF;
        return false;
    }
    VERTEX_rpo(newvex) = rpo;
    if (rpovexlst == nullptr) { return true; }

    //Meanwhile update vertex list that ordered in RPO.
    if (newvex_prior_marker) {
        rpovexlst->insert_before(newvex, marker);
    } else {
        rpovexlst->insert_after(newvex, marker);
    }
    return true;
}

} //namespace xcom
