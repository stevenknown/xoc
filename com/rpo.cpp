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


static RPOVal compRPOIfVexPriorMarker(Vertex const* newvex,
                                      Vertex const* marker,
                                      RPOMgr * rpomgr)
{
    ASSERT0(newvex && marker && marker->rpo() != RPO_UNDEF);
    //newvex is prior to marker.
    //Collect the maxmimum RPO of predecessors of marker.
    RPOVal maxpredrpo = MIN_HOST_INT_VALUE;
    xcom::AdjVertexIter it;
    for (xcom::Vertex const* pred = Graph::get_first_in_vertex(marker, it);
         pred != nullptr; pred = Graph::get_next_in_vertex(it)) {
        if (pred->id() == marker->id()) { continue; }
        if (pred->id() == newvex->id()) { continue; }
        if (pred->rpo() == RPO_UNDEF) {
            //Exist invalid rpo, recompute them first.
            return RPO_UNDEF;
        }
        if (pred->rpo() >= marker->rpo()) { continue; }
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
        // VEX13 rpo:19
        //  |
        //  v v．．．．．．．．．．
        // VEX2 rpo:20    |
        //  |             |
        //  v             |
        // VEX4 rpo:30----
        return RPO_UNDEF;
    }
    rpo = rpomgr->tryFindUsableRPO(begin, end);
    #endif
    return rpo;
}


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
            if (succ->id() == newvex->id()) { continue; }
            if (succ->rpo() == RPO_UNDEF) {
                //Exist invalid rpo, recompute them first.
                return RPO_UNDEF;
            }
            if (succ->rpo() <= marker->rpo()) { continue; }
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
    if (begin > end) { return RPO_UNDEF; }
    rpo = rpomgr->tryFindUsableRPO(begin, end);
    #endif
    return rpo;
}


//Try to update RPO of newvex according to RPO of marker.
//newvex_prior_marker: true if newvex's lexicographical order is prior
//to marker.
//Return true if this function find a properly RPO for 'newvex', otherwise
//return false.
bool RPOMgr::tryUpdateRPO(MOD Vertex * newvex, Vertex const* marker,
                          bool newvex_prior_marker)
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
    return true;
}

} //namespace xcom
