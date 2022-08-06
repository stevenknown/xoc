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

void Tree::insertParent(VexIdx v, VexIdx parent)
{
    Vertex * vex = getVertex(v);
    ASSERT0(vex);
    Vertex * oldparent = const_cast<Tree*>(this)->getParent(vex);
    if (oldparent == nullptr) {
        addEdge(parent, v);
        return;
    }
    removeEdge(getEdge(oldparent, vex));
    addEdge(oldparent->id(), parent);
    addEdge(parent, v);
}


void Tree::remove(VexIdx v)
{
    Vertex * vex = getVertex(v);
    ASSERT0(vex);
    Vertex * oldparent = const_cast<Tree*>(this)->getParent(vex);
    if (oldparent == nullptr) {
        //vex is the root of the Tree.
        removeVertex(vex);
        return;
    }
    AdjVertexIter it;
    for (Vertex * s = Graph::get_first_out_vertex(vex, it);
         s != nullptr; s = Graph::get_next_out_vertex(it)) {
        addEdge(oldparent, s);
    }
    removeVertex(vex);
}


void Tree::insertKid(VexIdx v, VexIdx kid)
{
    ASSERT0(getVertex(v));
    addEdge(v, kid);
}


VexIdx Tree::computeHeight()
{
    ASSERT0(m_root);
    VERTEX_height(m_root) = HEIGHT_INIT_VAL;
    BFSTreeIter ti(*this);
    VexIdx maxheight = HEIGHT_INIT_VAL;
    for (Vertex * t = ti.get_first(); t != nullptr; t = ti.get_next(t)) {
        if (t == m_root) { continue; }
        Vertex const* parent = getParent(t);
        ASSERT0(parent);
        VERTEX_height(t) = VERTEX_height(parent) + 1;
        maxheight = MAX(VERTEX_height(t), maxheight);
    }
    return maxheight;
}


//
//START BFSTreeIter
//
Vertex * BFSTreeIter::get_first()
{
    return getTree().getRoot();
}


Vertex * BFSTreeIter::get_next(Vertex const* t)
{
    Vertex * v = GraphIterOut::get_next(t);
    ASSERT0(v == nullptr || v->getInDegree() <= 1);
    return v;
}
//END DFSTreeIter


//
//START VisitTree
//
void VisitTree::perform()
{
    ASSERT0(m_root);
    xcom::Stack<Vertex const*> stk;
    xcom::BitSet visited(m_maxnum / BITS_PER_BYTE);
    Vertex const* v;
    stk.push(m_root);
    while ((v = stk.get_top()) != nullptr) {
        if (!visited.is_contain(v->id())) {
            visited.bunion(v->id());
            visitWhenFirstMeet(v);
        }
        bool all_visited = true;
        AdjVertexIter oit;
        for (Vertex const* kid = Graph::get_first_out_vertex(v, oit);
             kid != nullptr; kid = Graph::get_next_out_vertex(oit)) {
            if (kid == v) { continue; }
            if (!visited.is_contain(kid->id())) {
                all_visited = false;
                stk.push(kid);
                break;
            }
        }
        if (all_visited) {
            stk.pop();
            //Do post-processing while all kids of vertex has been processed.
            visitWhenAllKidHaveBeenVisited(v);
        }
    }
}
//END VisitTree

} //namespace xcom
