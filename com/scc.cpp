/*@
Copyright (c) 2013-2014, Su Zhenyu steven.known@gmail.com
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

#define USE_NONRECURSIVE_ALGO
#define UNDEF_GROUP 0

//
//START SCC
//
SCC::SCC(Graph * g)
{
    m_g = nullptr;
    init(g);
}


SCC::~SCC()
{
    destroy();
}


void SCC::init(Graph * g)
{
    if (is_init()) { return; }
    m_g = g;
    m_scc.init();
    m_sbsmgr.init();
}


void SCC::destroy()
{
    if (!is_init()) { return; }
    m_g = nullptr;
    for (VertexSet * vs = m_scc.get_head();
         vs != nullptr; vs = m_scc.get_next()) {
        vs->clean();
    }
    m_scc.destroy();
}


//Searchs for SCC starting from 'root'.
//onpath: records vertices till in accessing stack.
//visited: records vertices that have been visited.
void SCC::scanNoRecur(Vertex const* root, VertexSet & onpath,
                      VertexSet & visited, GAI & gai, UINT & count,
                      Group2VertexSet & group2bs,
                      Stack<Vertex const*> & path)
{
    ASSERTN(is_init(), ("not yet initialized."));
    Stack<Vertex const*> stk_vex;
    stk_vex.push(root);
    Vertex const* v;
    while ((v = stk_vex.get_top()) != nullptr) {
        if (!visited.is_contain(v->id())) {
            visited.bunion(v->id());
            gai.set_index(v->id(), count);
            gai.set_group(v->id(), count);
            visited.bunion(v->id());
            count++;
            ASSERT0(!onpath.is_contain(v->id()));
            onpath.bunion(v->id());
            path.push(v);
        }

        //Walk through each successors of 'v'.
        bool all_visited = true;
        for (EdgeC * el = v->getOutList(); el != nullptr; el = el->get_next()) {
            Vertex const* succ = el->getEdge()->to();
            if (!visited.is_contain(succ->id())) {
                all_visited = false;
                stk_vex.push(succ);
                break;
            }
        }
        if (!all_visited) { continue; }

        stk_vex.pop();
        UINT v_group = gai.get_group(v->id());
        bool find_scc = false;
        for (EdgeC * el = v->getOutList(); el != nullptr; el = el->get_next()) {
            Vertex const* succ = el->getEdge()->to();
            if (onpath.is_contain(succ->id())) {
                UINT succ_group = gai.get_group(succ->id());
                v_group = MIN(v_group, succ_group);
                if (v_group == succ_group) {
                    find_scc = true;
                }
                continue;
            }
            UINT succ_group = UNDEF_GROUP;
            //The successor has been visited by other path.
            //The successor may be included in other scc.
            ASSERT0(v_group != succ_group);
        }
        gai.set_group(v->id(), v_group);
 
        if (find_scc) {
            addToGroup(v, v_group, group2bs);
        }

        if (v_group == gai.get_index(v->id())) {
            for (Vertex const* t = path.get_top(); t != v;) {
                onpath.diff(t->id());
                path.pop();
                t = path.get_top();
            }
            ASSERT0(path.get_top() == v);
            path.pop();
            onpath.diff(v->id());
        }
    }
    return;
}


//Searchs for SCC starting from 'v'.
//onpath: records vertices till in accessing stack.
//visited: records vertices that have been visited.
void SCC::scanRecur(Vertex const* v, VertexSet & onpath, VertexSet & visited,
                    GAI & gai, UINT & count, Group2VertexSet & group2bs,
                    Stack<Vertex const*> & path)
{
    ASSERTN(is_init(), ("not yet initialized."));
    visited.bunion(v->id());
    gai.set_index(v->id(), count);
    gai.set_group(v->id(), count);
    count++;
    ASSERT0(!onpath.is_contain(v->id()));
    onpath.bunion(v->id());
    path.push(v);
    UINT v_group = gai.get_group(v->id());

    //Walk through each successors of 'v'
    bool find_scc = false;
    for (EdgeC * el = v->getOutList(); el != nullptr; el = el->get_next()) {
        Vertex const* succ = el->getEdge()->to();
        if (!visited.is_contain(succ->id())) {
            scanRecur(succ, onpath, visited, gai, count, group2bs, path);
        }

        if (onpath.is_contain(succ->id())) {
            UINT succ_group = gai.get_group(succ->id());
            v_group = MIN(v_group, succ_group);
            if (v_group == succ_group) {
                find_scc = true;
            }
            continue;
        }

        UINT succ_group = gai.get_group(succ->id());
        //The successor has been visited by other path.
        //The successor may be included in other scc.
        ASSERT0(v_group != succ_group);
    }
    gai.set_group(v->id(), v_group);

    if (find_scc) {
        addToGroup(v, v_group, group2bs);
    }
 
    if (v_group == gai.get_index(v->id())) {
        for (Vertex const* t = path.get_top(); t != v;) {
            onpath.diff(t->id());
            path.pop();
            t = path.get_top();
        }
        ASSERT0(path.get_top() == v);
        path.pop();
        onpath.diff(v->id());
    }
}


void SCC::addToGroup(Vertex const* v, UINT group, Group2VertexSet & group2bs)
{
    VertexSet * p = group2bs.get(group);
    if (p == nullptr) {
        p = m_sbsmgr.allocSBitSet();
        group2bs.set(group, p);
        m_scc.append_tail(p);
    }
    p->bunion(v->id());
}


//Find each strongly connected components, include minor or nest scc.
//e.g:scc1 includes graph vertex 1,2,3,4; scc2 includes node 5,6.
//    Both of them can be found.
void SCC::findSCC()
{
    ASSERTN(is_init(), ("not yet initialized."));
    //Search for connected components
    VertexSet visited(getSbsMgr()->getSegMgr());
    VertexSet onpath(getSbsMgr()->getSegMgr());
    GAI gai(m_g->is_dense());
    UINT count = UNDEF_GROUP + 1;
    Group2VertexSet group2bs;
    VertexIter cur = VERTEX_UNDEF;
    Stack<Vertex const*> path;

    for (Vertex const* v = m_g->get_first_vertex(cur);
         v != nullptr; v = m_g->get_next_vertex(cur)) {
        if (visited.is_contain(v->id())) {
            continue;
        }
        onpath.clean();
        path.clean();
        #ifdef USE_NONRECURSIVE_ALGO
        scanNoRecur(v, onpath, visited, gai, count, group2bs, path);
        #else
        scanRecur(v, onpath, visited, gai, count, group2bs, path);
        #endif
    }
}


//Return true if 'v' is in SCC, and records the SCC in 'scc_vertex_set'.
//scc_vertex_set: records the SCC if it is not NULL.
bool SCC::isInSCC(UINT vid, OUT VertexSet ** scc_vertex_set) const
{
    VertexSetIter it;
    for (VertexSet * scc = m_scc.get_head(&it);
         scc != nullptr; scc = m_scc.get_next(&it)) {
        if (scc->is_contain(vid)) {
            if (scc_vertex_set != nullptr) {
                *scc_vertex_set = scc;
            }
            return true;
        }
    }
    return false;
}


void SCC::dump(FILE * h) const
{
    ASSERT0(h);
    fprintf(h, "\nSCC INFO:\n");
    VertexSetIter it;
    for (VertexSet const* scc = m_scc.get_head(&it);
         scc != nullptr; scc = m_scc.get_next(&it)) {
        scc->dump(h);
    }
    fflush(h);
}

} //namespace xcom
