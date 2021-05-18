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

#ifndef __SCC_H_
#define __SCC_H_

namespace xcom {

//Group and Index
class GAI {
    BYTE m_is_dense:1;
    union {
        Vector<UINT> * dense;
        TMap<UINT, UINT> * sparse;
    } group;
    union {
        Vector<UINT> * dense;
        TMap<UINT, UINT> * sparse;
    } index;
    COPY_CONSTRUCTOR(GAI);
public:
    GAI(bool is_dense)
    {
        if (is_dense) {
            group.dense = new Vector<UINT>();
            index.dense = new Vector<UINT>();
        } else {
            group.sparse = new TMap<UINT, UINT>();
            index.sparse = new TMap<UINT, UINT>();
        }
        m_is_dense = is_dense;
    }
    ~GAI()
    {
        if (m_is_dense) {
            delete group.dense;
            delete index.dense;
        } else {
            delete group.sparse;
            delete index.sparse;
        }
    }

    void set_group(UINT idx, UINT group_val)
    {
        if (m_is_dense) {
            group.dense->set(idx, group_val);
        } else {
            group.sparse->setAlways(idx, group_val);
        }
    }

    UINT get_group(UINT idx) const
    {
        if (m_is_dense) {
            return group.dense->get(idx);
        }
        return group.sparse->get(idx);
    }

    void set_index(UINT i, UINT idx)
    {
        if (m_is_dense) {
            index.dense->set(i, idx);
        } else {
            index.sparse->setAlways(i, idx);
        }
    }

    UINT get_index(UINT i) const
    {
        if (m_is_dense) {
            return index.dense->get(i);
        }
        return index.sparse->get(i);
    }
};


//The class finds all Strongly Connected Component on given graph.
//USAGE:
//  Graph g;
//  SCC s(&g);
//  s.findSCC();
//  s.dump(h); //Dump SCC.
class SCC {
public:
    //The map is used to loop up container rapidly.
    typedef DefSBitSet VertexSet;
    typedef List<VertexSet const*> ConstVertexSetList;
    typedef List<VertexSet*> VertexSetList;
    typedef C<VertexSet*> * VertexSetIter;
    typedef TMap<UINT, VertexSet*> Group2VertexSet;

protected:
    //BitSetMgr m_bsmgr;
    DefMiscBitSetMgr m_sbsmgr;

    Graph * m_g;
    VertexSetList m_scc;

    void addToGroup(Vertex const* v, UINT group, Group2VertexSet & group2bs);
    bool is_init() const { return m_g != nullptr; }

    //Searchs for scc starting from 'v'.
    //onpath: records vertices still in accessing stack.
    //visited: records vertices have been found.
    void scanRecur(Vertex const* v, VertexSet & onpath, VertexSet & visited,
                   GAI & gai, UINT & count, Group2VertexSet & group2bs,
                   Stack<Vertex const*> & stk);

    //Searchs for SCC starting from 'root'.
    //onpath: records vertices till in accessing stack.
    //visited: records vertices that have been visited.
    void scanNoRecur(Vertex const* root, VertexSet & onpath,
                     VertexSet & visited, GAI & gai, UINT & count,
                     Group2VertexSet & group2bs,
                     Stack<Vertex const*> & stk);
public:
    SCC(Graph * g);
    ~SCC();

    void destroy();

    //Find each strongly connected components on graph.
    //e.g:scc1 includes graph vertex 1,2,3,4; scc2 includes node 5,6.
    //    Both of them can be found.
    void findSCC();

    DefMiscBitSetMgr * getSbsMgr() { return &m_sbsmgr; }
    VertexSetList & getSCCList() { return m_scc; }

    void init(Graph * g);

    //Return true if 'v' is in SCC, and records the SCC in 'scc_vertex_set'.
    //scc_vertex_set: records the SCC if it is not NULL.
    bool isInSCC(Vertex const* v,
                 OUT VertexSet ** scc_vertex_set = nullptr) const
    { return isInSCC(v->id(), scc_vertex_set); }

    //Return true if 'v' is in SCC, and records the SCC in 'scc_vertex_set'.
    bool isInSCC(UINT vid, OUT VertexSet ** scc_vertex_set = nullptr) const;

    void dump(FILE * h) const;
};

} //namespace xcom
#endif
