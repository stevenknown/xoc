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
#ifndef __TREE_H_
#define __TREE_H_

namespace xcom {

class Tree : public Graph {
    COPY_CONSTRUCTOR(Tree);
    Vertex * m_root;
public:
    Tree() : m_root(nullptr) {}

    //The function computes tree node height. The initial value of tree node
    //is HEIGHT_UNDEF.
    //Return the maximum height of the tree.
    VexIdx computeHeight();

    //The function get the parent Vertex of tree node.
    Vertex * getParent(Vertex const* v) const
    {
        ASSERT0(getVertex(v->id()) == v);
        ASSERTN(v->getInDegree() <= 1, ("multiple parents"));
        return v->getNthInVertex(0);
    }

    //The function get the kid Vertex of tree node.
    Vertex * getKid(Vertex const* v) const
    {
        ASSERT0(getVertex(v->id()) == v);
        return v->getNthOutVertex(0);
    }
    Vertex * getRoot() const { return m_root; }

    bool hasLowerHeight(Vertex const* a, Vertex const* b) const
    { return a->getHeight() < b->getHeight(); }
    bool hasLowerHeight(VexIdx a, VexIdx b) const
    { return hasLowerHeight(getVertex(a), getVertex(b)); }
    bool hasKid(Vertex const* v) const { return getKid(v) != nullptr; }
    bool hasParent(Vertex const* v) const { return getParent(v) != nullptr; }

    //The function inserts a new kid tree node to 'v'.
    //e.g: parent
    //   ___|||___
    //  |    |    |
    //  k1   v    k3
    //       |
    //       k2
    // after inserting a new kid v:
    //e.g: parent
    //   ___|||___
    //  |    |    |
    //  k1   v    k3
    //       ||__
    //       |   |
    //       k2  kid
    void insertKid(VexIdx v, VexIdx kid);

    //The function inserts a new parent tree node to 'v'.
    //e.g: parent
    //   ___|||___
    //  |    |    |
    //  k1   v    k3
    // after inserting a new parent v:
    //e.g: parent
    //   ___|||________
    //  |    |         |
    //  k1  newparent  k3
    //       |
    //       v
    void insertParent(VexIdx v, VexIdx parent);

    //The function remove v from tree.
    void remove(VexIdx v);

    void setRoot(VexIdx v) { m_root = getVertex(v); ASSERT0(m_root); }
    void setRoot(Vertex * v)
    { ASSERT0(v == nullptr || isVertex(v)); m_root = v; }
};


//BFS iterate tree node.
class BFSTreeIter : public GraphIterOut {
public:
    BFSTreeIter(Tree & tree) : GraphIterOut(tree, tree.getRoot()) {}
    Vertex * get_first();
    Vertex * get_next(Vertex const* t);
    Tree const& getTree() { return (Tree const&)m_g; }
};


class VisitTree {
    COPY_CONSTRUCTOR(VisitTree);
    Tree const& m_tree;

    //Record the root vertex on Tree.
    Vertex const* m_root;

    //Record the maximum number of vertex.
    //Note it is not necessary to set this field when running the pass.
    //It is just used to achieve better performance.
    UINT m_maxnum;
public:
    VisitTree(Tree const& dt, VexIdx root) : m_tree(dt)
    {
        m_root = dt.getVertex(root);
        ASSERT0(m_root);
        m_maxnum = 10;
    }
    virtual ~VisitTree() {}

    Tree const& getTree() const { return m_tree; }

    //The function is a callback interface.
    //The function will be invoked when all kid of v have been accessed.
    //v: the vertex on Tree.
    virtual void visitWhenAllKidHaveBeenVisited(Vertex const* v)
    {
        //Target Dependent Code.
    }

    //The function is a callback interface.
    //The function will be invoked when first accessing the vertex v.
    //v: the vertex on Tree.
    virtual void visitWhenFirstMeet(Vertex const* v)
    {
        //Target Dependent Code.
    }

    //Set the maximum number of vertex.
    //Note it is not necessary to set this field when running the pass.
    //It is just used to achieve better performance.
    void setMaxNumOfVertex(UINT maxnum) { m_maxnum = maxnum; }

    void perform();
};

} //namespace xcom
#endif
