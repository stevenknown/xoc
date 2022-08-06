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
#ifndef __LCA_H_
#define __LCA_H_

namespace xcom {

//The class computes lowest common ancestor using naive method.
//USAGE:
//  Tree t;
//  NaiveLCA s(&t);
//  s.query(a, b);
class NaiveLCA {
    Tree * m_tree;
public:
    NaiveLCA(Tree * t) : m_tree(t) {}
    //Query for lowest common ancestor of 'a' and 'b'.
    //Return VERTEX_UNDEF if there is no LCA between a and b.
    //Note the complexity of each query is time O(logn), but is O(logn) usually,
    //whereas the algo does not need auxiliary space.
    VexIdx query(VexIdx a, VexIdx b);
};


//The class computes lowest common ancestor using binary lifting.
//USAGE:
//  Tree t;
//  BinLCA s(&t);
//  s.query(a, b);
class BinLCA {
    COPY_CONSTRUCTOR(BinLCA);
    Tree const* m_tree;
    VexIdx m_maxheight;
    typedef Vector<Vertex*> VertexVec;
    typedef TMap<VexIdx, VertexVec*> SparseIdx2VecMap;
    typedef TMapIter<VexIdx, VertexVec*> SparseIdx2VecMapIter;
    typedef Vector<VertexVec*> DenseIdx2VecMap;
    SparseIdx2VecMap * m_s_idx2vec;
    DenseIdx2VecMap * m_d_idx2vec;
private:
    VertexVec * allocVertexVec(VexIdx vid);
    VexIdx findMaxHeight() const;
    VertexVec * getVec(VexIdx vid) const;
    //d: distance from v.
    Vertex * getAncestor(Vertex const* v, UINT d) const;
    void initAncestorDisTab();
    VexIdx queryRecur(Vertex const* f, VexIdx a, VexIdx b,
                      TTab<VexIdx> & visited);
    void setVec(VexIdx vid, VertexVec * vec) const;
    //d: distance from v.
    void setAncestor(Vertex const* v, UINT d, Vertex * a);
    void standInLine(Vertex const*& av, Vertex const*& bv);
public:
    BinLCA(Tree const* t) { m_tree = nullptr; init(t); }
    ~BinLCA() { destroy(); }
    void clean();

    void dump(FILE * h) const;
    void destroy();

    void init(Tree const* t);

    //Set max height by user. If user set the max height, LCA will not
    //refind it again, thus speedup the query.
    void setMaxHeight(VexIdx max) { m_maxheight = max; }

    //Query for lowest common ancestor of 'a' and 'b'.
    //Return VERTEX_UNDEF if there is no LCA between a and b.
    //Note the complexity of each query is time O(logn) and spatial O(nlogn).
    VexIdx query(VexIdx a, VexIdx b);
};

} //namespace xcom
#endif
