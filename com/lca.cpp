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

//
//START NaiveLCA
//
VexIdx NaiveLCA::query(VexIdx a, VexIdx b)
{
    Vertex const* av = m_tree->getVertex(a);
    Vertex const* bv = m_tree->getVertex(b);
    ASSERT0(av && bv);
    ASSERT0(av->getHeight() != HEIGHT_UNDEF && bv->getHeight() != HEIGHT_UNDEF);
    if (av->getHeight() > bv->getHeight()) {
        for (; av != nullptr && av->getHeight() > bv->getHeight();
             av = m_tree->getParent(av)) {}
        ASSERTN(av, ("av and bv are not belong to same tree."));
        ASSERTN(av->getHeight() != HEIGHT_UNDEF, ("illegal height"));
    } else if (bv->getHeight() > av->getHeight()) {
        for (; bv != nullptr && bv->getHeight() > av->getHeight();
             bv = m_tree->getParent(bv)) {}
        ASSERTN(bv, ("av and bv are not belong to same tree."));
        ASSERTN(bv->getHeight() != HEIGHT_UNDEF, ("illegal height"));
    }
    ASSERT0(av->getHeight() == bv->getHeight());
    for (; av != bv; av = m_tree->getParent(av), bv = m_tree->getParent(bv)) {
        ASSERT0(av && bv);
        ASSERT0(av->getHeight() && bv->getHeight());
    }
    ASSERTN(av, ("av and bv are not belong to same tree."));
    return av->id();
}
//END NaiveLCA


//
//START BinLCA
//
void BinLCA::clean()
{
    Tree const* t = m_tree;
    destroy();
    init(t);
}


void BinLCA::init(Tree const* t)
{
    if (m_tree != nullptr) { return; }
    m_tree = t;
    m_maxheight = HEIGHT_UNDEF;
    if (t->is_dense()) {
        m_d_idx2vec = new DenseIdx2VecMap;
        m_s_idx2vec = nullptr;
    } else {
        m_d_idx2vec = nullptr;
        m_s_idx2vec = new SparseIdx2VecMap;
    }
}


void BinLCA::destroy()
{
    if (m_tree == nullptr) { return; }
    if (m_s_idx2vec != nullptr) {
        SparseIdx2VecMapIter it;
        VertexVec * vec = nullptr;
        for (VexIdx i = m_s_idx2vec->get_first(it, &vec);
             i != VERTEX_UNDEF; i = m_s_idx2vec->get_next(it, &vec)) {
            ASSERT0(vec);
            delete vec;
        }
        delete m_s_idx2vec;
        m_s_idx2vec = nullptr;
    }
    if (m_d_idx2vec != nullptr) {
        for (UINT i = 0; i < m_d_idx2vec->get_elem_count(); i++) {
            VertexVec * vec = m_d_idx2vec->get(i);
            if (vec != nullptr) {
                delete vec;
            }
        }
        delete m_d_idx2vec;
        m_d_idx2vec = nullptr;
    }
    m_tree = nullptr;
}


VexIdx BinLCA::findMaxHeight() const
{
    VexIdx maxheight = HEIGHT_INIT_VAL;
    VertexIter it;
    for (Vertex const* v = m_tree->get_first_vertex(it);
         v != nullptr; v = m_tree->get_next_vertex(it)) {
        if (!m_tree->hasKid(v)) {
            maxheight = MAX(v->getHeight(), maxheight);
        }
    }
    return maxheight;
}


void BinLCA::setVec(VexIdx vid, VertexVec * vec) const
{
    if (m_s_idx2vec != nullptr) {
        m_s_idx2vec->set(vid, vec);
        return;
    }
    ASSERT0(m_d_idx2vec);
    m_d_idx2vec->set(vid, vec);
}


BinLCA::VertexVec * BinLCA::getVec(VexIdx vid) const
{
    if (m_s_idx2vec != nullptr) { return m_s_idx2vec->get(vid); }
    ASSERT0(m_d_idx2vec);
    return m_d_idx2vec->get(vid);
}


BinLCA::VertexVec * BinLCA::allocVertexVec(VexIdx vid)
{
    VertexVec * vec = getVec(vid);
    if (vec == nullptr) {
        vec = new VertexVec;
        setVec(vid, vec);
    }
    return vec;
}


Vertex * BinLCA::getAncestor(Vertex const* v, UINT d) const
{
    VertexVec const* vec = getVec(v->id());
    if (vec == nullptr) { return nullptr; }
    return vec->get(d);
}


void BinLCA::setAncestor(Vertex const* v, UINT d, Vertex * a)
{
    ASSERT0(v && a);
    VertexVec * vec = getVec(v->id());
    ASSERT0(vec);
    vec->set(d, a);
}


void BinLCA::standInLine(Vertex const*& av, Vertex const*& bv)
{
    VexIdx ah = av->getHeight();
    VexIdx bh = bv->getHeight();
    ASSERT0(ah != HEIGHT_UNDEF && bh != HEIGHT_UNDEF);
    Vertex const* pv;
    VexIdx dis;
    if (ah < bh) {
        pv = bv;
        dis = bh - ah;
    } else {
        pv = av;
        dis = ah - bh;
    }
    UINT maxdistance = m_maxheight - 1;
    for (UINT d = 0; d <= maxdistance; d++) {
        if (dis & (1 << d)) {
            pv = getAncestor(pv, d);
        }
    }
    if (ah < bh) {
        bv = pv;
    } else {
        av = pv;
    }
}


void BinLCA::initAncestorDisTab()
{
    VertexIter it;
    for (Vertex const* v = m_tree->get_first_vertex(it);
         v != nullptr; v = m_tree->get_next_vertex(it)) {
        VertexVec * vec = allocVertexVec(v->id());
        Vertex * parent = m_tree->getParent(v);
        if (parent != nullptr) {
            vec->set(0, parent);
        }
    }
    if (m_maxheight <= 1) { return; }
    UINT maxdistance = m_maxheight - 1;
    for (UINT d = 1; d <= maxdistance; d++) {
        VertexIter it;
        for (Vertex const* v = m_tree->get_first_vertex(it);
             v != nullptr; v = m_tree->get_next_vertex(it)) {
            Vertex * a = getAncestor(v, d - 1);
            if (a == nullptr) {
                continue;
            }
            ASSERT0(a);
            Vertex * a2 = getAncestor(a, d - 1);
            if (a2 == nullptr) {
                continue;
            }
            setAncestor(v, d, a2);
        }
    }
}


VexIdx BinLCA::query(VexIdx a, VexIdx b)
{
    if (m_maxheight == HEIGHT_UNDEF) {
        m_maxheight = findMaxHeight();
        ASSERT0(m_maxheight != HEIGHT_UNDEF);
    }
    initAncestorDisTab();
    Vertex const* av = m_tree->getVertex(a);
    Vertex const* bv = m_tree->getVertex(b);
    standInLine(av, bv);
    if (av == bv) { return av->id(); }
    UINT maxdistance = m_maxheight - 1;
    for (UINT d = maxdistance; d >= 0; d--) {
        Vertex const* anc_a = getAncestor(av, d);
        Vertex const* anc_b = getAncestor(bv, d);
        //Ancestor of av or bv at distance d may be nullptr.
        //ASSERT0(anc_a && anc_b);
        if (anc_a != anc_b) {
            av = anc_a;
            bv = anc_b;
        }
        if (d == 0) {
            //LCA will be the parent of the last update of av.
            break;
        }
    }
    ASSERT0(av);
    Vertex const* lca = getAncestor(av, 0);
    ASSERT0(lca);
    return lca->id();
}


void BinLCA::dump(FILE * h) const
{
    ASSERT0(h);
    fprintf(h, "\nANCESTOR DISTANCE TAB:[distance:vertex]");
    ASSERT0(m_tree->getRoot());
    BFSTreeIter it(*const_cast<Tree*>(m_tree));
    for (Vertex const* v = it.get_first(); v != nullptr;
         v = it.get_next(v)) {
        fprintf(h, "\nv%d:", v->id());
        for (VexIdx i = HEIGHT_INIT_VAL; i <= m_maxheight; i++) {
            fprintf(h, "\n  d%d:", i); //print the distance to each ancestor.
            Vertex * a = getAncestor(v, i);
            if (a == nullptr) {
                fprintf(h, "--");
                continue;
            }
            fprintf(h, "v%d", a->id());
        }
    }
    fflush(h);
}
//END BinLCA

} //namespace xcom
