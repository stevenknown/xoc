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
#ifndef __SEG_TREE_H_
#define __SEG_TREE_H_

namespace xcom {

class Seg;
class SegInfo;

typedef UINT SegType;

#define SEG_MIN 0
#define SEG_MAX ((SegType)-1)
#undef max

class SegInfo {
    //THE CLASS ALLOWS COPY-CONSTRUCTION.
public:
    UINT count;
public:
    SegInfo() { count = 0; }

    void add(Seg const* seg);
    void add(SegInfo const& info);

    void clean() { count = 0; }

    void max(SegInfo const& l, SegInfo const& r);
};

class SegUpdateCtx {
    //THE CLASS ALLOWS COPY-CONSTRUCTION.
public:
    UINT update_count;
public:
    SegUpdateCtx() { update_count = 0; }
};

class Seg {
    //THE CLASS ALLOWS COPY-CONSTRUCTION.
public:
    SegType start;
    SegType end;
    SegInfo info;
public:
    Seg(UINT c) {} //To avoid sstl template complaint.
    Seg(SegType pstart, SegType pend)
        : start(pstart), end(pend) {}
};

typedef Vector<Seg> SegVec;

//This class represents a Segment Tree.
class SegTree : public Tree {
    COPY_CONSTRUCTOR(SegTree);
protected:
    //True to accumulate the update-count while user calling compute().
    //If the flag is true, the SegInfo of root tree node will answer how many
    //segments are overlapped to the user given segment.
    bool m_is_accumulate_seg;
    Vertex * m_root;
    SMemPool * m_pool;
protected:
    Seg * allocSeg() { return (Seg*)xmalloc(sizeof(Seg)); }

    Vertex * buildRecur(SegType start, SegType end);

    void computeRecur(
        Vertex const* v, SegType start, SegType end, MOD SegUpdateCtx & ctx);
    void computeMinMax(SegVec const& sv, OUT SegType & min, OUT SegType & max);

    virtual void dumpNodeBuf(Vertex const* v, OUT StrBuf & buf) const override;
    virtual void dumpVertexDesc(
        Vertex const* v, OUT DefFixedStrBuf & buf) const override;

    void queryRecur(
        Vertex const* v, SegType start, SegType end, OUT SegInfo & si);

    void updateSegIfCompleteMatch(MOD Seg * seg, OUT SegUpdateCtx & ctx);
    void updateSegBothLchildAndRchildFinish(
        MOD Seg * vseg, Seg const* lseg, Seg const* rseg,
        SegUpdateCtx & ctx, SegUpdateCtx & lctx, SegUpdateCtx & rctx);
    void updateSegAfterLchildFinish(
        MOD Seg * vseg, Seg const* lseg, Seg const* rseg,
        SegUpdateCtx & ctx, SegUpdateCtx & tctx);
    void updateSegAfterRchildFinish(
        MOD Seg * vseg, Seg const* lseg, Seg const* rseg,
        SegUpdateCtx & ctx, SegUpdateCtx & tctx);

    void * xmalloc(UINT size)
    {
        ASSERTN(m_pool, ("pool does not initialized"));
        void * p = xcom::smpoolMallocConstSize(size, m_pool);
        ASSERT0(p != nullptr);
        ::memset((void*)p, 0, size);
        return p;
    }
public:
    SegTree() : m_root(nullptr)
    {
        m_pool = smpoolCreate(sizeof(Seg) * 4, MEM_CONST_SIZE);
        m_is_accumulate_seg = false;
    }
    ~SegTree() { smpoolDelete(m_pool); }

    Vertex const* addSeg(Seg const& seg);

    void build(SegVec const& sv);

    //The function computes information for a vector of segs.
    void compute(SegVec const& sv);

    //The function computes the number of segments that contain or overlap
    //to the value-range that described by 'seg'.
    //e.g: the given 'seg' is {start:2, end:4}, the segments on the tree that
    //overlapped to {2-4} are {1,3}, {1,2}, {3,3}, {2,2}, {4,5}, {4,4}.
    void compute(Seg const& seg);
    void compute(Vertex const* root, Seg const& seg);

    void dumpDOT(CHAR const* fn = "segtree.dot") const { Tree::dumpDOT(fn); }

    //Get the segment of given tree vertex.
    Seg * getSeg(Vertex const* v) const { ASSERT0(v); return (Seg*)v->info(); }
    Seg * getAndGenSeg(Vertex * v);

    bool isAccOverlapSeg() const { return m_is_accumulate_seg; }

    Vertex * setSeg(VexIdx v, Seg const& seg)
    {
        Vertex * vex = addVertex(v);
        setSeg(vex, seg);
        return vex;
    }
    void setSeg(Vertex * v, Seg const& seg)
    { setSeg(v, seg.start, seg.end); }
    Vertex * setSeg(VexIdx v, SegType start, SegType end)
    {
        Vertex * vex = addVertex(v);
        ASSERTN(vex, ("V%u is not on graph"));
        setSeg(vex, start, end);
        return vex;
    }
    void setSeg(Vertex * v, SegType start, SegType end);
    void setSeg(SegVec const& sv);
    void setAccOverlapSeg() { m_is_accumulate_seg = true; }

    void query(SegType start, SegType end, OUT SegInfo & si);
};

} //namespace xcom

#endif
