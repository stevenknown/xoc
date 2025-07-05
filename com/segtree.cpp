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
//START SegInfo
//
void SegInfo::add(Seg const* seg)
{
    ASSERT0(seg);
    count += seg->info.count;
}


void SegInfo::add(SegInfo const& info)
{
    count += info.count;
}


void SegInfo::max(SegInfo const& l, SegInfo const& r)
{
    count = MAX(l.count, r.count);
}
//END SegInfo


void SegTree::dumpNodeBuf(Vertex const* v, OUT StrBuf & buf) const
{
    Seg const* seg = getSeg(v);
    ASSERT0(seg);
    buf.strcat("V%u,[%u, %u],count:%u",
               v->id(), seg->start, seg->end, seg->info.count);
}


void SegTree::dumpVertexDesc(Vertex const* v, OUT DefFixedStrBuf & buf) const
{
    Seg const* seg = getSeg(v);
    ASSERT0(seg);
    buf.strcat("V%u,[%u, %u],count:%u",
               v->id(), seg->start, seg->end, seg->info.count);
}


void SegTree::setSeg(SegVec const& sv)
{
    VexIdx vi = VERTEX_UNDEF + 1;
    for (VecIdx i = 0; i < (VecIdx)sv.get_elem_count(); i++, vi++) {
        Seg seg = const_cast<SegVec&>(sv).get(i);
        setSeg(vi, seg);
    }
}


void SegTree::setSeg(Vertex * v, SegType start, SegType end)
{
    ASSERT0(v);
    Seg * seg = getAndGenSeg(v);
    ASSERT0(seg);
    ASSERT0(start >= SEG_MIN && start <= SEG_MAX);
    ASSERT0(end >= SEG_MIN && end <= SEG_MAX);
    ASSERT0(start <= end);
    seg->start = start;
    seg->end = end;
}


Seg * SegTree::getAndGenSeg(Vertex * v)
{
    Seg * seg = getSeg(v);
    if (seg != nullptr) { return seg; }
    seg = allocSeg();
    VERTEX_info(v) = (void*)seg;
    return seg;
}


void SegTree::computeMinMax(
    SegVec const& sv, OUT SegType & min, OUT SegType & max)
{
    min = SEG_MAX;
    max = SEG_MIN;
    for (VecIdx i = 0; i < (VecIdx)sv.get_elem_count(); i++) {
        Seg seg = const_cast<SegVec&>(sv).get(i);
        min = MIN(min, seg.start);
        max = MAX(max, seg.end);
    }
}


Vertex * SegTree::buildRecur(SegType start, SegType end)
{
    ASSERT0(start >= SEG_MIN && start <= SEG_MAX);
    ASSERT0(end >= SEG_MIN && end <= SEG_MAX);
    ASSERT0(start <= end);
    if (start == end) {
        return setSeg(getVertexNum() + 1, start, end);
    }
    SegType mid = (start + end) / 2;
    Vertex * cur = setSeg(getVertexNum() + 1, start, end);
    Vertex * lchild = buildRecur(start, mid);
    Vertex * rchild = buildRecur(mid + 1, end);
    ASSERT0(lchild && rchild);
    insertKid(cur, lchild);
    insertKid(cur, rchild);
    return cur;
}


void SegTree::build(SegVec const& sv)
{
    SegType min, max;
    computeMinMax(sv, min, max);
    ASSERT0(min >= SEG_MIN && min <= SEG_MAX);
    ASSERT0(max >= SEG_MIN && max <= SEG_MAX);
    ASSERT0(min <= max);
    setRoot(buildRecur(min, max));
}


void SegTree::updateSegIfCompleteMatch(MOD Seg * seg, OUT SegUpdateCtx & ctx)
{
    ctx.update_count = 1;
    seg->info.count += ctx.update_count;
}


void SegTree::updateSegBothLchildAndRchildFinish(
    MOD Seg * vseg, Seg const* lseg, Seg const* rseg,
    SegUpdateCtx & ctx, SegUpdateCtx & lctx, SegUpdateCtx & rctx)
{
    ctx.update_count = MAX(lctx.update_count, rctx.update_count);
    if (m_is_accumulate_seg) {
        vseg->info.count += ctx.update_count;
        return;
    }
    vseg->info.count = MAX(lseg->info.count, rseg->info.count);
}


void SegTree::updateSegAfterLchildFinish(
    MOD Seg * vseg, Seg const* lseg, Seg const* rseg,
    SegUpdateCtx & ctx, SegUpdateCtx & tctx)
{
    ctx.update_count = tctx.update_count;
    if (m_is_accumulate_seg) {
        vseg->info.count += ctx.update_count;
        return;
    }
    vseg->info.count = MAX(lseg->info.count, rseg->info.count);
}


void SegTree::updateSegAfterRchildFinish(
    MOD Seg * vseg, Seg const* lseg, Seg const* rseg,
    SegUpdateCtx & ctx, SegUpdateCtx & tctx)
{
    ctx.update_count = tctx.update_count;
    if (m_is_accumulate_seg) {
        vseg->info.count += ctx.update_count;
        return;
    }
    vseg->info.count = MAX(lseg->info.count, rseg->info.count);
}


void SegTree::computeRecur(
    Vertex const* v, SegType start, SegType end, MOD SegUpdateCtx & ctx)
{
    ASSERT0(v);
    ASSERT0(start <= end);
    Seg * vseg = getSeg(v);
    ASSERT0(vseg);
    if (vseg->start == start && vseg->end == end && start == end) {
        //If the user's start and end range completely match current segement,
        //the computation stop and update current segment info of tree node.
        updateSegIfCompleteMatch(vseg, ctx);
        return;
    }
    SegType mid = (vseg->start + vseg->end) / 2;
    Vertex const* lchild = getLchild(v);
    ASSERT0(lchild);
    Vertex const* rchild = getRchild(v);
    ASSERT0(rchild);
    Seg const* lseg = getSeg(lchild);
    Seg const* rseg = getSeg(rchild);
    if (end <= mid) {
        //If user's start and end range belong to the subset of left-half
        //segment, only compute the lchild of tree node.
        Vertex const* lchild = getLchild(v);
        ASSERT0(lchild);
        SegUpdateCtx tctx;
        computeRecur(lchild, start, end, tctx);
        updateSegAfterLchildFinish(vseg, lseg, rseg, ctx, tctx);
        return;
    }
    if (start > mid) {
        //If user's start and end range belong to the subset of right-half
        //segment, only compute the rchild of tree node.
        Vertex const* rchild = getRchild(v);
        ASSERT0(rchild);
        SegUpdateCtx tctx;
        computeRecur(rchild, start, end, tctx);
        updateSegAfterRchildFinish(vseg, lseg, rseg, ctx, tctx);
        return;
    }
    //User's start and end range spans across both left and right half
    //segment, thus both two half segments need to involved in the computation.
    SegUpdateCtx lctx;
    SegUpdateCtx rctx;
    computeRecur(lchild, start, mid, lctx);
    computeRecur(rchild, mid + 1, end, rctx);
    updateSegBothLchildAndRchildFinish(vseg, lseg, rseg, ctx, lctx, rctx);
}


void SegTree::queryRecur(
    Vertex const* v, SegType start, SegType end, OUT SegInfo & si)
{
    ASSERT0(start <= end);
    if (v == nullptr) { return; }
    Seg const* vseg = getSeg(v);
    ASSERT0(vseg);
    if (vseg->start == start && vseg->end == end && start == end) {
        si.add(vseg);
        return;
    }
    SegType mid = (vseg->start + vseg->end) / 2;
    if (end <= mid) {
        queryRecur(getLchild(v), start, end, si);
        return;
    }
    if (start > mid) {
        queryRecur(getRchild(v), start, end, si);
        return;
    }
    SegInfo lsi;
    SegInfo rsi;
    queryRecur(getLchild(v), start, mid, lsi);
    queryRecur(getRchild(v), mid + 1, end, rsi);
    si.max(lsi, rsi);
}


void SegTree::query(SegType start, SegType end, OUT SegInfo & si)
{
    Vertex const* root = getRoot();
    ASSERT0(root);
    queryRecur(root, start, end, si);
}


void SegTree::compute(Vertex const* root, Seg const& seg)
{
    ASSERT0(root);
    SegUpdateCtx ctx;
    computeRecur(root, seg.start, seg.end, ctx);
}


void SegTree::compute(Seg const& seg)
{
    Vertex const* root = getRoot();
    ASSERT0(root);
    compute(root, seg);
}


void SegTree::compute(SegVec const& sv)
{
    Vertex const* root = getRoot();
    ASSERT0(root);
    for (VecIdx i = 0; i < (VecIdx)sv.get_elem_count(); i++) {
        Seg seg = const_cast<SegVec&>(sv).get(i);
        compute(root, seg);
    }
}

} //namespace xcom
