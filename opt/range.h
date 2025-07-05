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
DISCLAIMED. IN NO EVENT SHALL THE CONTRIBUTORS BE LIABLE FOR ANY
DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
(INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND
ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
(INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
@*/
#ifndef _RANGE_H_
#define _RANGE_H_

namespace xoc {

//
//START Range
//
#define RG_start(r) ((r).m_start)
#define RG_end(r) ((r).m_end)
class Range {
public:
    Pos m_start;
    Pos m_end;
public:
    //Range is point.
    Range() : m_start(0), m_end(0) {}
    Range(Pos p) : m_start(p), m_end(p) {}
    Range(Pos s, Pos e) : m_start(s), m_end(e) {}

    void dump(Region const* rg) const;
    //Dump the range with graphics.
    //'init_pos': the start position in dump range, it may be initialized by
    //            POS_INIT_VAL or the end() of previous range in dump RangeVec.
    //e.g.:
    // CASE1: range:<14-15>
    //        graphics: |              --
    //
    // CASE2: RangeVec:<0-3> <14-15>
    //        graphics: |----          --
    void dumpG(Pos init_pos, Region const* rg) const;
    //Dump the range between 'in_start' and 'in_end' position with graphics.
    void dumpGWithPos(Pos init_pos, Region const* rg,
                      Pos in_start, Pos in_end) const;
    bool is_less(Range const src) const { return end() < src.start(); }
    bool is_great(Range const src) const { return start() > src.end(); }
    bool is_contain(Pos pos) const { return start() <= pos && end() >= pos; }
    //Return true if current range intersects with src.
    //e.g:four cases of intersection:
    // cur: |----|
    // src:    |---|
    //
    // cur:   |----|
    // src: |---------|
    //
    // cur:   |----|
    // src: |----|
    //
    // cur: |----|
    // src:  |--|
    bool is_intersect(Range const src) const
    {
        return (src.end() >= end() && src.start() <= end()) ||
               (src.end() <= end() && src.end() >= start());
    }
    Pos end() const { return m_end; }
    Pos start() const { return m_start; }

    void setEnd(Pos e) { m_end = e; }
};
//END Range


//
//START RangeVec
//
class RangeVec : public Vector<Range> {
    COPY_CONSTRUCTOR(RangeVec);
public:
    RangeVec() {}
    Range const* get_vec() const
    { return const_cast<RangeVec*>(this)->Vector<Range>::get_vec(); }
};
//END RangeVec

} //namespace xoc
#endif
