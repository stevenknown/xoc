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
#ifndef _REVISE_CFG_H_
#define _REVISE_CFG_H_

namespace xoc {

//Map IR to LabelInfo.
typedef xcom::TMap<IR const*, LabelInfo const*> IR2Lab;
typedef xcom::TMapIter<IR const*, LabelInfo const*> IR2LabIter;

//Map IR to BB's id.
typedef xcom::TMap<IR const*, UINT> IR2BBId;

//The class sorts the order of predecessor of given BB according to PHI
//operand layout.
class SortPredByBBId {
    COPY_CONSTRUCTOR(SortPredByBBId);
    IRCFG * m_cfg;
    IR2BBId m_phiopnd2bbid;
public:
    SortPredByBBId(IRCFG * cfg) : m_cfg(cfg) {}
    void collectPhiOpnd2PredBB(IRBB const* bb);
    void collectPhiOpnd2PredBB();

    //Sort the order of predecessor of given BB according to PHI operand
    //layout.
    void sort(IRBB const* bb) const;

    //Revise CFG edge for BB that has phi.
    //NOTE:CFG should have been built before revise Vertex order.
    void sort() const;
};


//The class sorts the order of predecessor of given BB according to PHI
//operand layout.
class SortPredByLab {
    COPY_CONSTRUCTOR(SortPredByLab);
    IRCFG * m_cfg;
    IR2Lab m_phiopnd2lab;
public:
    SortPredByLab(IRCFG * cfg) : m_cfg(cfg) {}

    //Sort the order of predecessor of given BB according to PHI
    //operand layout.
    void sort(IRBB const* bb, IR2Lab const& ir2lab) const;

    //Revise CFG edge for BB that has phi.
    //NOTE:CFG should have been built before revise Vertex order.
    void sort(IR2Lab const& ir2lab) const;
};

} //namespace xoc
#endif
