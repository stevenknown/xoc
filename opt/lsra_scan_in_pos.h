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
#ifndef _LSRA_SCAN_IN_POS_H_
#define _LSRA_SCAN_IN_POS_H_

namespace xoc {

class ScanInPosOrder {
    COPY_CONSTRUCTOR(ScanInPosOrder);
    LSRAImpl & m_impl;
    LinearScanRA & m_ra;
    BBList * m_bb_list;
    IRCFG * m_cfg;
public:
    ScanInPosOrder(LSRAImpl & impl) : m_impl(impl), m_ra(impl.getRA())
    {
        m_bb_list = impl.getBBList();
        m_cfg = impl.getCFG();
    }

    void collectUnhandledForUse(IR const* ir, ConstIRIter & irit);
    void collectUnhandledForDef(IR const* ir);

    void perform() { scanBBList(m_bb_list); }

    void scanRHS(IR * ir, Pos upos, ConstIRIter & irit);
    void scanLHS(IR * ir, Pos dpos);
    void scanIR(IR * ir, UpdatePos & up, ConstIRIter & irit);
    void scanIRList(BBIRList & irlst, UpdatePos & up, ConstIRIter & irit);
    void scanBBList(BBList * bblst);

    void tryAssignRegForDefPos(Pos curpos, IR const* ir);
    void tryAssignRegForUsePos(Pos curpos, IR const* ir);

    bool verifyResourceForDefPos(IR const* ir) const;
    bool verifyResourceForUsePos(IR const* ir) const;
};

} //namespace xoc
#endif
