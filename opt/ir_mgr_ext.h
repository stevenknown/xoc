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
#ifndef __IR_MGR_EXT_H__
#define __IR_MGR_EXT_H__

namespace xoc {

class IRMgrExt : public IRMgr {
    COPY_CONSTRUCTOR(IRMgrExt);
public:
    explicit IRMgrExt(Region * rg);
    virtual ~IRMgrExt() {}

    //Build atomic cas operation of compare and swap on memory.
    //memory: Opearated memory.
    //oldval: Value to be compared with the value in memory.
    //newval: If "oldval" equals to the value in memory, "newval" will be set
    //        to the memory.
    //reslst: Multiple results will be modified.
    IR * buildAtomCas(Type const* type, IR * memory, IR * oldval, IR * newval,
                      IR * reslst);

    //Build atomic inc operation of fetch and add on memory.
    //memory: Opearated memory.
    //reslst: Multiple results will be modified.
    //addend: (optional) Number to be added to memory, absent on T1.
    IR * buildAtomInc(Type const* type, IR * memory, IR * reslst,
                      IR * addend = nullptr);

    IR * buildBroadCast(IR * src, IR * res_list, Type const* ty);

    IR * buildVIStore(IR * base, TMWORD ofst, IR * rhs, IR * dummyuse,
                      Type const* ty);
    IR * buildVStore(Var * lhs, TMWORD ofst, IR * rhs, IR * dummyuse,
                     Type const* ty);
    IR * buildVStorePR(PRNO resprno, IR * rhs, IR * dummyuse, Type const* ty);

    #ifdef REF_TARGMACH_INFO
    IR * buildPhyReg(xgen::Reg reg, RegPhi * regphi);
    #endif

    //Return expression list that describe multiple isomorphic result.
    virtual IR * getAlterResDescList(IR * stmt) const;

    //Return true if stmt has multiple results.
    virtual bool hasMultiRes(IR * stmt) const;

    virtual bool perform(OptCtx & oc) { DUMMYUSE(oc); return false; }
};

} //namespace xoc
#endif
