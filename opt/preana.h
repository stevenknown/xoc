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
#ifndef _PRE_ANA_
#define _PRE_ANA_

namespace xoc {

//This class represents the preprocessing and analysis before performing
//optimizations.
class PreAnaBeforeOpt : public Pass {
    COPY_CONSTRUCTOR(PreAnaBeforeOpt);
protected:
    Region * m_rg;
    RegionMgr * m_rm;
    TypeMgr const* m_tm;
    MDSystem * m_mdsys;
    VarMgr * m_vm;
protected:
    bool scanCallStmt(IR const* ir);
    bool scanLDA(IR const* ir);
    bool scanID(IR const* ir);
    bool scanReturn(IR const* ir);
    bool scanExtOp(IR const* ir);
    bool scanKid(IR const* ir);
public:
    explicit PreAnaBeforeOpt(Region * rg) : Pass(rg), m_rg(rg)
    {
        ASSERT0(m_rg != nullptr);
        m_rm = m_rg->getRegionMgr();
        m_tm = m_rg->getTypeMgr();
        m_mdsys = m_rg->getMDSystem();
        m_vm = m_rg->getVarMgr();
    }
    virtual ~PreAnaBeforeOpt() {}

    virtual bool dump() const;

    virtual CHAR const* getPassName() const
    { return "Preprocessing and Analysis"; }
    PASS_TYPE getPassType() const { ASSERT0(0); return PASS_UNDEF; }

    //Prepare informations for analysis phase, such as record
    //which variables have been taken address for both
    //global and local variable.
    bool scanIRList(IR const* ir);
    bool scanBBList(BBList const* bblst);
    virtual bool perform(OptCtx & oc);
};

} //namespace xoc

#endif
