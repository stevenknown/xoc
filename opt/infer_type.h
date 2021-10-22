/*@
XOC Release License

Copyright (c) 2013-2014, Alibaba Group, All rights reserved.

    compiler@aliexpress.com

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

THIS SOFTWARE IS PROVIDED "AS IS" AND ANY EXPRESS OR IMPLIED
WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES
OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS
BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT
OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR
BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY,
WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR
OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN
IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

author: Su Zhenyu
@*/
#ifndef _INFER_TYPE_H_
#define _INFER_TYPE_H_

namespace xoc {

//Perform type inference for IR.
class InferType : public Pass {
    COPY_CONSTRUCTOR(InferType);
    TypeMgr * m_tm;
    Region * m_rg;
    IRCFG * m_cfg;
    CIRList * m_changed_irlist; //used for dump
protected:
    void addDump(IR const* ir) const;

    void dumpInit();
    void dumpFini();

    bool inferStmtMemAcc(IR * ir) const;
    bool inferExpMemAcc(IR * ir) const;
    bool inferUnaOP(IR * ir) const;
    bool inferBinOP(IR * ir) const;
    bool inferSelect(IR * ir) const;
    bool inferIR(IR * ir) const;
    bool inferBBList(BBList const* bbl) const;
    bool inferIRList(IR * irl) const;
public:
    explicit InferType(Region * rg)
    {
        ASSERT0(rg != nullptr);
        m_rg = rg;
        m_tm = rg->getTypeMgr();
        m_cfg = rg->getCFG();
        m_changed_irlist = nullptr;
    }
    virtual ~InferType() {}

    virtual bool dump() const;

    Region * getRegion() const { return m_rg; }
    virtual CHAR const* getPassName() const
    { return "InferType"; }
    virtual PASS_TYPE getPassType() const { return PASS_INFER_TYPE; }

    virtual bool perform(OptCtx & oc);
};

} //namespace xoc
#endif
