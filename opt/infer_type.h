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
#ifndef _INFER_TYPE_H_
#define _INFER_TYPE_H_

namespace xoc {

//Perform type inference for IR.
class InferType : public Pass {
    COPY_CONSTRUCTOR(InferType);
    TypeMgr * m_tm;
    IRCFG * m_cfg;
    IRList m_wl;
    ConstIRList * m_changed_irlist; //used for dump
    List<Var const*> * m_changed_varlist; //used for dump
protected:
    void addChanged(IR * ir);
    void addDump(IR const* ir) const;
    void addDump(Var const* var) const;

    void dumpInit();
    void dumpFini();

    //Infer variable's type.
    bool inferVarTypeByIRCode(IR const* ir) const;
    bool inferLeafExpMemAcc(IR * ir);
    bool inferIld(IR * ir);
    bool inferArray(IR * ir) const;
    bool inferStmtCall(IR * ir) const;
    bool inferStmtPhi(IR * ir) const;
    bool inferStmtMemAcc(IR * ir);
    bool inferExpMemAcc(IR * ir);
    bool inferUnaOP(IR * ir);
    bool inferBinOP(IR * ir);
    bool inferSelect(IR * ir);
    bool inferIR(IR * ir);
    bool inferBBList(BBList const* bbl);
    bool inferIRList(IR * irl);
    bool inferChangedList();
public:
    explicit InferType(Region * rg) : Pass(rg)
    {
        ASSERT0(rg != nullptr);
        m_tm = rg->getTypeMgr();
        m_cfg = rg->getCFG();
        m_changed_irlist = nullptr;
        m_changed_varlist = nullptr;
    }
    virtual ~InferType() { dumpFini(); }

    //The function dump pass relative information before performing the pass.
    //The dump information is always used to detect what the pass did.
    //Return true if dump successed, otherwise false.
    virtual bool dumpBeforePass() const { return Pass::dumpBeforePass(); }

    //The function dump pass relative information.
    //The dump information is always used to detect what the pass did.
    //Return true if dump successed, otherwise false.
    virtual bool dump() const;

    virtual CHAR const* getPassName() const { return "Infer Type"; }
    virtual PASS_TYPE getPassType() const { return PASS_INFER_TYPE; }

    virtual bool perform(OptCtx & oc);
};

} //namespace xoc
#endif
