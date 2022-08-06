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
#ifndef _INVERT_BRTGT_H_
#define _INVERT_BRTGT_H_

namespace xoc {

class InvertBrTgt;

//The optimization attempt to invert branch target.
class InvertBrTgt : public Pass {
    COPY_CONSTRUCTOR(InvertBrTgt);
    CIRList * m_changed_irlist; //used for dump
    OptCtx * m_oc;
protected:
    void addDump(IR const* ir) const;
    void dumpInit();
    void dumpFini();
public:
    explicit InvertBrTgt(Region * rg) : Pass(rg)
    {
        ASSERT0(rg != nullptr);
        m_oc = nullptr;
        m_changed_irlist = nullptr;
    }
    virtual ~InvertBrTgt() { dumpFini(); }

    //The function dump pass relative information before performing the pass.
    //The dump information is always used to detect what the pass did.
    //Return true if dump successed, otherwise false.
    virtual bool dumpBeforePass() const { return Pass::dumpBeforePass(); }

    //The function dump pass relative information.
    //The dump information is always used to detect what the pass did.
    //Return true if dump successed, otherwise false.
    virtual bool dump() const;

    virtual CHAR const* getPassName() const
    { return "Invert Branch Target"; }
    virtual PASS_TYPE getPassType() const { return PASS_INVERT_BRTGT; }
    OptCtx * getOptCtx() const { return m_oc; }

    static bool invertLoop(InvertBrTgt * invt, IR * br, IRBB * br_tgt,
                             IR * jmp, IRBB * jmp_tgt);

    virtual bool perform(OptCtx & oc);
};

} //namespace xoc
#endif
