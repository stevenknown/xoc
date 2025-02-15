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
#ifndef __SCALAR_OPT_H__
#define __SCALAR_OPT_H__

namespace xoc {

class ScalarOpt : public Pass {
    COPY_CONSTRUCTOR(ScalarOpt);
protected:
    PassMgr * m_pass_mgr;
    IRCFG * m_cfg;
    DUMgr * m_dumgr;
protected:
    //The function return true if current region should participate in
    //optimizations.
    virtual bool isParticipateInOpt() const;

    void updateCounter(
        Pass const* pass, UINT & cp_count, UINT & licm_count, UINT & rp_count,
        UINT & gcse_count, UINT & dce_count);
    bool worthToDo(
        Pass const* pass, UINT cp_count, UINT licm_count, UINT rp_count,
        UINT gcse_count, UINT dce_count);
public:
    explicit ScalarOpt(Region * rg) : Pass(rg)
    {
        m_pass_mgr = rg->getPassMgr();
        m_cfg = rg->getCFG();
        m_dumgr = rg->getDUMgr();
    }
    virtual ~ScalarOpt() {}

    //The function dump pass relative information before performing the pass.
    //The dump information is always used to detect what the pass did.
    //Return true if dump successed, otherwise false.
    virtual bool dumpBeforePass() const { return Pass::dumpBeforePass(); }

    //The function dump pass relative information.
    //The dump information is always used to detect what the pass did.
    //Return true if dump successed, otherwise false.
    virtual bool dump() const { return Pass::dump(); }

    virtual CHAR const* getPassName() const
    { return "Scalar Optimizations"; }
    virtual PASS_TYPE getPassType() const { return PASS_SCALAR_OPT; }

    virtual bool perform(OptCtx & oc);
};

} //namespace xoc
#endif
