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
#ifndef __IR_OPT_H__
#define __IR_OPT_H__

namespace xoc {

//Basis Class of pass.
class Pass {
protected:
    SimpCTX * m_simp;
public:
    Pass() { m_simp = NULL; }
    virtual ~Pass() {}
    COPY_CONSTRUCTOR(Pass);

    virtual CHAR const* get_pass_name() const
    {
        ASSERT(0, ("Optimization Dependent Code"));
        return NULL;
    }

    virtual PASS_TYPE get_pass_type() const
    {
        ASSERT(0, ("Optimization Dependent Code"));
        return PASS_UNDEF;
    }

    void set_simp_cont(SimpCTX * simp) { m_simp = simp; }

    virtual bool perform(OptCTX &)
    {
        ASSERT(0, ("Optimization Dependent Code"));
        return false;
    }
};

} //namespace xoc
#endif
