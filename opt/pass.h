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
#ifndef __PASS_H__
#define __PASS_H__

namespace xoc {

class SimpCtx;

//Basis Class of pass.
class Pass {
    COPY_CONSTRUCTOR(Pass);
protected:
    BYTE m_is_valid:1; //True if current pass information is available.
    Region * m_rg;
public:
    Pass() : m_is_valid(false), m_rg(nullptr) {}
    Pass(Region * rg) : m_is_valid(false), m_rg(rg) {}
    virtual ~Pass() {}

    //The function dump pass relative information.
    //The dump information is always used to detect what the pass did.
    //Return true if dump successed, otherwise false.
    virtual bool dump() const
    {
        //ASSERTN(0, ("Optimization Dependent Code"));
        //The recommended dump headline format is:
        //\n==---- DUMP PassName 'RegionName' ----==
        return true;
    }

    //The function dump pass relative information before performing the pass.
    //The dump information is always used to detect what the pass did.
    //Return true if dump successed, otherwise false.
    virtual bool dumpBeforePass() const
    {
        //ASSERTN(0, ("Optimization Dependent Code"));
        //The recommended dump headline format is:
        //\n==---- DUMP BEFORE PassName 'RegionName' ----==
        return true;
    }

    Region * getRegion() const { return m_rg; }
    virtual CHAR const* getPassName() const
    {
        ASSERTN(0, ("Pass Dependent Code"));
        return nullptr;
    }

    virtual PASS_TYPE getPassType() const
    {
        ASSERTN(0, ("Pass Dependent Code"));
        return PASS_UNDEF;
    }

    virtual bool is_valid() const { return m_is_valid; }

    virtual void set_valid(bool valid) { m_is_valid = valid; }

    //Return true means IR status changed, caller should consider whether
    //other optimizations should be reperform again. Otherwise return false.
    virtual bool perform(OptCtx &)
    {
        ASSERTN(0, ("Pass Dependent Code"));
        return false;
    }
};

} //namespace xoc
#endif
