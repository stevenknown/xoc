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
#ifndef _IR_LOOP_CVT_H_
#define _IR_LOOP_CVT_H_

namespace xoc {

// Perform Loop Convertion.
//Transform while-do loop to do-while loop.
//e.g: Given loop I, the loop induction variable is i, loop upper bound is N.
//
//while do loop is:
//    bb1:
//        if i > N, branch bb3;
//    bb2:
//        loop body;
//        goto bb1;
//    bb3:
//        ...
//
//after convertion:
//
//    bb1:
//        if i > N, branch bb3;
//    bb2:
//        loop body;
//        if i <= N, branch bb2;
//    bb3:
//        ...
class LoopCvt : public Pass {
protected:
    Region * m_rg;
    IRCFG * m_cfg;
    DUMgr * m_du;
    IRIter m_ii;

    bool is_while_do(LI<IRBB> const* li, IRBB ** gobackbb,
                    UINT * succ1, UINT * succ2);
    bool try_convert(LI<IRBB> * li, IRBB * gobackbb,
                    UINT succ1, UINT succ2);
    bool find_and_convert(List<LI<IRBB>*> & worklst);

public:
    explicit LoopCvt(Region * rg)
    {
        ASSERT0(rg != nullptr);
        m_rg = rg;
        m_du = rg->getDUMgr();
        m_cfg = m_rg->getCFG();
        ASSERT0(m_cfg && m_du);
    }
    COPY_CONSTRUCTOR(LoopCvt);
    virtual ~LoopCvt() {}

    Region * getRegion() const { return m_rg; }
    virtual CHAR const* getPassName() const { return "Loop Convertion"; }
    PASS_TYPE getPassType() const { return PASS_LOOP_CVT; }

    virtual bool perform(OptCtx & oc);
};

} //namespace xoc
#endif
