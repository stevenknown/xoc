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
#ifndef _IR_DSE_H_
#define _IR_DSE_H_

namespace xoc {

class DSE;

class DSECtx : public PassCtx {
public:
    xcom::DomTree const& domtree;
    IRCFG * m_cfg;
    DSE * m_dse;
    MDSSAMgr * m_mdssamgr;
public:
    DSECtx(DSE * dse, OptCtx & oc, ActMgr * am, xcom::DomTree const& dt);
    DSE * getDSE() const { return m_dse; }
    IRCFG * getCFG() const { return m_cfg; }
    MDSSAMgr * getMDSSAMgr() const { return m_mdssamgr; }
};

//Perform Dead Store Elimination.
class DSE : public Pass {
    COPY_CONSTRUCTOR(DSE);
protected:
    MDSSAMgr * m_mdssamgr;
    ActMgr m_am;
protected:
    bool doStmt(
        IR * ir, MOD BBIRList & irlst, MOD BBIRListIter & it, DSECtx & ctx);
    bool doBBInDomTreeOrder(OptCtx & oc);

    bool initDepPass(MOD OptCtx & oc);

    bool useMDSSADU() const;
public:
    explicit DSE(Region * rg);
    virtual ~DSE() {}

    //Check if ir is appropriate for optimization.
    virtual bool canBeCandidate(IR const* ir) const
    {
        ASSERT0(ir && ir->is_stmt());

        //NOTE:Don't regard CallStmt as DSE candidate, because the DEF
        //of CallStmt contains inexact DEF of each MD. Thus we cannot determine
        //exactly that the CallStmt is dead-store.
        //e.g:Both foo and bar defined MD10, we shouldn't say bar
        //exact-covers foo.
        //void main() {
        //  foo(); //DEF:MD10V1
        //  bar(); //DEF:MD10v2
        //}
        return !ir->isMayThrow(true) && !ir->hasSideEffect(true) &&
               !ir->isNoMove(true) && !ir->isCallStmt();
    }

    bool doBB(IRBB * bb, DSECtx & ctx);

    //The function dump pass relative information before performing the pass.
    //The dump information is always used to detect what the pass did.
    //Return true if dump successed, otherwise false.
    virtual bool dumpBeforePass() const { return Pass::dumpBeforePass(); }

    //The function dump pass relative information.
    //The dump information is always used to detect what the pass did.
    //Return true if dump successed, otherwise false.
    virtual bool dump() const;

    virtual CHAR const* getPassName() const { return "Dead Store Elimination"; }
    virtual PASS_TYPE getPassType() const { return PASS_DSE; }
    ActMgr & getActMgr() { return m_am; }
    MDSSAMgr * getMDSSAMgr() const { return m_mdssamgr; }

    virtual bool perform(OptCtx & oc);
};

} //namespace xoc
#endif
