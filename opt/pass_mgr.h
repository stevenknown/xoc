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
#ifndef __PASS_MGR_H__
#define __PASS_MGR_H__

namespace xoc {

typedef TMap<PASS_TYPE, Pass*> PassTab;
typedef TMapIter<PASS_TYPE, Pass*> PassTabIter;
typedef TMap<PASS_TYPE, xcom::Graph*> GraphPassTab;
typedef TMapIter<PASS_TYPE, xcom::Graph*> GraphPassTabIter;
typedef List<PASS_TYPE> PassTypeList;

class PassMgr {
    COPY_CONSTRUCTOR(PassMgr);
protected:
    Region * m_rg;
    RegionMgr * m_rumgr;
    TypeMgr * m_tm;
    PassTab m_registered_pass;
    GraphPassTab m_registered_graph_based_pass;

protected:
    xcom::Graph * registerGraphBasedPass(PASS_TYPE opty);

public:
    PassMgr(Region * rg);
    virtual ~PassMgr() { destroyAllPass(); }

    virtual Pass * allocCDG();
    virtual Pass * allocCFG();
    virtual Pass * allocAA();
    virtual Pass * allocDUMgr();
    virtual Pass * allocCopyProp();
    virtual Pass * allocGCSE();
    virtual Pass * allocLCSE();
    virtual Pass * allocRP();
    virtual Pass * allocPRE();
    virtual Pass * allocIVR();
    virtual Pass * allocLICM();
    virtual Pass * allocDCE();
    virtual Pass * allocLFTR();
    virtual Pass * allocInferType();
    virtual Pass * allocInvertBrTgt();
    virtual Pass * allocDSE();
    virtual Pass * allocRCE();
    virtual Pass * allocGVN();
    virtual Pass * allocLoopCvt();
    virtual Pass * allocPRSSAMgr();
    virtual Pass * allocMDSSAMgr();
    virtual Pass * allocCCP();
    virtual Pass * allocExprTab();
    virtual Pass * allocCfsMgr();
    virtual Pass * allocIPA();
    virtual Pass * allocInliner();
    virtual Pass * allocRefineDUChain();
    virtual Pass * allocScalarOpt();
    virtual Pass * allocMDLivenessMgr();
    virtual Pass * allocRefine();
    virtual Pass * allocGSCC();

    //This function check validation of options in oc, perform
    //recomputation if it is invalid.
    //...: the options/passes that anticipated to recompute.
    void checkValidAndRecompute(OptCtx * oc, ...);
    void checkValidAndRecompute(OptCtx * oc, PassTypeList & optlist);

    void destroyAllPass();
    void destroyPass(Pass * pass);
    void destroyPass(PASS_TYPE passtype);

    Pass * registerPass(PASS_TYPE opty);

    Pass * queryPass(PASS_TYPE opty) { return m_registered_pass.get(opty); }
};

} //namespace xoc
#endif
