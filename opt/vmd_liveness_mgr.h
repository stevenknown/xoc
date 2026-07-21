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
#ifndef __VMD_LIVENESS_MGR_H__
#define __VMD_LIVENESS_MGR_H__

namespace xoc {

typedef LiveSetIter VMDLivenessSetIter;
class VMDLivenessSet : public LiveSet {
};

class VMDLivenessMgr : public LivenessMgr {
    COPY_CONSTRUCTOR(VMDLivenessMgr);
protected:
    bool m_need_reach_def;
    MDSSAMgr * m_mdssamgr;
    UseDefMgr * m_mdssaudmgr;
    Vector<LiveSet*> m_livein_reach;
    Vector<LiveSet*> m_liveout_reach;
    Vector<LiveSet*> m_kill;
protected:
    void cleanReachSet();
    virtual void computeExpImpl(
        IR const* exp, MOD LiveSet * use, MOD LiveSet * gen) override;
    virtual void computeStmtImpl(
        IR const* stmt, MOD LiveSet * use, MOD LiveSet * gen) override;
    void computeMDPhiOpndList(MDPhi const* mdphi, MOD LiveSet * use);
    void computeMDPhi(MDPhi const* mdphi, MOD LiveSet * use, MOD LiveSet * gen);
    void computeLocalIterMDPhiList(
        IRBB const* bb, LiveSet * use, LiveSet * gen);
    virtual void computeLocal(IRBB const* bb) override;
    virtual void computeGlobal(IRCFG const* cfg) override;
    void computeKillReach(IRCFG const* cfg);
    void computeGlobalLive(IRCFG const* cfg);
    void computeGlobalReach(IRCFG const* cfg);

    void dumpVMDLivenessSet(VMDLivenessSet const* set, bool detail) const;
    void dumpAllSet() const;

    LiveSet * get_kill(UINT bbid) const { return m_kill.get(bbid); }
    LiveSet * gen_livein_reach(UINT bbid);
    LiveSet * gen_liveout_reach(UINT bbid);
    LiveSet * gen_kill(UINT bbid);

    bool initDepPass();
    virtual void initSet(BBList const& bblst) override;

    void mergeMDPhiOpndToLiveIn();
    void mergeMDPhiOpndList(MDPhi const* mdphi, MOD LiveSet * livein);

    bool needReachDef() const { return m_need_reach_def; }

    bool useMDSSADU() const;
    virtual void updateSetByLHS(
        BSIdx idx, MOD LiveSet * use, MOD LiveSet * gen) override;
    virtual void updateSetByRHS(BSIdx idx, MOD LiveSet * use) override;
    void updateSetByLHSAndConsiderVersion(
        VMD const* lhsvmd, MOD LiveSet * use, MOD LiveSet * gen);
public:
    VMDLivenessMgr(Region * rg);
    virtual ~VMDLivenessMgr() { cleanReachSet(); }
    bool dump() const;

    LiveSet * get_livein_reach(UINT bbid) const
    { return m_livein_reach.get(bbid); }
    LiveSet * get_liveout_reach(UINT bbid) const
    { return m_liveout_reach.get(bbid); }
    virtual CHAR const* getPassName() const { return "VMDLivenessMgr"; }
    PASS_TYPE getPassType() const { return PASS_VMDLIVENESS_MGR; }
    MDSSAMgr * getMDSSAMgr() const { return m_mdssamgr; }

    //Ask the Mgr to compute Reach-Def-VMD while computing liveness info.
    void setCompReachDef(bool need_reach_def)
    { m_need_reach_def = need_reach_def; }

    virtual bool perform(OptCtx & oc) override;
};

} //namespace xoc
#endif
