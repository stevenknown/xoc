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
#ifndef __LIVENESS_MGR_H__
#define __LIVENESS_MGR_H__

namespace xoc {

typedef DefSBitSetCore LiveSet;
typedef DefSEGIter LiveSetIter;
typedef Vector<LiveSet*> LivenessVec;

//Auxiliary livenessMgr that used to remove redundant liveness info.
class AuxLivenessMgr {
protected:
    Region const* m_rg;
    DefMiscBitSetMgr m_sbs_mgr;
    LivenessVec m_new_livein_vec;
    LivenessVec m_new_liveout_vec;
protected:
    //Free new liveness vector.
    void destroy();

    //Initialize new liveness vector.
    void init();
public:
    AuxLivenessMgr(Region const* rg) : m_rg(rg) { init(); }

    ~AuxLivenessMgr() { destroy(); }

    LiveSet * genNewLiveIn(UINT bbid);

    LiveSet * genNewLiveOut(UINT bbid);

    LiveSet * getNewLiveOut(UINT bbid) const
    { return m_new_liveout_vec.get(bbid); }

    LiveSet * getNewLiveIn(UINT bbid) const
    { return m_new_livein_vec.get(bbid); }

    DefMiscBitSetMgr & getSBSMgr() { return m_sbs_mgr; }
};


class LivenessMgr : public Pass {
    COPY_CONSTRUCTOR(LivenessMgr);
    friend class VFExp;
protected:
    BYTE m_keep_local:1; //true to retain local-set info when perform() ends.
    MDSystem * m_md_sys;
    DefMiscBitSetMgr m_sbs_mgr;
    Vector<LiveSet*> m_def;
    Vector<LiveSet*> m_use;
    Vector<LiveSet*> m_livein;
    Vector<LiveSet*> m_liveout;
protected:
    void cleanLocal();
    void cleanGlobal();
    virtual void computeExpImpl(
        IR const* exp, MOD LiveSet * use, MOD LiveSet * gen);
    virtual void computeStmtImpl(
        IR const* stmt, MOD LiveSet * use, MOD LiveSet * gen);
    void computeExp(IR const* stmt, MOD LiveSet * use, MOD LiveSet * gen);
    void computeStmt(IR const* stmt, MOD LiveSet * use, MOD LiveSet * gen);

    LiveSet * gen_liveout(UINT bbid);
    LiveSet * gen_livein(UINT bbid);
    LiveSet * gen_def(UINT bbid);
    LiveSet * gen_use(UINT bbid);
protected:
    void updateSetByStmt(BSIdx idx, MOD LiveSet * use, MOD LiveSet * gen);
    void updateSetByExp(BSIdx idx, MOD LiveSet * use);
public:
    LivenessMgr(Region * rg);
    ~LivenessMgr() { clean(); }

    void add_liveout(IRBB const* bb, PRNO prno);
    void add_livein(IRBB const* bb, PRNO prno);

    void computeLocal(IRBB const* bb);
    void computeLocal(BBList const& bblst);
    void computeGlobal(IRCFG const* cfg);
    size_t count_mem() const;
    void clean() { cleanGlobal(); cleanLocal(); }

    //The function dump pass relative information.
    //The dump information is always used to detect what the pass did.
    //Return true if dump successed, otherwise false.
    virtual bool dump() const;

    //There is problem that entry_bb will be attached redundant LiveIn and
    //LiveOut info after completed liveness computing via the function of
    //'computeGlobal'. The main reason for this phenomenon is that there is
    //loop edge in the CFG. LiveIn and LiveOut info will be flowed back into
    //entry_bb along with the loop edge. Thus it needs to remove redudant
    //LiveIn and LiveOut info from liveness info in whole CFG.
    void eliminateRedundantLiveness(IRCFG const* cfg);

    //The implement function of computed new livein and liveout. Return 'false'
    //if the livein and liveout are no more changed compared with last time.
    //'auxmgr': auxiliary livenessMgr for removed redundant liveness info.
    //'redundant_live': the full set of redundant liveness that come from
    //                  the livein of entry vertex.
    bool eliminateRedundantLivenessImpl(
        IRCFG const* cfg, AuxLivenessMgr & auxmgr,
        LiveSet const& redundant_live);

    //Get a union set of new liveout info from all predecessors of 'v'.
    //These new liveout info come from 'm_new_liveout_vec' in 'auxmgr'.
    void getPreVertexNewLiveOut(OUT LiveSet & new_liveout, Vertex const* v,
                                AuxLivenessMgr & auxmgr);

    virtual CHAR const* getPassName() const { return "LivenessMgr"; }
    PASS_TYPE getPassType() const { return PASS_LIVENESS_MGR; }
    DefMiscBitSetMgr * getSBSMgr() { return &m_sbs_mgr; }
    LiveSet * get_def(UINT bbid) const { return m_def.get(bbid); }
    LiveSet * get_use(UINT bbid) const { return m_use.get(bbid); }
    LiveSet * get_livein(UINT bbid) const { return m_livein.get(bbid); }
    LiveSet * get_liveout(UINT bbid) const { return m_liveout.get(bbid); }

    //By default, local set is initialized by computeLocal().
    void initSet(BBList const& bblst);
    void init_livein(UINT bbid);
    bool is_livein(UINT bbid, PRNO prno) const
    {
        ASSERTN(get_livein(bbid), ("miss liveness info"));
        return get_livein(bbid)->is_contain(prno);
    }
    bool is_liveout(UINT bbid, PRNO prno) const
    {
        ASSERTN(get_liveout(bbid), ("miss liveness info"));
        return get_liveout(bbid)->is_contain(prno);
    }

    //Reset 'm_livein' and 'm_liveout' with new livein and liveout that
    //come from 'm_new_livein_vec' and 'm_new_liveout_vec' in 'auxmgr'.
    //'auxmgr': auxiliary livenessMgr for removed redundant liveness info.
    void resetLivenessAfterRemoveRedundantLiveness(
        IRCFG const* cfg, AuxLivenessMgr & auxmgr);

    //Keep local-set information.
    void set_keep_local(bool keep) { m_keep_local = (BYTE)keep; }
    void set_livein(UINT bbid, LiveSet const* live_set);
    void set_liveout(UINT bbid, LiveSet const* live_set);

    //Set the liveness info for an empty BB.
    //empty_bb: the empty BB.
    //from: the predecessor BB of the empty_BB.
    void setLivenessForEmptyBB(IRBB const* empty_bb, IRBB const* from);

    bool perform(BBList const* bblst, IRCFG const* cfg, OptCtx & oc);
    virtual bool perform(OptCtx & oc);
};

} //namespace xoc
#endif
