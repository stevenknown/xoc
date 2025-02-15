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
#ifndef __VAR_LIVENESS_MGR_H__
#define __VAR_LIVENESS_MGR_H__

#define LT_ID_UNDEF 0

namespace xoc {

typedef DefSBitSetCore VarLiveSet;
typedef DefSEGIter VarLiveSetIter;

//
//START VarLivenessMgr
//
class VarLivenessMgr {
    COPY_CONSTRUCTOR(VarLivenessMgr);
protected:
    bool m_keep_local; //true to retain local-set info when perform() ends.
    Region * m_rg;
    Vector<VarLiveSet*> m_def;
    Vector<VarLiveSet*> m_use;
    Vector<VarLiveSet*> m_livein;
    Vector<VarLiveSet*> m_liveout;
    DefMiscBitSetMgr m_sbs_mgr;
protected:
    void cleanLocal();
    void cleanGlobal();
    void computeExp(IR const* stmt, MOD PRLiveSet * use, MOD PRLiveSet * gen);
    void computeStmt(IR const* stmt, MOD VarLiveSet * use,
                     MOD VarLiveSet * gen);

    VarLiveSet * gen_liveout(UINT bbid);
    VarLiveSet * gen_livein(UINT bbid);
    VarLiveSet * gen_def(UINT bbid);
    VarLiveSet * gen_use(UINT bbid);

    void processSTPR(IR const* x, ConstIRIter & it,
                     MOD VarLiveSet * use,
                     MOD VarLiveSet * gen);
public:
    VarLivenessMgr(Region * rg) : m_rg(rg) { m_keep_local = false; }
    ~VarLivenessMgr() { clean(); }

    void add_liveout(IRBB const* bb, PRNO prno)
    { gen_liveout(bb->id())->bunion((BSIdx)prno, m_sbs_mgr); }
    void add_livein(IRBB const* bb, PRNO prno)
    { gen_livein(bb->id())->bunion((BSIdx)prno, m_sbs_mgr); }

    void clean() { cleanGlobal(); cleanLocal(); }
    virtual bool canBeStmtCand(IR const* stmt)
    {
        ASSERT0(stmt && stmt->is_stmt() && stmt->hasIdinfo());
        return true;
    }
    virtual bool canBeExpCand(IR const* stmt, IR const* ir)
    {
        ASSERT0(ir && stmt && ir->is_exp() && stmt->is_stmt());
        ASSERT0(ir->getStmt() == stmt);
        ASSERT0(ir->hasIdinfo());
        return true;
    }

    void computeLocal(IRBB const* bb);
    void computeLocal(BBList const& bblst);
    void computeGlobal(IRCFG const* cfg);

    //The function dump pass relative information.
    //The dump information is always used to detect what the pass did.
    //Return true if dump successed, otherwise false.
    bool dump() const;

    //There is a problem that entry_bb will be attached redundant LiveIn and
    //LiveOut info after completed liveness computing via the function of
    //'computeGlobal'. The main reason for this phenomenon is that there is
    //loop edge in the CFG. LiveIn and LiveOut info will be flowed back into
    //entry_bb along with the loop edge. Thus it needs to remove redudant
    //LiveIn and LiveOut info from entry_bb and it's successor node until
    //reached a node with more than one degree.
    void eliminateRedundantLivenessInEntryBB(IRCFG const* cfg);

    PASS_TYPE getPassType() const { return PASS_LIVENESS_MGR; }
    VarLiveSet * get_def(UINT bbid) const { return m_def.get(bbid); }
    VarLiveSet * get_use(UINT bbid) const { return m_use.get(bbid); }
    VarLiveSet * get_livein(UINT bbid) const { return m_livein.get(bbid); }
    VarLiveSet * get_liveout(UINT bbid) const
    { return m_liveout.get(bbid); }
    DefMiscBitSetMgr & getSBSMgr() { return m_sbs_mgr; }
    Region * getRegion() const { return m_rg; }

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

    //Keep local-set information.
    void set_keep_local(bool keep) { m_keep_local = (BYTE)keep; }
    void set_livein(UINT bbid, PRLiveSet const* live_set)
    {
        ASSERT0(live_set);
        ASSERT0(bbid != BBID_UNDEF);
        PRLiveSet * livein = gen_livein(bbid);
        livein->copy(*live_set, m_sbs_mgr);
    }
    void set_liveout(UINT bbid, PRLiveSet const* live_set)
    {
        ASSERT0(live_set);
        ASSERT0(bbid != BBID_UNDEF);
        PRLiveSet * liveout = gen_liveout(bbid);
        liveout->copy(*live_set, m_sbs_mgr);
    }

    //Set the liveness info for an empty BB.
    //empty_bb: the empty BB.
    //from: the predecessor BB of the empty_BB.
    void setLivenessForEmptyBB(IRBB const* empty_bb, IRBB const* from);

    bool perform();
};
//END VarLivenessMgr

} //namespace xoc
#endif
