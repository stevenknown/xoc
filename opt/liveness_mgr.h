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

//Map between Var and PRNO.
class Var2PR : public TMap<Var const*, PRNO, CompareConstVar> {
};

typedef DefSBitSetCore PRLiveSet;

class LivenessMgr : public Pass {
    COPY_CONSTRUCTOR(LivenessMgr);
    BYTE m_handle_may:1; //true if consider maydef/mayuse info.
    BYTE m_keep_local:1; //true to retain local-set info when perform() ends.
    MDSystem * m_md_sys;
    Var2PR * m_var2pr;
    DefMiscBitSetMgr m_sbs_mgr;
    Vector<PRLiveSet*> m_def;
    Vector<PRLiveSet*> m_use;
    Vector<PRLiveSet*> m_livein;
    Vector<PRLiveSet*> m_liveout;
protected:
    void cleanLocal();
    void cleanGlobal();
    void computeExp(IR const* stmt, ConstIRIter & it,
                    MOD PRLiveSet * use,
                    MOD PRLiveSet * gen);
    void computeStmt(IR const* stmt, ConstIRIter & it,
                     MOD PRLiveSet * use,
                     MOD PRLiveSet * gen);

    PRLiveSet * gen_liveout(UINT bbid);
    PRLiveSet * gen_livein(UINT bbid);
    PRLiveSet * gen_def(UINT bbid);
    PRLiveSet * gen_use(UINT bbid);

    void processPHI(IR const* x, ConstIRIter & it,
                    MOD PRLiveSet * use,
                    MOD PRLiveSet * gen);
    void processSTPR(IR const* x, ConstIRIter & it,
                     MOD PRLiveSet * use,
                     MOD PRLiveSet * gen);
    void processSETELEM(IR const* x, ConstIRIter & it,
                        MOD PRLiveSet * use,
                        MOD PRLiveSet * gen);
    void processGETELEM(IR const* x, ConstIRIter & it,
                        MOD PRLiveSet * use,
                        MOD PRLiveSet * gen);
    void processCallStmt(IR const* x, ConstIRIter & it,
                         MOD PRLiveSet * use,
                         MOD PRLiveSet * gen);
    inline void processOpnd(IR const* exp, ConstIRIter & it,
                            MOD PRLiveSet * use, MOD PRLiveSet * gen);
    inline void processMayDef(PRNO prno, MOD PRLiveSet * gen,
                              MOD PRLiveSet * use);
    inline void processMay(IR const* pr, MOD PRLiveSet * gen,
                           MOD PRLiveSet * use, bool is_lhs);
    inline void processMayUse(PRNO prno, MOD PRLiveSet * use);
public:
    LivenessMgr(Region * rg) : Pass(rg)
    {
        m_md_sys = rg->getMDSystem();
        m_var2pr = nullptr;
        m_handle_may = false;
        m_keep_local = false;
    }
    ~LivenessMgr() { clean(); }

    void add_liveout(IRBB const* bb, PRNO prno)
    { gen_liveout(bb->id())->bunion((BSIdx)prno, m_sbs_mgr); }
    void add_livein(IRBB const* bb, PRNO prno)
    { gen_livein(bb->id())->bunion((BSIdx)prno, m_sbs_mgr); }

    void computeLocal(IRBB const* bb, MOD ConstIRIter & it);
    void computeLocal(BBList const& bblst);
    void computeGlobal(IRCFG const* cfg);
    size_t count_mem() const;
    void clean() { cleanGlobal(); cleanLocal(); }

    //The function dump pass relative information.
    //The dump information is always used to detect what the pass did.
    //Return true if dump successed, otherwise false.
    virtual bool dump() const;

    virtual CHAR const* getPassName() const { return "LivenessMgr"; }
    PASS_TYPE getPassType() const { return PASS_LIVENESS_MGR; }
    DefMiscBitSetMgr * getSBSMgr() { return &m_sbs_mgr; }
    PRLiveSet * get_def(UINT bbid) const { return m_def.get(bbid); }
    PRLiveSet * get_use(UINT bbid) const { return m_use.get(bbid); }
    PRLiveSet * get_livein(UINT bbid) const { return m_livein.get(bbid); }
    PRLiveSet * get_liveout(UINT bbid) const
    { return m_liveout.get(bbid); }

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

    //Set true to handle maydef and mayuse MDSet.
    void set_handle_may(bool handle) { m_handle_may = (BYTE)handle; }

    //Keep local-set information.
    void set_keep_local(bool keep) { m_keep_local = (BYTE)keep; }

    //Set map structure which used during the processing of maydef and mayuse.
    void setVar2PR(Var2PR * v2p) { m_var2pr = v2p; }

    bool perform(BBList const* bblst, IRCFG const* cfg, OptCtx & oc);
    virtual bool perform(OptCtx & oc);
};

} //namespace xoc
#endif
