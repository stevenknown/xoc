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
#ifndef _LINEAR_SCAN_H_
#define _LINEAR_SCAN_H_

namespace xoc {

class LifeTime;
class LifeTimeMgr;
class TargInfoMgr;

//
//START RematCtx
//
class RematCtx {
public:
    //Record the expression that used in rematerialization.
    IR const* material_exp;
};
//END RematCtx


class ActMgr {
    Region * m_rg;
    xcom::List<xcom::StrBuf*> m_act_list;
    UINT m_cnt;
public:
    ActMgr(Region * rg) : m_rg(rg) { m_cnt = 1; }
    ~ActMgr();
    void dump(CHAR const* format, ...);
    void dumpAll() const;
};

typedef List<LifeTime*>::Iter LTSetIter;
typedef xcom::TMap<LifeTime*, xcom::C<LifeTime*>*> LT2Holder;
class LTSet : public xcom::EList<LifeTime*, LT2Holder> {
    COPY_CONSTRUCTOR(LTSet);
public:
    LTSet() {}
};

typedef xcom::TTabIter<LifeTime*> LTTabIter;
class LTTab : public xcom::TTab<LifeTime*> {
};

//The class represents the basic structure and interface of linear-scan register
//allocation.
class LinearScanRA : public Pass {
    typedef TMap<PRNO, Var*> PRNO2Var;
    COPY_CONSTRUCTOR(LinearScanRA);
    bool m_is_apply_to_region;
    LifeTimeMgr * m_lt_mgr;
    TargInfoMgr * m_ti_mgr;
    IRCFG * m_cfg;
    IRMgr * m_irmgr;
    BBList * m_bb_list;
    UINT m_func_level_var_count;
    LTSet m_unhandled;
    LTSet m_handled;
    LTSet m_active;
    LTSet m_inactive;
    Vector<Reg> m_prno2reg;
    IRTab m_spill_tab;
    IRTab m_reload_tab;
    IRTab m_remat_tab;
    IRTab m_move_tab;
    DedicatedMgr m_dedicated_mgr;
    PRNO2Var m_prno2var;
    ActMgr m_act_mgr;
public:
    explicit LinearScanRA(Region * rg);
    virtual ~LinearScanRA();

    void addUnhandled(LifeTime * lt);
    void addActive(LifeTime * lt);
    void addInActive(LifeTime * lt);
    void addHandled(LifeTime * lt);

    virtual IR * buildRemat(PRNO prno, RematCtx const& rematctx,
                            Type const* ty);
    virtual IR * buildMove(PRNO from, PRNO to, Type const* fromty,
                           Type const* toty);
    virtual IR * buildSpill(PRNO prno, Type const* ty);
    virtual IR * buildReload(PRNO prno, Var * spill_loc, Type const* ty);

    //The function check whether 'lt' value is simple enough to rematerialize.
    //And return the information through rematctx.
    virtual bool checkLTCanBeRematerialized(LifeTime const* lt,
                                            OUT RematCtx & rematctx);
    virtual void collectDedicatedPR(BBList const* bblst,
                                    OUT DedicatedMgr & mgr);

    void dumpPR2Reg(PRNO prno) const;
    void dumpPR2Reg() const;
    void dump4List() const;
    bool dump(bool dumpir = true) const;

    void freeReg(Reg reg);
    void freeReg(LifeTime const* lt);

    //Construct a name for Var that will lived in Region.
    CHAR const* genFuncLevelNewVarName(OUT xcom::StrBuf & name);
    Var * getSpillLoc(PRNO prno);
    Var * genSpillLoc(PRNO prno, Type const* ty);
    Var * genFuncLevelVar(Type const* type, UINT align);
    Reg getReg(PRNO prno) const;
    REGFILE getRegFile(Reg r) const;
    Reg getReg(LifeTime const* lt) const;
    LifeTime * getLT(PRNO prno) const;
    CHAR const* getRegName(Reg r) const;
    LTSet & getUnhandled() { return m_unhandled; }
    LTSet & getActive() { return m_active; }
    LTSet & getInActive() { return m_inactive; }
    LTSet & getHandled() { return m_handled; }
    IRTab & getSpillTab() { return m_spill_tab; }
    IRTab & getReloadTab() { return m_reload_tab; }
    IRTab & getRematTab() { return m_remat_tab; }
    IRTab & getMoveTab() { return m_move_tab; }
    BBList * getBBList() const { return m_bb_list; }
    IRCFG * getCFG() const { return m_cfg; }
    TargInfoMgr & getTIMgr() { return *m_ti_mgr; }
    LifeTimeMgr & getLTMgr() { return *m_lt_mgr; }
    Reg getDedicatedReg(LifeTime const* lt) const
    { return getDedicatedReg(lt->getPrno()); }
    Reg getDedicatedReg(PRNO prno) const { return m_dedicated_mgr.get(prno); }
    DedicatedMgr & getDedicatedMgr() { return m_dedicated_mgr; }
    ActMgr & getActMgr() { return m_act_mgr; }
    virtual CHAR const* getPassName() const
    { return "Linear Scan Register Allocation"; }
    PASS_TYPE getPassType() const { return PASS_LINEAR_SCAN_RA; }
 
    bool isInsertOp() const
    {
        LinearScanRA * pthis = const_cast<LinearScanRA*>(this);
        return pthis->getSpillTab().get_elem_count() != 0 ||
               pthis->getReloadTab().get_elem_count() != 0 ||
               pthis->getMoveTab().get_elem_count() != 0;
    }

    //Return true if ir is rematerializing operation.
    virtual bool isRematLikeOp(IR const* ir) const;
    bool isSpillOp(IR const* ir) const;
    bool isReloadOp(IR const* ir) const;
    bool isRematOp(IR const* ir) const;
    bool isMoveOp(IR const* ir) const;
    bool isDedicated(PRNO prno) const
    { return m_dedicated_mgr.is_dedicated(prno); }

    //Return true if register r1 alias to r2.
    virtual bool isAlias(Reg r1, Reg r2) const { return r1 == r2; }

    bool hasReg(PRNO prno) const;
    bool hasReg(LifeTime const* lt) const;

    virtual bool preferCallee(LifeTime const* lt) const;
    virtual bool perform(OptCtx & oc);

    //Reset all resource before allocation.
    void reset();

    void setDedicatedReg(PRNO prno, Reg r) { m_dedicated_mgr.add(prno, r); }
    void setReg(PRNO prno, Reg reg);
    void setSpill(IR * ir) { m_spill_tab.append(ir); }
    void setReload(IR * ir) { m_reload_tab.append(ir); }
    void setRemat(IR * ir) { m_remat_tab.append(ir); }
    void setMove(IR * ir) { m_move_tab.append(ir); }
    void setApplyToRegion(bool doit) { m_is_apply_to_region = doit; }

    void updateSSA(OptCtx & oc) const;

    bool verify4List() const;
    bool verifyAfterRA() const;
};

} //namespace xoc
#endif
