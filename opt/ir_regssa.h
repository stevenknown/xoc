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
#ifndef _IR_REGSSA_H_
#define _IR_REGSSA_H_

namespace xoc {

class ActMgr;
class RegSSAUpdateCtx;
class LinearScanRA;
class TargInfoMgr;

typedef xcom::TMap<Reg, VReg*> Reg2VReg;
typedef xcom::TMap<UINT, RegPhiList*> BB2RegPhiList;
typedef xcom::TMapIter<UINT, RegPhiList*> BB2RegPhiListIter;
typedef xcom::List<RegDef*> RegDefIter;
typedef xcom::TTab<Reg> LiveInRegTab;
typedef xcom::TTabIter<Reg> LiveInRegTabIter;
typedef xcom::DefSBitSet DefRegSet;
typedef xcom::DefSBitSetIter DefRegSetIter;
typedef xcom::Stack<VReg*> VRegStack;

typedef enum tagREGSSA_STATUS {
    //Describe miscellaneous information for IR.
    REGSSA_STATUS_DOM_IS_INVALID_POS = 0,
    REGSSA_STATUS_DOM_IS_INVALID = 1ULL<<REGSSA_STATUS_DOM_IS_INVALID_POS,
} REGSSA_STATUS;

class RegSSAStatus : public Status {
public:
    //Return true if current status indicates complete success.
    bool is_succ() const { return get_status_num() == 0; }

    //Return the name of status.
    virtual CHAR const* getStatusName(FlagSetIdx s) const override;
};

enum VR_COLLECT_FLAG {
    VR_COLLECT_UNDEF = 0x0,

    //Collect immediate USEs of the DEF.
    //The collection does not cross PHI.
    VR_COLLECT_IMM_USE = 0x1,

    //Do collection cross PHI operand.
    VR_COLLECT_CROSS_PHI = 0x2,

    //Do collection inside the given loop region.
    VR_COLLECT_INSIDE_LOOP = 0x4,

    //Do collection to the outside-immediate USE to the given loop region.
    //e.g:given loop which has an outside-immeidate USE.
    //  t3=1
    //  LOOP_START:
    //  t1=RegPhi(t2,t3)
    //  truebr ... LOOP_END
    //  t2=t1
    //  goto LOOP_START
    //
    //  LOOP_END:
    //  t5=RegPhi(t1,t6) #S1
    //  t7=t5 #S2
    //In this example, #S1's t1 is the outside-immediate USE.
    //Note #S2 refers t5, but the t5 is not outside-immediate USE.
    VR_COLLECT_OUTSIDE_LOOP_IMM_USE = 0x8,
};

class VRCollectFlag : public UFlag {
public:
    VRCollectFlag(UINT v) : UFlag(v) {}
};

//The class represents the context information to collection.
class VRCollectCtx {
public:
    LI<IRBB> const* m_li;
    VRCollectFlag flag;
public:
    VRCollectCtx(VRCollectFlag f) : m_li(nullptr), flag(f) {}
    LI<IRBB> const* getLI() const { return m_li; }
    void setLI(LI<IRBB> const* li) { m_li = li; }
    bool verify() const;
};

//Collect all DEFs that overlapped with 'ref'.
//The collection will conform rules that declared in 'ctx'.
//Note the function will not clear 'set' because caller may perform unify
//operation.
class VRCollectDef {
    COPY_CONSTRUCTOR(VRCollectDef);
    RegSSAMgr const* m_mgr;
    RegSSAInfo const* m_info;
    VRCollectCtx const& m_ctx;
    Reg m_ref;
    RegDUMgr const* m_dumgr;
protected:
    void collect(OUT IRSet * set) const;
    void collectDefThroughDefChain(RegDef const* def, OUT IRSet * set) const;
public:
    //ref: given Reg, if it is NULL, the function will collect all DEFs.
    //     Otherwise, the collection continues until encounter the
    //     killing-def of 'ref'.
    //ctx: if the collection will keep iterating DEF according to rules
    //     declared in ctx. e.g: do collection by cross PHI operand.
    //set: record the return result.
    VRCollectDef(RegSSAMgr const* mgr, RegSSAInfo const* info,
                 VRCollectCtx const& ctx, Reg ref, OUT IRSet * set);
};

//Collect all USE, where USE is IR expression.
//Note the function will not clear 'set' because caller may perform unify
//operation.
class VRCollectUse {
    COPY_CONSTRUCTOR(VRCollectUse);
    class RegDefVisitor : public xcom::TTab<UINT>  {
    public:
        bool is_visited(UINT id) const { return find(id); }
        void set_visited(UINT id) { append(id); }
    };
    RegSSAMgr const* m_mgr;
    VRCollectCtx const& m_ctx;
    Reg m_ref;
    RegDUMgr const* m_dumgr;
protected:
    void collectForVROpnd(VROpnd const* vropnd, OUT IRSet * set) const;
    void collectForRegSSAInfo(RegSSAInfo const* info, OUT IRSet * set) const;
    void collectUseCrossPhi(RegPhi const* phi, MOD RegDefVisitor & vis,
                            OUT IRSet * set) const;
    void collectOutsideLoopImmUseForVROpnd(
        VROpnd const* vropnd, MOD RegDefVisitor & vis, OUT IRSet * set) const;
    void collectUseForVROpnd(VROpnd const* vropnd, MOD RegDefVisitor & vis,
                             OUT IRSet * set) const;
public:
    //ctx: indicates the terminating condition that the function should
    //     stop and behaviors what the collector should take when encountering
    //     specific IR operator. e.g: do collection by cross PHI operand.
    //set: record the return result.
    //info: collect USE for each VROpnd of 'info'.
    VRCollectUse(RegSSAMgr const* mgr, RegSSAInfo const* info,
                 VRCollectCtx const& ctx, OUT IRSet * set);

    //vropnd: collect USE for 'vropnd'.
    VRCollectUse(RegSSAMgr const* mgr, VReg const* vreg,
                 VRCollectCtx const& ctx, OUT IRSet * set);
};


class BB2DefRegSet : public xcom::Vector<DefRegSet*> {
    COPY_CONSTRUCTOR(BB2DefRegSet);
public:
    BB2DefRegSet() {}
    void dump(Region const* rg) const;
};


class BB2VRegMap : public xcom::Vector<Reg2VReg*> {
    COPY_CONSTRUCTOR(BB2VRegMap);
    bool checkClean()
    {
        //Verify if vpmap of each BB has been deleted.
        for (VecIdx i = 0; i <= get_last_idx(); i++) {
            ASSERT0(get(i) == nullptr);
        }
        return true;
    }
public:
    BB2VRegMap() {}
    BB2VRegMap(UINT n) : Vector<Reg2VReg*>(n) {}
    ~BB2VRegMap() { ASSERT0(checkClean()); }

    Reg2VReg * gen(UINT bbid)
    {
        Reg2VReg * reg2vreg = get(bbid);
        if (reg2vreg == nullptr) {
            reg2vreg = new Reg2VReg();
            set(bbid, reg2vreg);
        }
        return reg2vreg;
    }

    void erase(UINT bbid)
    {
        Reg2VReg * reg2vreg = get(bbid);
        if (reg2vreg != nullptr) {
            delete reg2vreg;
            set(bbid, nullptr);
        }
    }

    void setElemNum(UINT n) { grow(n); }
};


//Mapping from Reg id to Stack of VReg.
class Reg2VRegStack : public xcom::Vector<VRegStack*> {
    COPY_CONSTRUCTOR(Reg2VRegStack);
public:
    Reg2VRegStack() {}
    ~Reg2VRegStack() { destroy(); }

    void clean() { destroy(); Vector<VRegStack*>::init(); }

    void dump(Region const* rg) const;
    void destroy();

    VRegStack * gen(Reg reg);
    VReg * get_top(VReg const* vreg) const { return get_top(vreg->reg()); }
    VReg * get_top(Reg reg) const;

    void push(VReg * vreg) { push(vreg->reg(), vreg); }
    void push(Reg reg, VReg * vreg);
};


//The class define the iterator that used to iterate IR occurrence for
//each VROpnd in RegSSA mode.
class ConstRegSSAUSEIRIter {
    COPY_CONSTRUCTOR(ConstRegSSAUSEIRIter);
public:
    VROpndSet * vropndset;
    VROpndSetIter vropndset_iter;
    BSIdx current_pos_in_vropndset;
    VReg::UseSetIter useset_iter;
    BSIdx current_pos_in_useset;
    VReg::UseSet const* current_useset;
    RegSSAMgr const* m_regssamgr;
    RegDUMgr const* m_dumgr;
    Region const* m_rg;
public:
    ConstRegSSAUSEIRIter(RegSSAMgr const* regssamgr);
    void clean()
    {
        vropndset_iter = nullptr;
        current_pos_in_vropndset = BS_UNDEF;
        useset_iter.clean();
        current_pos_in_useset = BS_UNDEF;
        current_useset = nullptr;
    }

    //Iterative access USE in RegSSAInfo. The USE always an IR occurrence that
    //describes a memory expression.
    //The funtion initialize the iterator.
    //def: the RegDef of the chain.
    //it: iterator. It should be clean already.
    //Readonly function.
    //Note the function may iterate same IR multiple times because it may
    //belong different VROpnd.
    //e.g: global int g; local int b;
    //     g = b;
    //The RegSSA info of ST is:
    // st:i32 'g'
    //  --DEFREF:(Reg2V2, PrevDEF:Reg2V1, NextDEF : Reg2V3)|UsedBy:ld b(id:15)
    //  --DEFREF : (Reg5V2, PrevDEF:Reg5V1) | UsedBy : ld b(id:15), id(id:23)
    //  ld b is both USE of VROpnd(Reg2V2) and VROpnd(Reg5V2).
    IR const* get_first(IR const* def);

    //Iterative access USE in RegSSAInfo. The USE always an IR occurrence that
    //describes a memory expression.
    //The function return the next USE according to 'it'.
    //it: iterator.
    //Readonly function.
    //Note the function may iterate same IR multiple times because it may
    //belong different VROpnd.
    //e.g: global int g; local int b;
    //     g = b;
    //The RegSSA info of ST is:
    // st:i32 'g'
    //  --DEFREF:(Reg2V2, PrevDEF:Reg2V1, NextDEF : Reg2V3)|UsedBy:ld b(id:15)
    //  --DEFREF : (Reg5V2, PrevDEF:Reg5V1) | UsedBy : ld b(id:15), id(id:23)
    //  ld b is both USE of VROpnd(Reg2V2) and VROpnd(Reg5V2).
    IR const* get_next();
};


class ConstRegDefIter : public xcom::List<RegDef const*> {
    COPY_CONSTRUCTOR(ConstRegDefIter);
protected:
    RegSSAMgr const* m_regssamgr;
    xcom::TTab<UINT> m_is_visited;
public:
    ConstRegDefIter(RegSSAMgr const* regssamgr) : m_regssamgr(regssamgr) {}

    bool is_visited(RegDef const* def) const
    { return m_is_visited.find(def->id()); }

    //Iterative access RegDef chain.
    //The funtion initialize the iterator.
    //When the iterator meets RegPhi, it will keep iterating the DEF of each
    //operand of RegPhi.
    //def: the beginning RegDef of the chain.
    //it: iterator. It should be clean already.
    RegDef const* get_first(RegDef const* def);

    //Iterative access RegDef chain.
    //The function return the next RegDef node according to 'it'.
    //When the iterator meets RegPhi, it will keep iterating the DEF of each
    //operand of RegPhi.
    //it: iterator.
    RegDef const* get_next();

    //Iterative access RegDef chain.
    //The funtion initialize the iterator.
    //When the iterator meets RegPhi, it will keep iterating the DEF of each
    //operand of RegPhi.
    //def: the beginning RegDef of the chain.
    //use: indicate the USE expression of the 'def'.
    //it: iterator. It should be clean already.
    //Readonly function.
    RegDef const* get_first_untill_killing_def(
        RegDef const* def, IR const* use);

    //Iterative access RegDef chain.
    //The function return the next RegDef node according to 'it'.
    //When the iterator meets RegPhi, it will keep iterating the DEF of each
    //operand of RegPhi.
    //it: iterator.
    //use: indicate the USE expression of the 'def'.
    //Readonly function.
    RegDef const* get_next_untill_killing_def(IR const* use);

    void set_visited(RegDef const* def) { m_is_visited.append(def->id()); }
};


typedef VROpndSetIter VRegLiveSetIter;
class VRegLiveSet : public VROpndSet {
    COPY_CONSTRUCTOR(VRegLiveSet);
    xcom::DefMiscBitSetMgr * m_sbsmgr;
public:
    VRegLiveSet(VReg const* vreg, xcom::DefMiscBitSetMgr * sbsmgr)
    {
        bunion(vreg->id(), *sbsmgr);
        m_sbsmgr = sbsmgr;
    }
    VRegLiveSet(VROpndSet const& set, xcom::DefMiscBitSetMgr * sbsmgr)
    {
        VROpndSet::copy(set, *sbsmgr);
        m_sbsmgr = sbsmgr;
    }
    ~VRegLiveSet()
    {
        //Should call clean() before destruction,
        //otherwise it will incur SegMgr assertion.
        clean();
    }

    bool all_killed() const { return is_empty(); }

    void copy(VRegLiveSet const& src)
    { VROpndSet::copy((VROpndSet const&)src, *m_sbsmgr); }
    void copy(VROpndSet const& vropndset)
    { VROpndSet::copy(vropndset, *m_sbsmgr); }
    void clean() { VROpndSet::clean(*m_sbsmgr); }

    void dump(RegSSAMgr const* mgr) const;

    bool is_live(UINT id) const { return is_contain(id); }

    void set_killed(UINT id) { diff(id, *m_sbsmgr); }
};


typedef xcom::TMapIter<UINT, VRegLiveSet*> BBID2VRegLiveSetIter;
class BBID2VRegLiveSet : public xcom::TMap<UINT, VRegLiveSet*> {
    COPY_CONSTRUCTOR(BBID2VRegLiveSet);
    xcom::DefMiscBitSetMgr m_sbsmgr;
public:
    BBID2VRegLiveSet() {}
    ~BBID2VRegLiveSet()
    {
        BBID2VRegLiveSetIter it;
        VRegLiveSet * set;
        for (get_first(it, &set); set != nullptr; get_next(it, &set)) {
            delete set;
        }
    }
    void dump(RegSSAMgr const* mgr) const;

    void free(UINT bbid)
    {
        VRegLiveSet * set = remove(bbid);
        if (set != nullptr) { delete set; }
    }

    VRegLiveSet * genAndCopy(UINT bbid, VRegLiveSet const& src)
    { return genAndCopy(bbid, (VROpndSet const&)src); }
    VRegLiveSet * genAndCopy(UINT bbid, VROpndSet const& src)
    {
        VRegLiveSet * liveset = get(bbid);
        if (liveset == nullptr) {
            liveset = new VRegLiveSet(src, &m_sbsmgr);
            set(bbid, liveset);
        } else {
            liveset->copy(src);
        }
        return liveset;
    }
    VRegLiveSet * genAndCopy(UINT bbid, VReg const* vreg)
    {
        VRegLiveSet * liveset = new VRegLiveSet(vreg, &m_sbsmgr);
        ASSERT0(get(bbid) == nullptr);
        set(bbid, liveset);
        return liveset;
    }
};


//
//SATRT VRRenameExp
//
//The class performs renaming for each NonPR Memory expression.
class VRRenameExp {
    COPY_CONSTRUCTOR(VRRenameExp);
protected:
    RegSSAMgr * m_mgr;
    ActMgr * m_am;
    Region * m_rg;
    OptCtx * m_oc;
public:
    VRRenameExp(RegSSAMgr * mgr, OptCtx * oc, ActMgr * am);

    RegSSAMgr * getMgr() const { return m_mgr; }
    ActMgr * getActMgr() const { return m_am; }

    //root: the root IR that expected to start to set live-in.
    //      Note root can be stmt or expression.
    //startir: the start position in 'startbb', it can be NULL.
    //         If it is NULL, the function first finding the Phi list of
    //         'startbb', then keep finding its predecessors until meet the
    //         CFG entry.
    //startbb: the BB that begin to do searching. It can NOT be NULL.
    void rename(MOD IR * root, IR const* startir, IRBB const* startbb);
};
//END VRRenameExp


//
//START VRRenameDef
//
//The class generates VReg for new Stmt or new RegPhi, then inserts VReg into
//DefDef chain and renames all immediate USEs of the original DEF meanwhile.
//Note after renaming, if the class find a memory reference X that references
//a version Reg that is same to new Stmt or new RegPhi, the class will build
//a DefUse chain between the memory reference X and the new Stmt or new RegPhi.
class VRRenameDef {
    friend class VRRenameDefVisitFunc;
    COPY_CONSTRUCTOR(VRRenameDef);
    bool m_is_build_ddchain; //Set to true to build DefDef chain by DomTree.
    DomTree const& m_domtree;
    VRegLiveSet * m_liveset;
    RegSSAMgr * m_mgr;
    RegDUMgr * m_dumgr;
    IRCFG * m_cfg;
    Region * m_rg;
    ActMgr * m_am;
    BBID2VRegLiveSet m_bbid2liveset;
private:
    void connect(Vertex const* defvex, VRegLiveSet * defliveset,
                 IRBB const* start_bb, IR const* start_ir);

    //stmtbbid: indicates the BB of inserted stmt
    //v: the vertex on DomTree.
    //bb: the BB that to be renamed
    //dompred: indicates the predecessor of 'bb' in DomTree
    //Note stmtbbid have to dominate 'bb'.
    void connectDefInBBTillPrevDef(
        IRBB const* bb, BBIRListIter & irlistit, MOD VRegLiveSet & liveset);
    void connectIRTillPrevDef(
        IRBB const* bb, BBIRListIter & irlistit, MOD VRegLiveSet & liveset);
    void connectPhiTillPrevDef(
        IRBB const* bb, BBIRListIter & irlistit, MOD VRegLiveSet & liveset);
    void connectDefInterBBTillPrevDef(
        Vertex const* defvex, MOD VRegLiveSet & stmtliveset,
        IRBB const* start_bb);

    void dumpRenameBB(IRBB const* bb);
    void dumpRenameVReg(IR const* ir, VReg const* vreg);
    void dumpInsertDDChain(IR const* ir, VReg const* vreg);
    void dumpInsertDDChain(RegPhi const* phi, VReg const* vreg);
    void dumpRenamePhi(RegPhi const* phi, UINT opnd_pos);

    BBID2VRegLiveSet & getBBID2VRegLiveSet() { return m_bbid2liveset; }

    void iterBBPhiListToKillLivedVReg(IRBB const* bb, VRegLiveSet & liveset);
    void iterSuccBBPhiListToRename(
        Vertex const* defvex, IRBB const* succ,
        UINT opnd_idx, MOD VRegLiveSet & liveset);

    //defvex: domtree vertex.
    void iterSuccBB(Vertex const* defvev, MOD VRegLiveSet & liveset);

    void killLivedVReg(RegPhi const* phi, MOD VRegLiveSet & liveset);

    void processStmt(MOD IR * newstmt);
    void processPhi(RegPhi const* newphi);

    void renamePhiOpnd(RegPhi const* phi, UINT opnd_idx, MOD VReg * vreg);

    //stmtbb: the BB of inserted stmt
    //newinfo: RegSSAInfo that intent to be swap-in.
    bool renameVRegForDesignatedPhiOpnd(
        RegPhi * phi, UINT opnd_pos, VRegLiveSet & liveset);

    //vreg: intent to be swap-in.
    //irtree: may be stmt or exp.
    //irit: for local used.
    void renameVRegForIRTree(
        IR * irtree, VReg * vreg, MOD IRIter & irit, bool & no_exp_has_ssainfo);

    //ir: may be stmt or exp
    //irit: for local used.
    void renameLivedVRegForIRTree(
        IR * ir, MOD IRIter & irit, VRegLiveSet const& liveset);
    void renameIRTillNextDef(
        IRBB const* bb, BBIRListIter & irlistit, MOD VRegLiveSet & liveset);

    //stmtbbid: indicates the BB of inserted stmt
    //bb: the BB that to be renamed
    //dompred: indicates the predecessor of 'bb' in DomTree
    //Note stmtbbid have to dominate 'bb'.
    void renameUseInBBTillNextDef(
        Vertex const* defvex, IRBB const* bb, bool include_philist,
        BBIRListIter & irlistit, OUT VRegLiveSet & liveset);

    //start_ir: if it is nullptr, the renaming will start at the first IR in bb.
    //          otherwise the renaming will start at the NEXT IR of start_ir.
    void renameFollowUseIntraBBTillNextDef(
        Vertex const* defvex, MOD VRegLiveSet & stmtliveset,
        IRBB const* start_bb, IR const* start_ir);

    //defvex: root vertex of domtree region that is being renamed.
    void renameFollowUseInterBBTillNextDef(
        Vertex const* defvex, MOD VRegLiveSet & stmtliveset,
        IRBB const* start_bb);
    void rename(Vertex const* defvex, VRegLiveSet * defliveset,
                IRBB const* start_bb, IR const* start_ir);

    //Insert vreg after phi.
    //Return true if inserted PHI into DD chain.
    bool tryInsertDDChainForDesigatedVReg(
        RegPhi * phi, VReg * vreg, MOD VRegLiveSet & liveset);

    //Insert vreg after 'ir'.
    //vreg: new generated VReg that to be inserted.
    //before: true to insert 'vreg' in front of 'ir'.
    //Return true if inserted PHI into DD chain.
    bool tryInsertDDChainForDesigatedVReg(
        IR * ir, VReg * vreg, bool before, MOD VRegLiveSet & liveset);

    //Return true if inserted 'ir' into DD chain.
    bool tryInsertDDChainForStmt(
        IR * ir, bool before, MOD VRegLiveSet & liveset);

    //Return true if inserted PHI into DD chain.
    bool tryInsertDDChainForPhi(RegPhi * phi, MOD VRegLiveSet & liveset);
public:
    VRRenameDef(DomTree const& dt, bool build_ddchain,
                RegSSAMgr * mgr, ActMgr * am);

    void clean();

    RegSSAMgr * getMgr() const { return m_mgr; }
    ActMgr * getActMgr() const { return m_am; }

    //The function will generate RegSSAInfo if it does not have one.
    //newstmt: record the stmt that inserted.
    void rename(MOD IR * newstmt);

    //The function will generate RegSSAInfo if it does not have one.
    //newphi: record the phi that inserted.
    void rename(RegPhi const* newphi);
};
//END VRRenameDef


//
//START RecomputeDefDefAndDefUseChain
//
class VRRecomputeDefDefAndDefUseChain {
    COPY_CONSTRUCTOR(VRRecomputeDefDefAndDefUseChain);
    xcom::DomTree const& m_domtree;
    RegSSAMgr * m_mgr;
    OptCtx const& m_oc;
    ActMgr * m_am;
    Region * m_rg;
    IRCFG * m_cfg;
public:
    VRRecomputeDefDefAndDefUseChain(
        xcom::DomTree const& domtree, RegSSAMgr * mgr,
        OptCtx const& oc, ActMgr * am);

    ActMgr * getActMgr() const { return m_am; }
    DomTree const& getDomTree() const { return m_domtree; }

    //These functions compute the DefDef chain and the DefUse chain for
    //given Stmt|RegPhi.
    //Note these functions are often invoked when new Stmt or new RegPhi
    //generated.
    //These functions will insert given Stmt|RegPhi into DefDef chain of each
    //VReg and iterate all memory references which are related to the VReg that
    //given Stmt|RegPhi carried from the start BB (Stmt|RegPhi's BB) to the next
    //versioned VReg DEF. During the iteration of VReg, these functions also
    //build DefUse chain to ensure the correctness of dependence of the new
    //Stmt|RegPhi.
    void recompute(MOD IR * ir);
    void recompute(xcom::List<IR*> const& irlist);
    void recompute(RegPhiList const* philist);
    void recompute(RegPhi const* phi);
    void recomputeDefForPhiOpnd(RegPhi const* phi);
    void recomputeDefForPhiOpnd(RegPhiList const* philist);
    void recomputeDefForRHS(MOD IR * stmt);
};
//END VRRecomputeDefDefAndDefUseChain


//
//START VRFindAndSetLiveInDef
//
class VRFindAndSetLiveInDef {
    COPY_CONSTRUCTOR(VRFindAndSetLiveInDef);
protected:
    RegSSAMgr * m_mgr;
    OptCtx const& m_oc;
public:
    VRFindAndSetLiveInDef(RegSSAMgr * mgr, OptCtx const& oc)
        : m_mgr(mgr), m_oc(oc) {}

    void findAndSet(
        MOD IR * exp, IRBB const* startbb, OUT RegSSAStatus & st,
        RegSSAUpdateCtx const* ctx = nullptr)
    {
        findAndSet(exp, const_cast<IRBB*>(startbb)->getLastIR(),
                   startbb, st, ctx);
    }

    //Note DOM info must be available.
    //exp: the expression that expected to set live-in.
    //startir: the start position in 'startbb', it can be NULL.
    //         If it is NULL, the function first finding the Phi list of
    //         'startbb', then keep finding its predecessors until meet the
    //         CFG entry.
    //startbb: the BB that begin to do searching. It can NOT be NULL.
    void findAndSet(
        MOD IR * exp, IR const* startir, IRBB const* startbb,
        OUT RegSSAStatus & st, RegSSAUpdateCtx const* ctx = nullptr);

    void findAndSetForTree(
        IR * exp, IRBB const* startbb, OUT RegSSAStatus & st,
        RegSSAUpdateCtx const* ctx = nullptr)
    {
        findAndSetForTree(exp, const_cast<IRBB*>(startbb)->getLastIR(),
                          startbb, st, ctx);
    }

    //Note DOM info must be available.
    //exp: the expression that expected to set live-in.
    //startir: the start position in 'startbb', it can be NULL.
    //         If it is NULL, the function first finding the Phi list of
    //         'startbb', then keep finding its predecessors until meet the
    //         CFG entry.
    //startbb: the BB that begin to do searching. It can NOT be NULL.
    void findAndSetForTree(
        IR * exp, IR const* startir, IRBB const* startbb, OUT RegSSAStatus & st,
        RegSSAUpdateCtx const* ctx = nullptr);

    void findAndSet(
        IR * exp, VReg const* prevdef_res, bool prevdef_is_phi,
        IRBB const* prevdef_bb, IR const* prevdef_occ,
        OUT RegSSAStatus & st, RegSSAUpdateCtx const* ctx);

    void findAndSetForLst(
        IRList const& lst, VReg const* prevdef_res, bool prevdef_is_phi,
        IRBB const* prevdef_bb, IR const* prevdef_occ,
        OUT RegSSAStatus & st, RegSSAUpdateCtx const* ctx);

    OptCtx const& getOptCtx() const { return m_oc; }
};
//END VRFindAndSetLiveInDef


class ReconstructRegSSAVF : public xcom::VisitTreeFuncBase {
    //Record the vertex on CFG that need to revise.
    xcom::VexTab const& m_vextab;
    xcom::Graph const* m_cfg;
    RegSSAMgr * m_regssamgr;
    OptCtx * m_oc;
    Region const* m_rg;
    ActMgr * m_am;
    DomTree const& m_dt;
protected:
    void renameBBIRList(IRBB const* bb) const;
    void renameBBPhiList(IRBB const* bb) const;
public:
    ReconstructRegSSAVF(
        xcom::VexTab const& vextab, DomTree const& dt, xcom::Graph const* cfg,
        RegSSAMgr * mgr, OptCtx * oc, ActMgr * am);

    ActMgr * getActMgr() const { return m_am; }

    //The interface of VisitTree to access each Vertex.
    //v: the vertex on DomTree.
    bool visitWhenFirstMeet(Vertex const* v, Stack<Vertex const*> &)
    {
        Vertex const* cfgv = m_cfg->getVertex(v->id());
        ASSERT0(cfgv);
        if (!m_vextab.find(cfgv->id())) { return true; }
        IRBB * bb = m_rg->getBB(cfgv->id());
        ASSERT0(bb);
        renameBBPhiList(bb);
        renameBBIRList(bb);
        return true;
    }
    void visitWhenAllKidHaveBeenVisited(
        Vertex const*, Stack<Vertex const*> &) {}
};


//The class reconstructs RegSSA info for given region in DomTree order.
//The region recorded in 'm_vextab' of ReconstructRegSSAVF.
class ReconstructRegSSA : public xcom::VisitTree<ReconstructRegSSAVF> {
public:
    ReconstructRegSSA(
        xcom::DomTree const& dt, xcom::Vertex const* root,
        ReconstructRegSSAVF & vf)
        : VisitTree((Tree const&)dt, root->id(), vf) {}
    void reconstruct() { visit(getRoot()); }
};


//RegSSA Update Context
//The class records and propagates auxiliary information to maintain RegSSA
//information during miscellaneous optimizations.
#define RegSSAUPDATECTX_update_duchain(x) ((x)->m_update_duchain_by_dominfo)
#define RegSSAUPDATECTX_removed_vropnd_list(x) ((x)->m_removed_vropnd_irlist)
class RegSSAUpdateCtx : public PassCtx {
    //THE CLASS ALLOWS COPY-CONSTRUCTION.
public:
    //Pass info top-down.
    //True to ask RegSSAMgr to maintain RegSSA DU chain by DomInfo.
    bool m_update_duchain_by_dominfo;
    xcom::DomTree const* m_dom_tree;

    //Pass info top-down.
    //It is optional. If the member is not NULL, it will record IR stmt|exp
    //that VROpnd has been removed out from its RegSSAInfo.
    IRList * m_removed_vropnd_irlist;
public:
    RegSSAUpdateCtx(OptCtx & oc, ActMgr * am = nullptr);

    void dump(Region const* rg) const;

    IRList * getRemovedVROpndIRList() const { return m_removed_vropnd_irlist; }
    xcom::DomTree const* getDomTree() const { return m_dom_tree; }

    bool need_update_duchain() const
    { return RegSSAUPDATECTX_update_duchain(this); }

    RegSSAUpdateCtx const& operator = (RegSSAUpdateCtx const&);

    void setRemovedVROpndIRList(IRList * lst)
    { RegSSAUPDATECTX_removed_vropnd_list(this) = lst; }
    void setDomTree(xcom::DomTree const* domtree) { m_dom_tree = domtree; }

    //Try to record ir in an user given list if it is not NULL.
    void tryRecordRemovedVROpndIR(IR * ir) const
    {
        ASSERT0(ir);
        ASSERT0(ir->is_stmt() || ir->is_exp());
        if (m_removed_vropnd_irlist != nullptr) {
            //The list will ensure ir is unique in the list.
            m_removed_vropnd_irlist->append_tail(ir);
        }
    }

    //Unify the members info which propagated bottom up.
    void unionBottomUpInfo(RegSSAUpdateCtx const&) const {}
};


//The class construct RegSSA form and manage the RegSSA information for
//stmt and expression.
//RegSSAInfo is only avaiable to Memory Reference IR operations, include
//IR_LD, IR_ST, IR_ILD, IR_IST, IR_ARRAY, IR_STARRAY, IR_ID.
//RegSSA information is constructed for each Reg of IR, thus for a given
//IR stmt/expression, one can get a set of virtual Reg. Each virtual Reg
//have an unique DEF IR stmt, and a list of USE IR expressions. There
//is double link in bewteen any of two virtual Reg that describe same real Reg.
//The DU Chain of stmt and expression is consist a DEF and its USE set.
//The DU Chain of stmt and stmt is consist of a list of linked DEF of
//virtual Reg with different version.
//If you are going to remove USE of virtual Reg, remove IR expression from
//USE set in paticular virtual Reg, and remove the virtual Reg from current
//IR's RegSSAInfo.
class RegSSAMgr : public Pass {
    friend class RegPhi;
    friend class RegSSAConstructRenameVisitVF;
    friend class LocalRegSSADump;
    COPY_CONSTRUCTOR(RegSSAMgr);
protected:
    bool m_is_semi_pruned;
    TypeMgr * m_tm;
    IRCFG * m_cfg;
    IRMgr * m_irmgr;
    xcom::DefMiscBitSetMgr * m_sbs_mgr;
    xcom::DefSegMgr * m_seg_mgr;
	LinearScanRA * m_ra;
	TargInfoMgr * m_timgr;
    ActMgr * m_am;
    IRIter m_iter; //for tmp use.

    //Record version stack during renaming.
    Reg2VRegStack m_map_reg2stack;

    //record version number counter for pr.
    xcom::Vector<UINT> m_max_version;

    RegDUMgr m_dumgr;
protected:
    //Add an USE to given regssainfo.
    //use: occurence to be added.
    //regssainfo: add 'use' to the UseSet of VROpnd that recorded
    //            in 'regssainfo'.
    //Note regssainfo must be unique for each IR.
    void addUseToRegSSAInfo(IR const* use, RegSSAInfo * regssainfo);
    void addUseSetToRegSSAInfo(IRSet const& set, RegSSAInfo * regssainfo);
    void addUseSetToVReg(IRSet const& set, MOD VReg * vreg);
    void addDefChain(RegDef * def1, RegDef * def2);

    //NOTE the function only should be called at constructor.
    void cleanInConstructor()
    {
        m_is_valid = false;
        m_is_semi_pruned = true;
    }
    void cleanIRSSAInfo(IRBB * bb);
    void cutoffDefChain(RegDef * def);
    bool canPhiReachRealRegDef(
        RegSSAInfo const* regssainfo, RegDef const* realdef) const;

    //The function destroy data structures that allocated during SSA
    //construction, and these data structures are only useful in construction.
    void cleanLocalUsedData();
    void collectDefinedReg(IRBB const* bb, OUT DefRegSet & maydef) const;
    void collectDefinedRegAndInitVReg(IN IRBB * bb, OUT DefRegSet & maydef);
    void collectUseReg(IR const* ir, OUT LiveInRegTab & livein_reg);

    //livein_reg: record Regs that defined in 'bb'.
    void computeLiveInReg(IRBB const* bb, OUT LiveInRegTab & livein_reg);

    //Destruction of RegSSA.
    //The function perform SSA destruction via scanning BB in preorder
    //traverse dominator tree.
    //Return true if inserting copy at the head of fallthrough BB
    //of current BB's predessor.
    void destruction(DomTree & domtree, RegSSAUpdateCtx const* ctx);
    void destructBBSSAInfo(IRBB * bb, RegSSAUpdateCtx const* ctx);
    void destructionInDomTreeOrder(
        IRBB * root, DomTree & domtree, RegSSAUpdateCtx const* ctx);

    //The function dump all possible DEF of 'vropnd' by walking through the
    //Def Chain.
    void dumpDefByWalkDefChain(
        List<RegDef const*> & wl, IRSet & visited, VReg const* vropnd) const;
    void dumpExpDUChainIter(
        IR const* ir, MOD ConstIRIter & it, OUT bool * parting_line) const;
    void dumpDUChainForStmt(IR const* ir, bool & parting_line) const;
    void dumpDUChainForStmt(IR const* ir, ConstIRIter & it) const;
    void dumpBBRef(IN IRBB * bb, UINT indent);
    void dumpIRWithRegSSAForStmt(IR const* ir) const;
    void dumpIRWithRegSSAForExpTree(IR const* ir) const;
    void dumpRegSSAInfoForExp(
        OUT xcom::DefFixedStrBuf & buf, IR const* ir) const;
    void dumpRegSSAInfoForStmt(
        OUT xcom::DefFixedStrBuf & buf, IR const* ir) const;
    bool doOpndHaveValidDef(RegPhi const* phi) const;

    //Return true if DEF of operands are the same one.
    //CASE1: if all opnds have same defintion or defined by current phi,
    //then phi is redundant.
    //common_def: record the common_def if the definition of all opnd is
    //            the same. NOTE: common_def is NULL if DEF of all operands
    //            are the same livein-def.
    //TODO: p=phi(m,p), if the only use of p is phi, then phi is redundant.
    bool doOpndHaveSameDef(RegPhi const* phi, OUT VReg ** common_def) const;

    //The function finds DEF for ID of Phi by walking through DomInfo.
    //id: input ID.
    //olddef: old DEF of id.
    void findNewDefForID(
        IR * id, VReg const* olddef_res, bool olddef_is_phi,
        IRBB const* olddef_bb, IR const* olddef_occ, OptCtx const& oc,
        OUT RegSSAStatus & st);

    //Find live-in VReg through given start IR at start BB.
    //Note DOM info must be available.
    //reg: find the live-in VReg for the Reg.
    //bb: the work BB, namely, the function will do searching job in this BB.
    //startir: the start position in 'startbb', it can be NULL.
    //         If it is NULL, the function first finding the Phi list of
    //         'startbb', then keep finding its predecessors until meet the
    //         CFG entry.
    //startbb: the BB that begin to do searching. It can NOT be NULL.
    VReg * findLiveInDefFrom(
        Reg reg, IRBB const* bb, IR const* startir,
        IRBB const* startbb) const;
    void freeBBPhiList(IRBB * bb, RegSSAUpdateCtx const* ctx);
    void freePhiList(RegSSAUpdateCtx const* ctx);

    VReg * genVReg(Reg reg, UINT version)
    { return getRegDUMgr()->allocVReg(reg, version); }
    RegDef * genRegDefStmt(IR * ir, VReg * result);

    //Replace opnd of PHI of 'succ' with top SSA version.
    void handlePhiInSuccBB(
        IRBB * succ, UINT opnd_idx, Reg2VRegStack & reg2vregstk);
    void handleBBRename(
        IRBB * bb, DefRegSet const& effect_regs, DefRegSet const& defed_regs,
        MOD BB2VRegMap & bb2vregmap, Reg2VRegStack & reg2vregstk);

    void init()
    {
        if (m_dumgr.m_regssainfo_pool != nullptr) { return; }
        m_is_valid = false;
    }
    void initVReg(IN IR * ir, OUT DefRegSet & maydef);

    //Insert a new PHI into bb according to given Reg.
    //Note the operand of PHI will be initialized in initial-version.
    RegPhi * insertPhi(Reg reg, IN IRBB * bb)
    {
        UINT num_opnd = bb->getVex()->getInDegree();
        return insertPhi(reg, bb, num_opnd);
    }

    //Return true if phi is killing-def.
    bool isPhiKillingDef(RegPhi const* phi) const;

    //Return true if stmt dominate use's stmt, otherwise return false.
    bool isStmtDomUseInsideLoop(
        IR const* stmt, IR const* use, LI<IRBB> const* li) const;
    void initVRegStack(
        DefRegSet const& defregs, OUT Reg2VRegStack & reg2verstk);
    void initDepPass(OptCtx & oc);

    void renamePhiOpndInSuccBB(IRBB * bb, Reg2VRegStack & reg2vregstk);
    void renamePhiResult(IN IRBB * bb, Reg2VRegStack & reg2vregstk);
    void renameUse(IR * ir, Reg2VRegStack & reg2vregstk);
    void renameDef(IR * ir, IRBB * bb, Reg2VRegStack & reg2vregstk);
    void rename(DefRegSet const& effect_regs, BB2DefRegSet & bb2defregs,
                DomTree const& domtree, MOD Reg2VRegStack & reg2vregstk);
    void renameBB(IRBB * bb, Reg2VRegStack & reg2vregstk);

    //The function removes 'vropnd' from RegSSAInfo of each ir in its UseSet.
    //Note the UseSet will be clean.
    void removeVROpndForAllUse(MOD VReg * vropnd, RegSSAUpdateCtx const& ctx);

    //The function changes VROpnd of 'from' to 'to', for each elements in 'from'
    //UseSet.
    //Note all elements in UseSet of 'from' will be removed.
    void replaceVROpndForAllUse(MOD VReg * to, MOD VReg * from);

    //The function remove and clean all informations of 'vreg' from RegSSAMgr.
    void removeVReg(VReg * vreg) { getRegDUMgr()->removeVReg(vreg); }

    //Remove all VReg in set from RegSSAMgr. The function will clean all
    //information about these VRegs.
    void removeVRegInSet(VROpndSet const& set);

    //wl: is an optional parameter to record BB which expected to deal with.
    //    It is a work-list that is used to drive iterative collection and
    //    elimination of redundant PHI.
    //Return true if phi removed.
    bool removePhiHasNoValidDef(
        List<IRBB*> * wl, RegPhi * phi, OptCtx const& oc);

    //wl: is an optional parameter to record BB which expected to deal with.
    //    It is a work-list that is used to drive iterative collection and
    //    elimination of redundant PHI.
    //Return true if phi removed.
    bool removePhiHasCommonDef(
        List<IRBB*> * wl, RegPhi * phi, OptCtx const& oc);

    //Remove PHI that without any USE.
    //Return true if any PHI removed, otherwise return false.
    bool removePhiNoUse(RegPhi * phi, OptCtx const& oc);

    //Record all modified Regs which will be versioned later.
    void recordEffectReg(IRBB const* bb, OUT DefRegSet & effect_reg);

    //Remove RegDef from DefDef chain.
    //Note the function does not deal with RegSSAInfo of IR occurrence, and just
    //process DefDef chain that built on RegDef.
    //regdef: will be removed from DefDef chain, and be modified as well.
    //prev: previous Def to regdef, and will be modified.
    //e.g:D1->D2
    //     |->D3
    //     |  |->D5
    //     |  |->D6
    //     |->D4
    //  where D1 is predecessor of D2, D3 and D4; D3 is predecssor of D5, D6.
    //  After remove D3:
    //e.g:D1->D2
    //     |->D5
    //     |->D6
    //     |->D4
    //  where D1 is predecessor of D2, D5, D6, D4.
    void removeDefFromDDChainHelper(RegDef * regdef, RegDef * prev);

    bool prunePhi(List<IRBB*> & wl, OptCtx const& oc);
    bool prunePhiForBB(IRBB const* bb, List<IRBB*> * wl, OptCtx const& oc);

    //Insert PHI for VReg.
    //defbbs: record BBs which defined the VReg identified by 'reg'.
    //visited: record visited BB id
    void placePhiForReg(
        Reg reg, List<IRBB*> const* defbbs, DfMgr const& dfm,
        xcom::BitSet & visited, List<IRBB*> & wl, BB2DefRegSet & defregs_vec);

    //Place phi and assign the v0 for each PR.
    //effect_reg: record the Reg which need to versioning.
    void placePhi(
        DfMgr const& dfm, MOD DefRegSet & effect_reg, DefMiscBitSetMgr & bs_mgr,
        BB2DefRegSet & defined_reg_vec, List<IRBB*> & wl);

    //Union successors in NextSet from 'from' to 'to'.
    void unionSuccessors(RegDef const* from, RegDef const* to);

    bool verifyDUChainAndOccForPhi(RegPhi const* phi) const;
    bool verifyPhiOpndList(RegPhi const* phi, UINT prednum) const;
    bool verifyRegSSAInfoUniqueness() const;
    void verifyDef(RegDef const* def, VReg const* vropnd) const;
    //Check SSA uses.
    void verifyUseSet(VReg const* vropnd) const;
    void verifyRegSSAInfoForIR(IR const* ir) const;
    bool verifyRefedVReg() const;
    bool verifyAllVReg() const;
public:
    explicit RegSSAMgr(Region * rg);
    ~RegSSAMgr()
    {
        //CAUTION: If you do not finish out-of-SSA prior to destory(),
        //the reference to IR's RegSSA info will lead to undefined behaviors.
        //ASSERTN(!is_valid(), ("should be destructed"));
        destroy(nullptr);
    }

    //Add occurence to each vropnd in regssainfo.
    //ir: occurence to be added.
    //ref: the reference that is isomorphic to 'ir'.
    //     It is used to retrieve RegSSAInfo.
    void addRegSSAOccForTree(IR * ir, IR const* ref);

    //After adding BB or change BB successor,
    //you need add the related PHI operand if BB successor has PHI stmt.
    //NOTE: the function will attempt to find the latest live-in version
    //of the new operand Reg of PHI.
    void addSuccessorDesignatedPhiOpnd(
        IRBB * bb, IRBB * succ, OptCtx const& oc, OUT RegSSAStatus & st);

    //Add stmt 'ir' into RegSSAMgr.
    //The function will change DD chain that indicated by 'marker'.
    void addStmtToRegSSAMgr(IR * ir, RegSSAUpdateCtx const& ctx);

    //Build DU chain from 'def' to 'exp'.
    //The function will add VROpnd of 'def' to 'exp'.
    void buildDUChain(RegDef const* def, MOD IR * exp);

    //Build DU chain from 'def' to each IR in set.
    //The function will add VROpnd of 'def' to 'exp'.
    void buildDUChain(RegDef const* def, IRSet const& set);

    //Build DU chain from 'def' to each IR in lst.
    //The function will add VROpnd of 'def' to 'exp'.
    void buildDUChain(RegDef const* def, IRList const& lst);

    //Construction of RegSSA form.
    //Note: Non-SSA DU Chains will be maintained after construction.
    void construction(OptCtx & oc);

    //Construction of RegSSA form.
    bool construction(DomTree & domtree, OptCtx & oc);
    size_t count_mem() const;

    //DU chain operation.
    //Change Def stmt from 'olddef' to 'newdef'.
    //olddef: original stmt.
    //newdef: target stmt.
    //e.g: olddef->{USE1,USE2} change to newdef->{USE1,USE2}.
    void changeDef(IR * olddef, IR * newdef);

    //DU chain operation.
    //Change Def stmt from orginal RegDef to 'newdef'.
    //oldvreg: original VReg.
    //newdef: target RegDef.
    //e.g: olddef->{USE1,USE2} change to newdef->{USE1,USE2}.
    void changeDef(VReg * oldvreg, RegDef * newdef);

    //DU chain operation.
    //Change Defined VReg from 'oldvreg' to 'newvreg'.
    //Note the function is similar to changeDef(), however it handle
    //VReg rather than IR.
    //oldvreg: original VReg.
    //newvreg: target VReg.
    //e.g: oldvreg:Reg5V1->{USE1,USE2}, newvreg:Reg5V2->{USE3,USE4}
    //after change: Reg5V2->{USE1,USE2,USE3,USE4}.
    void changeVReg(VReg * oldvreg, VReg * newvreg);

    //DU chain operation.
    //Change Use expression from 'olduse' to 'newuse'.
    //olduse: single source expression.
    //newuse: single target expression.
    //e.g: Change RegSSA DU chain DEF->olduse to DEF->newuse.
    void changeUse(IR * olduse, IR * newuse);

    //DU chain operation.
    //Change VROpnd of exp to a set of VROpnd that defined outside the 'li'.
    //exp: expression to be changed, its RegSSAInfo should be available.
    //li: given loopinfo.
    //e.g: given oldef->USE, change to newdef->USE.
    void changeDefToOutsideLoopDef(IR * exp, LI<IRBB> const* li);
    void changeDefToOutsideLoopDefForTree(IR * exp, LI<IRBB> const* li);

    //DU chain operation.
    //Change Use expression from 'olduse' to 'newuse'.
    //olduse: source expression as tree root.
    //newuse: target expression as tree root.
    //e.g: Change RegSSA DU chain DEF->olduse to DEF->newuse.
    void changeUseForTree(IR * olduse, IR * newuse, RegSSAUpdateCtx const& ctx);

    //Coalesce DU chain, actually the version of Reg, from 'src' to 'tgt'.
    //'src' and 'tgt' refered the same Reg.
    //This function replace definition of USE of src to tgt's defintion.
    //There is always removeStmt() following the function call.
    //'src' and 'tgt' is the form of copy operation.
    //e.g: p0 =...
    //     p1 = p0
    //     ...= p1
    //=> after coalescing, p1 is src, p0 is tgt
    //     p0 = ...
    //     ------ //removed
    //     ... = p0
    void coalesceDUChain(IR const* src, IR const* tgt);
    void cleanAllRegSSAInfo();

    //The function copy RegSSAInfo from 'src' to tgt.
    void copyRegSSAInfo(IR * tgt, IR const* src);

    //The function copy RegSSAInfo from tree 'src' to tree tgt.
    //Note src and tgt must be isomorphic.
    void copyRegSSAInfoForTree(IR * tgt, IR const* src);

    //The function copy RegSSAInfo from 'src' to ir. Then add ir as an USE
    //of the new RegSSAInfo.
    RegSSAInfo * copyAndAddRegSSAOcc(IR * ir, RegSSAInfo const* src);
    void collectDefinedRegForBBList(
        MOD DefMiscBitSetMgr & bs_mgr, OUT BB2DefRegSet & bb2defregs) const;

    //The function collects all USE expressions of 'def' into 'useset'.
    //def: stmt that defined NonPR memory reference.
    void collectUseSet(IR const* def, VRCollectFlag f, OUT IRSet * useset);

    //The function collects all USE expressions of 'def' into 'useset'.
    //def: stmt that defined NonPR memory reference.
    //li: loopinfo. If 'f' contained loop related flags, li can not be NULL.
    void collectUseSet(
        IR const* def, LI<IRBB> const* li, VRCollectFlag f, OUT IRSet * useset);

    //The function constructs SSA mode for all NonPR operations that recorded
    //in 'ssarg'.
    //The IR list and related BBs should have been recorded in SSA-region.
    //ssarg: a SSA-region that records a list of BB that describes the region.
    //NOTE:
    // 1. the SSA-region is independent to Region.
    // 2. The function will transform all NonPR operations that belong to
    //    the SSA-region.
    bool constructDesignatedRegion(MOD SSARegion & ssarg);

    //Destroy all objects in RegSSAMgr.
    void destroy(RegSSAUpdateCtx const* ctx);

    //Destruction of RegSSA.
    void destruction(OptCtx & oc);

    //Dump Phi List.
    void dumpPhiList(RegPhiList const* philist) const;

    //Dump RegSSA reference info.
    virtual bool dump() const;

    //Dump RegSSA DU chain.
    void dumpDUChain() const;

    //Dump RegSSA VROpnd reference.
    void dumpVROpndRef() const;

    //Dump IRBB list with RegSSA info.
    void dumpBBList() const;

    //The function dumps VReg structure and SSA DU info.
    void dumpAllVReg() const;

    //Dump IR tree and RegSSAInfo if any.
    //ir: can be stmt or expression.
    //flag: the flag to dump IR.
    void dumpIRWithRegSSA(IR const* ir, DumpFlag flag = IR_DUMP_COMBINE) const;

    //Dump IR tree and RegSSAInfo if any.
    //NOTE: the function will dump IR with the setting on 'ctx'.
    //ir: can be stmt or expression.
    //ctx: the IR dump context that guide how to dump IR.
    void dumpIRWithRegSSA(IR const* ir, MOD IRDumpCtx<> * ctx) const;
    void dumpBBListWithRegSSAInfo() const;

    //Duplicate Phi operand that is at the given position, and insert after
    //given position sequently.
    //pos: given position
    //num: the number of duplication.
    //Note caller should guarrentee the number of operand is equal to the
    //number predecessors of BB of Phi.
    void dupAndInsertPhiOpnd(IRBB const* bb, UINT pos, UINT num);

    //Find killing must-def IR stmt for expression ir.
    //Return the IR stmt if found.
    //aggressive: true to apply a costly and aggressive method to look up
    //    killing-def. Note the aggressive method will traverse VReg via DefDef
    //    chain if the cover-def is PHI or nearest def is indepdent to 'ir'
    //    actually.
    //e.g: g is global variable, it is exact.
    //x is a pointer that we do not know where it pointed to.
    //    1. *x += 1; # *x may overlapped with g
    //    2. g = 0; # exactly defined g
    //    3. call foo(); # foo may overlapped with g
    //    4. return g;
    //In the case, the last reference of g in stmt 4 may be defined by
    //stmt 1, 2, 3, there is no nearest killing def.
    IR * findKillingDefStmt(IR const* ir, bool aggressive) const;

    //Find killing must-def virtual DEF of ir.
    //Return the RegDef if found.
    //e.g: g is global variable, it is exact.
    //x is a pointer that we do not know where it pointed to.
    //    1. *x += 1; # *x may overlapped with g
    //    2. g = 0; # exactly defined g
    //    3. call foo(); # foo may overlapped with g
    //    4. return g;
    //In the case, the last reference of g in stmt 4 may be defined by
    //stmt 1, 2, 3, there is no nearest killing def.
    RegDef * findKillingRegDef(IR const* ir) const;

    //Find the nearest virtual DEF of 'ir'.
    //ir: expression.
    //skip_independent_def: true to ask the function to only look for
    //                      the may-dependent RegDef.
    RegDef * findNearestDef(IR const* ir, bool skip_independent_def) const;

    //Find the MustDef of 'ir'.
    RegDef * findMustRegDef(IR const* ir) const;

    //Find VReg from ir list and phi list.
    VReg * findLastMayDef(IRBB const* bb, Reg reg) const;

    //Find VReg from ir list and phi list.
    VReg * findLastMayDefFrom(IRBB const* bb, IR const* start, Reg reg) const;
    VReg * findVRegFromPhiList(IRBB const* bb, Reg reg) const;

    //Find the unique DEF of 'exp' that is inside given loop.
    //set: it is optional, if it is not NULL, the function will record all DEF
    //     found into the set as a return result.
    IR * findUniqueDefInLoopForMustRef(
        IR const* exp, LI<IRBB> const* li, Region const* rg,
        OUT IRSet * set) const;

    //The function try to find the unique RegDef for given def that is outside
    //of the loop.
    //Return the RegDef if found, otherwise nullptr.
    RegDef const* findUniqueOutsideLoopDef(
        RegDef const* phi, LI<IRBB> const* li) const;

    //Find live-in def-stmt through given start IR at start BB.
    //Note DOM info must be available.
    //reg: find the live-in DEF for the Reg.
    //startir: the start position in 'startbb', it can be NULL.
    //         If it is NULL, the function first finding the Phi list of
    //         'startbb', then keep finding its predecessors until meet the
    //         CFG entry.
    //startbb: the BB that begin to do searching. It can NOT be NULL.
    //reason: record the reason if the function can not find Dom-LiveIn-Def.
    //Return nullptr if the funtion can not find a exist Dom-LiveIn-Def of
    //'reg'. It may be a Region-LiveIn-VReg.
    VReg * findDomLiveInDefFrom(
        Reg reg, IR const* startir, IRBB const* startbb, OptCtx const& oc,
        OUT RegSSAStatus & st) const;

    //The function do searching that begin at the IDom BB of marker.
    //Note DOM info must be available.
    VReg * findDomLiveInDefFromIDomOf(
        IRBB const* marker, Reg reg, OptCtx const& oc,
        OUT RegSSAStatus & st) const;
    RegDef const* findNearestCoverDefThatCanReach(IR const* ir) const;

    //Find the VROpnd if 'ir' must OR may referenced 'reg'.
    //Return the VReg if found.
    VReg * findMayRef(IR const* ir, Reg reg) const;

    RegDUMgr * getRegDUMgr() { return &m_dumgr; }
    IRCFG * getCFG() const { return m_cfg; }
    IRMgr * getIRMgr() const { return m_irmgr; }
    ActMgr * getActMgr() { return m_am; }

    //Get specific virtual operand.
    VROpnd * getVROpnd(UINT i) const { return m_dumgr.getVROpnd(i); }
    VReg * getVReg(UINT i) const { return (VReg*)getVROpnd(i); }
    virtual CHAR const* getPassName() const { return "RegSSA Manager"; }
    PASS_TYPE getPassType() const { return PASS_REGSSA_MGR; }

    //Get the BB for given expression.
    static IRBB * getExpBB(IR const* ir);

    //Get RegSSAInfo if exist.
    RegSSAInfo * getRegSSAInfoIfAny(IR const* ir) const
    { return hasRegSSAInfo(ir) ? m_dumgr.getRegSSAInfo(ir) : nullptr; }

    //Get PhiList if any.
    RegPhiList * getPhiList(UINT bbid) const
    { return m_dumgr.getBBPhiList(bbid); }
    RegPhiList * getPhiList(IRBB const* bb) const
    { return getPhiList(bb->id()); }

    //Gen PhiList for given bbid.
    RegPhiList * genPhiList(UINT bbid)
    { return m_dumgr.genBBPhiList(bbid); }
    xcom::DefMiscBitSetMgr * getSBSMgr() const { return m_sbs_mgr; }

    //Generate RegSSAInfo and generate VReg for referrenced Reg that
    //both include.
    //The function will generate RegSSAInfo for 'exp' according to the refinfo.
    //that defined inside li. The new info for 'exp' will be VReg that defined
    //outside of li or the initial version of VReg.
    void genRegSSAInfoToOutsideLoopDef(
        IR * exp, RegSSAInfo const* refinfo, LI<IRBB> const* li);

    //Generate RegSSAInfo and generate VROpnd for referrenced Reg that
    //both include must-ref Reg and may-ref Regs.
    RegSSAInfo * genRegSSAInfoAndSetDedicatedVersionVReg(IR * ir, UINT version);

    //Generate RegSSAInfo and generate VROpnd for referrenced Reg that both
    //include must-ref Reg and may-ref Regs.
    //ir: must be stmt.
    RegSSAInfo * genRegSSAInfoAndSetNewVesionVReg(IR * ir);

    //The function is a wrapper of RegDUMgr's function.
    RegSSAInfo * genRegSSAInfo(MOD IR * ir)
    { return getRegDUMgr()->genRegSSAInfo(ir); }

    //The function will generate new version which is used to idenify Reg.
    UINT genNewVersion(Reg reg)
    {
        UINT newversion = m_max_version.get(reg) + 1;
        m_max_version.set(reg, newversion);
        return newversion;
    }
    RegPhi * genRegPhi(Reg reg, UINT num_opnd, IRBB * bb, VReg * result);
    RegPhi * genRegPhi(Reg reg, IR * opnd_list, IRBB * bb, VReg * result);
    RegPhi * genRegPhi(Reg reg, IR * opnd_list, IRBB * bb)
    {
        VReg * result = genNewVersionVReg(reg);
        RegPhi * phi = genRegPhi(reg, opnd_list, bb, result);
        VREG_def(result) = phi;
        return phi;
    }
    RegPhi * genRegPhi(Reg reg, UINT num_opnd, IRBB * bb)
    {
        VReg * result = genNewVersionVReg(reg);
        RegPhi * phi  = genRegPhi(reg, num_opnd, bb, result);
        VREG_def(result) = phi;
        return phi;
    }

    //The function generates new VReg with initial version.
    VReg * genInitVersionVReg(Reg reg)
    { return genVReg(reg, REGSSA_INIT_VERSION); }

    //The function generates new VReg with latest version.
    VReg * genNewVersionVReg(Reg reg)
    { return genVReg(reg, genNewVersion(reg)); }

	LinearScanRA const* getRA() const { return m_ra; }
    Reg getReg(IR const* ir) const;
    SRegSet const* getAliasRegSet(Reg reg) const;
	TargInfoMgr * getTIMgr() const { return m_timgr; }

    //Return true if ir has been assigned physical-register.
    bool hasReg(IR const* ir) const { return getReg(ir) != REG_UNDEF; }

    //Return true if ir might have RegSSAInfo.
    static bool hasRegSSAInfo(IR const* ir)
    { return ir->isPROp() || ir->getCode() == IR_PHYREG; }

    //Return true if ir might have RegSSAInfo.
    static bool hasExpRegSSAInfo(IR const* ir)
    { return ir->is_pr() || ir->getCode() == IR_PHYREG; }

    //Return true if ir might have RegSSAInfo.
    static bool hasStmtRegSSAInfo(IR const* ir) { return ir->isPROp(); }

    //Return true if exist USE to 'ir'.
    //The is a helper function to provid simple query, an example to
    //show how to retrieval VROpnd and USE occurences as well.
    //ir: must be stmt.
    bool hasUse(IR const* ir) const;

    //Return true if exist DEF to 'ir'.
    //The is a helper function to provid simple query, an example to
    //show how to retrieval VROpnd and DEF occurences as well.
    //ir: must be exp.
    bool hasDef(IR const* ir) const;

    //Return true if bb has PHI.
    bool hasPhi(UINT bbid) const
    {
        return getPhiList(bbid) != nullptr &&
               getPhiList(bbid)->get_elem_count() > 0;
    }
    bool hasPhi(IRBB const* bb) const { return hasPhi(bb->id()); }

    //Return true if current bb has Phi with all same operand.
    //e.g: regphi IDx = (IDy, IDy, IDy), return true.
    //     regphi IDx = (0x3, IDy, IDy), return false.
    //Note if 'bb' does NOT have any PHI, the function will return true.
    bool hasPhiWithAllSameOperand(IRBB const* bb) const;

    //Return true if the value of ir1 and ir2 are definitely same, otherwise
    //return false to indicate unknown.
    bool hasSameValue(IR const* ir1, IR const* ir2) const;

    //Return true if VRegs of stmt cross version when moving stmt
    //outside of loop.
    bool isCrossLoopHeadPhi(
        IR const* stmt, LI<IRBB> const* li, OUT bool & cross_nonphi_def) const;

    //Return true if the DU chain between 'def' and 'use' can be ignored during
    //DU chain manipulation.
    //def: one of DEF of exp.
    //exp: expression.
    bool isOverConservativeDUChain(RegDef const* def, IR const* exp) const;

    //Return true if the DU chain between 'def' and 'use' can be ignored during
    //DU chain manipulation.
    //ir1: related DEF or USE to same VReg, can be stmt/exp.
    //ir2: related DEF or USE to same VReg, can be stmt/exp.
    bool isOverConservativeDUChain(IR const* ir1, IR const* ir2) const;

    //Return true if there is at least one USE 'vreg' has been placed in given
    //IRBB 'bbid'.
    //vreg: the function will iterate all its USE occurrences.
    //it: for tmp used.
    bool isUseWithinBB(
        VReg const* vreg, MOD VReg::UseSetIter & it, UINT bbid) const;

    //Initial VReg stack for each Reg in 'bb2defregs'.
    void initVRegStack(
        BB2DefRegSet const& bb2defregs, OUT Reg2VRegStack & reg2verstk);

    //Insert a new PHI into bb according to given Reg.
    //Note the operand and PHI's result will be initialized in initial-version.
    RegPhi * insertPhi(Reg reg, IN IRBB * bb, UINT num_opnd);

    //Insert a new PHI into bb according to given Reg.
    //Note the operand will be initialized in initial-version, whereas
    //the PHI's result will be initialized in latest-version.
    RegPhi * insertPhiWithNewVersion(Reg reg, IN IRBB * bb, UINT num_opnd);

    //The function insert def1 in front of def2 in DefDef chain.
    //Note def1 should dominate def2.
    void insertDefBefore(RegDef * def1, MOD RegDef * def2);
    void insertDefBefore(VReg const* def1, VReg const* def2)
    { insertDefBefore(def1->getDef(), def2->getDef()); }

    //Return true if ir dominates all its USE expressions which inside loop.
    //In ssa mode, stmt's USE may be placed in operand list of PHI.
    bool isStmtDomAllUseInsideLoop(IR const* ir, LI<IRBB> const* li) const;
    bool isDom(RegDef const* def1, RegDef const* def2) const;

    //Return true if all vropnds of 'def' can reach 'exp'.
    bool isMustDef(IR const* def, IR const* exp) const;
    bool isExactCover(Reg reg1, Reg reg2) const;
    bool isAlias(Reg reg1, Reg reg2) const;
    bool isAlias(IR const* ir, Reg reg2) const;

    //Return true if ir1's register may overlap to ir2's register.
    //ir1: stmt or expression.
    //ir2: stmt or expression.
    bool isDependent(IR const* ir1, IR const* ir2) const;

    //Return true if ir describes a region live-in Reg reference.
    //The function returns true if ir is region live-in in all paths that
    //begins at the region entry to current ir.
    //e.g: *p = 10; #S1 //The stmt does not have MustRef, its MayRef is Reg2.
    //     ... = g; #S2 //g's MustRef is Reg7, MayRef is Reg2.
    //     the function will return false because #S1 may define Reg2.
    bool isRegionLiveIn(IR const* ir) const;

    //Move PHI from 'from' to 'to'.
    //The function often used in updating PHI when adding new dominater
    //BB to 'to'.
    void movePhi(IRBB * from, IRBB * to);

    //Reinitialize RegSSA manager.
    //The function will clean all informations and recreate them.
    void reinit(OptCtx const& oc);

    //Remove RegSSA Use-Def chain.
    //Note the function will remove IR tree from all VROpnds and RegSSAMgr.
    //And do NOT remove stmt from BBIRList before call the function.
    //The function does NOT deal with sibling node of ir.
    //e.g:ir = ...
    //       = ir //S1
    //If S1 will be deleted, ir should be removed from its UseSet in RegSSAInfo.
    //NOTE: If ir is a IR tree, say ild(x, ld(y)), remove ild(x) means
    //ld(y) will be removed too. Meanwhile ld(y)'s RegSSAInfo will be
    //updated as well.
    void removeRegSSAOccForTree(IR const* ir, RegSSAUpdateCtx const& ctx);

    //Remove DEF-USE chain if exist in between 'stmt' and 'exp'.
    //The function will remove 'exp' from occurence set.
    //stmt: IR stmt that is DEF of 'exp'.
    //exp: IR expression to be removed.
    void removeDUChain(IR const* stmt, IR const* exp);

    //Remove DU chain from 'def' to 'exp'.
    //The function will add VROpnd of phi to 'exp'.
    void removeDUChain(RegDef const* def, IR * exp);

    //Remove all RegSSAInfo of 'stmt' from RegSSAMgr.
    //The RegSSAInfo includes Def and UseSet info.
    //Note this function only handle stmt's RegSSAInfo, thus it will not
    //process its RHS expression.
    void removeStmtRegSSAInfo(IR const* stmt, RegSSAUpdateCtx const& ctx);

    //Remove given IR expression from UseSet of each vropnd in RegSSAInfo.
    //Note current RegSSAInfo is the SSA info of 'exp', the VROpndSet will be
    //emtpy when exp is removed from all VROpnd's useset.
    //exp: IR expression to be removed.
    //NOTE: the function only process exp itself.
    void removeExpFromAllVROpnd(IR const* exp);

    //Remove Use-Def chain.
    //exp: the expression to be removed.
    //e.g: ir = ...
    //    = ir //S1
    //If S1 will be deleted, ir should be removed from its useset in RegSSAInfo.
    //NOTE: the function only process exp itself.
    void removeUse(IR const* exp);

    //The function removes 'phi' from RegSSAMgr.
    //It will cut off DU chain of each phi's operands, and the DU chain of phi
    //itself as well, then free all resource.
    //phi: to be removed.
    //prev: previous DEF that is used to maintain DefDef chain, and it can be
    //      NULL if there is no previous DEF.
    void removePhiFromRegSSAMgr(
        RegPhi * phi, RegDef * prev, RegSSAUpdateCtx const& ctx);

    //The function remove all RegPhis in 'bb'.
    //Note caller should guarantee phi is useless and removable.
    void removePhiList(IRBB * bb, RegSSAUpdateCtx const& ctx);

    //Remove RegDef from DefDef chain.
    //e.g:D1<->D2
    //     |<->D3
    //     |   |<->D5
    //     |   |<->D6
    //     |<->D4
    //  where predecessor of D3 is D1, successors of D3 are D5, D6
    //  After remove D3:
    //e.g:D1<->D2
    //    D1<->D5
    //    D1<->D6
    //    D1<->D4
    //    D3<->nullptr
    //  where predecessor of D5, D6 is D1, successor of D1 includes D5, D6.
    void removeDefFromDDChain(RegDef * regdef, RegSSAUpdateCtx const& ctx);

    //Remove RegDef from Def-Def chain.
    //phi: will be removed from Def-Def chain, and be modified as well.
    //prev: designated previous DEF.
    //e.g:D1<->D2
    //     |<->D3
    //     |   |<->D5
    //     |   |<->D6
    //     |->D4
    //  where predecessor of D3 is D1, successors of D3 are D5, D6
    //  After remove D3:
    //    D1<->D2
    //     |<->D5
    //     |<->D6
    //     |<->D4
    //    D3<->nullptr
    //  where predecessor of D5, D6 is D1, successor of D1 includes D5, D6.
    void removePhiFromDDChain(
        RegPhi * phi, RegDef * prev, RegSSAUpdateCtx const& ctx);
    void removeRegDefFromDDChain(
        RegDef * regdef, RegDef * prev, RegSSAUpdateCtx const& ctx);
    bool removeRedundantPhi(IRBB const* bb, OptCtx const& oc)
    { return prunePhiForBB(bb, nullptr, oc); }

    //Return true if any PHI was removed.
    bool removeRedundantPhi(OptCtx const& oc);

    //Before removing bb or changing BB successor,
    //you need remove the related PHI operand if BB 'succ' has PHI stmt.
    //ctx: pass the update-info top down.
    //NOTE: the function is always successful in maintaining RegSSA info, thus
    //      no need to require RegSSAStatus.
    void removeSuccessorDesignatedPhiOpnd(
        IRBB const* succ, UINT pos, RegSSAUpdateCtx const& ctx);

    //Check each USE of stmt, remove the expired expression which is not
    //reference the memory any more that stmt defined.
    //Return true if DU changed.
    bool removeExpiredDU(IR const* ir);

    //The function reconstructs entire PRSSA.
    bool reconstruction(OptCtx & oc)
    {
        destruction(oc);
        construction(oc);
        return true;
    }

    void setInitVersionVReg(xcom::List<IR const*> const& lst);
    void setInitVersionVReg(IR const* ir);
    void setInitVersionVReg(IR const* ir, OUT RegSSAInfo * regssainfo);
    void setNewVesionVReg(IR * ir, OUT RegSSAInfo * regssainfo);
    void setDedicatedVersionVReg(
        IR const* ir, OUT RegSSAInfo * regssainfo, UINT version);

    //The function will attempt to remove the USE that located in outside loop.
    //Note the function will NOT cross RegPhi.
    bool tryRemoveOutsideLoopUse(RegDef * def, LI<IRBB> const* li);

    bool verifyDUChainAndOcc() const;
    bool verifyDDChain() const;

    //The function verify the operand and VReg info for RegPhi.
    //NOTE: some pass, e.g:DCE, will remove RegPhi step by step, thus
    //do NOT invoke the function during the removing.
    bool verifyPhi() const;

    //Note the verification is relatively slow.
    bool verifyVersion(OptCtx const& oc) const;
    bool verifyVReg(VReg const* vreg, BitSet * defset = nullptr) const;
    bool verify() const;
    static bool verifyRegSSAInfo(Region const* rg, OptCtx const& oc);

    virtual bool perform(OptCtx & oc) { construction(oc); return true; }
};

} //namespace xoc
#endif
