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
#ifndef _IR_MDSSA_H_
#define _IR_MDSSA_H_

namespace xoc {

class ActMgr;
class MDSSAUpdateCtx;

typedef xcom::TMap<UINT, VMD*> MD2VMD;
typedef xcom::TMap<UINT, MDPhiList*> BB2MDPhiList;
typedef xcom::TMapIter<UINT, MDPhiList*> BB2MDPhiListIter;
typedef xcom::List<MDDef*> MDDefIter;
typedef xcom::TTab<UINT> LiveInMDTab;
typedef xcom::TTabIter<UINT> LiveInMDTabIter;
typedef xcom::DefSBitSet DefMDSet;
typedef xcom::DefSBitSetIter DefMDSetIter;
typedef xcom::Stack<VMD*> VMDStack;

typedef enum tagMDSSA_STATUS {
    //Describe miscellaneous information for IR.
    MDSSA_STATUS_DOM_IS_INVALID_POS = 0,
    MDSSA_STATUS_DOM_IS_INVALID = 1ULL<<MDSSA_STATUS_DOM_IS_INVALID_POS,
} MDSSA_STATUS;

class MDSSAStatus : public Status {
public:
    //Return true if current status indicates complete success.
    bool is_succ() const { return get_status_num() == 0; }

    //Return the name of status.
    virtual CHAR const* getStatusName(FlagSetIdx s) const override;
};

enum COLLECT_FLAG {
    COLLECT_UNDEF = 0x0,

    //Collect immediate USEs of the DEF.
    //The collection does not cross PHI.
    COLLECT_IMM_USE = 0x1,

    //Do collection cross PHI operand.
    COLLECT_CROSS_PHI = 0x2,

    //Do collection inside the given loop region.
    COLLECT_INSIDE_LOOP = 0x4,

    //Do collection to the outside-immediate USE to the given loop region.
    //e.g:given loop which has an outside-immeidate USE.
    //  t3=1
    //  LOOP_START:
    //  t1=MDPhi(t2,t3)
    //  truebr ... LOOP_END
    //  t2=t1
    //  goto LOOP_START
    //
    //  LOOP_END:
    //  t5=MDPhi(t1,t6) #S1
    //  t7=t5 #S2
    //In this example, #S1's t1 is the outside-immediate USE.
    //Note #S2 refers t5, but the t5 is not outside-immediate USE.
    COLLECT_OUTSIDE_LOOP_IMM_USE = 0x8,
};

class CollectFlag : public UFlag {
public:
    CollectFlag(UINT v) : UFlag(v) {}
};

//The class represents the context information to collection.
class CollectCtx {
public:
    LI<IRBB> const* m_li;
    CollectFlag flag;
public:
    CollectCtx(CollectFlag f) : m_li(nullptr), flag(f) {}
    LI<IRBB> const* getLI() const { return m_li; }
    void setLI(LI<IRBB> const* li) { m_li = li; }
    bool verify() const;
};

//Collect all DEFs that overlapped with 'ref'.
//The collection will conform rules that declared in 'ctx'.
//Note the function will not clear 'set' because caller may perform unify
//operation.
class CollectDef {
    COPY_CONSTRUCTOR(CollectDef);
    MDSSAMgr const* m_mgr;
    MDSSAInfo const* m_info;
    CollectCtx const& m_ctx;
    MD const* m_ref;
    UseDefMgr const* m_udmgr;
protected:
    void collect(OUT IRSet * set) const;
    void collectDefThroughDefChain(MDDef const* def, OUT IRSet * set) const;
public:
    //ref: given MD, if it is NULL, the function will collect all DEFs.
    //     Otherwise, the collection continues until encounter the
    //     killing-def of 'ref'.
    //ctx: if the collection will keep iterating DEF according to rules
    //     declared in ctx. e.g: do collection by cross PHI operand.
    //set: record the return result.
    CollectDef(MDSSAMgr const* mgr, MDSSAInfo const* info,
               CollectCtx const& ctx, MD const* ref, OUT IRSet * set);
};

//Collect all USE, where USE is IR expression.
//Note the function will not clear 'set' because caller may perform unify
//operation.
class CollectUse {
    COPY_CONSTRUCTOR(CollectUse);
    class MDDefVisitor : public xcom::TTab<UINT>  {
    public:
        bool is_visited(UINT id) const { return find(id); }
        void set_visited(UINT id) { append(id); }
    };
    MDSSAMgr const* m_mgr;
    CollectCtx const& m_ctx;
    MD const* m_ref;
    UseDefMgr const* m_udmgr;
protected:
    void collectForVOpnd(VOpnd const* vopnd, OUT IRSet * set) const;
    void collectForMDSSAInfo(MDSSAInfo const* info, OUT IRSet * set) const;
    void collectUseCrossPhi(MDPhi const* phi, MOD MDDefVisitor & vis,
                            OUT IRSet * set) const;
    void collectOutsideLoopImmUseForVOpnd(
        VOpnd const* vopnd, MOD MDDefVisitor & vis, OUT IRSet * set) const;
    void collectUseForVOpnd(VOpnd const* vopnd, MOD MDDefVisitor & vis,
                            OUT IRSet * set) const;
public:
    //ctx: indicates the terminating condition that the function should
    //     stop and behaviors what the collector should take when encountering
    //     specific IR operator. e.g: do collection by cross PHI operand.
    //set: record the return result.
    //info: collect USE for each VOpnd of 'info'.
    CollectUse(MDSSAMgr const* mgr, MDSSAInfo const* info,
               CollectCtx const& ctx, OUT IRSet * set);

    //vopnd: collect USE for 'vopnd'.
    CollectUse(MDSSAMgr const* mgr, VMD const* vmd,
               CollectCtx const& ctx, OUT IRSet * set);
};


class BB2DefMDSet : public xcom::Vector<DefMDSet*> {
    COPY_CONSTRUCTOR(BB2DefMDSet);
public:
    BB2DefMDSet() {}
    void dump(Region const* rg) const;
};


class BB2VMDMap : public xcom::Vector<MD2VMD*> {
    COPY_CONSTRUCTOR(BB2VMDMap);
    bool checkClean()
    {
        //Verify if vpmap of each BB has been deleted.
        for (VecIdx i = 0; i <= get_last_idx(); i++) {
            ASSERT0(get(i) == nullptr);
        }
        return true;
    }
public:
    BB2VMDMap() {}
    BB2VMDMap(UINT n) : Vector<MD2VMD*>(n) {}
    ~BB2VMDMap() { ASSERT0(checkClean()); }

    MD2VMD * gen(UINT bbid)
    {
        MD2VMD * mdid2vmd = get(bbid);
        if (mdid2vmd == nullptr) {
            mdid2vmd = new MD2VMD();
            set(bbid, mdid2vmd);
        }
        return mdid2vmd;
    }

    void erase(UINT bbid)
    {
        MD2VMD * mdid2vmd = get(bbid);
        if (mdid2vmd != nullptr) {
            delete mdid2vmd;
            set(bbid, nullptr);
        }
    }

    void setElemNum(UINT n) { grow(n); }
};


//Mapping from MD id to Stack of VMD.
class MD2VMDStack : public xcom::Vector<VMDStack*> {
    COPY_CONSTRUCTOR(MD2VMDStack);
public:
    MD2VMDStack() {}
    ~MD2VMDStack() { destroy(); }

    void clean() { destroy(); Vector<VMDStack*>::init(); }

    void dump(Region const* rg) const;
    void destroy();

    VMDStack * gen(UINT mdid);
    VMD * get_top(VMD const* md) const { return get_top(md->mdid()); }
    VMD * get_top(UINT mdid) const;

    void push(VMD * vmd) { push(vmd->mdid(), vmd); }
    void push(UINT mdid, VMD * vmd);
};


//The class define the iterator that used to iterate IR occurrence for
//each VOpnd in MDSSA mode.
class ConstMDSSAUSEIRIter {
    COPY_CONSTRUCTOR(ConstMDSSAUSEIRIter);
public:
    VOpndSet * vopndset;
    VOpndSetIter vopndset_iter;
    BSIdx current_pos_in_vopndset;
    VMD::UseSetIter useset_iter;
    BSIdx current_pos_in_useset;
    VMD::UseSet const* current_useset;
    MDSSAMgr const* m_mdssamgr;
    UseDefMgr const* m_udmgr;
    Region const* m_rg;
public:
    ConstMDSSAUSEIRIter(MDSSAMgr const* mdssamgr);

    void clean()
    {
        vopndset_iter = nullptr;
        current_pos_in_vopndset = BS_UNDEF;
        useset_iter.clean();
        current_pos_in_useset = BS_UNDEF;
        current_useset = nullptr;
    }

    //Iterative access USE in MDSSAInfo. The USE always an IR occurrence that
    //describes a memory expression.
    //The funtion initialize the iterator.
    //def: the MDDef of the chain.
    //it: iterator. It should be clean already.
    //Readonly function.
    //Note the function may iterate same IR multiple times because it may
    //belong different VOpnd.
    //e.g: global int g; local int b;
    //     g = b;
    //The MDSSA info of ST is:
    // st:i32 'g'
    //  --DEFREF:(MD2V2, PrevDEF:MD2V1, NextDEF : MD2V3) | UsedBy : ld b(id:15)
    //  --DEFREF : (MD5V2, PrevDEF:MD5V1) | UsedBy : ld b(id:15), id(id:23)
    //  ld b is both USE of VOpnd(MD2V2) and VOpnd(MD5V2).
    IR const* get_first(IR const* def);

    //Iterative access USE in MDSSAInfo. The USE always an IR occurrence that
    //describes a memory expression.
    //The function return the next USE according to 'it'.
    //it: iterator.
    //Readonly function.
    //Note the function may iterate same IR multiple times because it may
    //belong different VOpnd.
    //e.g: global int g; local int b;
    //     g = b;
    //The MDSSA info of ST is:
    // st:i32 'g'
    //  --DEFREF:(MD2V2, PrevDEF:MD2V1, NextDEF : MD2V3) | UsedBy : ld b(id:15)
    //  --DEFREF : (MD5V2, PrevDEF:MD5V1) | UsedBy : ld b(id:15), id(id:23)
    //  ld b is both USE of VOpnd(MD2V2) and VOpnd(MD5V2).
    IR const* get_next();
};


class ConstMDDefIter : public xcom::List<MDDef const*> {
    COPY_CONSTRUCTOR(ConstMDDefIter);
protected:
    MDSSAMgr const* m_mdssamgr;
    xcom::TTab<UINT> m_is_visited;
public:
    ConstMDDefIter(MDSSAMgr const* mdssamgr) : m_mdssamgr(mdssamgr) {}

    bool is_visited(MDDef const* def) const
    { return m_is_visited.find(def->id()); }

    //Iterative access MDDef chain.
    //The funtion initialize the iterator.
    //When the iterator meets MDPhi, it will keep iterating the DEF of each
    //operand of MDPhi.
    //def: the beginning MDDef of the chain.
    //it: iterator. It should be clean already.
    MDDef const* get_first(MDDef const* def);

    //Iterative access MDDef chain.
    //The function return the next MDDef node according to 'it'.
    //When the iterator meets MDPhi, it will keep iterating the DEF of each
    //operand of MDPhi.
    //it: iterator.
    MDDef const* get_next();

    //Iterative access MDDef chain.
    //The funtion initialize the iterator.
    //When the iterator meets MDPhi, it will keep iterating the DEF of each
    //operand of MDPhi.
    //def: the beginning MDDef of the chain.
    //use: indicate the USE expression of the 'def'.
    //it: iterator. It should be clean already.
    //Readonly function.
    MDDef const* get_first_untill_killing_def(MDDef const* def, IR const* use);

    //Iterative access MDDef chain.
    //The function return the next MDDef node according to 'it'.
    //When the iterator meets MDPhi, it will keep iterating the DEF of each
    //operand of MDPhi.
    //it: iterator.
    //use: indicate the USE expression of the 'def'.
    //Readonly function.
    MDDef const* get_next_untill_killing_def(IR const* use);

    void set_visited(MDDef const* def) { m_is_visited.append(def->id()); }
};


typedef VOpndSetIter VMDLiveSetIter;
class VMDLiveSet : public VOpndSet {
    COPY_CONSTRUCTOR(VMDLiveSet);
    xcom::DefMiscBitSetMgr * m_sbsmgr;
public:
    VMDLiveSet(VMD const* vmd, xcom::DefMiscBitSetMgr * sbsmgr)
    {
        bunion(vmd->id(), *sbsmgr);
        m_sbsmgr = sbsmgr;
    }
    VMDLiveSet(VOpndSet const& set, xcom::DefMiscBitSetMgr * sbsmgr)
    {
        VOpndSet::copy(set, *sbsmgr);
        m_sbsmgr = sbsmgr;
    }
    ~VMDLiveSet()
    {
        //Should call clean() before destruction,
        //otherwise it will incur SegMgr assertion.
        clean();
    }

    bool all_killed() const { return is_empty(); }

    void copy(VMDLiveSet const& src)
    { VOpndSet::copy((VOpndSet const&)src, *m_sbsmgr); }
    void copy(VOpndSet const& vopndset) { VOpndSet::copy(vopndset, *m_sbsmgr); }
    void clean() { VOpndSet::clean(*m_sbsmgr); }

    void dump(MDSSAMgr const* mgr) const;

    bool is_live(UINT id) const { return is_contain(id); }

    void set_killed(UINT id) { diff(id, *m_sbsmgr); }
};


typedef xcom::TMapIter<UINT, VMDLiveSet*> BBID2VMDLiveSetIter;
class BBID2VMDLiveSet : public xcom::TMap<UINT, VMDLiveSet*> {
    COPY_CONSTRUCTOR(BBID2VMDLiveSet);
    xcom::DefMiscBitSetMgr m_sbsmgr;
public:
    BBID2VMDLiveSet() {}
    ~BBID2VMDLiveSet()
    {
        BBID2VMDLiveSetIter it;
        VMDLiveSet * set;
        for (get_first(it, &set); set != nullptr; get_next(it, &set)) {
            delete set;
        }
    }
    void dump(Region const* rg) const;

    void free(UINT bbid)
    {
        VMDLiveSet * set = remove(bbid);
        if (set != nullptr) { delete set; }
    }

    VMDLiveSet * genAndCopy(UINT bbid, VMDLiveSet const& src)
    { return genAndCopy(bbid, (VOpndSet const&)src); }
    VMDLiveSet * genAndCopy(UINT bbid, VOpndSet const& src)
    {
        VMDLiveSet * liveset = get(bbid);
        if (liveset == nullptr) {
            liveset = new VMDLiveSet(src, &m_sbsmgr);
            set(bbid, liveset);
        } else {
            liveset->copy(src);
        }
        return liveset;
    }
    VMDLiveSet * genAndCopy(UINT bbid, VMD const* vmd)
    {
        VMDLiveSet * liveset = new VMDLiveSet(vmd, &m_sbsmgr);
        ASSERT0(get(bbid) == nullptr);
        set(bbid, liveset);
        return liveset;
    }
};


//
//SATRT RenameExp
//
//The class performs renaming for each NonPR Memory expression.
class RenameExp {
    COPY_CONSTRUCTOR(RenameExp);
protected:
    MDSSAMgr * m_mgr;
    ActMgr * m_am;
    Region * m_rg;
    OptCtx * m_oc;
public:
    RenameExp(MDSSAMgr * mgr, OptCtx * oc, ActMgr * am);

    MDSSAMgr * getMgr() const { return m_mgr; }
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
//END RenameExp


//
//START RenameDef
//
//The class generates VMD for new Stmt or new MDPhi, then inserts VMD into
//DefDef chain and renames all immediate USEs of the original DEF meanwhile.
//Note after renaming, if the class find a memory reference X that references
//a version MD that is same to new Stmt or new MDPhi, the class will build
//a DefUse chain between the memory reference X and the new Stmt or new MDPhi.
class RenameDef {
    friend class RenameDefVisitFunc;
    COPY_CONSTRUCTOR(RenameDef);
    bool m_is_build_ddchain; //Set to true to build DefDef chain by DomTree.
    DomTree const& m_domtree;
    VMDLiveSet * m_liveset;
    MDSSAMgr * m_mgr;
    UseDefMgr * m_udmgr;
    IRCFG * m_cfg;
    Region * m_rg;
    ActMgr * m_am;
    BBID2VMDLiveSet m_bbid2liveset;
private:
    void connect(Vertex const* defvex, VMDLiveSet * defliveset,
                 IRBB const* start_bb, IR const* start_ir);

    //stmtbbid: indicates the BB of inserted stmt
    //v: the vertex on DomTree.
    //bb: the BB that to be renamed
    //dompred: indicates the predecessor of 'bb' in DomTree
    //Note stmtbbid have to dominate 'bb'.
    void connectDefInBBTillPrevDef(
        IRBB const* bb, BBIRListIter & irlistit, MOD VMDLiveSet & liveset);
    void connectIRTillPrevDef(
        IRBB const* bb, BBIRListIter & irlistit, MOD VMDLiveSet & liveset);
    void connectPhiTillPrevDef(
        IRBB const* bb, BBIRListIter & irlistit, MOD VMDLiveSet & liveset);
    void connectDefInterBBTillPrevDef(
        Vertex const* defvex, MOD VMDLiveSet & stmtliveset,
        IRBB const* start_bb);

    void dumpRenameBB(IRBB const* bb);
    void dumpRenameVMD(IR const* ir, VMD const* vmd);
    void dumpInsertDDChain(IR const* ir, VMD const* vmd);
    void dumpInsertDDChain(MDPhi const* phi, VMD const* vmd);
    void dumpRenamePhi(MDPhi const* phi, UINT opnd_pos);

    BBID2VMDLiveSet & getBBID2VMDLiveSet() { return m_bbid2liveset; }

    void iterBBPhiListToKillLivedVMD(IRBB const* bb, VMDLiveSet & liveset);
    void iterSuccBBPhiListToRename(
        Vertex const* defvex, IRBB const* succ,
        UINT opnd_idx, MOD VMDLiveSet & liveset);

    //defvex: domtree vertex.
    void iterSuccBB(Vertex const* defvev, MOD VMDLiveSet & liveset);

    void killLivedVMD(MDPhi const* phi, MOD VMDLiveSet & liveset);

    void processStmt(MOD IR * newstmt);
    void processPhi(MDPhi const* newphi);

    void renamePhiOpnd(MDPhi const* phi, UINT opnd_idx, MOD VMD * vmd);

    //stmtbb: the BB of inserted stmt
    //newinfo: MDSSAInfo that intent to be swap-in.
    bool renameVMDForDesignatedPhiOpnd(
        MDPhi * phi, UINT opnd_pos, VMDLiveSet & liveset);

    //vmd: intent to be swap-in.
    //irtree: may be stmt or exp.
    //irit: for local used.
    void renameVMDForIRTree(
        IR * irtree, VMD * vmd, MOD IRIter & irit, bool & no_exp_has_ssainfo);

    //ir: may be stmt or exp
    //irit: for local used.
    void renameLivedVMDForIRTree(
        IR * ir, MOD IRIter & irit, VMDLiveSet const& liveset);
    void renameIRTillNextDef(
        IRBB const* bb, BBIRListIter & irlistit, MOD VMDLiveSet & liveset);

    //stmtbbid: indicates the BB of inserted stmt
    //bb: the BB that to be renamed
    //dompred: indicates the predecessor of 'bb' in DomTree
    //Note stmtbbid have to dominate 'bb'.
    void renameUseInBBTillNextDef(
        Vertex const* defvex, IRBB const* bb, bool include_philist,
        BBIRListIter & irlistit, OUT VMDLiveSet & liveset);

    //start_ir: if it is nullptr, the renaming will start at the first IR in bb.
    //          otherwise the renaming will start at the NEXT IR of start_ir.
    void renameFollowUseIntraBBTillNextDef(
        Vertex const* defvex, MOD VMDLiveSet & stmtliveset,
        IRBB const* start_bb, IR const* start_ir);

    //defvex: root vertex of domtree region that is being renamed.
    void renameFollowUseInterBBTillNextDef(
        Vertex const* defvex, MOD VMDLiveSet & stmtliveset,
        IRBB const* start_bb);
    void rename(Vertex const* defvex, VMDLiveSet * defliveset,
                IRBB const* start_bb, IR const* start_ir);

    //Insert vmd after phi.
    //Return true if inserted PHI into DD chain.
    bool tryInsertDDChainForDesigatedVMD(
        MDPhi * phi, VMD * vmd, MOD VMDLiveSet & liveset);

    //Insert vmd after 'ir'.
    //vmd: new generated VMD that to be inserted.
    //before: true to insert 'vmd' in front of 'ir'.
    //Return true if inserted PHI into DD chain.
    bool tryInsertDDChainForDesigatedVMD(
        IR * ir, VMD * vmd, bool before, MOD VMDLiveSet & liveset);

    //Return true if inserted 'ir' into DD chain.
    bool tryInsertDDChainForStmt(
        IR * ir, bool before, MOD VMDLiveSet & liveset);

    //Return true if inserted PHI into DD chain.
    bool tryInsertDDChainForPhi(MDPhi * phi, MOD VMDLiveSet & liveset);
public:
    RenameDef(DomTree const& dt, bool build_ddchain, MDSSAMgr * mgr,
              ActMgr * am);

    void clean();

    MDSSAMgr * getMgr() const { return m_mgr; }
    ActMgr * getActMgr() const { return m_am; }

    //The function will generate MDSSAInfo if it does not have one.
    //newstmt: record the stmt that inserted.
    void rename(MOD IR * newstmt);

    //The function will generate MDSSAInfo if it does not have one.
    //newphi: record the phi that inserted.
    void rename(MDPhi const* newphi);
};
//END RenameDef


//
//START RecomputeDefDefAndDefUseChain
//
class RecomputeDefDefAndDefUseChain {
    COPY_CONSTRUCTOR(RecomputeDefDefAndDefUseChain);
    xcom::DomTree const& m_domtree;
    MDSSAMgr * m_mgr;
    OptCtx const& m_oc;
    ActMgr * m_am;
    Region * m_rg;
    IRCFG * m_cfg;
public:
    RecomputeDefDefAndDefUseChain(
        xcom::DomTree const& domtree, MDSSAMgr * mgr,
        OptCtx const& oc, ActMgr * am);

    ActMgr * getActMgr() const { return m_am; }
    DomTree const& getDomTree() const { return m_domtree; }

    //These functions compute the DefDef chain and the DefUse chain for
    //given Stmt|MDPhi.
    //Note these functions are often invoked when new Stmt or new MDPhi
    //generated.
    //These functions will insert given Stmt|MDPhi into DefDef chain of each
    //VMD and iterate all memory references which are related to the VMD that
    //given Stmt|MDPhi carried from the start BB (Stmt|MDPhi's BB) to the next
    //versioned VMD DEF. During the iteration of VMD, these functions also
    //build DefUse chain to ensure the correctness of dependence of the new
    //Stmt|MDPhi.
    void recompute(MOD IR * ir);
    void recompute(xcom::List<IR*> const& irlist);
    void recompute(MDPhiList const* philist);
    void recompute(MDPhi const* phi);
    void recomputeDefForPhiOpnd(MDPhi const* phi);
    void recomputeDefForPhiOpnd(MDPhiList const* philist);
    void recomputeDefForRHS(MOD IR * stmt);
};
//END RecomputeDefDefAndDefUseChain


//
//START FindAndSetLiveInDef
//
class FindAndSetLiveInDef {
    COPY_CONSTRUCTOR(FindAndSetLiveInDef);
protected:
    MDSSAMgr * m_mgr;
    OptCtx const& m_oc;
public:
    FindAndSetLiveInDef(MDSSAMgr * mgr, OptCtx const& oc) :
        m_mgr(mgr), m_oc(oc) {}

    void findAndSet(
        MOD IR * exp, IRBB const* startbb, OUT MDSSAStatus & st,
        MDSSAUpdateCtx const* ctx = nullptr)
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
        OUT MDSSAStatus & st, MDSSAUpdateCtx const* ctx = nullptr);

    void findAndSetForTree(
        IR * exp, IRBB const* startbb, OUT MDSSAStatus & st,
        MDSSAUpdateCtx const* ctx = nullptr)
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
        IR * exp, IR const* startir, IRBB const* startbb, OUT MDSSAStatus & st,
        MDSSAUpdateCtx const* ctx = nullptr);

    void findAndSet(
        IR * exp, VMD const* prevdef_res, bool prevdef_is_phi,
        IRBB const* prevdef_bb, IR const* prevdef_occ,
        OUT MDSSAStatus & st, MDSSAUpdateCtx const* ctx);

    void findAndSetForLst(
        IRList const& lst, VMD const* prevdef_res, bool prevdef_is_phi,
        IRBB const* prevdef_bb, IR const* prevdef_occ,
        OUT MDSSAStatus & st, MDSSAUpdateCtx const* ctx);

    OptCtx const& getOptCtx() const { return m_oc; }
};
//END FindAndSetLiveInDef


class ReconstructMDSSAVF : public xcom::VisitTreeFuncBase {
    //Record the vertex on CFG that need to revise.
    xcom::VexTab const& m_vextab;
    xcom::Graph const* m_cfg;
    MDSSAMgr * m_mdssamgr;
    OptCtx * m_oc;
    Region const* m_rg;
    ActMgr * m_am;
    DomTree const& m_dt;
protected:
    void renameBBIRList(IRBB const* bb) const;
    void renameBBPhiList(IRBB const* bb) const;
public:
    ReconstructMDSSAVF(xcom::VexTab const& vextab, DomTree const& dt,
                       xcom::Graph const* cfg, MDSSAMgr * mgr, OptCtx * oc,
                       ActMgr * am);

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


//The class reconstructs MDSSA info for given region in DomTree order.
//The region recorded in 'm_vextab' of ReconstructMDSSAVF.
class ReconstructMDSSA : public xcom::VisitTree<ReconstructMDSSAVF> {
public:
    ReconstructMDSSA(xcom::DomTree const& dt, xcom::Vertex const* root,
                     ReconstructMDSSAVF & vf)
        : VisitTree((Tree const&)dt, root->id(), vf) {}
    void reconstruct() { visit(getRoot()); }
};


//MDSSA Update Context
//The class records and propagates auxiliary information to maintain MDSSA
//information during miscellaneous optimizations.
#define MDSSAUPDATECTX_update_duchain(x) ((x)->m_update_duchain_by_dominfo)
#define MDSSAUPDATECTX_removed_vopnd_list(x) ((x)->m_removed_vopnd_irlist)
class MDSSAUpdateCtx : public PassCtx {
    //THE CLASS ALLOWS COPY-CONSTRUCTION.
public:
    //Pass info top-down.
    //True to ask MDSSAMgr to maintain MDSSA DU chain by DomInfo.
    bool m_update_duchain_by_dominfo;
    xcom::DomTree const* m_dom_tree;

    //Pass info top-down.
    //It is optional. If the member is not NULL, it will record IR stmt|exp
    //that VOpnd has been removed out from its MDSSAInfo.
    IRList * m_removed_vopnd_irlist;
public:
    MDSSAUpdateCtx(OptCtx & oc, ActMgr * am = nullptr);

    void dump(Region const* rg) const;

    IRList * getRemovedVOpndIRList() const { return m_removed_vopnd_irlist; }
    xcom::DomTree const* getDomTree() const { return m_dom_tree; }

    bool need_update_duchain() const
    { return MDSSAUPDATECTX_update_duchain(this); }

    MDSSAUpdateCtx const& operator = (MDSSAUpdateCtx const&);

    void setRemovedVOpndIRList(IRList * lst)
    { MDSSAUPDATECTX_removed_vopnd_list(this) = lst; }
    void setDomTree(xcom::DomTree const* domtree) { m_dom_tree = domtree; }

    //Try to record ir in an user given list if it is not NULL.
    void tryRecordRemovedVOpndIR(IR * ir) const
    {
        ASSERT0(ir);
        ASSERT0(ir->is_stmt() || ir->is_exp());
        if (m_removed_vopnd_irlist != nullptr) {
            //The list will ensure ir is unique in the list.
            m_removed_vopnd_irlist->append_tail(ir);
        }
    }

    //Unify the members info which propagated bottom up.
    void unionBottomUpInfo(MDSSAUpdateCtx const& c) const {}
};


//The class construct MDSSA form and manage the MDSSA information for
//stmt and expression.
//MDSSAInfo is only avaiable to Memory Reference IR operations, include
//IR_LD, IR_ST, IR_ILD, IR_IST, IR_ARRAY, IR_STARRAY, IR_ID.
//MDSSA information is constructed for each MD of IR, thus for a given
//IR stmt/expression, one can get a set of virtual MD. Each virtual MD
//have an unique DEF IR stmt, and a list of USE IR expressions. There
//is double link in bewteen any of two virtual MD that describe same real MD.
//The DU Chain of stmt and expression is consist a DEF and its USE set.
//The DU Chain of stmt and stmt is consist of a list of linked DEF of
//virtual MD with different version.
//If you are going to remove USE of virtual MD, remove IR expression from
//USE set in paticular virtual MD, and remove the virtual MD from current
//IR's MDSSAInfo.
class MDSSAMgr : public Pass {
    friend class MDPhi;
    friend class MDSSAConstructRenameVisitVF;
    friend class LocalMDSSADump;
    COPY_CONSTRUCTOR(MDSSAMgr);
protected:
    BYTE m_is_semi_pruned:1;
    MDSystem * m_md_sys;
    TypeMgr * m_tm;
    IRCFG * m_cfg;
    IRMgr * m_irmgr;
    xcom::DefMiscBitSetMgr * m_sbs_mgr;
    xcom::DefSegMgr * m_seg_mgr;
    IRIter m_iter; //for tmp use.

    //Record version stack during renaming.
    MD2VMDStack m_map_md2stack;

    //record version number counter for pr.
    xcom::Vector<UINT> m_max_version;

    UseDefMgr m_usedef_mgr;
    ActMgr * m_am;
protected:
    //Add an USE to given mdssainfo.
    //use: occurence to be added.
    //mdssainfo: add 'use' to the UseSet of VOpnd that recorded in 'mdssainfo'.
    //Note mdssainfo must be unique for each IR.
    void addUseToMDSSAInfo(IR const* use, MDSSAInfo * mdssainfo);
    void addUseSetToMDSSAInfo(IRSet const& set, MDSSAInfo * mdssainfo);
    void addUseSetToVMD(IRSet const& set, MOD VMD * vmd);
    void addDefChain(MDDef * def1, MDDef * def2);

    //NOTE the function only should be called at constructor.
    void cleanInConstructor()
    {
        m_is_valid = false;
        m_is_semi_pruned = true;
    }
    void cleanIRSSAInfo(IRBB * bb);
    void cleanMDSSAInfoAI();
    void cutoffDefChain(MDDef * def);
    bool canPhiReachRealMDDef(
        MDSSAInfo const* mdssainfo, MDDef const* realdef) const;

    //The function destroy data structures that allocated during SSA
    //construction, and these data structures are only useful in construction.
    void cleanLocalUsedData();
    void collectDefinedMD(IRBB const* bb, OUT DefMDSet & maydef) const;
    void collectDefinedMDAndInitVMD(IN IRBB * bb, OUT DefMDSet & maydef);
    void collectUseMD(IR const* ir, OUT LiveInMDTab & livein_md);

    //livein_md: record MDs that defined in 'bb'.
    void computeLiveInMD(IRBB const* bb, OUT LiveInMDTab & livein_md);

    //Destruction of MDSSA.
    //The function perform SSA destruction via scanning BB in preorder
    //traverse dominator tree.
    //Return true if inserting copy at the head of fallthrough BB
    //of current BB's predessor.
    void destruction(DomTree & domtree, MDSSAUpdateCtx const* ctx);
    void destructBBSSAInfo(IRBB * bb, MDSSAUpdateCtx const* ctx);
    void destructionInDomTreeOrder(
        IRBB * root, DomTree & domtree, MDSSAUpdateCtx const* ctx);

    //The function dump all possible DEF of 'vopnd' by walking through the
    //Def Chain.
    void dumpDefByWalkDefChain(List<MDDef const*> & wl, IRSet & visited,
                               VMD const* vopnd) const;
    void dumpExpDUChainIter(
        IR const* ir, MOD ConstIRIter & it, OUT bool * parting_line) const;
    void dumpDUChainForStmt(IR const* ir, bool & parting_line) const;
    void dumpDUChainForStmt(IR const* ir, ConstIRIter & it) const;
    void dumpBBRef(IN IRBB * bb, UINT indent);
    void dumpIRWithMDSSAForStmt(IR const* ir, bool & parting_line) const;
    void dumpIRWithMDSSAForExp(IR const* ir, bool & parting_line) const;
    bool doOpndHaveValidDef(MDPhi const* phi) const;

    //Return true if DEF of operands are the same one.
    //CASE1: if all opnds have same defintion or defined by current phi,
    //then phi is redundant.
    //common_def: record the common_def if the definition of all opnd is
    //            the same. NOTE: common_def is NULL if DEF of all operands
    //            are the same livein-def.
    //TODO: p=phi(m,p), if the only use of p is phi, then phi is redundant.
    bool doOpndHaveSameDef(MDPhi const* phi, OUT VMD ** common_def) const;

    //The function finds DEF for ID of Phi by walking through DomInfo.
    //id: input ID.
    //olddef: old DEF of id.
    void findNewDefForID(
        IR * id, VMD const* olddef_res, bool olddef_is_phi,
        IRBB const* olddef_bb, IR const* olddef_occ, OptCtx const& oc,
        OUT MDSSAStatus & st);

    //Find live-in VMD through given start IR at start BB.
    //Note DOM info must be available.
    //mdid: find the live-in VMD for the MD.
    //bb: the work BB, namely, the function will do searching job in this BB.
    //startir: the start position in 'startbb', it can be NULL.
    //         If it is NULL, the function first finding the Phi list of
    //         'startbb', then keep finding its predecessors until meet the
    //         CFG entry.
    //startbb: the BB that begin to do searching. It can NOT be NULL.
    VMD * findLiveInDefFrom(
        MDIdx mdid, IRBB const* bb, IR const* startir,
        IRBB const* startbb) const;
    void freeBBPhiList(IRBB * bb, MDSSAUpdateCtx const* ctx);
    void freePhiList(MDSSAUpdateCtx const* ctx);

    VMD * genVMD(UINT mdid, UINT version)
    { return getUseDefMgr()->allocVMD(mdid, version); }
    MDDef * genMDDefStmt(IR * ir, VMD * result);

    //Replace opnd of PHI of 'succ' with top SSA version.
    void handlePhiInSuccBB(IRBB * succ, UINT opnd_idx, MD2VMDStack & md2vmdstk);
    void handleBBRename(IRBB * bb, DefMDSet const& effect_mds,
                        DefMDSet const& defed_mds,
                        MOD BB2VMDMap & bb2vmdmap,
                        MD2VMDStack & md2vmdstk);

    void init()
    {
        if (m_usedef_mgr.m_mdssainfo_pool != nullptr) { return; }
        m_is_valid = false;
    }
    void initVMD(IN IR * ir, OUT DefMDSet & maydef);

    //Insert a new PHI into bb according to given MDIdx.
    //Note the operand of PHI will be initialized in initial-version.
    MDPhi * insertPhi(UINT mdid, IN IRBB * bb)
    {
        UINT num_opnd = bb->getVex()->getInDegree();
        return insertPhi(mdid, bb, num_opnd);
    }

    //Return true if phi is killing-def.
    bool isPhiKillingDef(MDPhi const* phi) const;

    //Return true if stmt dominate use's stmt, otherwise return false.
    bool isStmtDomUseInsideLoop(IR const* stmt, IR const* use,
                                LI<IRBB> const* li) const;
    void initVMDStack(DefMDSet const& defmds, OUT MD2VMDStack & md2verstk);

    void renamePhiOpndInSuccBB(IRBB * bb, MD2VMDStack & md2vmdstk);
    void renamePhiResult(IN IRBB * bb, MD2VMDStack & md2vmdstk);
    void renameUse(IR * ir, MD2VMDStack & md2vmdstk);
    void renameDef(IR * ir, IRBB * bb, MD2VMDStack & md2vmdstk);
    void rename(DefMDSet const& effect_mds, BB2DefMDSet & bb2defmds,
                DomTree const& domtree, MOD MD2VMDStack & md2vmdstk);
    void renameBB(IRBB * bb, MD2VMDStack & md2vmdstk);

    //The function removes 'vopnd' from MDSSAInfo of each ir in its UseSet.
    //Note the UseSet will be clean.
    void removeVOpndForAllUse(MOD VMD * vopnd, MDSSAUpdateCtx const& ctx);

    //The function changes VOpnd of 'from' to 'to', for each elements in 'from'
    //UseSet.
    //Note all elements in UseSet of 'from' will be removed.
    void replaceVOpndForAllUse(MOD VMD * to, MOD VMD * from);

    //The function remove and clean all informations of 'vmd' from MDSSAMgr.
    void removeVMD(VMD * vmd) { getUseDefMgr()->removeVMD(vmd); }

    //Remove all VMD in set from MDSSAMgr. The function will clean all
    //information about these VMDs.
    void removeVMDInSet(VOpndSet const& set);

    //wl: is an optional parameter to record BB which expected to deal with.
    //    It is a work-list that is used to drive iterative collection and
    //    elimination of redundant PHI.
    //Return true if phi removed.
    bool removePhiHasNoValidDef(List<IRBB*> * wl, MDPhi * phi,
                                OptCtx const& oc);

    //wl: is an optional parameter to record BB which expected to deal with.
    //    It is a work-list that is used to drive iterative collection and
    //    elimination of redundant PHI.
    //Return true if phi removed.
    bool removePhiHasCommonDef(List<IRBB*> * wl, MDPhi * phi, OptCtx const& oc);

    //Remove PHI that without any USE.
    //Return true if any PHI removed, otherwise return false.
    bool removePhiNoUse(MDPhi * phi, OptCtx const& oc);

    //Record all modified MDs which will be versioned later.
    void recordEffectMD(IRBB const* bb, OUT DefMDSet & effect_md);

    //Remove MDDef from DefDef chain.
    //Note the function does not deal with MDSSAInfo of IR occurrence, and just
    //process DefDef chain that built on MDDef.
    //mddef: will be removed from DefDef chain, and be modified as well.
    //prev: previous Def to mddef, and will be modified.
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
    void removeDefFromDDChainHelper(MDDef * mddef, MDDef * prev);

    bool prunePhi(List<IRBB*> & wl, OptCtx const& oc);
    bool prunePhiForBB(IRBB const* bb, List<IRBB*> * wl, OptCtx const& oc);

    //Insert PHI for VMD.
    //defbbs: record BBs which defined the VMD identified by 'mdid'.
    //visited: record visited BB id
    void placePhiForMD(UINT mdid, List<IRBB*> const* defbbs,
                       DfMgr const& dfm, xcom::BitSet & visited,
                       List<IRBB*> & wl,
                       BB2DefMDSet & defmds_vec);

    //Place phi and assign the v0 for each PR.
    //effect_md: record the MD which need to versioning.
    void placePhi(DfMgr const& dfm, MOD DefMDSet & effect_md,
                  DefMiscBitSetMgr & bs_mgr,
                  BB2DefMDSet & defined_md_vec,
                  List<IRBB*> & wl);

    //Union successors in NextSet from 'from' to 'to'.
    void unionSuccessors(MDDef const* from, MDDef const* to);

    bool verifyDUChainAndOccForPhi(MDPhi const* phi) const;
    bool verifyPhiOpndList(MDPhi const* phi, UINT prednum) const;
    bool verifyMDSSAInfoUniqueness() const;
    void verifyDef(MDDef const* def, VMD const* vopnd) const;
    //Check SSA uses.
    void verifyUseSet(VMD const* vopnd) const;
    void verifyMDSSAInfoForIR(IR const* ir) const;
    bool verifyRefedVMD() const;
    bool verifyAllVMD() const;
public:
    explicit MDSSAMgr(Region * rg);
    ~MDSSAMgr()
    {
        //CAUTION: If you do not finish out-of-SSA prior to destory(),
        //the reference to IR's MDSSA info will lead to undefined behaviors.
        //ASSERTN(!is_valid(), ("should be destructed"));
        destroy(nullptr);
    }

    //Add occurence to each vopnd in mdssainfo.
    //ir: occurence to be added.
    //ref: the reference that is isomorphic to 'ir'.
    //     It is used to retrieve MDSSAInfo.
    void addMDSSAOccForTree(IR * ir, IR const* ref);

    //After adding BB or change BB successor,
    //you need add the related PHI operand if BB successor has PHI stmt.
    //NOTE: the function will attempt to find the latest live-in version
    //of the new operand MD of PHI.
    void addSuccessorDesignatedPhiOpnd(
        IRBB * bb, IRBB * succ, OptCtx const& oc, OUT MDSSAStatus & st);

    //Add stmt 'ir' into MDSSAMgr.
    //The function will change DD chain that indicated by 'marker'.
    void addStmtToMDSSAMgr(IR * ir, MDSSAUpdateCtx const& ctx);

    //Build DU chain from 'def' to 'exp'.
    //The function will add VOpnd of 'def' to 'exp'.
    void buildDUChain(MDDef const* def, MOD IR * exp);

    //Build DU chain from 'def' to each IR in set.
    //The function will add VOpnd of 'def' to 'exp'.
    void buildDUChain(MDDef const* def, IRSet const& set);

    //Build DU chain from 'def' to each IR in lst.
    //The function will add VOpnd of 'def' to 'exp'.
    void buildDUChain(MDDef const* def, IRList const& lst);

    //Construction of MDSSA form.
    //Note: Non-SSA DU Chains will be maintained after construction.
    void construction(OptCtx & oc);

    //Construction of MDSSA form.
    bool construction(DomTree & domtree, OptCtx & oc);
    size_t count_mem() const;

    //DU chain operation.
    //Change Def stmt from 'olddef' to 'newdef'.
    //olddef: original stmt.
    //newdef: target stmt.
    //e.g: olddef->{USE1,USE2} change to newdef->{USE1,USE2}.
    void changeDef(IR * olddef, IR * newdef);

    //DU chain operation.
    //Change Def stmt from orginal MDDef to 'newdef'.
    //oldvmd: original VMD.
    //newdef: target MDDef.
    //e.g: olddef->{USE1,USE2} change to newdef->{USE1,USE2}.
    void changeDef(VMD * oldvmd, MDDef * newdef);

    //DU chain operation.
    //Change Defined VMD from 'oldvmd' to 'newvmd'.
    //Note the function is similar to changeDef(), however it handle VMD rather
    //than IR.
    //oldvmd: original VMD.
    //newvmd: target VMD.
    //e.g: oldvmd:MD5V1->{USE1,USE2}, newvmd:MD5V2->{USE3,USE4}
    //after change: MD5V2->{USE1,USE2,USE3,USE4}.
    void changeVMD(VMD * oldvmd, VMD * newvmd);

    //DU chain operation.
    //Change Use expression from 'olduse' to 'newuse'.
    //olduse: single source expression.
    //newuse: single target expression.
    //e.g: Change MDSSA DU chain DEF->olduse to DEF->newuse.
    void changeUse(IR * olduse, IR * newuse);

    //DU chain operation.
    //Change VOpnd of exp to a set of VOpnd that defined outside the 'li'.
    //exp: expression to be changed, its MDSSAInfo should be available.
    //li: given loopinfo.
    //e.g: given oldef->USE, change to newdef->USE.
    void changeDefToOutsideLoopDef(IR * exp, LI<IRBB> const* li);
    void changeDefToOutsideLoopDefForTree(IR * exp, LI<IRBB> const* li);

    //DU chain operation.
    //Change Use expression from 'olduse' to 'newuse'.
    //olduse: source expression as tree root.
    //newuse: target expression as tree root.
    //e.g: Change MDSSA DU chain DEF->olduse to DEF->newuse.
    void changeUseForTree(IR * olduse, IR * newuse, MDSSAUpdateCtx const& ctx);

    //Coalesce DU chain, actually the version of MD, from 'src' to 'tgt'.
    //'src' and 'tgt' refered the same MD.
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
    void cleanMDSSAInfo(IR * ir) { getUseDefMgr()->cleanMDSSAInfo(ir); }

    //The function copy MDSSAInfo from 'src' to tgt.
    void copyMDSSAInfo(IR * tgt, IR const* src);

    //The function copy MDSSAInfo from tree 'src' to tree tgt.
    //Note src and tgt must be isomorphic.
    void copyMDSSAInfoForTree(IR * tgt, IR const* src);

    //The function copy MDSSAInfo from 'src' to ir. Then add ir as an USE of the
    //new MDSSAInfo.
    MDSSAInfo * copyAndAddMDSSAOcc(IR * ir, MDSSAInfo const* src);
    void collectDefinedMDForBBList(MOD DefMiscBitSetMgr & bs_mgr,
                                   OUT BB2DefMDSet & bb2defmds) const;

    //The function collects all USE expressions of 'def' into 'useset'.
    //def: stmt that defined NonPR memory reference.
    void collectUseSet(IR const* def, CollectFlag f, OUT IRSet * useset);

    //The function collects all USE expressions of 'def' into 'useset'.
    //def: stmt that defined NonPR memory reference.
    //li: loopinfo. If 'f' contained loop related flags, li can not be NULL.
    void collectUseSet(IR const* def, LI<IRBB> const* li,
                       CollectFlag f, OUT IRSet * useset);

    //The function constructs SSA mode for all NonPR operations that recorded
    //in 'ssarg'.
    //The IR list and related BBs should have been recorded in SSA-region.
    //ssarg: a SSA-region that records a list of BB that describes the region.
    //NOTE:
    // 1. the SSA-region is independent to Region.
    // 2. The function will transform all NonPR operations that belong to
    //    the SSA-region.
    bool constructDesignatedRegion(MOD SSARegion & ssarg);

    //Destroy all objects in MDSSAMgr.
    void destroy(MDSSAUpdateCtx const* ctx);

    //Destruction of MDSSA.
    void destruction(OptCtx & oc);

    //Dump Phi List.
    void dumpPhiList(MDPhiList const* philist) const;

    //Dump MDSSA reference info.
    virtual bool dump() const;

    //Dump MDSSA DU chain.
    void dumpDUChain() const;

    //Dump MDSSA VOpnd reference.
    void dumpVOpndRef() const;

    //Dump IRBB list with MDSSA info.
    void dumpBBList() const;

    //The function dumps VMD structure and SSA DU info.
    void dumpAllVMD() const;

    //Dump IR tree and MDSSAInfo if any.
    //ir: can be stmt or expression.
    //flag: the flag to dump IR.
    void dumpIRWithMDSSA(IR const* ir, DumpFlag flag = IR_DUMP_COMBINE) const;

    //Dump IR tree and MDSSAInfo if any.
    //NOTE: the function will dump IR with the setting on 'ctx'.
    //ir: can be stmt or expression.
    //ctx: the IR dump context that guide how to dump IR.
    void dumpIRWithMDSSA(IR const* ir, MOD IRDumpCtx<> * ctx) const;

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
    //    killing-def. Note the aggressive method will traverse VMD via DefDef
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
    //Return the MDDef if found.
    //e.g: g is global variable, it is exact.
    //x is a pointer that we do not know where it pointed to.
    //    1. *x += 1; # *x may overlapped with g
    //    2. g = 0; # exactly defined g
    //    3. call foo(); # foo may overlapped with g
    //    4. return g;
    //In the case, the last reference of g in stmt 4 may be defined by
    //stmt 1, 2, 3, there is no nearest killing def.
    MDDef * findKillingMDDef(IR const* ir) const;

    //Find the nearest virtual DEF of 'ir'.
    //ir: expression.
    //skip_independent_def: true to ask the function to only look for
    //                      the may-dependent MDDef.
    MDDef * findNearestDef(IR const* ir, bool skip_independent_def) const;

    //Find the MustDef of 'ir'.
    MDDef * findMustMDDef(IR const* ir) const;

    //Find VMD from ir list and phi list.
    VMD * findLastMayDef(IRBB const* bb, MDIdx mdid) const;

    //Find VMD from ir list and phi list.
    VMD * findLastMayDefFrom(IRBB const* bb, IR const* start, MDIdx mdid) const;
    VMD * findVMDFromPhiList(IRBB const* bb, MDIdx mdid) const;

    //Find the unique DEF of 'exp' that is inside given loop.
    //set: it is optional, if it is not NULL, the function will record all DEF
    //     found into the set as a return result.
    IR * findUniqueDefInLoopForMustRef(
        IR const* exp, LI<IRBB> const* li, Region const* rg,
        OUT IRSet * set) const;

    //The function try to find the unique MDDef for given def that is outside
    //of the loop.
    //Return the MDDef if found, otherwise nullptr.
    MDDef const* findUniqueOutsideLoopDef(
        MDDef const* phi, LI<IRBB> const* li) const;

    //Find live-in def-stmt through given start IR at start BB.
    //Note DOM info must be available.
    //mdid: find the live-in DEF for the MD.
    //startir: the start position in 'startbb', it can be NULL.
    //         If it is NULL, the function first finding the Phi list of
    //         'startbb', then keep finding its predecessors until meet the
    //         CFG entry.
    //startbb: the BB that begin to do searching. It can NOT be NULL.
    //reason: record the reason if the function can not find Dom-LiveIn-Def.
    //Return nullptr if the funtion can not find a exist Dom-LiveIn-Def of
    //'mdid'. It may be a Region-LiveIn-VMD.
    VMD * findDomLiveInDefFrom(
        MDIdx mdid, IR const* startir, IRBB const* startbb, OptCtx const& oc,
        OUT MDSSAStatus & st) const;

    //The function do searching that begin at the IDom BB of marker.
    //Note DOM info must be available.
    VMD * findDomLiveInDefFromIDomOf(
        IRBB const* marker, MDIdx mdid, OptCtx const& oc,
        OUT MDSSAStatus & st) const;
    MDDef const* findNearestCoverDefThatCanReach(IR const* ir) const;

    //Find the VOpnd if 'ir' must OR may referenced 'md'.
    //Return the VMD if found.
    VMD * findMayRef(IR const* ir, MDIdx mdid) const;

    UseDefMgr * getUseDefMgr() { return &m_usedef_mgr; }
    IRCFG * getCFG() const { return m_cfg; }
    IRMgr * getIRMgr() const { return m_irmgr; }
    MDSystem const* getMDSystem() const { return m_md_sys; }
    ActMgr * getActMgr() { return m_am; }

    //Get specific virtual operand.
    VOpnd * getVOpnd(UINT i) const { return m_usedef_mgr.getVOpnd(i); }
    VMD * getVMD(UINT i) const { return (VMD*)getVOpnd(i); }
    virtual CHAR const* getPassName() const { return "MDSSA Manager"; }
    PASS_TYPE getPassType() const { return PASS_MDSSA_MGR; }

    //Get the BB for given expression.
    static IRBB * getExpBB(IR const* ir)
    { return ir->is_id() ? ID_phi(ir)->getBB() : ir->getStmt()->getBB(); }

    //Get MDSSAInfo if exist.
    static MDSSAInfo * getMDSSAInfoIfAny(IR const* ir)
    { return hasMDSSAInfo(ir) ? UseDefMgr::getMDSSAInfo(ir) : nullptr; }

    //Get PhiList if any.
    MDPhiList * getPhiList(UINT bbid) const
    { return m_usedef_mgr.getBBPhiList(bbid); }
    MDPhiList * getPhiList(IRBB const* bb) const
    { return getPhiList(bb->id()); }

    //Gen PhiList for given bbid.
    MDPhiList * genPhiList(UINT bbid)
    { return m_usedef_mgr.genBBPhiList(bbid); }
    xcom::DefMiscBitSetMgr * getSBSMgr() const { return m_sbs_mgr; }

    //Generate MDSSAInfo and generate VMD for referrenced MD that both include
    //The function will generate MDSSAInfo for 'exp' according to the refinfo.
    //that defined inside li. The new info for 'exp' will be VMD that defined
    //outside of li or the initial version of VMD.
    void genMDSSAInfoToOutsideLoopDef(IR * exp, MDSSAInfo const* refinfo,
                                      LI<IRBB> const* li);

    //Generate MDSSAInfo and generate VOpnd for referrenced MD that both include
    //must-ref MD and may-ref MDs.
    MDSSAInfo * genMDSSAInfoAndSetDedicatedVersionVMD(IR * ir, UINT version);

    //Generate MDSSAInfo and generate VOpnd for referrenced MD that both
    //include must-ref MD and may-ref MDs.
    //ir: must be stmt.
    MDSSAInfo * genMDSSAInfoAndSetNewVesionVMD(IR * ir);

    //The function is a wrapper of UseDefMgr's function.
    MDSSAInfo * genMDSSAInfo(MOD IR * ir)
    { return getUseDefMgr()->genMDSSAInfo(ir); }

    //The function will generate new version which is used to idenify MD.
    UINT genNewVersion(UINT mdid)
    {
        UINT newversion = m_max_version.get(mdid) + 1;
        m_max_version.set(mdid, newversion);
        return newversion;
    }
    MDPhi * genMDPhi(MDIdx mdid, UINT num_opnd, IRBB * bb, VMD * result);
    MDPhi * genMDPhi(MDIdx mdid, IR * opnd_list, IRBB * bb, VMD * result);
    MDPhi * genMDPhi(MDIdx mdid, IR * opnd_list, IRBB * bb)
    {
        VMD * result = genNewVersionVMD(mdid);
        MDPhi * phi = genMDPhi(mdid, opnd_list, bb, result);
        VMD_def(result) = phi;
        return phi;
    }
    MDPhi * genMDPhi(MDIdx mdid, UINT num_opnd, IRBB * bb)
    {
        VMD * result = genNewVersionVMD(mdid);
        MDPhi * phi  = genMDPhi(mdid, num_opnd, bb, result);
        VMD_def(result) = phi;
        return phi;
    }

    //The function generates new VMD with initial version.
    VMD * genInitVersionVMD(UINT mdid)
    { return genVMD(mdid, MDSSA_INIT_VERSION); }

    //The function generates new VMD with latest version.
    VMD * genNewVersionVMD(UINT mdid)
    { return genVMD(mdid, genNewVersion(mdid)); }

    //Return true if ir might have MDSSAInfo.
    static bool hasMDSSAInfo(IR const* ir)
    { return ir->isMemRefNonPR() || ir->isCallStmt(); }

    //Return true if exist USE to 'ir'.
    //The is a helper function to provid simple query, an example to
    //show how to retrieval VOpnd and USE occurences as well.
    //ir: must be stmt.
    bool hasUse(IR const* ir) const;

    //Return true if exist DEF to 'ir'.
    //The is a helper function to provid simple query, an example to
    //show how to retrieval VOpnd and DEF occurences as well.
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
    //e.g: mdphi IDx = (IDy, IDy, IDy), return true.
    //     mdphi IDx = (0x3, IDy, IDy), return false.
    //Note if 'bb' does NOT have any PHI, the function will return true.
    bool hasPhiWithAllSameOperand(IRBB const* bb) const;

    //Return true if the value of ir1 and ir2 are definitely same, otherwise
    //return false to indicate unknown.
    static bool hasSameValue(IR const* ir1, IR const* ir2);

    //Return true if VMDs of stmt cross version when moving stmt
    //outside of loop.
    bool isCrossLoopHeadPhi(IR const* stmt, LI<IRBB> const* li,
                            OUT bool & cross_nonphi_def) const;

    //Return true if the DU chain between 'def' and 'use' can be ignored during
    //DU chain manipulation.
    //def: one of DEF of exp.
    //exp: expression.
    bool isOverConservativeDUChain(MDDef const* def, IR const* exp) const;

    //Return true if there is at least one USE 'vmd' has been placed in given
    //IRBB 'bbid'.
    //vmd: the function will iterate all its USE occurrences.
    //it: for tmp used.
    bool isUseWithinBB(VMD const* vmd, MOD VMD::UseSetIter & it,
                       UINT bbid) const;

    //Initial VMD stack for each MD in 'bb2defmds'.
    void initVMDStack(BB2DefMDSet const& bb2defmds,
                      OUT MD2VMDStack & md2verstk);

    //Return true if the DU chain between 'def' and 'use' can be ignored during
    //DU chain manipulation.
    //ir1: related DEF or USE to same VMD, can be stmt/exp.
    //ir2: related DEF or USE to same VMD, can be stmt/exp.
    static bool isOverConservativeDUChain(IR const* ir1, IR const* ir2,
                                          Region const* rg);

    //Insert a new PHI into bb according to given MDIdx.
    //Note the operand and PHI's result will be initialized in initial-version.
    MDPhi * insertPhi(UINT mdid, IN IRBB * bb, UINT num_opnd);

    //Insert a new PHI into bb according to given MDIdx.
    //Note the operand will be initialized in initial-version, whereas
    //the PHI's result will be initialized in latest-version.
    MDPhi * insertPhiWithNewVersion(UINT mdid, IN IRBB * bb, UINT num_opnd);

    //The function insert def1 in front of def2 in DefDef chain.
    //Note def1 should dominate def2.
    void insertDefBefore(MDDef * def1, MOD MDDef * def2);
    void insertDefBefore(VMD const* def1, VMD const* def2)
    { insertDefBefore(def1->getDef(), def2->getDef()); }

    //Return true if ir dominates all its USE expressions which inside loop.
    //In ssa mode, stmt's USE may be placed in operand list of PHI.
    bool isStmtDomAllUseInsideLoop(IR const* ir, LI<IRBB> const* li) const;
    bool isDom(MDDef const* def1, MDDef const* def2) const;

    //Return true if all vopnds of 'def' can reach 'exp'.
    bool isMustDef(IR const* def, IR const* exp) const;

    //Return true if ir describes a region live-in MD reference.
    //The function returns true if ir is region live-in in all paths that
    //begins at the region entry to current ir.
    //e.g: *p = 10; #S1 //The stmt does not have MustRef, its MayRef is MD2.
    //     ... = g; #S2 //g's MustRef is MD7, MayRef is MD2.
    //     the function will return false because #S1 may define MD2.
    bool isRegionLiveIn(IR const* ir) const;

    //Move PHI from 'from' to 'to'.
    //The function often used in updating PHI when adding new dominater
    //BB to 'to'.
    void movePhi(IRBB * from, IRBB * to);

    //Reinitialize MDSSA manager.
    //The function will clean all informations and recreate them.
    void reinit(OptCtx const& oc);

    //Remove MDSSA Use-Def chain.
    //Note the function will remove IR tree from all VOpnds and MDSSAMgr.
    //And do NOT remove stmt from BBIRList before call the function.
    //The function does NOT deal with sibling node of ir.
    //e.g:ir = ...
    //       = ir //S1
    //If S1 will be deleted, ir should be removed from its UseSet in MDSSAInfo.
    //NOTE: If ir is a IR tree, say ild(x, ld(y)), remove ild(x) means
    //ld(y) will be removed too. Meanwhile ld(y)'s MDSSAInfo will be
    //updated as well.
    void removeMDSSAOccForTree(IR const* ir, MDSSAUpdateCtx const& ctx);

    //Remove DEF-USE chain if exist in between 'stmt' and 'exp'.
    //The function will remove 'exp' from occurence set.
    //stmt: IR stmt that is DEF of 'exp'.
    //exp: IR expression to be removed.
    void removeDUChain(IR const* stmt, IR const* exp);

    //Remove DU chain from 'def' to 'exp'.
    //The function will add VOpnd of phi to 'exp'.
    void removeDUChain(MDDef const* def, IR * exp);

    //Remove all MDSSAInfo of 'stmt' from MDSSAMgr.
    //The MDSSAInfo includes Def and UseSet info.
    //Note this function only handle stmt's MDSSAInfo, thus it will not
    //process its RHS expression.
    void removeStmtMDSSAInfo(IR const* stmt, MDSSAUpdateCtx const& ctx);

    //Remove given IR expression from UseSet of each vopnd in MDSSAInfo.
    //Note current MDSSAInfo is the SSA info of 'exp', the VOpndSet will be
    //emtpy when exp is removed from all VOpnd's useset.
    //exp: IR expression to be removed.
    //NOTE: the function only process exp itself.
    void removeExpFromAllVOpnd(IR const* exp);

    //Remove Use-Def chain.
    //exp: the expression to be removed.
    //e.g: ir = ...
    //    = ir //S1
    //If S1 will be deleted, ir should be removed from its useset in MDSSAInfo.
    //NOTE: the function only process exp itself.
    void removeUse(IR const* exp);

    //The function removes 'phi' from MDSSAMgr.
    //It will cut off DU chain of each phi's operands, and the DU chain of phi
    //itself as well, then free all resource.
    //phi: to be removed.
    //prev: previous DEF that is used to maintain DefDef chain, and it can be
    //      NULL if there is no previous DEF.
    void removePhiFromMDSSAMgr(MDPhi * phi, MDDef * prev,
                               MDSSAUpdateCtx const& ctx);

    //The function remove all MDPhis in 'bb'.
    //Note caller should guarantee phi is useless and removable.
    void removePhiList(IRBB * bb, MDSSAUpdateCtx const& ctx);

    //Remove MDDef from DefDef chain.
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
    void removeDefFromDDChain(MDDef * mddef, MDSSAUpdateCtx const& ctx);

    //Remove MDDef from Def-Def chain.
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
        MDPhi * phi, MDDef * prev, MDSSAUpdateCtx const& ctx);
    void removeMDDefFromDDChain(
        MDDef * mddef, MDDef * prev, MDSSAUpdateCtx const& ctx);
    bool removeRedundantPhi(IRBB const* bb, OptCtx const& oc)
    { return prunePhiForBB(bb, nullptr, oc); }

    //Return true if any PHI was removed.
    bool removeRedundantPhi(OptCtx const& oc);

    //Before removing bb or changing BB successor,
    //you need remove the related PHI operand if BB 'succ' has PHI stmt.
    //ctx: pass the update-info top down.
    //NOTE: the function is always successful in maintaining MDSSA info, thus
    //      no need to require MDSSAStatus.
    void removeSuccessorDesignatedPhiOpnd(
        IRBB const* succ, UINT pos, MDSSAUpdateCtx const& ctx);

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

    void setInitVersionVMD(xcom::List<IR const*> const& lst);
    void setInitVersionVMD(IR const* ir);
    void setInitVersionVMD(IR const* ir, OUT MDSSAInfo * mdssainfo);
    void setNewVesionVMD(IR * ir, OUT MDSSAInfo * mdssainfo);
    void setDedicatedVersionVMD(
        IR const* ir, OUT MDSSAInfo * mdssainfo, UINT version);

    //The function will attempt to remove the USE that located in outside loop.
    //Note the function will NOT cross MDPHI.
    bool tryRemoveOutsideLoopUse(MDDef * def, LI<IRBB> const* li);

    bool verifyDUChainAndOcc() const;
    bool verifyDDChain() const;

    //The function verify the operand and VMD info for MDPhi.
    //NOTE: some pass, e.g:DCE, will remove MDPhi step by step, thus
    //do NOT invoke the function during the removing.
    bool verifyPhi() const;

    //Note the verification is relatively slow.
    bool verifyVersion(OptCtx const& oc) const;
    bool verifyVMD(VMD const* vmd, BitSet * defset = nullptr) const;
    bool verify() const;
    static bool verifyMDSSAInfo(Region const* rg, OptCtx const& oc);

    virtual bool perform(OptCtx & oc) { construction(oc); return true; }
};

} //namespace xoc
#endif
