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
#ifndef _SOLVE_SET_
#define _SOLVE_SET_

namespace xoc {


typedef DefSBitSetIter SolveSetIter;

class SolveSet : public DefDBitSetCore {
    COPY_CONSTRUCTOR(SolveSet);
public:
    SolveSet(bool is_sparse) : DefDBitSetCore(is_sparse) {}
};

//Ud chains describe all of the might uses of the prior DEFINITION of md.
//Du chains describe all effective USEs of once definition of md.
//e.g:
//  d1:a=   d2:a=   d3:a=
//  \      |      /
//     b   =   a
//     |       |
//  d4:..=b      d5:...=b
//  Ud chains:  a use d1,d2,d3 stmt
//  Du chains:  b's value used by d4,d5 stmt
//If ir is stmt, this class indicates a set of USE expressions.
//If ir is expression, this class indicates a set of DEF stmts.

//The class is allocator of SBitSet that is used by Hash Tab.
class DefDBitSetCoreHashAllocator {
    COPY_CONSTRUCTOR(DefDBitSetCoreHashAllocator);
    DefMiscBitSetMgr * m_sbs_mgr;

public:
    DefDBitSetCoreHashAllocator(DefMiscBitSetMgr * sbsmgr)
    { ASSERT0(sbsmgr); m_sbs_mgr = sbsmgr; }

    DefSBitSetCore * alloc() { return m_sbs_mgr->allocDBitSetCore(); }

    void free(DefSBitSetCore * set)
    { m_sbs_mgr->freeDBitSetCore((SolveSet*)set); }

    DefMiscBitSetMgr * getBsMgr() const { return m_sbs_mgr; }
};


#define HASH_DBITSETCORE

class DefDBitSetCoreReserveTab : public
    SBitSetCoreHash<DefDBitSetCoreHashAllocator> {
    COPY_CONSTRUCTOR(DefDBitSetCoreReserveTab);
    #ifdef HASH_DBITSETCORE
    #else
    List<SolveSet*> m_allocated;
    #endif

public:
    DefDBitSetCoreReserveTab(DefDBitSetCoreHashAllocator * allocator) :
        SBitSetCoreHash<DefDBitSetCoreHashAllocator>(allocator) {}
    virtual ~DefDBitSetCoreReserveTab()
    {
        #ifdef HASH_DBITSETCORE
        #else
        //NO need to free resource here, all allocated resource will be
        //recorded in alternative structures and free.
        //xcom::C<SolveSet*> * ct;
        //for (m_allocated.get_head(&ct);
        //     ct != m_allocated.end(); ct = m_allocated.get_next(ct)) {
        //    SolveSet * s = ct->val();
        //    ASSERT0(s);
        //    get_allocator()->free(s);
        //}
        #endif
    }

    SolveSet * append(SolveSet const& set)
    {
        #ifdef HASH_DBITSETCORE
        return (SolveSet*)SBitSetCoreHash
               <DefDBitSetCoreHashAllocator>::append(set);
        #else
        SolveSet * s = (SolveSet*)m_allocator->alloc();
        ASSERT0(s);
        m_allocated.append_tail(s);
        s->copy(set, *m_allocator->getBsMgr());
        return s;
        #endif
    }

    void dump(FILE * h) const
    {
        #ifdef HASH_DBITSETCORE
        dump_hashed_set(h);
        #else
        if (h == nullptr) { return; }
        fprintf(h, "\n==---- DUMP DefDBitSetCoreReserveTab ----==");
        xcom::C<SolveSet*> * ct;
        for (m_allocated.get_head(&ct);
             ct != m_allocated.end(); ct = m_allocated.get_next(ct)) {
            SolveSet * s = ct->val();
            ASSERT0(s);
            fprintf(h, "\n");
            s->dump(h);
        }
        fflush(h);
        #endif
    }
};

class SolveSetMgr {
    COPY_CONSTRUCTOR(SolveSetMgr);
    Region * m_rg;
    IRCFG * m_cfg;
    BBList * m_bblst;
    MDSystem * m_md_sys;
    //Used by overall global functions in DUMgr. such as reach-in set.
    //The set might be queried by other passes.
    xcom::DefMiscBitSetMgr m_global_sbs_mgr;
    xcom::DefMiscBitSetMgr m_local_sbs_mgr;

    //Available reach-def computes the definitions
    //which must be the last definition of result variable,
    //but it may not reachable meanwhile.
    //e.g:
    //    BB1:
    //    a=1  //S1
    //    *p=3
    //    a=4  //S2
    //    goto BB3
    //
    //    BB2:
    //    a=2 //S3
    //    goto BB3
    //
    //    BB3:
    //    f(a)
    //
    //    Here we do not known where p pointed to.
    //    The available-reach-def of BB3 is {S1, S3}
    //
    //Compare to available reach-def, reach-def computes the definition
    //which may-live at each BB.
    //e.g:
    //    BB1:
    //    a=1  //S1
    //    *p=3
    //    goto BB3
    //
    //    BB2:
    //    a=2 //S2
    //    goto BB3
    //
    //    BB3:
    //    f(a)
    //
    //    Here we do not known where p pointed to.
    //    The reach-def of BB3 is {S1, S2}
    //avail reach-in def of STMT
    Vector<SolveSet*> m_avail_reach_def_in;
    //avail reach-out def of STMT
    Vector<SolveSet*> m_avail_reach_def_out;
    Vector<SolveSet*> m_reach_def_in; //reach-in def of STMT
    Vector<SolveSet*> m_reach_def_out; //reach-out def of STMT
    Vector<SolveSet*> m_may_gen_def; //generated-def of STMT
    Vector<SolveSet*> m_must_gen_def; //generated-def of STMT
    BSVec<SolveSet*> m_gen_ir_expr; //generate EXPR
    BSVec<SolveSet*> m_avail_exp_in; //available-in EXPR
    BSVec<SolveSet*> m_avail_exp_out; //available-out EXPR
    Vector<SolveSet*> m_must_killed_def; //must-killed def of STMT
    Vector<SolveSet*> m_may_killed_def; //may-killed def of STMT
    Vector<SolveSet*> m_livein_bb; //live-in BB
    BSVec<SolveSet*> m_killed_ir_exp; //killed EXPR
private:
    //The macro declares a series of functions to operate given SolveSet.
    //Note LocalSet will be destroy and reinialized after perform(), whereas
    //GlobalSet will reserve all information until the manager destructed.
    //e.g:given SolveSet is ReachDefIn, functions include:
    //Return true if given set will be allocated as GlobalSet.
    //bool isKeepReachDefIn() const;
    //
    //Set given set to be global if 'keep' is true, otherwise LocalSet.
    //void setKeepReachDefIn(bool keep);
    //
    //void copyReachDefIn(SolveSet * sbs, SolveSet const& src);
    //void bunionReachDefIn(SolveSet * sbs, SolveSet const& src);
    //void bunionReachDefIn(SolveSet * sbs, BSIdx elem);
    //void intersectReachDefIn(SolveSet * sbs, SolveSet const& src);
    //void diffReachDefIn(SolveSet * sbs, SolveSet const& src);
    //void diffReachDefIn(SolveSet * sbs, BSIdx elem);
    //void cleanReachDefIn(SolveSet * sbs);
    //
    //Destroy and reinitialize ReachDefIn set if it is LocalSet.
    //void resetReachDefInIfLocal();
    //
    //Destroy and reinitialize ReachDefIn set if it is GlobalSet.
    //void resetReachDefInIfGlobal();
    //
    //Generate ReachDefIn by given BB id.
    //SolveSet * genReachDefIn(UINT bbid);
    //
    //Get ReachDefIn for by given BB id.
    //SolveSet * getReachDefIn(UINT bbid);
    #define DECLARE_SOLVESET_OPERATION(SET_NAME) \
        bool m_is_##SET_NAME; \
        bool isKeep##SET_NAME() const { return m_is_##SET_NAME; } \
        void setKeep##SET_NAME(bool keep) { m_is_##SET_NAME = keep; } \
        void copy##SET_NAME(OUT SolveSet * sbs, SolveSet const& src); \
        void clean##SET_NAME(OUT SolveSet * sbs); \
        void bunion##SET_NAME(OUT SolveSet * sbs, SolveSet const& src); \
        void bunion##SET_NAME(OUT SolveSet * sbs, BSIdx elem); \
        void diff##SET_NAME(OUT SolveSet * sbs, SolveSet const& src); \
        void diff##SET_NAME(OUT SolveSet * sbs, BSIdx elem); \
        void intersect##SET_NAME(OUT SolveSet * sbs, SolveSet const& src); \
        void reset##SET_NAME##IfLocal(); \
        void reset##SET_NAME##IfGlobal(); \
        SolveSet * gen##SET_NAME(UINT bbid); \
        SolveSet * get##SET_NAME(UINT bbid) const

    //NOTE: MD referrence must be available.
    //mustdefs: record must modified MD for each bb.
    //maydefs: record may modified MD for each bb.
    //mayuse: record may used MD for each bb.
    //        collect mayuse (NOPR-DU) to compute Region referred MD.
    void computeMustExactDefMayDefMayUse(OUT Vector<MDSet*> * mustdefmds,
                                         OUT Vector<MDSet*> * maydefmds,
                                         OUT MDSet * mayusemds,
                                         UFlag flag);
    void computeMustExactDef(IR const* ir, OUT MDSet * bb_mustdefmds,
                             SolveSet * mustgen_stmt,
                             ConstMDIter & mditer, DefMiscBitSetMgr & bsmgr,
                             UFlag flag);
    void computeMayDef(IR const* ir, MDSet * bb_maydefmds,
                       SolveSet * maygen_stmt,
                       DefMiscBitSetMgr & bsmgr, UFlag flag);
    void computeKillSetByMayGenDef(UINT bbid, bool comp_must,
                                   bool comp_may,
                                   MDSet const* bb_mustdefmds,
                                   MDSet const* bb_maydefmds,
                                   DefMiscBitSetMgr & bsmgr,
                                   OUT SolveSet & output);

    //Compute must and may killed stmt.
    //NOTE: computation of maykill and mustkill both need may-gen-def.
    void computeKillSetByLiveInBB(UINT bbid, bool comp_must,
                                  bool comp_may,
                                  MDSet const* bb_mustdefmds,
                                  MDSet const* bb_maydefmds,
                                  DefMiscBitSetMgr & bsmgr,
                                  OUT SolveSet & output);

    //Compute must and may killed stmt.
    //mustdefs: record must modified MD for each bb.
    //maydefs: record may modified MD for each bb.
    //NOTE: computation of maykill and mustkill both need may-gen-def.
    void computeKillSet(DefDBitSetCoreReserveTab & dbitsetchash,
                        Vector<MDSet*> const* mustexactdefmds,
                        Vector<MDSet*> const* maydefmds,
                        DefMiscBitSetMgr & bsmgr);

    void computeGenExprForMayDef(IR const* ir, OUT SolveSet * gen_ir_expr,
                                 DefMiscBitSetMgr & bsmgr);

    //Compute generated-EXPR for BB.
    void computeGenExprForBB(IRBB * bb, OUT SolveSet & expr_univers,
                             DefMiscBitSetMgr & bsmgr);

    //The function collect the IR expression that being the GenExpr.
    //expr_univers: local set.
    void computeGenExprForStmt(IR const* ir, OUT SolveSet * gen_ir_expr,
                               OUT SolveSet & expr_univers,
                               DefMiscBitSetMgr & bsmgr);
    void computeLiveInBB(DefMiscBitSetMgr & bsmgr);

    //Compute local-gen IR-EXPR set and killed IR-EXPR set.
    //'expr_universe': record the universal of all ir-expr of region.
    void computeAuxSetForExpression(DefDBitSetCoreReserveTab & bshash,
                                    OUT SolveSet * expr_universe,
                                    Vector<MDSet*> const* maydefmds,
                                    DefMiscBitSetMgr & bsmgr);

    //Compute maydef, mustdef, mayuse information for current region.
    void computeRegionMDDU(Vector<MDSet*> const* mustexactdef_mds,
                           Vector<MDSet*> const* maydef_mds,
                           MDSet const* mayuse_mds);

    void computeMustExactDefMayDefMayUseForBB(IRBB * bb, ConstMDIter & mditer,
        OUT Vector<MDSet*> * mustdefmds, OUT Vector<MDSet*> * maydefmds,
        OUT MDSet * mayusemds, MDSet * bb_mustdefmds, MDSet * bb_maydefmds,
        SolveSet * mustgen_stmt, SolveSet * maygen_stmt,
        UFlag flag, DefMiscBitSetMgr & bsmgr);
    void collectNonPRMayDef(IR const* ir, DefMiscBitSetMgr & bsmgr,
                            OUT MDSet * maydefmds) const;

    //This equation needs May Kill Def and Must Gen Def.
    bool ForAvailReachDef(IRBB const* bb, List<IRBB*> * lst,
                          DefMiscBitSetMgr & bsmgr);
    bool ForReachDef(IRBB const* bb, List<IRBB*> * lst,
                     DefMiscBitSetMgr & bsmgr);
    bool ForAvailExpression(IRBB const* bb, List<IRBB*> * lst,
                            DefMiscBitSetMgr & bsmgr);

    Vector<SolveSet*> & getReachDefInVec() { return m_reach_def_in; }
    Vector<SolveSet*> & getReachDefOutVec() { return m_reach_def_out; }
    Vector<SolveSet*> & getAvailReachDefInVec() { return m_avail_reach_def_in; }
    Vector<SolveSet*> & getAvailReachDefOutVec()
    { return m_avail_reach_def_out; }
    Vector<SolveSet*> & getGenIRExprVec() { return m_gen_ir_expr; }
    Vector<SolveSet*> & getAvailExprInVec() { return m_avail_exp_in; }
    Vector<SolveSet*> & getAvailExprOutVec() { return m_avail_exp_out; }
    Vector<SolveSet*> & getMayGenDefVec() { return m_may_gen_def; }
    Vector<SolveSet*> & getMustGenDefVec() { return m_must_gen_def; }
    Vector<SolveSet*> & getMayKilledDefVec() { return m_may_killed_def; }
    Vector<SolveSet*> & getMustKilledDefVec() { return m_must_killed_def; }
    Vector<SolveSet*> & getKilledIRExprVec() { return m_killed_ir_exp; }
    Vector<SolveSet*> & getLiveInBBVec() { return m_livein_bb; }

    //Return true if 'def1' exactly modified md that 'def2' generated.
    //'def1': should be stmt.
    //'def2': should be stmt.
    bool isMustKill(IR const* def1, IR const* def2);

    //Return true if 'def1' may modify md-set that 'def2' generated.
    //'def1': should be stmt.
    //'def2': should be stmt.
    bool isMayKill(IR const* def1, IR const* def2);

    void resetAllKillSet();
    void resetLocalSet();

    void setKilledIRExpr(UINT bbid, SolveSet * set);
    void setMayKilledDef(UINT bbid, SolveSet * set);
    void setMustKilledDef(UINT bbid, SolveSet * set);
    void solveByRPO(RPOVexList const* rpovexlst, UFlag const flag,
                    MOD DefMiscBitSetMgr & bsmgr);
    void solveByWorkList(List<IRBB*> * tbbl, UFlag const flag,
                         MOD DefMiscBitSetMgr & bsmgr);
    //Solve reaching definitions problem for IR STMT and
    //computing LIVE IN and LIVE OUT IR expressions.
    //'expr_univers': the Universal SET for ExpRep.
    void solve(SolveSet const& expr_universe, UFlag const flag,
               MOD DefMiscBitSetMgr & bsmgr);
public:
    SolveSetMgr(Region * rg)
    {
        m_rg = rg;
        m_cfg = rg->getCFG();
        m_bblst = m_cfg->getBBList();
        m_md_sys = rg->getMDSystem();
        setKeepReachDefIn(true);
        //Usually only reach-def-in is useful for computing DU chain.
        setKeepReachDefOut(false);
        setKeepAvailReachDefIn(false);
        setKeepAvailReachDefOut(false);
        setKeepGenIRExpr(false);
        setKeepAvailExprIn(false);
        setKeepAvailExprOut(false);
        setKeepMayGenDef(false);
        setKeepMustGenDef(false);
        setKeepMayKilledDef(false);
        setKeepMustKilledDef(false);
        setKeepKilledIRExpr(false);
        setKeepLiveInBB(false);
    }
    ~SolveSetMgr();
    //Represent reach-def IR stmt information. Each element in the set is IR id.
    DECLARE_SOLVESET_OPERATION(ReachDefIn);
    //Represent reach-def IR stmt information. Each element in the set is IR id.
    DECLARE_SOLVESET_OPERATION(ReachDefOut);
    //Represent available reach-def IR stmt information.
    //Each element in the set is IR id.
    DECLARE_SOLVESET_OPERATION(AvailReachDefIn);
    //Represent available reach-def IR stmt information.
    //Each element in the set is IR id.
    DECLARE_SOLVESET_OPERATION(AvailReachDefOut);
    //Represent generated IR expression. Each element in the set is IR id.
    DECLARE_SOLVESET_OPERATION(GenIRExpr);
    //Represent available IR expression. Each element in the set is IR id.
    DECLARE_SOLVESET_OPERATION(AvailExprIn);
    //Represent available IR expression. Each element in the set is IR id.
    DECLARE_SOLVESET_OPERATION(AvailExprOut);
    DECLARE_SOLVESET_OPERATION(MayGenDef);
    DECLARE_SOLVESET_OPERATION(MustGenDef);
    //Represent must-killed def of stmt.
    DECLARE_SOLVESET_OPERATION(MustKilledDef);
    //Represent may-killed def of stmt.
    DECLARE_SOLVESET_OPERATION(MayKilledDef);
    //Represent killed expression.
    DECLARE_SOLVESET_OPERATION(KilledIRExpr);
    //Represent live-in BB.
    DECLARE_SOLVESET_OPERATION(LiveInBB);

    //Count up the memory has been allocated.
    size_t count_mem() const;

    //Dump mem usage for each internal set of bb.
    void dumpMemUsage() const;
    //is_bs: true to dump bitset info.
    void dump(bool is_dump_bs = false) const;

    Region * getRegion() const { return m_rg; }
    xcom::DefMiscBitSetMgr * getLocalSBSMgr() { return &m_local_sbs_mgr; }
    xcom::DefMiscBitSetMgr * getGlobalSBSMgr() { return &m_global_sbs_mgr; }
 
    void resetReachDefInSet();
    void resetGlobalSet();

    //Return true if region status changed.
    bool perform(MOD OptCtx & oc, UFlag flag);

    //Change the CFG and BBList.
    void setCFG(IRCFG * cfg) { m_cfg = cfg; m_bblst = m_cfg->getBBList(); }
};

} //namespace xoc
#endif
