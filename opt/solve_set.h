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
    { m_sbs_mgr->freeDBitSetCore((DefDBitSetCore*)set); }

    DefMiscBitSetMgr * getBsMgr() const { return m_sbs_mgr; }
};


#define HASH_DBITSETCORE

class DefDBitSetCoreReserveTab : public
    SBitSetCoreHash<DefDBitSetCoreHashAllocator> {
    COPY_CONSTRUCTOR(DefDBitSetCoreReserveTab);
    #ifdef HASH_DBITSETCORE
    #else
    List<DefDBitSetCore*> m_allocated;
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
        //xcom::C<DefDBitSetCore*> * ct;
        //for (m_allocated.get_head(&ct);
        //     ct != m_allocated.end(); ct = m_allocated.get_next(ct)) {
        //    DefDBitSetCore * s = ct->val();
        //    ASSERT0(s);
        //    get_allocator()->free(s);
        //}
        #endif
    }

    DefDBitSetCore * append(DefDBitSetCore const& set)
    {
        #ifdef HASH_DBITSETCORE
        return (DefDBitSetCore*)SBitSetCoreHash
               <DefDBitSetCoreHashAllocator>::append(set);
        #else
        DefDBitSetCore * s = (DefDBitSetCore*)m_allocator->alloc();
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
        xcom::C<DefDBitSetCore*> * ct;
        for (m_allocated.get_head(&ct);
             ct != m_allocated.end(); ct = m_allocated.get_next(ct)) {
            DefDBitSetCore * s = ct->val();
            ASSERT0(s);
            fprintf(h, "\n");
            s->dump(h);
        }
        fflush(h);
        #endif
    }
};

class SolveSet {
    COPY_CONSTRUCTOR(SolveSet);
    Region * m_rg;
    DUMgr * m_du;
    IRCFG * m_cfg;
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
    Vector<DefDBitSetCore*> m_avail_in_reach_def;
    //avail reach-out def of STMT
    Vector<DefDBitSetCore*> m_avail_out_reach_def;
    Vector<DefDBitSetCore*> m_in_reach_def; //reach-in def of STMT
    Vector<DefDBitSetCore*> m_out_reach_def; //reach-out def of STMT
    Vector<DefDBitSetCore*> m_may_gen_def; //generated-def of STMT
    Vector<DefDBitSetCore*> m_must_gen_def; //generated-def of STMT
    //must-killed def of STMT
    Vector<DefDBitSetCore*> m_must_killed_def;
    Vector<DefDBitSetCore*> m_may_killed_def; //may-killed def of STMT
    Vector<DefSBitSetCore*> m_livein_bb; //live-in BB
    BSVec<DefDBitSetCore*> m_gen_ir_exp; //generate EXPR
    BSVec<DefDBitSetCore*> m_killed_ir_exp; //killed EXPR
    BSVec<DefDBitSetCore*> m_avail_in_exp; //available-in EXPR
    BSVec<DefDBitSetCore*> m_avail_out_exp; //available-out EXPR
private:
    //NOTE: MD referrence must be available.
    //mustdefs: record must modified MD for each bb.
    //maydefs: record may modified MD for each bb.
    //mayuse: record may used MD for each bb.
    //        collect mayuse (NOPR-DU) to compute Region referred MD.
    void computeMustExactDefMayDefMayUse(OUT Vector<MDSet*> * mustdefmds,
                                         OUT Vector<MDSet*> * maydefmds,
                                         OUT MDSet * mayusemds,
                                         UINT flag);
    void computeMustExactDef(IR const* ir, OUT MDSet * bb_mustdefmds,
                             DefDBitSetCore * mustgen_stmt,
                             ConstMDIter & mditer, DefMiscBitSetMgr & bsmgr,
                             UINT flag);
    void computeMayDef(IR const* ir, MDSet * bb_maydefmds,
                       DefDBitSetCore * maygen_stmt,
                       DefMiscBitSetMgr & bsmgr, UINT flag);

    //Compute must and may killed stmt.
    //mustdefs: record must modified MD for each bb.
    //maydefs: record may modified MD for each bb.
    //NOTE: computation of maykill and mustkill both need may-gen-def.
    void computeKillSet(DefDBitSetCoreReserveTab & dbitsetchash,
                        Vector<MDSet*> const* mustexactdefmds,
                        Vector<MDSet*> const* maydefmds,
                        DefMiscBitSetMgr & bsmgr);
    //Compute generated-EXPR for BB.
    void computeGenForBB(IRBB * bb, OUT DefDBitSetCore & expr_univers,
                         DefMiscBitSetMgr & bsmgr);
    void computeLiveInBB(DefMiscBitSetMgr & bsmgr);

    //Compute local-gen IR-EXPR set and killed IR-EXPR set.
    //'expr_universe': record the universal of all ir-expr of region.
    void computeAuxSetForExpression(DefDBitSetCoreReserveTab & bshash,
                                    OUT DefDBitSetCore * expr_universe,
                                    Vector<MDSet*> const* maydefmds,
                                    DefMiscBitSetMgr & bsmgr);

    //Compute maydef, mustdef, mayuse information for current region.
    void computeRegionMDDU(Vector<MDSet*> const* mustexactdef_mds,
                           Vector<MDSet*> const* maydef_mds,
                           MDSet const* mayuse_mds);

    void computeMustExactDefMayDefMayUseForBB(IRBB * bb, ConstMDIter & mditer,
        OUT Vector<MDSet*> * mustdefmds, OUT Vector<MDSet*> * maydefmds,
        OUT MDSet * mayusemds, MDSet * bb_mustdefmds, MDSet * bb_maydefmds,
        DefDBitSetCore * mustgen_stmt, DefDBitSetCore * maygen_stmt,
        UINT flag, DefMiscBitSetMgr & bsmgr);
    void collectNonPRMayDef(IR const* ir, DefMiscBitSetMgr & bsmgr,
                            OUT MDSet * maydefmds) const;

    //This equation needs May Kill Def and Must Gen Def.
    bool ForAvailReachDef(UINT bbid, List<IRBB*> & preds,
                          List<IRBB*> * lst,
                          DefMiscBitSetMgr & bsmgr);
    bool ForReachDef(UINT bbid, List<IRBB*> & preds, List<IRBB*> * lst,
                     DefMiscBitSetMgr & bsmgr);
    bool ForAvailExpression(UINT bbid, List<IRBB*> & preds,
                            List<IRBB*> * lst, DefMiscBitSetMgr & bsmgr);

    DefDBitSetCore * getOutReachDef(UINT bbid);
    DefDBitSetCore * getMustGenDef(UINT bbid);
    DefDBitSetCore * getAvailOutReachDef(UINT bbid);
    DefSBitSetCore * getLiveInBB(UINT bbid);
    DefDBitSetCore * getGenIRExpr(UINT bbid);
    //Return liveout set for IR expression. Each element in the set is IR id.
    DefDBitSetCore * getAvailOutExpr(UINT bbid);
    DefDBitSetCore const* getMustKilledDef(UINT bbid) const;
    DefDBitSetCore const* getMayKilledDef(UINT bbid) const;
    DefDBitSetCore const* getKilledIRExpr(UINT bbid) const;

    DefDBitSetCore * genMayGenDef(UINT bbid);
    DefDBitSetCore * genMustGenDef(UINT bbid);
    DefDBitSetCore * genAvailInReachDef(UINT bbid);
    DefDBitSetCore * genAvailOutReachDef(UINT bbid);
    DefDBitSetCore * genGenIRExpr(UINT bbid);
    DefDBitSetCore * genAvailInExpr(UINT bbid);
    DefDBitSetCore * genAvailOutExpr(UINT bbid);
    DefDBitSetCore * genInReachDef(UINT bbid);
    DefDBitSetCore * genOutReachDef(UINT bbid);
    DefSBitSetCore * genLiveInBB(UINT bbid);

    //Return true if 'def1' exactly modified md that 'def2' generated.
    //'def1': should be stmt.
    //'def2': should be stmt.
    bool isMustKill(IR const* def1, IR const* def2);

    //Return true if 'def1' may modify md-set that 'def2' generated.
    //'def1': should be stmt.
    //'def2': should be stmt.
    bool isMayKill(IR const* def1, IR const* def2);

    void resetKillSet();
    void resetLocalSet();
    void resetLiveInBB();

    void setKilledIRExpr(UINT bbid, DefDBitSetCore * set);
    void setMayKilledDef(UINT bbid, DefDBitSetCore * set);
    void setMustKilledDef(UINT bbid, DefDBitSetCore * set);
    void solveByRPO(List<IRBB*> * tbbl, UINT const flag,
                    MOD DefMiscBitSetMgr & bsmgr);
    void solveByWorkList(List<IRBB*> * tbbl, UINT const flag,
                         MOD DefMiscBitSetMgr & bsmgr);
    //Solve reaching definitions problem for IR STMT and
    //computing LIVE IN and LIVE OUT IR expressions.
    //'expr_univers': the Universal SET for ExpRep.
    void solve(DefDBitSetCore const& expr_universe, UINT const flag,
               MOD DefMiscBitSetMgr & bsmgr);
public:
    SolveSet(Region * rg)
    {
        m_rg = rg;
        m_du = rg->getDUMgr();
        m_cfg = rg->getCFG();
        m_md_sys = rg->getMDSystem();
    }
    ~SolveSet();

    //Count up the memory has been allocated.
    size_t count_mem() const;

    //Dump mem usage for each internal set of bb.
    void dumpMemUsage() const;
    //is_bs: true to dump bitset info.
    void dump(bool is_bs = false) const;

    //Return livein set for IR expression. Each element in the set is IR id.
    DefDBitSetCore * getAvailInExpr(UINT bbid);
    DefDBitSetCore * getInReachDef(UINT bbid);
    DefDBitSetCore * getAvailInReachDef(UINT bbid);
    Region * getRegion() const { return m_rg; }
    xcom::DefMiscBitSetMgr * getLocalSBSMgr() { return &m_local_sbs_mgr; }
    xcom::DefMiscBitSetMgr * getGlobalSBSMgr() { return &m_global_sbs_mgr; }
 
    void resetReachDefInSet();
    void resetGlobalSet();

    //Return true if region status changed.
    bool perform(MOD OptCtx & oc, UINT flag);
};

} //namespace xoc
#endif
