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
#ifndef _IR_CFG_H_
#define _IR_CFG_H_

namespace xoc {

//Map BB id to IR.
typedef TMap<UINT, IR*> Pred2Opnd;

//Map IR to BB id.
class Opnd2Pred : public TMap<IR*, UINT> {
public:
    //Collect the mapping informatino for Phi list in bb.
    void collect(IRBB * bb, IRCFG * cfg);
};

//NOTICE:
//1. For accelerating perform operation of each vertex, e.g
//   compute dominator, please try best to add vertex with
//   topological order.
class IRCFG : public Pass, public CFG<IRBB, IR> {
    COPY_CONSTRUCTOR(IRCFG);
protected:
    Vector<IRBB*> m_bb_vec; //used to speedup querying of BB through id.
    Lab2BB m_lab2bb;
    TypeMgr * m_tm;
    CFG_SHAPE m_cs;
protected:
    UINT afterReplacePredInCase1(IRBB const* bb, IRBB const* succ,
                                 List<UINT> const& newpreds,
                                 OUT CfgOptCtx & ctx,
                                 UINT orgpos, UINT orgnum,
                                 UINT newpredstartpos);
    UINT afterReplacePredInCase2(IRBB const* bb, IRBB const* succ,
                                 List<UINT> const& newpreds,
                                 OUT CfgOptCtx & ctx,
                                 UINT orgpos, UINT orgnum,
                                 UINT newpredstartpos);

    void dumpGraph(FILE * h) const;
    void dumpForTest(UINT flag) const;

    void initBBVecViaBBList();
    void initEntryAndExit(CFG_SHAPE cs);

    virtual void preprocessBeforeRemoveBB(IRBB * bb, MOD CfgOptCtx & ctx);

    //The function will remove given edge from->to while maintaining the
    //DomInfo if 'ctx' asked.
    void removeEdge(Vertex const* from, Vertex const* to,
                    OUT CfgOptCtx & ctx);
    void removeAllInEdge(IRBB const* bb, OUT CfgOptCtx & ctx);
    void removeAllOutEdge(IRBB const* bb, OUT CfgOptCtx & ctx);
    void remove_bb_impl(C<IRBB*> * bbct);
    virtual void recomputeDomInfo(MOD OptCtx & ctx);

    //Remove trampoline branch.
    //Note the pass is different from what removeTrampolinEdge() does.
    //e.g:L2:
    //    truebr L4 | false L4
    //    goto L3 //redundant jump
    //    L4
    //    st = ...
    //    L3:
    //    ...
    //=>
    //    L2:
    //    falsebr L3 | truebr L3
    //    EMPTY BB
    //    L4:
    //    st = ...
    //    L3:
    bool removeTrampolinBranchForBB(BBListIter & it, OUT CfgOptCtx & ctx);
    bool removeTrampolinBranch(OUT CfgOptCtx & ctx);

    //The function remove all stmt in 'bb'.
    //Note caller should guarrantee stmt is useless and removable.
    void removeAllStmt(IRBB * bb, CfgOptCtx const& ctx);

    //The function remove all MDPhis in 'bb'.
    //Note caller should guarrantee phi is useless and removable.
    void removeAllMDPhi(IRBB * bb, CfgOptCtx const& ctx);
    void removeSuccDesignatedPhiOpnd(IRBB const* succ, UINT pos,
                                     CfgOptCtx const& ctx);

    //Remove bb from LoopInfo tree.
    void removeLoopInfo(IRBB const* bb, CfgOptCtx const& ctx);
    static void removeLoopInfo(IRBB const* bb, MOD LI<IRBB> * li,
                               MOD OptCtx * oc);

    //Sort the order of predecessor of given BB according to PHI operand layout.
    void sortPred(IRBB const* bb, IR * phi, TMap<IR*, LabelInfo*> & ir2label);

    bool useMDSSADU() const;
    bool usePRSSADU() const;
public:
    enum {
        DUMP_DEF = 0x0U, //the default dump option.
        DUMP_DETAIL = 0x1U, //Dump BB details, includes IR, AttachInfo, etc.
        DUMP_EH = 0x2U, //Dump exception handling info.
        DUMP_MDSSA = 0x4U, //Dump MDSSA info.

        //Kind of combination of dump options.
        DUMP_COMBINE = DUMP_DETAIL|DUMP_EH|DUMP_MDSSA,
    };
public:
    IRCFG(CFG_SHAPE cs, BBList * bbl, Region * rg,
          UINT vertex_hash_size = 16);
    IRCFG(IRCFG const& src, BBList * bbl,
          bool clone_edge_info, bool clone_vex_info);
    virtual ~IRCFG() {}

    //Add Label to BB, and establish map between Label and BB.
    void addLabel(IRBB * src, LabelInfo const* li)
    {
        src->addLabel(li);

        //Set label->bb map.
        m_lab2bb.setAlways(li, src);
    }

    //Note if dominfo is not maintained, SSA update can not prove to be
    //correct.
    //ctx: CFG optimizer will perform operations follow the 'ctx' mandate.
    xcom::Edge * addEdge(IRBB * from, IRBB * to, OUT CfgOptCtx & ctx);

    //Add new IRBB into CFG, but the BB list should be modified
    //out of this function.
    //Namely, you should use 'insertBBBetween()' to insert BB into list.
    //And you must consider the right insertion.
    void addBB(IRBB * bb)
    {
        ASSERT0(bb && m_bb_vec.get(bb->id()) == nullptr);
        ASSERTN(bb->id() != 0, ("bb id should start at 1"));
        m_bb_vec.set(bb->id(), bb);
        Vertex * v = addVertex(bb->id());
        setVertex(bb, v);
    }

    //After adding new bb or change bb successor,
    //you need add the related PHI operand if BB successor has PHI stmt.
    void addSuccessorDesignatedPhiOpnd(IRBB * bb, IRBB * succ);
    void addDomInfoToFallThroughBB(IRBB const* marker, IRBB const* newbb,
                                   IRBB const* oldnext);

    //Construct EH edge after cfg built.
    //This function use a conservative method, and this method
    //may generate lots of redundant exception edges.
    void buildEHEdgeNaive();

    //Construct EH edge after cfg built.
    void buildEHEdge();

    //Build CFG according to IRBB list.
    void build(OptCtx & oc);

    //Record the Exit BB here.
    virtual void computeExitList();
    virtual void cf_opt();
    void cloneLab2BB(Lab2BB const& src);
    void clone(IRCFG const& src, bool clone_edge_info, bool clone_vex_info);
    void computeDomAndIdom(MOD OptCtx & oc, BitSet const* uni = nullptr);
    void computePdomAndIpdom(MOD OptCtx & oc, BitSet const* uni = nullptr);
    void computeRPO(OptCtx & oc);

    //The function insert a tampolining BB bewteen prev and bb, that will
    //make prev no longer fallthrough to bb.
    //prev: the previous of 'next' BB, note prev must fallthrough to 'next'.
    //next: the next BB in BBList.
    IRBB * changeFallthroughBBToJumpBB(IRBB * prev, MOD IRBB * next,
                                       OptCtx * oc);

    //The function insert a tampolining BB bewteen bb and its next BB.
    IRBB * changeFallthroughBBToJumpBB(IRBB * bb, OptCtx * oc);

    void dumpVCG(CHAR const* name = nullptr, UINT flag = DUMP_COMBINE) const;
    void dumpDOT(CHAR const* name = nullptr, UINT flag = DUMP_COMBINE) const;
    void dumpDOT(FILE * h, UINT flag) const;
    void dumpDom() const { CFG<IRBB, IR>::dumpDom(m_rg); }
    void dumpDomTree() const { CFG<IRBB, IR>::dumpDomTree(m_rg); }
    virtual bool dump() const;

    void erase();

    virtual void findTargetBBOfMulticondBranch(IR const*, OUT List<IRBB*>&);
    virtual IRBB * findBBbyLabel(LabelInfo const* lab) const;
    virtual void findTargetBBOfIndirectBranch(IR const*, OUT List<IRBB*>&);
    void findEHRegion(IRBB const* catch_start,
                      xcom::BitSet const& mainstreambbs,
                      OUT xcom::BitSet & ehbbs);
    void findTryRegion(IRBB const* try_start, OUT xcom::BitSet & ehbbs);
    void findAllTryRegions(OUT xcom::BitSet & trybbs);

    //Allocate and initialize control flow graph.
    void initCFG(OptCtx & oc);
    virtual bool if_opt(IRBB * bb);
    virtual bool isRegionEntry(IRBB * bb) const { return BB_is_entry(bb); }
    virtual bool isRegionExit(IRBB * bb) const { return BB_is_exit(bb); }

    //Insert BB before bb.
    //e.g:BB1 BB2 BB3
    //      \  |  /
    //        BB4
    //  after inserting newbb,
    //    BB1 BB2 BB3
    //      \  |  /
    //        newbb
    //         |
    //        BB4
    void insertBBBefore(IN IRBB * bb, IN IRBB * newbb);

    //The function insert newbb after 'marker'.
    //Return the newbb.
    IRBB * insertFallThroughBBAfter(IRBB const* marker, MOD OptCtx * oc);

    //The function insert fallthrough newbb after 'marker'.
    void insertFallThroughBBAfter(IRBB const* marker, IRBB * newbb,
                                  MOD OptCtx * oc);

    //The function insert newbb bewteen 'from' and 'to'. As a result, the
    //function may break up fallthrough edge of 'to' if necessary.
    //Return trampoline BB if the function inserted it before 'to'.
    IRBB * insertBBBetween(IN IRBB const* from, IN BBListIter from_it,
                           MOD IRBB * to, IN BBListIter to_it,
                           IN IRBB * newbb, MOD OptCtx * oc);

    //The function is a simpler version to insert a new BB. It does not check
    //and insert newbb into CFG's BBList. The function is usually used when
    //caller expect to update the related Dom and SSA info exactly by themself.
    //Insert 'newbb' between 'from' and 'to'.
    //e.g: given edge from->to, the result is from->newbb->to.
    void insertBBBetween(IRBB const* from, IRBB * to, IRBB * newbb,
                         OUT CfgOptCtx & ctx);

    CFG_SHAPE getCfgShape() const { return m_cs; }

    //Return the first operation of 'bb'.
    IR * get_first_xr(IRBB * bb)
    {
        ASSERT0(bb && m_bb_vec.get(bb->id()));
        return BB_first_ir(bb);
    }

    //Return the last operation of 'bb'.
    IR * get_last_xr(IRBB * bb)
    {
        ASSERT0(bb && m_bb_vec.get(bb->id()));
        return BB_last_ir(bb);
    }
    UINT getNumOfBB() const
    { return const_cast<IRCFG*>(this)->getBBList()->get_elem_count(); }
    BBList * getBBList() const { return (BBList*)m_bb_list; }
    Lab2BB * getLabel2BBMap() { return &m_lab2bb; }
    IRBB * getBB(UINT id) const { return m_bb_vec.get(id); }
    virtual bool goto_opt(IRBB * bb);
    virtual CHAR const* getPassName() const { return "CFG"; }
    virtual PASS_TYPE getPassType() const { return PASS_CFG; }

    //Find natural loop and scan loop body to find call and early exit, etc.
    void LoopAnalysis(OUT OptCtx & oc);

    //Compute which predecessor is pred to bb.
    //e.g: If pred is the first predecessor, return 0.
    //pred: BB id of predecessor.
    //Note 'pred' must be one of predecessors of 'bb'.
    UINT WhichPred(IRBB const* pred, IRBB const* bb) const
    { return Graph::WhichPred(pred->id(), bb->getVex()); }

    //Note maintain DomInfo may be costly.
    virtual void removeDomInfo(C<IRBB*> * bbct, MOD CfgOptCtx & ctx);

    //You should clean the relation between Label and BB before removing BB.
    virtual void removeBB(C<IRBB*> * bbct, OUT CfgOptCtx & ctx);

    //We only remove 'bb' from CFG, vector and bb-list.
    //You should clean the relation between Label and BB before removing BB.
    virtual void removeBB(IRBB * bb, OUT CfgOptCtx & ctx)
    {
        ASSERT0(bb);
        C<IRBB*> * bbct;
        m_bb_list->find(bb, &bbct);
        removeBB(bbct, ctx);
    }
    virtual void remove_xr(IRBB * bb, IR * ir, CfgOptCtx const& ctx);

    //The function will remove given edge from->to while maintaining the
    //DomInfo if 'ctx' asked.
    //Note if dominfo is not maintained, SSA update can not prove to be
    //correct.
    //ctx: CFG optimizer will perform default updation
    //and transform, otherwise perform operations follow the 'ctx' mandate.
    void removeEdge(IRBB * from, IRBB * to, OUT CfgOptCtx & ctx);

    //The function remove labels that no one referenced.
    bool removeRedundantLabel();

    //Remove trampoline edge.
    //e.g:BB1:
    //    goto BB3;
    //    BB2:
    //    some-code1;
    //    BB3:
    //    some-code2;
    //  where BB1->BB2->BB3 are fallthrough path.
    //stmt of BB2 is just 'goto bb3', then BB1->BB2 is tramp-edge.
    //And the resulting edges are BB1->BB3, BB2->BB3 respectively.
    //Return true if at least one tramp-edge removed.
    bool removeTrampolinEdge(OUT CfgOptCtx & ctx);

    //Remove trampoline BB.
    //e.g: bb1->bb2->bb3
    //    stmt of bb2 is just 'goto bb3', and bb3 is the NEXT BB
    //    of bb2 in BBList.
    //    Then bb2 is tramp BB.
    //Return true if at least one tramp BB removed.
    //
    //ALGO:
    //    for each pred of BB
    //        if (pred is fallthrough && prev of BB == pred)
    //            remove edge pred->BB.
    //            add edge pred->BB's next.
    //            continue;
    //        end if
    //        duplicate LabelInfo from BB to BB's next.
    //        revise LabelInfo of pred to new target BB.
    //        remove edge pred->BB.
    //        add edge pred->BB's next.
    //    end for
    bool removeTrampolinBB(OUT CfgOptCtx & ctx);
    bool removeRedundantBranch(OUT CfgOptCtx & ctx);

    //Rebuild CFG.
    //Note if CFG rebuild, SSAInfo and MDSSAInfo should be recomputed.
    void rebuild(OptCtx & oc);

    //Cut off the mapping relation between Labels and BB.
    virtual void removeMapBetweenLabelAndBB(IRBB * bb);

    //Remove related PHI operand from successor BB.
    //Before removing current BB or change BB's successor,
    //you need remove the related PHI operand if BB successor has PHI.
    //Note if dominfo is not maintained, SSA update can not prove to be
    //correct.
    //ctx: if it is nullptr, CFG optimizer will perform default updation
    //and transform, otherwise perform operations follow the 'ctx' mandate.
    void removeSuccPhiOpnd(IRBB const* bb, CfgOptCtx const& ctx);

    //Construct CFG edge for BB has phi.
    void reorderPhiEdge(TMap<IR*, LabelInfo*> & ir2label);

    //The function replaces original predecessor with a list of
    //new predecessors.
    //bb: the predecessor will be replaced.
    //succ: the target BB.
    //newpreds: list of new predecessors.
    //Return the position of 'bb' that is in the predecessor list of 'succ'.
    virtual UINT replacePredWith(IRBB const* bb, IRBB const* succ,
                                 List<UINT> const& newpreds,
                                 OUT CfgOptCtx & ctx);

    //Fix out-edges if BB becomes fallthrough BB.
    //The function will remove out-edges of bb except the fallthrough edge, and
    //try adding fallthrough edge if it doesn't exist.
    //Note it is illegal if empty BB has non-taken branch.
    void reviseOutEdgeForFallthroughBB(IRBB * bb, BBListIter const& bbit,
                                       OUT CfgOptCtx & ctx);
    void reviseStmtMDSSA(VexTab const& vextab, Vertex const* root);

    virtual void setRPO(IRBB * bb, INT order) { BB_rpo(bb) = order; }
    virtual void setVertex(IRBB * bb, Vertex * v) { BB_vex(bb) = v; }
    virtual void setVertex(IRBB * from, IRBB * to, Edge * e)
    { BB_vex(from) = e->from(); BB_vex(to) = e->to(); }

    //Split BB into two BBs.
    //bb: BB to be splited.
    //split_point: the ir in 'bb' used to mark the split point that followed IRs
    //             will be moved to fallthrough newbb.
    //e.g:bb:
    //    ...
    //    split_point;
    //    ...
    //  =>
    //    bb:
    //    ...
    //    split_point; //the last ir in bb.
    //    newbb:
    //    ...
    IRBB * splitBB(IRBB * bb, IRListIter split_point, OptCtx & oc);

    //Some optimizations may append stmt into BB which has already down-boundary
    //stmt. That makes BB invalid. Split such invalid BB into two or more BBs.
    //The function will try to maintain RPO and DOM info.
    bool splitBBIfNeeded(IRBB * bb, OptCtx & oc);

    //Sort PHI operand in order of predecessors of BB of PHI.
    //pred2opnd: record a map between predecessor to operand.
    void sortPhiOpnd(IR * phi, Pred2Opnd & pred2opnd);

    //Try to update RPO of newbb accroding to RPO of marker.
    //newbb_prior_marker: true if newbb's lexicographical order is prior to
    //marker.
    //Return true if the function find a properly RPO for 'newbb', otherwise
    //return false.
    bool tryUpdateRPO(IRBB * newbb, IRBB const* marker,
                      bool newbb_prior_marker);
    bool tryUpdateRPOBeforeCFGChanged(IRBB * newbb, IRBB const* marker,
                                      bool newbb_prior_marker,
                                      MOD OptCtx * oc);

    //Move all Labels which attached on src BB to tgt BB.
    virtual void moveLabels(IRBB * src, IRBB * tgt);

    virtual bool perform(OptCtx & oc) { build(oc); return false; }

    //Perform miscellaneous control flow optimizations.
    //Return true if CFG changed.
    //Include remove dead bb which is unreachable, remove empty bb as many
    //as possible, simplify and remove the branch like "if (x==x)", remove
    //the trampolin branch.
    //Note these optimizations performed in the function should NOT use DOM,
    //PDOM, LOOPINFO, or RPO information.
    bool performMiscOpt(OptCtx & oc)
    {
        CfgOptCtx ctx(oc);
        return performMiscOpt(ctx);
    }
    bool performMiscOpt(MOD CfgOptCtx & ctx);

    //The function verify whether the branch target is match to the BB.
    bool verifyBranchTarget() const;

    //The function verify the relationship between BB and LabelInfo.
    bool verifyLabel2BB() const;

    //Verification at building SSA mode by ir parser.
    bool verifyPhiEdge(IR * phi, TMap<IR*, LabelInfo*> & ir2label) const;
    bool verifyDomAndPdom(OptCtx const& oc) const;
    bool verifyRPO(OptCtx const& oc) const;
    bool verifyLoopInfo(OptCtx const& oc) const;
    bool verify() const;
};

} //namespace xoc
#endif
