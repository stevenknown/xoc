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

//NOTICE:
//1. For accelerating perform operation of each vertex, e.g
//   compute dominator, please try best to add vertex with
//   topological order.
class IRCFG : public Pass, public CFG<IRBB, IR> {
    COPY_CONSTRUCTOR(IRCFG);
protected:
    Vector<IRBB*> m_bb_vec;
    Lab2BB m_lab2bb;
    TypeMgr * m_tm;
    CFG_SHAPE m_cs;

protected:
    void remove_bb_impl(C<IRBB*> * bbct);
    //CASE: Given pred1->bb, fallthrough edge,
    //  and pred2->bb, jumping edge.
    //  bb:
    //      goto L1
    //  next of bb:
    //      L1:
    //      ...
    //      ...
    //Remove bb and revise CFG.
    //ct: container in m_bb_list of CFG.
    //    It will be updated if related BB removed.
    bool removeTrampolinBBCase1(BBListIter * ct, CfgOptCtx const& ctx);
    bool removeTrampolinEdgeCase1(BBListIter bbct);
    bool removeTrampolinEdgeCase2(BBListIter bbct);

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
    bool removeTrampolinBranchForBB(BBListIter & it, CfgOptCtx const& ctx);
    bool removeTrampolinBranch(CfgOptCtx const& ctx);

    //The function remove all MDPhis in 'bb'.
    //Note caller should guarrantee phi is useless and removable.
    void removeAllMDPhi(IRBB * bb);
    void removeSuccDesignatedPhiOpnd(IRBB const* succ, UINT pos);

    //Sort the order of predecessor of given BB according to PHI operand layout.
    void sortPred(IRBB const* bb, IR * phi, TMap<IR*, LabelInfo*> & ir2label);
public:
    enum {
        DUMP_DEF = 0x0U, //the default dump option.
        DUMP_DETAIL = 0x1U, //Dump BB details, includes IR, AttachInfo, etc.
        DUMP_EH = 0x2U, //Dump exception handling info.
        DUMP_MDSSA = 0x4U, //Dump MDSSA info.

        //Kind of combination of dump options.
        DUMP_COMBINE = DUMP_DETAIL|DUMP_EH|DUMP_MDSSA,
    };

    IRCFG(CFG_SHAPE cs, BBList * bbl, Region * rg,
          UINT edge_hash_size = 16, UINT vertex_hash_size = 16);
    virtual ~IRCFG() {}

    //Add Label to BB, and establish map between Label and BB.
    void addLabel(IRBB * src, LabelInfo const* li)
    {
        src->addLabel(li);

        //Set label->bb map.
        m_lab2bb.setAlways(li, src);
    }
    virtual xcom::Edge * addEdge(IRBB * from, IRBB * to);
    xcom::Edge * addEdge(UINT from, UINT to)
    { return DGraph::addEdge(from, to); }

    //Add new IRBB into CFG, but the BB list should be modified
    //out of this function.
    //Namely, you should use 'insertBBbetween()' to insert BB into list.
    //And you must consider the right insertion.
    void addBB(IRBB * bb)
    {
        ASSERT0(bb && m_bb_vec.get(bb->id()) == nullptr);
        ASSERTN(bb->id() != 0, ("bb id should start at 1"));
        m_bb_vec.set(bb->id(), bb);
        addVertex(bb->id());
    }

    //After adding new bb or change bb successor,
    //you need add the related PHI operand if BB successor has PHI stmt.
    void addSuccessorDesignatedPhiOpnd(IRBB * bb, IRBB * succ);

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
    void computeDomAndIdom(MOD OptCtx & oc, BitSet const* uni = nullptr);
    void computePdomAndIpdom(MOD OptCtx & oc, BitSet const* uni = nullptr);
    void computeRPO(OptCtx & oc);

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
    void initCfg(OptCtx & oc);
    void initEntryAndExit(CFG_SHAPE cs);
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
    void insertBBbefore(IN IRBB * bb, IN IRBB * newbb);

    //The function insert newbb bewteen 'from' and 'to'. As a result, the
    //function may break up fallthrough edge of 'to' if necessary.
    void insertBBbetween(IN IRBB * from, IN BBListIter from_it,
                         IN IRBB * to, IN BBListIter to_it,
                         IN IRBB * newbb);
    //The function insert a tampolining BB bewteen prev and bb, that will
    //make prev no longer fallthrough to bb.
    //prev: the previous BB of bb, note prev must fallthrough to bb.
    IRBB * insertTrampolinBB(IRBB * prev, MOD IRBB * bb, BBListIter const bbit,
                             MOD BBList * bblst);
    bool isRPOValid() const;

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
    UINT getNumOfBB() const { return getVertexNum(); }
    UINT getNumOfSucc(IRBB const* bb) const
    { return getVertex(bb->id())->getOutDegree(); }
    UINT getNumOfPred(IRBB const* bb) const
    { return getVertex(bb->id())->getInDegree(); }
    BBList * getBBList() { return m_bb_list; }
    Lab2BB * getLabel2BBMap() { return &m_lab2bb; }
    IRBB * getBB(UINT id) const { return m_bb_vec.get(id); }
    virtual bool goto_opt(IRBB * bb);
    virtual CHAR const* getPassName() const { return "CFG"; }
    virtual PASS_TYPE getPassType() const { return PASS_CFG; }

    //Find natural loop and scan loop body to find call and early exit, etc.
    void LoopAnalysis(OptCtx & oc);

    //Compute which predecessor is pred to bb.
    //e.g: If pred is the first predecessor, return 0.
    //pred: BB id of predecessor.
    //Note 'pred' must be one of predecessors of 'bb'.
    UINT WhichPred(IRBB const* pred, IRBB const* bb) const
    {
        xcom::Vertex * bb_vex = getVertex(bb->id());
        ASSERT0(bb_vex);
        return Graph::WhichPred(pred->id(), bb_vex);
    }

    virtual void removeDomInfo(C<IRBB*> * bbct, CfgOptCtx const& ctx);

    //You should clean the relation between Label and BB before removing BB.
    virtual void removeBB(C<IRBB*> * bbct, CfgOptCtx const& ctx);

    //We only remove 'bb' from CFG, vector and bb-list.
    //You should clean the relation between Label and BB before removing BB.
    virtual void removeBB(IRBB * bb, CfgOptCtx const& ctx)
    {
        ASSERT0(bb);
        C<IRBB*> * bbct;
        m_bb_list->find(bb, &bbct);
        removeBB(bbct, ctx);
    }
    void remove_xr(IRBB * bb, IR * ir);
    virtual void removeEdge(IRBB * from, IRBB * to);
    //The function remove labels that no one referenced.
    bool removeRedundantLabel();
    bool removeTrampolinEdge();
    bool removeTrampolinBB(CfgOptCtx const& ctx);
    bool removeRedundantBranch();

    //Before removing bb or change bb successor,
    //you need remove the related PHI operand if BB successor has PHI stmt.
    void removeRelatedSuccBBPhiOpnd(IRBB * bb);
    void rebuild(OptCtx & oc);

    //Cut off the mapping relation between Labels and BB.
    virtual void resetMapBetweenLabelAndBB(IRBB * bb);

    //Remove related PHI operand from successor BB.
    //Before removing current BB or change BB's successor,
    //you need remove the related PHI operand if BB successor has PHI.
    virtual void removeSuccPhiOpnd(IRBB const* bb);

    //Construct CFG edge for BB has phi.
    void reorderPhiEdge(TMap<IR*, LabelInfo*> & ir2label);

    //The function replaces original predecessor with a list of
    //new predecessors.
    //Return the position of 'from' that is in the predecessor list of 'to'.
    virtual UINT replacePred(IRBB const* bb, IRBB const* succ,
                             List<UINT> const& newpreds);

    //Fix out-edges if BB becomes fallthrough BB.
    //The function will remove out-edges of bb except the fallthrough edge, and
    //try adding fallthrough edge if it doesn't exist.
    //Note it is illegal if empty BB has non-taken branch.
    void reviseOutEdgeForFallthroughBB(IRBB * bb, BBListIter & bbit);

     virtual void setRPO(IRBB * bb, INT order) { BB_rpo(bb) = order; }
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

    //Some optimization may append stmt into BB which has down-boundary stmt.
    //That makes BB invalid. Split such invalid BB into two or more BBs.
    //The function will try to maintain RPO and DOM info.
    bool splitBBIfNeeded(IRBB * bb, OptCtx & oc);

    //Try to update RPO of newbb accroding to RPO of marker.
    //newbb_prior_marker: true if newbb's lexicographical order is prior to
    //marker.
    //Return true if the function find a properly RPO for 'newbb', otherwise
    //return false.
    bool tryUpdateRPO(IRBB * newbb, IRBB const* marker,
                      bool newbb_prior_marker);

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
        CFGOPTCTX_update_dominfo(&ctx) = true;
        return performMiscOpt(ctx);
    }
    bool performMiscOpt(CfgOptCtx const& ctx);

    //The function verify whether the branch target is match to the BB.
    bool verifyBranchTarget() const;
    //The function verify the relationship between BB and LabelInfo.
    bool verifyLabel2BB() const;
    //Verification at building SSA mode by ir parser.
    bool verifyPhiEdge(IR * phi, TMap<IR*, LabelInfo*> & ir2label) const;
    bool verifyDomAndPdom(OptCtx const& oc) const;
    bool verifyRPO(OptCtx const& oc) const;
    bool verify() const;
};

} //namespace xoc
#endif
