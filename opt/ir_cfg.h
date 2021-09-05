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

typedef TMap<LabelInfo const*, IRBB*> Lab2BB;

//NOTICE:
//1. For accelerating perform operation of each vertex, e.g
//   compute dominator, please try best to add vertex with
//   topological order.
class IRCFG : public Pass, public CFG<IRBB, IR> {
    COPY_CONSTRUCTOR(IRCFG);
protected:
    Vector<IRBB*> m_bb_vec;
    Lab2BB m_lab2bb;
    Region * m_rg;
    TypeMgr * m_tm;
    CFG_SHAPE m_cs;

protected:
    void dump_node(bool detail, bool dump_mdssa);
    void dump_head(FILE * h);
    void dump_edge(bool dump_eh);

    void remove_bb_impl(IRBB * bb);
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
    bool removeTrampolinBBCase1(BBListIter * ct);
    bool removeTrampolinEdgeCase1(BBListIter bbct);
    bool removeTrampolinEdgeCase2(BBListIter bbct);

public:
    enum {
        DUMP_DEF = 0x0, //the default dump option.
        DUMP_DETAIL = 0x1, //Dump BB details, includes IR, AttachInfo, etc.
        DUMP_EH = 0x2, //Dump exception handling info.
        DUMP_MDSSA = 0x4, //Dump MDSSA info.

        //Kind of combination of dump options.
        DUMP_COMBINE = DUMP_DETAIL|DUMP_EH|DUMP_MDSSA,
    };

    IRCFG(CFG_SHAPE cs, BBList * bbl, Region * rg,
          UINT edge_hash_size = 16, UINT vertex_hash_size = 16);
    virtual ~IRCFG() {}

    //Add LABEL to bb, and establish map between label and bb.
    void addLabel(IRBB * src, LabelInfo const* li)
    {
        src->addLabel(li);

        //Set label->bb map.
        m_lab2bb.setAlways(li, src);
    }
    virtual xcom::Edge * addEdge(IRBB * from, IRBB * to)
    {
        xcom::Edge * e = DGraph::addEdge(from->id(), to->id());
        from->addSuccessorDesignatePhiOpnd(this, to);
        return e;
    }
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

    //Construct EH edge after cfg built.
    //This function use a conservative method, and this method
    //may generate lots of redundant exception edges.
    void buildEHEdgeNaive();

    //Construct EH edge after cfg built.
    void buildEHEdge();

    //Build CFG according to IRBB list.
    void build(OptCtx & oc);

    virtual void cf_opt();
    void computeDomAndIdom(MOD OptCtx & oc, BitSet const* uni = nullptr);
    void computePdomAndIpdom(MOD OptCtx & oc, BitSet const* uni = nullptr);

    //Record the Exit BB here.
    virtual void computeExitList()
    {
        //Clean the Exit flag.
        BBListIter ct = nullptr;
        for (m_exit_list.get_head(&ct);
             ct != m_exit_list.end();
             ct = m_exit_list.get_next(ct)) {
            IRBB * bb = ct->val();
            ASSERT0(bb);
            BB_is_exit(bb) = false;
        }

        CFG<IRBB, IR>::computeExitList();

        //Record the Exit flag as BB attribute to speed up accessing.
        for (m_exit_list.get_head(&ct);
             ct != m_exit_list.end();
             ct = m_exit_list.get_next(ct)) {
            IRBB * bb = ct->val();
            ASSERT0(bb);
            BB_is_exit(bb) = true;
        }
    }

    void dumpVCG(CHAR const* name = nullptr, UINT flag = DUMP_COMBINE);
    void dumpDOT(CHAR const* name = nullptr, UINT flag = DUMP_COMBINE);
    void dumpDOT(FILE * h, UINT flag);

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
    bool isRegionEntry(IRBB * bb) { return BB_is_entry(bb); }
    bool isRegionExit(IRBB * bb) { return BB_is_exit(bb); }

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

    //Return the inserted trampolining BB if exist.
    //This function will break fallthrough edge of 'to' if necessary.
    IRBB * insertBBbetween(IN IRBB * from, IN BBListIter from_ct,
                           IN IRBB * to, IN BBListIter to_ct,
                           IN IRBB * newbb);
    bool inverseAndRemoveTrampolineBranch();
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

    //Return the IRBB that is the No.'n' precedessor of given 'bb'.
    IRBB * getPredBBNth(IRBB const* bb, UINT n) const
    {
        Vertex const* v = getInVertexNth(getVertex(bb->id()), n);
        ASSERT0(v);
        return getBB(v->id());
    }

    //Return the IRBB that is the No.'n' successor of given 'bb'.
    IRBB * getSuccBBNth(IRBB const* bb, UINT n) const
    {
        Vertex const* v = getOutVertexNth(getVertex(bb->id()), n);
        ASSERT0(v);
        return getBB(v->id());
    }
    Region * getRegion() { return m_rg; }
    UINT getNumOfBB() const { return getVertexNum(); }
    UINT getNumOfSucc(IRBB const* bb) const
    { return getOutDegree(getVertex(bb->id())); }
    UINT getNumOfPred(IRBB const* bb) const
    { return getInDegree(getVertex(bb->id())); }
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

        UINT n = 0;
        bool find = false;
        for (xcom::EdgeC * in = VERTEX_in_list(bb_vex);
             in != nullptr; in = EC_next(in)) {
            xcom::Vertex * local_pred_vex = in->getFrom();
            if (local_pred_vex->id() == pred->id()) {
                find = true;
                break;
            }
            n++;
        }

        CHECK0_DUMMYUSE(find); //pred should be a predecessor of bb.
        return n;
    }

    //You should clean the relation between Label and BB before remove BB.
    virtual void removeBB(C<IRBB*> * bbct)
    {
        ASSERT0(bbct);
        ASSERT0(m_bb_list->in_list(bbct));
        IRBB * bb = bbct->val();
        bb->removeAllSuccessorsPhiOpnd(this);
        remove_bb_impl(bb);
        m_bb_list->remove(bbct);
    }

    //We only remove 'bb' from CF graph, vector and bb-list.
    //You should clean the relation between Label and BB before remove BB.
    virtual void removeBB(IRBB * bb)
    {
        ASSERT0(bb);
        remove_bb_impl(bb);
        m_bb_list->remove(bb);
    }
    void remove_xr(IRBB * bb, IR * ir);
    virtual void removeEdge(IRBB * from, IRBB * to)
    {
        from->removeSuccessorDesignatePhiOpnd(this, to);
        CFG<IRBB, IR>::removeEdge(from, to);
    }
    //The function remove labels that no one referenced.
    bool removeRedundantLabel();
    bool removeTrampolinEdge();
    bool removeTrampolinBB();
    bool removeRedundantBranch();
    void rebuild(OptCtx & oc);
    virtual void resetMapBetweenLabelAndBB(IRBB * bb);
    //Construct CFG edge for BB has phi.
    void revisePhiEdge(TMap<IR*, LabelInfo*> & ir2label);

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
    IRBB * splitBB(IRBB * bb, IRListIter split_point);

    //Try to update RPO of newbb accroding to RPO of marker.
    //newbb_prior_marker: true if newbb's lexicographical order is prior to
    //marker.
    //Return true if this function find a properly RPO for 'newbb', otherwise
    //return false.
    bool tryUpdateRPO(IRBB * newbb, IRBB const* marker,
                      bool newbb_prior_marker);

    //Move all Labels which attached on src BB to tgt BB.
    virtual void moveLabels(IRBB * src, IRBB * tgt);

    virtual bool perform(OptCtx & oc) { build(oc); return false; }

    //Perform miscellaneous control flow optimizations.
    //Include remove dead bb which is unreachable, remove empty bb as many
    //as possible, simplify and remove the branch like "if (x==x)", remove
    //the trampolin branch.
    bool performMiscOpt(OptCtx & oc);

    //The function verify whether the branch target is match to the BB.
    bool verifyBranchTarget() const;
    //The function verify the relationship between BB and LabelInfo.
    bool verifyLabel2BB() const;
    //Verification at building SSA mode by ir parser.
    bool verifyPhiEdge(IR * phi, TMap<IR*, LabelInfo*> & ir2label) const;
    bool verifyRPO(OptCtx const& oc) const;
    bool verify() const;
};

} //namespace xoc
#endif
