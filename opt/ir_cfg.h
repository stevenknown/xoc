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

typedef TMap<LabelInfo const*, IRBB*> LAB2BB;

//NOTICE:
//1. For accelerating perform operation of each vertex, e.g
//   compute dominator, please try best to add vertex with
//   topological order.
class IR_CFG : public Pass, public CFG<IRBB, IR> {
protected:
    Vector<IRBB*> m_bb_vec;
    LAB2BB m_lab2bb;
    Region * m_ru;
    TypeMgr * m_tm;

protected:
    void dump_node(FILE * h, bool detail);
    void dump_head(FILE * h);
    void dump_edge(FILE * h, bool dump_eh);

    void remove_bb_impl(IRBB * bb)
    {
        ASSERT0(bb);
        m_bb_vec.set(BB_id(bb), NULL);

        //C<LabelInfo const*> * ct;
        //for (lablst.get_head(&ct);
        //     ct != lablst.end(); ct = lablst.get_next(ct)) {
        //    m_lab2bb.remove(ct->val());
        //}

        removeVertex(BB_id(bb));
    }

public:
    IR_CFG(CFG_SHAPE cs, BBList * bbl, Region * rg,
           UINT edge_hash_size = 16, UINT vertex_hash_size = 16);
    COPY_CONSTRUCTOR(IR_CFG);
    virtual ~IR_CFG() {}

    //Add LABEL to bb, and establish map between label and bb.
    void add_lab(IRBB * src, LabelInfo const* li)
    {
        src->addLabel(li);

        //Set label->bb map.
        m_lab2bb.setAlways(li, src);
    }

    //Add new IRBB into CFG, but the BB list should be modified
    //out of this function.
    //Namely, you should use 'insertBBbetween()' to insert BB into list.
    //And you must consider the right insertion.
    inline void add_bb(IRBB * bb)
    {
        ASSERT0(bb && m_bb_vec.get(BB_id(bb)) == NULL);
        ASSERTN(BB_id(bb) != 0, ("bb id should start at 1"));
        m_bb_vec.set(BB_id(bb), bb);
        addVertex(BB_id(bb));
    }

    //Construct CFG edge for BB has phi.
      void buildAndRevisePhiEdge(TMap<IR*, LabelInfo*> & ir2label);

    //Construct EH edge after cfg built.
    //This function use a conservative method, and this method
    //may generate lots of redundant exception edges.
    void buildEHEdgeNaive();

    //Construct EH edge after cfg built.
    void buildEHEdge();

    virtual void cf_opt();
    void computeDomAndIdom(IN OUT OptCtx & oc, xcom::BitSet const* uni = NULL);
    void computePdomAndIpdom(IN OUT OptCtx & oc, xcom::BitSet const* uni = NULL);

    //Record the Exit BB here.
    virtual void computeExitList()
    {
        //Clean the Exit flag.
        xcom::C<IRBB*> * ct;
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

    virtual void dump_vcg(CHAR const* name)
    { CFG<IRBB, IR>::dump_vcg(name); }

    void dump_vcg(
            CHAR const* name = NULL,
            bool detail = true,
            bool dump_eh = true);
    void dump_dot(
            CHAR const* name = NULL,
            bool detail = true,
            bool dump_eh = true);

    void erase();

    virtual void findTargetBBOfMulticondBranch(IR const*, OUT List<IRBB*>&);
    virtual IRBB * findBBbyLabel(LabelInfo const* lab);
    virtual void findTargetBBOfIndirectBranch(IR const*, OUT List<IRBB*>&);
    void findEHRegion(
            IRBB const* catch_start,
            xcom::BitSet const& mainstreambbs,
            OUT xcom::BitSet & ehbbs);
    void findTryRegion(IRBB const* try_start, OUT xcom::BitSet & ehbbs);
    void findAllTryRegions(OUT xcom::BitSet & trybbs);

    //Allocate and initialize control flow graph.
    void initCfg(OptCtx & oc);
    virtual bool if_opt(IRBB * bb);
    bool isRegionEntry(IRBB * bb) { return BB_is_entry(bb); }
    bool isRegionExit(IRBB * bb) { return BB_is_exit(bb); }
    void insertBBbetween(
            IN IRBB * from,
            IN xcom::C<IRBB*> * from_ct,
            IN IRBB * to,
            IN xcom::C<IRBB*> * to_ct,
            IN IRBB * newbb);
    bool inverseAndRemoveTrampolineBranch();

    //Return the first operation of 'bb'.
    IR * get_first_xr(IRBB * bb)
    {
        ASSERT0(bb && m_bb_vec.get(BB_id(bb)));
        return BB_first_ir(bb);
    }

    //Return the last operation of 'bb'.
    IR * get_last_xr(IRBB * bb)
    {
        ASSERT0(bb && m_bb_vec.get(BB_id(bb)));
        return BB_last_ir(bb);
    }

    Region * getRegion() { return m_ru; }
    UINT getNumOfBB() const { return get_vertex_num(); }
    BBList * getBBList() { return m_bb_list; }
    LAB2BB * get_lab2bb_map() { return &m_lab2bb; }
    IRBB * getBB(UINT id) const { return m_bb_vec.get(id); }
    virtual bool goto_opt(IRBB * bb);
    virtual CHAR const* getPassName() const { return "CFG"; }
    virtual PASS_TYPE getPassType() const { return PASS_CFG; }

    //Find natural loop and scan loop body to find call and early exit, etc.
    void LoopAnalysis(OptCtx & oc);

    //Compute which predecessor is pred to bb.
    //e.g: If pred is the first predecessor, return 0.
    //pred: BB id of predecessor.
    UINT WhichPred(IRBB const* pred, IRBB const* bb) const
    {
        xcom::Vertex * bb_vex = get_vertex(BB_id(bb));
        ASSERT0(bb_vex);

        UINT n = 0;
        bool find = false;
        for (xcom::EdgeC * in = VERTEX_in_list(bb_vex);
             in != NULL; in = EC_next(in)) {
            xcom::Vertex * local_pred_vex = EDGE_from(EC_edge(in));
            if (VERTEX_id(local_pred_vex) == BB_id(pred)) {
                find = true;
                break;
            }
            n++;
        }

        CHECK_DUMMYUSE(find); //pred should be a predecessor of bb.

        return n;
    }

    //You should clean the relation between Label and BB before remove BB.
    virtual void remove_bb(C<IRBB*> * bbct)
    {
        ASSERT0(bbct);
        ASSERT0(m_bb_list->in_list(bbct));
        remove_bb_impl(bbct->val());
        m_bb_list->remove(bbct);
    }

    //We only remove 'bb' from CF graph, vector and bb-list.
       //You should clean the relation between Label and BB before remove BB.
    virtual void remove_bb(IRBB * bb)
    {
        ASSERT0(bb);
        remove_bb_impl(bb);
        m_bb_list->remove(bb);
    }
    void remove_xr(IRBB * bb, IR * ir);
    bool removeTrampolinEdge();
    bool removeTrampolinBB();
    bool removeRedundantBranch();
    virtual void resetMapBetweenLabelAndBB(IRBB * bb);

    virtual void setRPO(IRBB * bb, INT order) { BB_rpo(bb) = order; }

    virtual void moveLabels(IRBB * src, IRBB * tgt);

    virtual bool perform(OptCtx & oc) { DUMMYUSE(oc); return false; }

    //Perform miscellaneous control flow optimizations.
    //Include remove dead bb which is unreachable, remove empty bb as many
    //as possible, simplify and remove the branch like "if (x==x)", remove
    //the trampolin branch.
    bool performMiscOpt(OptCtx & oc);

    //Verification at building SSA mode by ir parser.
    bool verifyPhiEdge(IR * phi, TMap<IR*, LabelInfo*> & ir2label);
};

} //namespace xoc
#endif
