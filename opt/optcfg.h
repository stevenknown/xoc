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
#ifndef _OPTCFG_H_
#define _OPTCFG_H_

namespace xoc {

class RemoveUnreachBBCtx {
    //Record the vertexs that in/out edge changed.
    xcom::VexTab m_changed_vextab;
public:
    void add(Vertex const* v) { m_changed_vextab.append(v); }
    xcom::VexTab & getVexTab() { return m_changed_vextab; }
};


class RemoveEmptyBBCtx {
    xcom::List<UINT> * m_removed_bbid_list;
public:
    //If the flag is true, the optimizer will remove empty BB without too
    //much considerations.
    bool m_force_remove_empty_bb;
    UINT vertex_iter_time;
    CfgOptCtx & cfgoptctx_org;
    CfgOptCtx cfgoptctx_dup; //a duplication of original CfgOptCtx.
public:
    RemoveEmptyBBCtx(CfgOptCtx & coctx) :
        vertex_iter_time(0), cfgoptctx_org(coctx),
        cfgoptctx_dup(cfgoptctx_org)
    {
        m_removed_bbid_list = nullptr;
        m_force_remove_empty_bb = false;
    }
    ~RemoveEmptyBBCtx()
    { if (m_removed_bbid_list != nullptr) { delete m_removed_bbid_list; } }

    void add(UINT bbid)
    {
        if (m_removed_bbid_list != nullptr) {
            m_removed_bbid_list->append_tail(bbid);
        }
    }

    CfgOptCtx & chooseCtx()
    {
        if (removeTooManyTimes()) {
            //Remove DomInfo is costly function, if there are too many
            //BB to be removed, update DomInfo at once after the
            //optimization.
            CFGOPTCTX_need_update_dominfo(&cfgoptctx_dup) = false;
            return cfgoptctx_dup;
        }
        return cfgoptctx_org;
    }

    List<UINT> * getRemovedList() { return m_removed_bbid_list; }

    bool isForceRemove() const { return m_force_remove_empty_bb; }

    bool needUpdateDomInfo() const
    { return removeTooManyTimes() &&
             cfgoptctx_org.needUpdateDomInfo() &&
             cfgoptctx_org.oc.is_dom_valid(); }

    bool removeTooManyTimes() const
    {
        return vertex_iter_time >
               g_cfg_remove_empty_bb_maxtimes_to_update_dominfo;
    }

    void setRecordRemovedBB()
    {
        ASSERT0(m_removed_bbid_list == nullptr);
        m_removed_bbid_list = new List<UINT>();
    }

    //If 'force' is true, the optimizer will remove empty BB without too
    //much considerations.
    void setForceRemove(bool force) { m_force_remove_empty_bb = force; }
};


//The class represents CFG with a set of miscellaneous optimizations.
//NOTICE:
//1. For accelerating perform operation of each vertex, e.g
//   compute dominator, please try best to add vertex with
//   topological order.
//NOTE: BB should define and implement method 'id()' and member field 'm_rpo'.
template <class BB, class XR> class OptimizedCFG : public CFG<BB, XR> {
    COPY_CONSTRUCTOR(OptimizedCFG);
protected:
    //Return the ancestor class object pointer.
    CFG<BB, XR> * getCFG() { return (CFG<BB, XR>*)this; }
    xcom::Graph * getGraph() { return (xcom::Graph*)this; }

    virtual void recomputeDomInfo(MOD OptCtx & oc) = 0;

    //Note the function have to be invoked before CFG changed.
    void recordChangedVex(Vertex const* v, RemoveUnreachBBCtx * unrchctx) const
    {
        ASSERT0(unrchctx);
        unrchctx->add(v);
        AdjVertexIter it;
        for (Vertex const* t = Graph::get_first_out_vertex(v, it);
             t != nullptr; t = Graph::get_next_out_vertex(it)) {
            unrchctx->add(t);
        }
        AdjVertexIter it2;
        for (Vertex const* t = Graph::get_first_in_vertex(v, it2);
             t != nullptr; t = Graph::get_next_in_vertex(it2)) {
            unrchctx->add(t);
        }
    }

    //The function preprocesses the Dom, SSA, and some other information that
    //related CFG before CFG changed during RemoveBB.
    //Note the function have to be invoked before CFG change.
    virtual void preprocessBeforeRemoveBB(BB *, MOD CfgOptCtx &)
    { ASSERTN(0, ("Target Dependent Code")); }

    //Remove redundant branch and stmt.
    bool removeRedundantBranchCase2(BB *RESTRICT bb, BB const*RESTRICT next_bb,
                                    XR * xr, CfgOptCtx const& ctx);
    //Remove redundant branch edge and stmt.
    bool removeRedundantBranchCase1(BB *RESTRICT bb, BB const*RESTRICT next_bb,
                                    XR * xr, CfgOptCtx const& ctx);

    //Remove empty bb, and merger label info.
    bool removeEmptyBBHelper(BB * bb, BB * next_bb,
                             C<BB*> * bbct, C<BB*> * next_ct,
                             MOD RemoveEmptyBBCtx & rmctx);
public:
    OptimizedCFG(List<BB*> * bb_list, UINT vertex_hash_size = 16)
        : CFG<BB, XR>(bb_list, vertex_hash_size)
    {}
    virtual ~OptimizedCFG() {}

    //Return true if SSA data structure and control flow structure is valid
    //after removing the given BB.
    //bb: the empty BB that will be removed soon.
    bool isValidToKeepSSAIfRemoveBB(BB const* bb) const;

    //Move all Labels which attached on src BB to tgt BB.
    virtual void moveLabels(BB * src, BB * tgt) = 0;

    //Remove empty BB, and merger label info.
    //Note removing BB does NOT affect RPO.
    //bblst: if it is not NULL, record the removed BB.
    bool removeEmptyBB(OUT RemoveEmptyBBCtx & rmbbctx);

    //Try to remove empty bb, and merger label info.
    //Note remove BB will not affect the usage of RPO.
    bool removeSingleEmptyBB(BB * bb, MOD RemoveEmptyBBCtx & ctx);
    void removeUnreachSingleBB(xcom::C<BB*> * bbcontainer, MOD CfgOptCtx & ctx,
                               OUT RemoveUnreachBBCtx * unrchctx);

    //Remove unreachable BB from CFG entry.
    //Perform DFS to seek for unreachable BB, removing the 'dead-BB', and
    //free its ir-list.
    //Return true if some dead-BB removed.
    //Note removing unreached BB will not affect RPO and DomInfo.
    //unrchctx: if not null, record the affect caused by the function.
    bool removeUnreachBB(MOD CfgOptCtx & ctx,
                         OUT RemoveUnreachBBCtx * unrchctx);

    //Remove redundant branch edge.
    bool removeRedundantBranch(CfgOptCtx const& ctx);

    //The function replaces original predecessor bb with a list of
    //new predecessors.
    //bb: the predecessor will be replaced.
    //succ: the target BB.
    //newpreds: list of new predecessors.
    //Return the new position of 'bb' that is in the predecessor list of 'succ'.
    virtual VexIdx replacePredWith(BB const* bb, BB const* succ,
                                   List<UINT> const& newpreds,
                                   OUT CfgOptCtx & ctx);
};


template <class BB, class XR>
VexIdx OptimizedCFG<BB, XR>::replacePredWith(
    BB const* bb, BB const* succ, List<UINT> const& newpreds,
    OUT CfgOptCtx &)
{
    //If bb removed, the number of its successors will decrease.
    //Then the number of PHI of bb's successors must be replaced.
    //CASE1:BB7->BB8->BB9
    //      |         ^
    //      |_________|
    //There are phi at BB9, BB8 will be removed.
    //Not need add operand to PHI because the number of edge
    //of BB9 is unchanged.
    return getGraph()->replaceInVertex(bb->id(), succ->id(), newpreds);
}


template <class BB, class XR>
bool OptimizedCFG<BB, XR>::isValidToKeepSSAIfRemoveBB(BB const* bb) const
{
    CFG<BB, XR> const* pcthis = (CFG<BB, XR> const*)this;
    bool succhasphi = bb->hasPhiInSuccBB(pcthis);
    xcom::Vertex const* bbv = bb->getVex();
    if (((CFG<BB, XR> const*)this)->isInDegreeMoreThan(bbv, 1) && succhasphi) {
        //TODO: If you remove current BB, then you have to add more
        //than one predecessors to bb's succ, that will add more than
        //one operand to phi at bb's succ. It complicates the optimization.
        return false;
    }
    if (!succhasphi) { return true; }
    xcom::AdjVertexIter it;
    for (xcom::Vertex * succv = Graph::get_first_out_vertex(bbv, it);
         succv != nullptr; succv = Graph::get_next_out_vertex(it)) {
        BB const* succ = pcthis->getBB(succv->id());
        ASSERTN(succ, ("without bb corresponded"));
        if (!succ->hasPhiWithAllSameOperand(this)) {
            //CASE:Do NOT remove the bb if its successor BB has Phi with
            //different operands.
            //e.g: given BB7 is an empty BB, usually produced by other passes
            //after some powerful optimizations, such as copy-propagation, DCE.
            //  BB1----
            //  |      |
            //  v      |
            //  BB4--  |
            //  |    | |
            //  v    | |
            //  BB7  | | //BB7 is empty BB.
            //  | ___| |
            //  || ____|
            //  |||
            //  vvv
            //  BB9:
            //  phi $7 = (0x0(BB4), 0x1(BB7), 0x0(BB9))
            //In above example, phi in BB9 has three operands which indicates 3
            //constant value that coming from 3 predecessors. Because BB7 is
            //empty BB, current function attempts to remove BB7 to simplfy CFG.
            //Whereas removing the operand 0x1(BB7) that correspond to BB7 to
            //satified PHI constrains in SSA mode. However, actually phi's
            //operand which corresond to BB7 is 0x1, represents the different
            //value compared to other operand 0x0. If it is removed, all
            //operand of phi are same. Other passes, say PRSSAMgr, will
            //remove the phi, and incur even more chain reactions, say RCE,
            //will remove TrueBR/FalseBR and merge more fallthrough BB.
            //Finally lead to an incorrect result.
            //Thus, to avoid subsequently insane optimization, do NOT remove
            //emtpy BB if its successor has not-all-same operand.
            return false;
        }
    }
    return true;
}


template <class BB, class XR>
bool OptimizedCFG<BB, XR>::removeEmptyBBHelper(
    BB * bb, BB * next_bb, C<BB*> * bbct, C<BB*> * next_ct,
    MOD RemoveEmptyBBCtx & rmctx)
{
    if (next_bb == nullptr) {
        //'bb' is the last empty BB.
        ASSERT0(next_ct == nullptr);
        DUMMYUSE(next_ct);
        //CASE:Do NOT remove entry or exit.
        //Some redundant CFG has multi BB which satifies
        //CFG entry qualifications.
        if (bb->getLabelList().get_elem_count() == 0 &&
            !getCFG()->isRegionExit(bb)) {
            //BB does not have any label.
            CfgOptCtx & ctx = rmctx.chooseCtx();
            CFGOPTCTX_vertex_iter_time(&ctx) = 0;
            preprocessBeforeRemoveBB(bb, ctx);
            rmctx.vertex_iter_time += CFGOPTCTX_vertex_iter_time(&ctx);

            CFGOPTCTX_vertex_iter_time(&ctx) = 0;
            getCFG()->removeBB(bbct, ctx);
            rmctx.vertex_iter_time += CFGOPTCTX_vertex_iter_time(&ctx);
            return true;
        }
        return false;
    }
    //Only apply restricted removing if CFG is invalid.
    //Especially BB list is ready, whereas CFG is not.
    if (!rmctx.chooseCtx().oc.is_cfg_valid()) { return false; }
    if (!rmctx.chooseCtx().do_merge_label() && bb->hasLabel()) {
        return false;
    }
    ASSERT0(getCFG()->getSuccsNum(bb) == 1);
    if (!rmctx.isForceRemove() && !isValidToKeepSSAIfRemoveBB(bb)) {
        return false;
    }
    //Move labels of bb to next_bb.
    moveLabels(bb, next_bb);
    ASSERT0(getCFG()->getNthSucc(bb, 0) == next_bb);

    //The function have to be invoked before CFG change.
    CfgOptCtx & ctx = rmctx.chooseCtx();

    //replacePredWith will revise the number of Phi operands.
    //ctx.vertex_iter_time = 0;
    //preprocessBeforeRemoveBB(bb, ctx);
    //rmctx.vertex_iter_time += ctx.vertex_iter_time;

    //Tranform CFG.
    List<UINT> newpreds;
    getCFG()->get_preds(newpreds, bb);
    CFGOPTCTX_vertex_iter_time(&ctx) = 0;
    replacePredWith(bb, next_bb, newpreds, ctx);
    rmctx.vertex_iter_time += CFGOPTCTX_vertex_iter_time(&ctx);

    //Invoke interface to remove related BB and Vertex.
    //The map between bb and labels should have maintained in above code.
    CFGOPTCTX_vertex_iter_time(&ctx) = 0;
    getCFG()->removeBB(bbct, ctx);
    rmctx.vertex_iter_time += CFGOPTCTX_vertex_iter_time(&ctx);
    return true;
}


template <class BB, class XR>
bool OptimizedCFG<BB, XR>::removeSingleEmptyBB(
    BB * bb, MOD RemoveEmptyBBCtx & ctx)
{
    START_TIMER(t, "Remove Single Empty BB");
    if (!getCFG()->isEmptyBB(bb) || getCFG()->isRegionEntry(bb) ||
        bb->isExceptionHandler()) {
        return false;
    }
    xcom::C<BB*> * ct;
    getCFG()->getBBList()->find(bb, &ct);
    ASSERT0(ct);
    xcom::C<BB*> * next_ct = getCFG()->getBBList()->get_next(ct);
    BB * next_bb = nullptr;
    if (next_ct != nullptr) {
        next_bb = next_ct->val();
    }
    bool doit = removeEmptyBBHelper(bb, next_bb, ct, next_ct, ctx);
    if (doit) {
        if (ctx.needUpdateDomInfo()) {
            START_TIMER(u, "Remove Single Empty BB::Recompute DomInfo");
            recomputeDomInfo(ctx.chooseCtx().oc);
            END_TIMER(u, "Remove Single Empty BB::Recompute DomInfo");
        }
    }
    END_TIMER(t, "Remove Single Empty BB");
    return doit;
}


template <class BB, class XR>
bool OptimizedCFG<BB, XR>::removeEmptyBB(OUT RemoveEmptyBBCtx & rmctx)
{
    START_TIMER(t, "Remove Empty BB");
    xcom::C<BB*> * ct;
    xcom::C<BB*> * next_ct;
    bool doit = false;
    for (getCFG()->getBBList()->get_head(&ct), next_ct = ct;
         ct != nullptr; ct = next_ct) {
        next_ct = getCFG()->getBBList()->get_next(next_ct);
        BB * bb = ct->val();
        ASSERT0(bb);
        BB * next_bb = nullptr;
        if (next_ct != nullptr) {
            next_bb = next_ct->val();
        }

        //TODO: confirm if this is correct:
        //  isRegionExit() need to update if CFG changed or ir_bb_list
        //  reconstructed.
        //  e.g:void m(bool r, bool y)
        //      {
        //          bool l;
        //          l = y || r;
        //          return 0;
        //      }
        //After initCFG(), there are 2 BBs, BB1 and BB3.
        //When IR_LOR simpilified, and new BB generated, func-exit BB flag
        //has to be updated as well.
        if (getCFG()->isEmptyBB(bb) &&
            !getCFG()->isRegionEntry(bb) && !bb->isExceptionHandler()) {
            UINT bbid = bb->id();
            bool removed = removeEmptyBBHelper(
                bb, next_bb, ct, next_ct, rmctx);
            if (removed) {
                rmctx.add(bbid);
            }
            doit |= removed;
        }
    }
    if (rmctx.needUpdateDomInfo()) {
        START_TIMER(u, "Remove Empty BB::Recompute DomInfo");
        recomputeDomInfo(rmctx.chooseCtx().oc);
        END_TIMER(u, "Remove Empty BB::Recompute DomInfo");
    }
    END_TIMER(t, "Remove Empty BB");
    return doit;
}


template <class BB, class XR>
bool OptimizedCFG<BB, XR>::removeRedundantBranchCase2(
    BB *RESTRICT bb, BB const*RESTRICT next_bb, XR * xr, CfgOptCtx const& ctx)
{
    //CASE:
    //  BB1:
    //    some-code;
    //    goto L1  <--- redundant branch
    //  BB2:
    //    L1:
    BB * tgt_bb = getCFG()->findBBbyLabel(xr->getLabel());
    ASSERT0(tgt_bb != nullptr);
    if (tgt_bb == next_bb) {
        getCFG()->remove_xr(bb, xr, ctx);
        return true;
    }
    return false;
}


template <class BB, class XR>
bool OptimizedCFG<BB, XR>::removeRedundantBranchCase1(
    BB *RESTRICT bb, BB const*RESTRICT next_bb, XR * xr, CfgOptCtx const& ctx)
{
    ASSERT0(bb && xr);
    //CASE:
    //  BB1:
    //    falsebr BB2 <--- redundant branch
    //  BB2:
    //    some-code;
    xcom::Vertex * v = bb->getVex();
    xcom::EdgeC * last_el = nullptr;
    bool find = false; //find another successor with different target.
    for (xcom::EdgeC * el = v->getOutList(); el != nullptr; el = EC_next(el)) {
        last_el = el;
        BB * succ = getCFG()->getBB(el->getToId());
        if (succ != next_bb) {
            find = true;
            break;
        }
    }
    if (last_el != nullptr && !find) {
        //There is only one target for cond-br.
        //Thus the cond-br is redundant.
        if (!bb->hasMDPhi(this)) {
            //CASE2:If you remove current BB, then you have to add more
            //  than one predecessors to bb's succ, that will add more than
            //  one operand to phi at bb's successors. It complicates
            //  optimizations.
            for (xcom::EdgeC * el = last_el; el->get_prev() != nullptr;) {
                xcom::EdgeC * tmp = el;
                el = el->get_prev();
                xcom::Graph::removeEdge(tmp->getEdge());
            }
        }
        getCFG()->remove_xr(bb, xr, ctx);
        return true;
    }
    return false;
}


template <class BB, class XR>
bool OptimizedCFG<BB, XR>::removeRedundantBranch(CfgOptCtx const& ctx)
{
    START_TIMER(t, "Remove Redundant Branch");
    xcom::C<BB*> * ct, * next_ct;
    bool doit = false;
    for (getCFG()->getBBList()->get_head(&ct), next_ct = ct;
         ct != getCFG()->getBBList()->end(); ct = next_ct) {
        next_ct = getCFG()->getBBList()->get_next(next_ct);
        BB * bb = ct->val();
        BB * next_bb = nullptr; //next_bb is fallthrough BB.
        if (next_ct != nullptr) {
            next_bb = next_ct->val();
        }

        XR * xr = getCFG()->get_last_xr(bb);
        if (xr == nullptr) {
            //CASE1:Although bb is empty, it may have some labels attached,
            //  which may have dedicated usage. Do not remove it for
            //  convservative purpose.
            //CASE2:If you remove current BB, then you have to add more
            //  than one predecessors to bb's succ, that will add more than
            //  one operand to phi at bb's successors. It complicates
            //  optimizations.
            ASSERTN(getCFG()->isCFGEntry(bb) ||
                    getCFG()->isCFGExit(bb) ||
                    bb->hasPhiInSuccBB(this) ||
                    bb->hasMDPhi(this) ||
                    bb->getLabelList().get_elem_count() != 0,
                    ("should call removeEmptyBB() first."));
            continue;
        }
        if (xr->hasSideEffect(true)) {
            continue;
        }
        if (xr->isConditionalBr()) {
            doit |= removeRedundantBranchCase1(bb, next_bb, xr, ctx);
            continue;
        }
        if (xr->isUnconditionalBr() && !xr->isIndirectBr()) {
            doit |= removeRedundantBranchCase2(bb, next_bb, xr, ctx);
            continue;
        }
    }
    END_TIMER(t, "Remove Redundant Branch");
    return doit;
}


template <class BB, class XR>
void OptimizedCFG<BB, XR>::removeUnreachSingleBB(
    xcom::C<BB*> * bbcontainer, MOD CfgOptCtx & ctx,
    OUT RemoveUnreachBBCtx * unrchctx)
{
    START_TIMER(t, "Remove Unreach Single BB");
    ASSERT0(bbcontainer);
    //EH may be redundant and can be removed.
    //ASSERTN(!bb->isExceptionHandler(),
    //        ("For conservative purpose, "
    //         "exception handler should be reserved."));
    BB * bb = bbcontainer->val();
    getCFG()->removeMapBetweenLabelAndBB(bb);
    preprocessBeforeRemoveBB(bb, ctx);
    if (unrchctx != nullptr) {
        recordChangedVex(bb->getVex(), unrchctx);
    }
    //Unreachable-code will confuse the DomInfo updation.
    CfgOptCtx tctx(ctx);
    OptCtx toc(ctx.oc);
    //Invalid DomInfo to inform CFG related API to stop updation.
    toc.setInvalidDom();
    tctx.setOptCtx(toc);
    CFGOPTCTX_need_update_dominfo(&tctx) = false;
    getCFG()->removeBB(bbcontainer, tctx);
    END_TIMER(t, "Remove Unreach Single BB");
}


template <class BB, class XR>
bool OptimizedCFG<BB, XR>::removeUnreachBB(
    MOD CfgOptCtx & ctx, OUT RemoveUnreachBBCtx * unrchctx)
{
    bool removed = false;
    ASSERT0(getCFG()->getBBList());
    if (getCFG()->getBBList()->get_elem_count() == 0) { return false; }

    START_TIMER(ti, "Remove Unreach BB");
    //There is only one entry point.
    xcom::BitSet visited;
    visited.bunion(getCFG()->getBBList()->get_elem_count());
    visited.diff(getCFG()->getBBList()->get_elem_count());
    ASSERT0(getCFG()->getEntry());
    visited.bunion(getCFG()->getEntry()->id());
    GraphIterOut iterout(*this, getCFG()->getEntry()->getVex());
    for (Vertex const* t = iterout.get_first();
         t != nullptr; t = iterout.get_next(t)) {
        visited.bunion(t->id());
    }
    xcom::C<BB*> * next_ct;
    xcom::C<BB*> * ct;
    for (getCFG()->getBBList()->get_head(&ct);
         ct != getCFG()->getBBList()->end(); ct = next_ct) {
        BB * bb = ct->val();
        next_ct = getCFG()->getBBList()->get_next(ct);
        if (!visited.is_contain(bb->id())) {
            removeUnreachSingleBB(ct, ctx, unrchctx);
            removed = true;
        }
    }
    END_TIMER(ti, "Remove Unreach BB");
    return removed;
}

} //namespace xoc
#endif
