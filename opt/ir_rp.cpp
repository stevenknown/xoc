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
#include "cominc.h"
#include "comopt.h"

namespace xoc {

class RefHashFunc {
    GVN * m_gvn;
public:
    void initMem(GVN * gvn)
    {
        ASSERT0(gvn);
        m_gvn = gvn;
    }

    //The function will modify m_iter.
    UINT get_hash_value(IR * t, UINT bucket_size) const
    {
        ASSERT0(bucket_size != 0 && isPowerOf2(bucket_size));
        UINT hval = 0;
        ConstIRIter iter;
        switch (t->getCode()) {
        case IR_LD:
            hval = t->getCode() + (t->getOffset() + 1) +
                   (UINT)(size_t)t->getType();
            break;
        case IR_ILD:
            for (IR const* x = iterInitC(t, iter);
                 x != NULL; x = iterNextC(iter)) {
                UINT v = x->getCode() + (x->getOffset() + 1) +
                    (UINT)(size_t)x->getType();
                if (x->is_id()) {
                    v += ((UINT)(size_t)ID_info(x)) * 5;
                }
                hval += v;
            }
            break;
        case IR_ST:
            hval = ((UINT)IR_LD) + (t->getOffset() + 1) +
                   (UINT)(size_t)t->getType();
            break;
        case IR_IST:
            for (IR const* x = iterInitC(IST_base(t), iter);
                 x != NULL; x = iterNextC(iter)) {
                UINT v = x->getCode() + (x->getOffset() + 1) +
                        (UINT)(size_t)x->getType();
                if (x->is_id()) {
                    v += ((UINT)(size_t)ID_info(x)) * 5;
                }
                hval += v;
            }
            hval += ((UINT)IR_ILD) + (t->getOffset() + 1) +
                    (UINT)(size_t)t->getType();
            break;
        case IR_ARRAY:
            for (IR const* x = iterInitC(t, iter);
                 x != NULL; x = iterNextC(iter)) {
                UINT v = x->getCode() + (x->getOffset() + 1) +
                        (UINT)(size_t)x->getType();
                if (x->is_id()) {
                    v += ((UINT)(size_t)ID_info(x)) * 5;
                }
                hval += v;
            }
            break;
        default: UNREACHABLE(); //unsupport.
        }
        return hash32bit(hval) & (bucket_size - 1);
    }

    bool compareArray(IR * t1, IR * t2) const
    {
        ASSERT0(m_gvn);
        if (t1 == t2) { return true; }

        ASSERT0(m_gvn->mapIR2VN(ARR_base(t1)) &&
                m_gvn->mapIR2VN(ARR_base(t2)));

        if (m_gvn->mapIR2VN(ARR_base(t1)) != m_gvn->mapIR2VN(ARR_base(t2))) {
            return false;
        }

        if (((CArray*)t1)->getDimNum() != ((CArray*)t2)->getDimNum()) {
            return false;
        }

        IR * s1 = ARR_sub_list(t1);
        IR * s2 = ARR_sub_list(t2);
        for (; s1 != NULL && s2 != NULL; s1 = IR_next(s1), s2 = IR_next(s2)) {
            ASSERT0(m_gvn->mapIR2VN(s1) && m_gvn->mapIR2VN(s2));
            if (m_gvn->mapIR2VN(s1) != m_gvn->mapIR2VN(s2)) {
                return false;
            }
        }

        if (s1 != NULL || s2 != NULL) { return false; }

        if (ARR_ofst(t1) != ARR_ofst(t2)) { return false; }

        if (IR_dt(t1) != IR_dt(t2)) { return false; }

        return true;
    }

    bool compareIndirectAccess(IR * t1, IR * t2) const
    {
        ASSERT0(m_gvn);
        if (t1 == t2) { return true; }

        IR const* base1 = NULL;
        if (t1->is_ild()) { base1 = ILD_base(t1); }
        else if (t1->is_ist()) { base1 = IST_base(t1); }
        ASSERT0(base1);

        IR const* base2 = NULL;
        if (t2->is_ild()) { base2 = ILD_base(t2); }
        else if (t2->is_ist()) { base2 = IST_base(t2); }
        ASSERT0(base2);

        ASSERT0(m_gvn->mapIR2VN(base1) && m_gvn->mapIR2VN(base2));
        if (m_gvn->mapIR2VN(base1) != m_gvn->mapIR2VN(base2)) {
            return false;
        }

        if (t1->getOffset() != t2->getOffset()) { return false; }

        if (IR_dt(t1) != IR_dt(t2)) { return false; }

        return true;
    }

    bool compareDirectAccess(IR * t1, IR * t2) const
    {
        ASSERT0(m_gvn);
        if (t1 == t2) { return true; }

        ASSERT0(m_gvn->mapIR2VN(t1) && m_gvn->mapIR2VN(t2));
        if (m_gvn->mapIR2VN(t1) != m_gvn->mapIR2VN(t2)) {
            return false;
        }

        if (t1->getOffset() != t2->getOffset()) { return false; }

        if (IR_dt(t1) != IR_dt(t2)) { return false; }

        return true;
    }

    bool compare(IR * t1, IR * t2) const
    {
        if (t1->is_array() && t2->is_array()) {
            return compareArray(t1, t2);
        } else if ((t1->is_ild() || t1->is_ist()) &&
                   (t2->is_ild() || t2->is_ist())) {
            return compareIndirectAccess(t1, t2);
        } else if ((t1->is_ild() || t1->is_ist()) &&
                   (t2->is_ild() || t2->is_ist())) {
            return compareDirectAccess(t1, t2);
        }
        return false;
    }
};


class RefTab : public Hash<IR*, RefHashFunc> {
public:
    RefTab(UINT bucksize) : Hash<IR*, RefHashFunc>(bucksize) {}

    void initMem(GVN * gvn)
    { m_hf.initMem(gvn); }
};


class PromotedTab : public DefSBitSet {
public:
    PromotedTab(DefSegMgr * sm) : DefSBitSet(sm) {}

    //Add whole ir tree into table.
    void add(IR * ir, IRIter & ii)
    {
        ii.clean();
        for (IR * x = iterInit(ir, ii);
             x != NULL; x = iterNext(ii)) {
            bunion(IR_id(x));
        }
    }
};


#ifdef _DEUBG_
static void dump_delegate_tab(RefTab & dele_tab, TypeMgr * dm)
{
    if (g_tfile == NULL) { return; }
    ASSERT0(dm);

    note("\n==---- DUMP Delegate Table ----==");
    INT cur = 0;
    for (IR * dele = dele_tab.get_first(cur);
         cur >= 0; dele = dele_tab.get_next(cur)) {
        dumpIR(dele, dm);
    }
    fflush(g_tfile);
}
#endif


//
//START RegPromot
//
void RegPromot::dumpInexact(TTab<IR*> & access)
{
    if (g_tfile == NULL) { return; }
    note("\n---- DUMP inexact access ----");
    TabIter<IR*> iter;
    for (IR * ir = access.get_first(iter);
        ir != NULL; ir = access.get_next(iter)) {
        dumpIR(ir, m_rg, NULL, IR_DUMP_SRC_LINE | IR_DUMP_INNER_REGION);
        note("\n");
    }
    note("\n");
    fflush(g_tfile);
}


void RegPromot::dumpExact(TMap<MD const*, IR*> & access, List<IR*> & occs)
{
    if (g_tfile == NULL) { return; }
    note("\n---- DUMP exact access ----");
    TMapIter<MD const*, IR*> iter;
    IR * ref;
    for (MD const* md = access.get_first(iter, &ref);
         ref != NULL; md = access.get_next(iter, &ref)) {
        md->dump(m_rg->getTypeMgr());
        dumpIR(ref, m_rg, NULL, IR_DUMP_SRC_LINE | IR_DUMP_INNER_REGION);
        note("\n");
    }

    note("\n---- DUMP exact occ list ----");
    for (IR * x = occs.get_head(); x != NULL; x = occs.get_next()) {
        dumpIR(x, m_rg, NULL, IR_DUMP_SRC_LINE | IR_DUMP_INNER_REGION);
        note("\n");
    }
    note("\n");

    fflush(g_tfile);
}


MDLT * RegPromot::getMDLifeTime(MD * md)
{
    MDLT * lt;
    if ((lt = m_md2lt_map->get(md)) != NULL) {
        return lt;
    }
    lt = (MDLT*)xmalloc(sizeof(MDLT));
    MDLT_id(lt) = ++m_mdlt_count;
    MDLT_md(lt) = md;
    MDLT_livebbs(lt) = m_bs_mgr.create();
    m_md2lt_map->set(md, lt);
    return lt;
}


void RegPromot::cleanLiveBBSet()
{
    //Clean.
    Vector<MDLT*> * bs_vec = m_md2lt_map->get_tgt_elem_vec();
    for (INT i = 0; i <= bs_vec->get_last_idx(); i++) {
        MDLT * lt = bs_vec->get(i);
        if (lt != NULL) {
            ASSERT0(MDLT_livebbs(lt) != NULL);
            MDLT_livebbs(lt)->clean();
        }
    }
}


void RegPromot::dump_mdlt()
{
    if (g_tfile == NULL) { return; }
    MDSet mdbs;
    xcom::DefMiscBitSetMgr * sbsmgr = getSBSMgr();
    MDLivenessMgr * livemgr = getMDLivenessMgr();
    BBList * bbl = m_rg->getBBList();
    for (IRBB * bb = bbl->get_head(); bb != NULL; bb = bbl->get_next()) {
        MDSet * livein = livemgr->getLiveInMDSet(bb);
        MDSet * liveout = livemgr->getLiveOutMDSet(bb);
        if (livein->is_empty() && liveout->is_empty()) { continue; }
        mdbs.bunion(*livein, *sbsmgr);
        mdbs.bunion(*liveout, *sbsmgr);
    }
    mdbs.dump(m_md_sys);

    StrBuf buf(32);
    note("\n==---- DUMP MD LIFE TIME ----==");
    MDSetIter iter;
    for (INT i = mdbs.get_first(&iter); i >= 0; i = mdbs.get_next(i, &iter)) {
        MD * md = m_md_sys->getMD(i);
        ASSERT0(md != NULL);
        MDLT * lt = m_md2lt_map->get(md);
        ASSERT0(lt != NULL);
        xcom::BitSet * livebbs = MDLT_livebbs(lt);
        ASSERT0(livebbs != NULL);

        //Print MD name.
        note("\nMD%d", MD_id(md));
        prt(":");

        //Print live BB.
        if (livebbs == NULL || livebbs->is_empty()) { continue; }
        INT start = 0;
        for (INT u = livebbs->get_first(); u >= 0; u = livebbs->get_next(u)) {
            for (INT j = start; j < u; j++) {
                buf.sprint("%d,", j);
                for (UINT k = 0; k < buf.strlen(); k++) {
                    prt(" ");
                }
            }
            prt("%d,", u);
            start = u + 1;
        }
    }
    fflush(g_tfile);
    mdbs.clean(*sbsmgr);
}


void RegPromot::buildLifeTime()
{
    cleanLiveBBSet();

    //Rebuild life time.
    BBList * bbl = m_rg->getBBList();
    MDLivenessMgr * livemgr = getMDLivenessMgr();
    for (IRBB * bb = bbl->get_head(); bb != NULL; bb = bbl->get_next()) {
        MDSet * livein = livemgr->getLiveInMDSet(bb);
        MDSet * liveout = livemgr->getLiveOutMDSet(bb);
        if (livein->is_empty() && liveout->is_empty()) { continue; }

        MDSetIter iter;
        for (INT i = livein->get_first(&iter);
             i >= 0; i = livein->get_next(i, &iter)) {
            MDLT_livebbs(getMDLifeTime(m_md_sys->getMD(i)))->bunion(BB_id(bb));
        }
        for (INT i = liveout->get_first(&iter);
             i >= 0; i = liveout->get_next(i, &iter)) {
            MDLT_livebbs(getMDLifeTime(m_md_sys->getMD(i)))->bunion(BB_id(bb));
        }
    }

    //dump_mdlt();
}


void RegPromot::addExactAccess(OUT TMap<MD const*, IR*> & exact_access,
                               OUT List<IR*> & exact_occs,
                               MD const* exact_md,
                               IR * ir)
{
    ASSERT0(exact_md && exact_md->is_exact());
    if (!exact_access.find(exact_md)) {
        exact_access.set(exact_md, ir);
    }
    ASSERT0(!exact_occs.find(ir));
    exact_occs.append_tail(ir);
}


void RegPromot::addInexactAccess(TTab<IR*> & inexact_access, IR * ir)
{
    inexact_access.append_and_retrieve(ir);
}


bool RegPromot::checkArrayIsLoopInvariant(IN IR * ir, LI<IRBB> const* li)
{
    ASSERT0(ir->is_array() && li);
    for (IR * s = ARR_sub_list(ir); s != NULL; s = s->get_next()) {
        if (!isLoopInvariant(s, li, m_rg)) {
            return false;
        }
    }
    if (!isLoopInvariant(ARR_base(ir), li, m_rg)) {
        return false;
    }
    return true;
}


//Return true if the caller can keep doing the analysis.
//That means there are no memory referrences clobbered the
//candidate in exact_occs.
//Or else the analysis for current loop should be terminated.
//Return false if find unpromotable memory reference, this may
//prevent entire loop be promoted.
bool RegPromot::handleArrayRef(IN IR * ir,
                               LI<IRBB> const* li,
                               OUT TMap<MD const*, IR*> & exact_access,
                               OUT List<IR*> & exact_occs,
                               OUT TTab<IR*> & inexact_access)
{
    ASSERT0(ir->isArrayOp());
    if (ARR_ofst(ir) != 0) {
        //The array reference can not be promoted.
        //Check the promotable candidates if current stmt
        //modify the related MD.
        clobberAccess(ir, exact_access, exact_occs, inexact_access);
        return true;
    }

    MD const* mustuse = ir->getRefMD();
    if (mustuse == NULL || !mustuse->is_effect()) { return false; }

    if (mustuse->is_volatile()) {
        clobberAccess(ir, exact_access, exact_occs, inexact_access);
        return true;
    }

    if (mustuse->is_exact()) {
        if (!m_dont_promote.is_overlap(mustuse)) {
            //Exact memory access.
            addExactAccess(exact_access, exact_occs, mustuse, ir);
        }
        return true;
    }

    if (m_dont_promote.is_overlap(mustuse)) {
        return true;
    }

    //MD is inexact. Check if it is loop invariant.
    if (ir->is_starray() || !checkArrayIsLoopInvariant(ir, li)) {
        //If ir is STARRAY that modify inexact MD.
        //It may clobber all other array with same array base.
        clobberAccess(ir, exact_access, exact_occs, inexact_access);
        return true;
    }

    TabIter<IR*> ti;
    for (IR * ref = inexact_access.get_first(ti);
         ref != NULL; ref = inexact_access.get_next(ti)) {
        UINT st = analyzeArrayStatus(ir, ref);
        if (st == RP_SAME_ARRAY) { continue; }
        if (st == RP_DIFFERENT_ARRAY) { continue; }

        //The result can not be promoted.
        //Check the promotable candidates if current stmt modify the related MD.
        clobberAccess(ir, exact_access, exact_occs, inexact_access);
        return true;
    }
    addInexactAccess(inexact_access, ir);
    return true;
}


//Return true if the caller can keep doing the analysis.
//That means there are no memory referrences clobbered the
//candidate in exact_occs.
//Return false if find unpromotable memory reference, this may
//prevent entire loop be promoted.
//ir: stmt or expression to be handled.
bool RegPromot::handleGeneralRef(IR * ir,
                                 LI<IRBB> const* li,
                                 OUT TMap<MD const*, IR*> & exact_access,
                                 OUT List<IR*> & exact_occs,
                                 OUT TTab<IR*> & inexact_access)
{
    ASSERT0(ir->isMemoryRef());
    ASSERT0(!ir->isArrayOp());
    if (ir->getOffset() != 0) {
        //TODO:not yet support, x is MC type.
        //clobberAccess(ir, exact_access, exact_occs,
        //                    inexact_access);
        //return true;
    }
    MD const* mustref = ir->getRefMD();
    //if ((mustref == NULL || !mustref->is_effect()) && ir->is_stmt()) {
    //    //It is dispensable to clobber access if entire loop is not analysable.
    //    clobberAccess(ir, exact_access, exact_occs, inexact_access);
    //
    //    //TO BE CONFIRMED:ineffect MD should not lead entire loop unanalyzable.
    //    //return false;
    //}
    if (mustref != NULL && mustref->is_volatile()) {
        clobberAccess(ir, exact_access, exact_occs, inexact_access);
        return true;
    }
    if (ir->id()==12) {
        int a = 0;
    }
    //if (mustref != NULL && mustref->is_exact()) {
    //        if (ir != NULL) {
    //            if (ir->is_stmt()) {
    //                //Mark exact memory access to be promotable.
    //                addExactAccess(exact_access, exact_occs, mustref, ir);
    //            } else if (isLoopInvariant(ir, li, m_rg)) {
    //                //Mark exact memory access to be promotable.
    //                addExactAccess(exact_access, exact_occs, mustref, ir);
    //            }
    //        }
    //    } else {
    //        addDontPromote(mustref);
    //    }
    //    //Need to analyze MayRef.
      
    //    //mustref already be in the dont_promot table.
    //    //return true;
    //}

    if (mustref != NULL && m_dont_promote.is_overlap(mustref)) {
        //If ir should not be promoted, then all the others mem-ref
        //that overlapped with it should not be promoted too.
        clobberAccess(ir, exact_access, exact_occs, inexact_access);
        return true;
    }

    if (ir->is_ild() || ir->is_ist()) {
        //MD is inexact. Check if it is loop invariant.
        if (!checkIndirectAccessIsLoopInvariant(ir, li)) {
            clobberAccess(ir, exact_access, exact_occs, inexact_access);
            return true;
        }
    } else if (ir->is_ld()) {
        if (!isLoopInvariant(ir, li, m_rg)) {
            clobberAccess(ir, exact_access, exact_occs, inexact_access);
            return true;
        }
    } else if (ir->is_st()) {
        ; //nothing to do
    } else {
        UNREACHABLE(); //TODO
    }

    if (ir->is_stmt()) {
        //Determine wherther current ir clobber elements in access list.
        TabIter<IR*> ti;
        for (IR * ref = inexact_access.get_first(ti);
             ref != NULL; ref = inexact_access.get_next(ti)) {
            if (m_gvn->isSameMemLoc(ir, ref)) { continue; }
            if (m_gvn->isDiffMemLoc(ir, ref)) { continue; }

            //Current ir can not be promoted. Check the promotable candidates
            //if current ir overrided related MDs.
            clobberAccess(ir, exact_access, exact_occs, inexact_access);
            return true;
        }
        if (mustref == NULL || !mustref->is_exact()) {
            addInexactAccess(inexact_access, ir);
        } else if (mustref != NULL && mustref->is_exact()) {
            if (!m_dont_promote.is_overlap(mustref)) {
                if (ir->is_stmt()) {
                    //Mark exact memory access to be promotable.
                    addExactAccess(exact_access, exact_occs, mustref, ir);
                } else if (isLoopInvariant(ir, li, m_rg)) {
                    //Mark exact memory access to be promotable.
                    addExactAccess(exact_access, exact_occs, mustref, ir);
                }
            } else {
                addDontPromote(mustref);
            }
            //Need to analyze MayRef.
        
            //mustref already be in the dont_promot table.
            //return true;
        }
        return true;
    }

    ASSERT0(ir->is_exp());
    if (mustref == NULL || !mustref->is_exact()) {
        addInexactAccess(inexact_access, ir);
    } else if (mustref != NULL && mustref->is_exact()) {
        if (!m_dont_promote.is_overlap(mustref)) {
            if (ir != NULL) {
                if (ir->is_stmt()) {
                    //Mark exact memory access to be promotable.
                    addExactAccess(exact_access, exact_occs, mustref, ir);
                } else if (isLoopInvariant(ir, li, m_rg)) {
                    //Mark exact memory access to be promotable.
                    addExactAccess(exact_access, exact_occs, mustref, ir);
                }
            }
        } else {
            addDontPromote(mustref);
        }
        //Need to analyze MayRef.
    
        //mustref already be in the dont_promot table.
        //return true;
    }
    return true;
}


void RegPromot::addDontPromote(MD const* md)
{
    ASSERT0(md);
    m_dont_promote.bunion(md, *getSBSMgr());
}


void RegPromot::addDontPromote(MDSet const& mds)
{
    m_dont_promote.bunion(mds, *getSBSMgr());
}


//'ir' can not be promoted.
//Check the promotable candidates if ir overlapped with the related
//MD and MDSet.
//This function consider both MustRef MD and MayRef MDSet.
void RegPromot::clobberAccess(IR * ir,
                              OUT TMap<MD const*, IR*> & exact_access,
                              OUT List<IR*> & exact_occs,
                              OUT TTab<IR*> & inexact_access)
{
    DUMMYUSE(exact_occs);
    MD const* mustref = ir->getRefMD();
    MDSet const* mayref = ir->getRefMDSet();

    if (mustref != NULL) { addDontPromote(mustref); }
    if (mayref != NULL) { addDontPromote(*mayref); }

    TMapIter<MD const*, IR*> iter;
    Vector<MD const*> need_to_be_removed;
    INT cnt = 0;
    if (mustref != NULL) {
        for (MD const* md = exact_access.get_first(iter, NULL);
             md != NULL; md = exact_access.get_next(iter, NULL)) {
            if (mustref == md || mustref->is_overlap(md)) {
                //Current ir may modify the candidate's md.
                //We think the candidate is not suite to promot any more.
                need_to_be_removed.set(cnt, md);
                cnt++;
            }
        }
    }
    if (mayref != NULL && !mayref->is_empty()) {
        for (MD const* md = exact_access.get_first(iter, NULL);
             md != NULL; md = exact_access.get_next(iter, NULL)) {
            if (mayref->is_contain(md)) {
                //Current ir may modify the candidate's md.
                //We think the candidate is not suite to promot any more.
                need_to_be_removed.set(cnt, md);
                cnt++;
            }
        }
    }

    for (cnt = cnt - 1; cnt >= 0; cnt--) {
        MD const* md = need_to_be_removed.get(cnt);
        exact_access.remove(md);
        //Do not remove occ in occ_list here.
        //We will remove the related occ in occ_list after
        //scanBB() all at once.
    }

    TabIter<IR*> iter2;
    Vector<IR*> need_to_be_removed2;
    cnt = 0;
    if (mustref != NULL) {
        for (IR * acc = inexact_access.get_first(iter2);
             acc != NULL; acc = inexact_access.get_next(iter2)) {
            MD const* acc_md = acc->getRefMD();
            MDSet const* acc_mds = acc->getRefMDSet();
            if (acc_md != NULL) {
                if (mustref == acc_md || mustref->is_overlap(acc_md)) {
                    //ir is not suite to promot any more, all mds which
                    //overlapped with it are also not promotable.
                    need_to_be_removed2.set(cnt, acc);
                    cnt++;
                }
            } else if (acc_mds != NULL && acc_mds->is_overlap(mustref, m_rg)) {
                //ir is not suite to promot any more, all mds which
                //overlapped with it are also not promotable.
                need_to_be_removed2.set(cnt, acc);
                cnt++;
            }
        }
    }
    if (mayref != NULL && !mayref->is_empty()) {
        for (IR * acc = inexact_access.get_first(iter2);
             acc != NULL; acc = inexact_access.get_next(iter2)) {
            MD const* acc_md = acc->getRefMD();
            MDSet const* acc_mds = acc->getRefMDSet();
            if ((acc_md != NULL && mayref->is_overlap(acc_md, m_rg)) ||
                (acc_mds != NULL &&
                 (acc_mds == mayref || mayref->is_intersect(*acc_mds)))) {
                //ir is not suite to promot any more, all mds which
                //overlapped with it are also not promotable.
                need_to_be_removed2.set(cnt, acc);
                cnt++;
            }
        }
    }

    for (cnt = cnt - 1; cnt >= 0; cnt--) {
        IR * e = need_to_be_removed2.get(cnt);
        inexact_access.remove(e);
    }
}


bool RegPromot::checkIndirectAccessIsLoopInvariant(IN IR * ir, LI<IRBB> const* li)
{
    ASSERT0(li);
    if (ir->is_ild()) {
        if (!isLoopInvariant(ILD_base(ir), li, m_rg)) {
            return false;
        }
    } else if (ir->is_ist()) {
        if (!isLoopInvariant(IST_base(ir), li, m_rg)) {
            return false;
        }
    }
    return true;
}


//Determine whether the memory reference is same object or different.
UINT RegPromot::analyzeIndirectAccessStatus(IR const* ref1, IR const* ref2)
{
    IR const* base1 = NULL;
    if (ref1->is_ild()) {
        base1 = ILD_base(ref1);
    } else if (ref1->is_ist()) {
        base1 = IST_base(ref1);
    } else {
        return RP_UNKNOWN;
    }

    IR const* base2 = NULL;
    if (ref2->is_ild()) {
        base2 = ILD_base(ref2);
    } else if (ref2->is_ist()) {
        base2 = IST_base(ref2);
    } else {
        return RP_UNKNOWN;
    }

    ASSERT0(base1->is_ptr() && base2->is_ptr());

    ASSERT0(m_gvn);

    VN const* vn1 = m_gvn->mapIR2VN(base1);
    VN const* vn2 = m_gvn->mapIR2VN(base2);
    if (vn1 == NULL || vn2 == NULL) { return RP_UNKNOWN; }

    UINT tysz1 = ref1->getTypeSize(m_tm);
    UINT tysz2 = ref2->getTypeSize(m_tm);
    UINT ofst1 = ref1->getOffset();
    UINT ofst2 = ref2->getOffset();
    if ((((ofst1 + tysz1) <= ofst2) ||
        ((ofst2 + tysz2) <= ofst1))) {
        return RP_DIFFERENT_OBJ;
    }
    if (ofst1 == ofst2 && tysz1 == tysz2) {
        return RP_SAME_OBJ;
    }
    return RP_UNKNOWN;
}


//Find promotable candidate memory references.
//Return true if current memory referense does not clobber other
//candidate in list. Or else return false means there are ambiguous
//memory reference.
bool RegPromot::scanOpnd(IR * ir,
                         LI<IRBB> const* li,
                         OUT TMap<MD const*, IR*> & exact_access,
                         OUT List<IR*> & exact_occs,
                         OUT TTab<IR*> & inexact_access,
                         IRIter & ii)
{
    ii.clean();
    for (IR * x = iterRhsInit(ir, ii);
         x != NULL; x = iterRhsNext(ii)) {
        if (!x->isMemoryOpnd() || x->is_pr()) { continue; }
        if (x->is_array()) {
            if (!handleArrayRef(x, li, exact_access, exact_occs,
                                inexact_access)) {
                return false;
            }
            continue;
        }

        if (!handleGeneralRef(x, li, exact_access, exact_occs,
                              inexact_access)) {
            return false;
        }
    }
    return true;
}


//Return false if find unpromotable memory reference, this may
//prevent entire loop be promoted.
bool RegPromot::scanResult(IN IR * ir,
                           LI<IRBB> const* li,
                           OUT TMap<MD const*, IR*> & exact_access,
                           OUT List<IR*> & exact_occs,
                           OUT TTab<IR*> & inexact_access)
{
    switch (ir->getCode()) {
    case IR_ST:
    case IR_IST:
        return handleGeneralRef(ir, li, exact_access, exact_occs,
                                inexact_access);
    case IR_STARRAY:
        return handleArrayRef(ir, li, exact_access, exact_occs,
                              inexact_access);
    default: break;
    }
    return true;
}


//Scan BB and find promotable memory reference.
//If this function find unpromotable access that with ambiguous
//memory reference, all relative promotable accesses which in list
//will not be promoted.
//e.g:a[0] = ...
//    a[i] = ...
//    a[0] is promotable, but a[i] is not, then a[0] can not be promoted.
//If there exist memory accessing that we do not know where it access,
//whole loop is unpromotable.
//Return false if loop is unpromotable.
bool RegPromot::scanBB(IN IRBB * bb,
                       LI<IRBB> const* li,
                       OUT TMap<MD const*, IR*> & exact_access,
                       OUT TTab<IR*> & inexact_access,
                       OUT List<IR*> & exact_occs,
                       IRIter & ii)
{
    for (IR * ir = BB_last_ir(bb);
         ir != NULL; ir = BB_prev_ir(bb)) {
        if (ir->isCallStmt() && !ir->isReadOnly()) {
            return false;
        }
    }
    for (IR * ir = BB_first_ir(bb);
         ir != NULL; ir = BB_next_ir(bb)) {
        if (!ir->isContainMemRef()) { continue; }
        if (ir->is_region()) { return false; }
        if (!scanResult(ir, li, exact_access, exact_occs, inexact_access)) {
            return false;
        }
        if (!scanOpnd(ir, li, exact_access, exact_occs,
                      inexact_access, ii)) {
            return false;
        }
    }
    return true;
}


IRBB * RegPromot::findSingleExitBB(LI<IRBB> const* li)
{
    IRBB * head = LI_loop_head(li);
    ASSERT0(head);
    ASSERT0(BB_last_ir(head));
    IR * x = BB_last_ir(head);
    if (x->isMultiConditionalBr()) {
        //If the end ir of loop header is multi-conditional-branch, we
        //are not going to handle it.
        return NULL;
    }

    IRBB * check_bb = head;
    List<IRBB*> succs;
    for (;;) {
        IR * z = BB_last_ir(check_bb);
        m_cfg->get_succs(succs, check_bb);
        if (z->isConditionalBr()) {
            ASSERT0(succs.get_elem_count() == 2);
            IRBB * succ = succs.get_head();
            if (!li->isInsideLoop(BB_id(succ))) {
                return succ;
            }

            succ = succs.get_next();
            if (!li->isInsideLoop(BB_id(succ))) {
                return succ;
            }

            return NULL;
        }

        if (z->isCallStmt() || z->isUnconditionalBr() || z->is_st() ||
            z->is_ist() || z->isWritePR()) {
            if (succs.get_elem_count() > 1) {
                //Stmt may throw exception.
                return NULL;
            }
            check_bb = succs.get_head();
            continue;
        }

        break; //Go out of the loop.
    }

    return NULL;
}


//Replace the use for oldir to newir.
//If oldir is not leaf, cut off the du chain to all of its kids.
void RegPromot::replaceUseForTree(IR * oldir, IR * newir)
{
    ASSERT0(oldir->is_exp() && newir->is_exp());
    if (oldir->is_ld()) {
        m_du->changeUse(newir, oldir, m_rg->getMiscBitSetMgr());
    } else if (oldir->is_ild()) {
        m_du->removeUseFromDefset(ILD_base(oldir));
        m_du->changeUse(newir, oldir, m_rg->getMiscBitSetMgr());
    } else if (oldir->is_array()) {
        m_du->removeUseFromDefset(ARR_base(oldir));
        m_du->removeUseFromDefset(ARR_sub_list(oldir));
        m_du->changeUse(newir, oldir, m_rg->getMiscBitSetMgr());
    } else {
        UNREACHABLE(); //TODO
    }
}


void RegPromot::handleRestore2Mem(
    TTab<IR*> & restore2mem,
    TMap<IR*, IR*> & delegate2stpr,
    TMap<IR*, IR*> & delegate2pr,
    TMap<IR*, DUSet*> & delegate2use,
    TMap<IR*, SList<IR*>*> & delegate2has_outside_uses_ir_list,
    TabIter<IR*> & ti,
    IRBB * exit_bb)
{
    //Restore value from delegate PR to delegate memory object.
    ti.clean();
    for (IR * delegate = restore2mem.get_first(ti);
         delegate != NULL; delegate = restore2mem.get_next(ti)) {
        IR * pr = m_rg->dupIRTree(delegate2pr.get(delegate));
        m_rg->allocRefForPR(pr);

        IR * stpr = delegate2stpr.get(delegate);
        buildPRDUChain(stpr, pr);

        IR * stmt = NULL;
        switch (delegate->getCode()) {
        case IR_ARRAY:
        case IR_STARRAY: {
            ASSERTN(delegate->getOffset() == 0, ("TODO: not yet support."));

            //Prepare base and subscript expression list.
            IR * base = m_rg->dupIRTree(ARR_base(delegate));
            IR * sublist = m_rg->dupIRTree(ARR_sub_list(delegate));

            //Copy DU chain and MD reference.
            m_du->copyRefAndAddDUChain(base, ARR_base(delegate), true);
            m_du->copyRefAndAddDUChain(sublist, ARR_sub_list(delegate), true);

            //Build Store Array operation.
            stmt = m_rg->buildStoreArray(
                base, sublist, IR_dt(delegate),
                ARR_elemtype(delegate),
                ((CArray*)delegate)->getDimNum(),
                ARR_elem_num_buf(delegate),
                pr);

            //Copy MD reference for store array operation.
            stmt->copyRef(delegate, m_rg);
            break;
        }
        case IR_IST: {
            IR * lhs = m_rg->dupIRTree(IST_base(delegate));
            m_du->copyRefAndAddDUChain(lhs, IST_base(delegate), true);

            stmt = m_rg->buildIStore(lhs, pr,
                IST_ofst(delegate), IR_dt(delegate));
            break;
        }
        case IR_ST:
            stmt = m_rg->buildStore(ST_idinfo(delegate),
                IR_dt(delegate), ST_ofst(delegate), pr);
            break;
        case IR_ILD: {
            IR * base = m_rg->dupIRTree(ILD_base(delegate));
            stmt = m_rg->buildIStore(base, pr,
                ILD_ofst(delegate), IR_dt(delegate));
            m_du->copyRefAndAddDUChain(base, ILD_base(delegate), true);
            break;
        }
        case IR_LD:
            stmt = m_rg->buildStore(LD_idinfo(delegate),
                                    IR_dt(delegate),
                                    LD_ofst(delegate), pr);
            break;
        default: UNREACHABLE(); //Unsupport.
        }

        ASSERT0(stmt);

        //Compute the DU chain of stmt.
        DUSet const* set = delegate2use.get(delegate);
        if (set != NULL) {
            m_du->copyDUSet(stmt, set);
            m_du->unionDef(set, stmt);
        }

        stmt->copyRef(delegate, m_rg);

        SList<IR*> * irlst = delegate2has_outside_uses_ir_list.get(delegate);
        ASSERT0(irlst);

        for (xcom::SC<IR*> * sc = irlst->get_head();
             sc != irlst->end(); sc = irlst->get_next(sc)) {
            IR * def = sc->val();
            ASSERT0(def && def->is_stpr());
            ASSERT0(STPR_no(def) == PR_no(pr));
            buildPRDUChain(def, pr);
        }

        BB_irlist(exit_bb).append_head(stmt);
    }
}


void RegPromot::buildPRDUChain(IR * def, IR * use)
{
    ASSERT0(def && def->is_stpr());
    ASSERT0(STPR_no(def) == PR_no(use));
    if (usePRSSADU()) {
        //WORKAROUND|FIXME:For now, we do not support incremental update PRSSA.
        //rebuild PRSSA if DU changed.
        //m_prssamgr->buildDUChain(def, use);
        m_need_rebuild_prssa = true;
    } else {
        m_du->buildDUChain(def, use);
    }
}


//Return true if 'ir' can be promoted.
//Note ir must be memory reference.
bool RegPromot::isPromotable(IR const* ir) const
{
    ASSERT0(ir->isMemoryRef());
    //I think the reference that may throw is promotable.
    if (IR_has_sideeffect(ir) && IR_no_move(ir)) { return false; }
    return true;
}


//Return true if there is IR be promoted, otherwise return false.
bool RegPromot::promoteExactAccess(LI<IRBB> const* li,
                                   IRIter & ii,
                                   TabIter<IR*> & ti,
                                   IRBB * preheader,
                                   IRBB * exit_bb,
                                   TMap<MD const*, IR*> & exact_access,
                                   List<IR*> & exact_occs)
{
    ASSERT0(preheader && exit_bb && li);

    //Map IR expression to STPR which generate the scalar value.
    //Map IR expression to STPR in preheader BB.
    TMap<IR*, IR*> delegate2stpr;

    //Map IR expression to promoted PR.
    TMap<IR*, IR*> delegate2pr;

    //Map delegate to a list of references which use memory outside the loop.
    TMap<IR*, SList<IR*>*> delegate2has_outside_uses_ir_list;

    //Map delegate to USE set.
    TMap<IR*, DUSet*> delegate2use;

    //Map delegate to DEF set.
    TMap<IR*, DUSet*> delegate2def;

    List<IR*> fixup_list; //record the IR that need to fix up duset.

    xcom::BitSet * bbset = LI_bb_set(li);
    CHECK_DUMMYUSE(bbset);
    ASSERT0(!bbset->is_empty()); //loop is empty.

    DefMiscBitSetMgr * sbs_mgr = m_rg->getMiscBitSetMgr();

    TMapIter<MD const*, IR*> mi;
    IR * delegate = NULL;
    for (MD const* md = exact_access.get_first(mi, &delegate); md != NULL;) {
        ASSERT0(delegate);
        if (!isPromotable(delegate)) {
            //Do not promote the reference.
            MD const* next_md = exact_access.get_next(mi, &delegate);
            exact_access.remove(md);
            md = next_md;
            continue;
        }

        createDelegateInfo(delegate, delegate2pr,
            delegate2has_outside_uses_ir_list);
        md = exact_access.get_next(mi, &delegate);
        ASSERT0(!((md == NULL) ^ (delegate == NULL)));
    }

    if (exact_access.get_elem_count() == 0) { return false; }

    for (IR * ref = exact_occs.get_head();
         ref != NULL; ref = exact_occs.get_next()) {
         MD const* md = ref->getRefMD();
        ASSERT0(md && md->is_exact());

        //Get the unique delegate.
        IR * delegate2 = exact_access.get(md);
        if (delegate2 == NULL) {
            continue;
        }

        computeOuterDefUse(ref, delegate2, delegate2def,
            delegate2use, sbs_mgr, li);
    }

    mi.clean();
    for (exact_access.get_first(mi, &delegate);
         delegate != NULL; exact_access.get_next(mi, &delegate)) {
        IR * pr = delegate2pr.get(delegate);
        ASSERT0(pr);
        handlePrelog(delegate, pr, delegate2stpr, delegate2def, preheader);
    }

    //Map delegate MD to it define stmt in the loop.
    //These MD need to restore to memory in exit BB.
    TTab<IR*> restore2mem;

    //This table record the IRs which should not be processed any more.
    //They may be freed.
    //e.g: a[i], if array referrence is freed, the occurrence of variable
    //i also be freed.
    PromotedTab promoted(m_rg->getMiscBitSetMgr()->getSegMgr());

    for (IR * ref = exact_occs.get_head();
         ref != NULL; ref = exact_occs.get_next()) {
        if (promoted.is_contain(IR_id(ref))) { continue; }

        MD const* md = ref->getRefMD();
        ASSERT0(md && md->is_exact());

        //Get the unique delegate.
        IR * delegate2 = exact_access.get(md);
        if (delegate2 == NULL) { continue; }

        IR * delegate_pr = delegate2pr.get(delegate2);
        ASSERT0(delegate_pr);

        handleAccessInBody(ref, delegate2, delegate_pr,
            delegate2has_outside_uses_ir_list, restore2mem,
            fixup_list, delegate2stpr, li, ii);

        //Each memory reference in the tree has been promoted.
        promoted.add(ref, ii);
    }

    ASSERT0(verifyIRandBB(m_rg->getBBList(), m_rg));

    handleRestore2Mem(restore2mem, delegate2stpr, delegate2pr, delegate2use,
                      delegate2has_outside_uses_ir_list, ti, exit_bb);

    removeRedundantDUChain(fixup_list);

    freeLocalStruct(delegate2use, delegate2def, delegate2pr, sbs_mgr);

    //Note, during the freeing process, the ref should not be
    //freed if it is IR_UNDEF.
    //This is because the ref is one of the kid of some other
    //stmt/exp which has already been freed.
    for (IR * ref = exact_occs.get_head();
         ref != NULL; ref = exact_occs.get_next()) {
        if (ref->is_undef()) {
            //ref is the kid of other stmt/exp, and
            //that stmt/exp has been freed.
            continue;
        }

        MD const* md = ref->getRefMD();
        ASSERT0(md);

        IR * delegate2 = exact_access.get(md);
        if (delegate2 == NULL) {
            //If delegate2 does not exist, the reference still in its place.
            continue;
        }

        m_rg->freeIRTree(ref);
    }
    return true;
}


//Return true if some node at IR tree may throw exception.
bool RegPromot::isMayThrow(IR * ir, IRIter & iter)
{
    iter.clean();
    IR const* k = iterInit(ir, iter);
    for (; k != NULL; k = iterNext(iter)) {
        if (k->isMemoryRef() && !k->isWritePR() && !k->isReadPR()) {
            return true;
        } else if (k->isCallStmt()) {
            return true;
        }

        if (k->is_div() || k->is_rem() || k->is_mod()) {
            return true;
        }
    }
    return false;
}


bool RegPromot::hasLoopOutsideUse(IR const* stmt, LI<IRBB> const* li)
{
    ASSERT0(stmt->is_stmt() && !stmt->isWritePR());
    if (useMDSSADU()) {
        return m_mdssamgr->hasUse(stmt);
    }
    
    DUSet const* useset = stmt->readDUSet();
    if (useset == NULL) { return false; }

    DUIter di = NULL;
    for (INT i = useset->get_first(&di);
        i >= 0; i = useset->get_next(i, &di)) {
        IR const* u = m_rg->getIR(i);
        ASSERT0(u->is_exp());
        ASSERT0(u->getStmt());
        IR * s = u->getStmt();
        ASSERT0(s->getBB());
        if (!li->isInsideLoop(BB_id(s->getBB()))) {
            return true;
        }
    }    
    return false;
}


//Fix up DU chain if there exist untrue dependence.
//'fixup_list': record the IR stmt/exp that need to fix up.
void RegPromot::removeRedundantDUChain(List<IR*> & fixup_list)
{
    Vector<IR const*> * rmvec = new Vector<IR const*>(10);
    for (IR * ref = fixup_list.get_head();
         ref != NULL; ref = fixup_list.get_next()) {
        ASSERT0(ref->is_stpr() || ref->is_pr());

        DUSet const* duset = ref->readDUSet();
        if (duset == NULL) { continue; }

        DUIter di = NULL;
        UINT cnt = 0;
        if (ref->is_stpr()) {
            UINT prno = STPR_no(ref);

            if (STPR_ssainfo(ref) != NULL) {
                SSAUseIter useiter = NULL;
                for (INT i = SSA_uses(STPR_ssainfo(ref)).get_first(&useiter);
                     useiter != NULL;
                     i = SSA_uses(STPR_ssainfo(ref)).get_next(i, &useiter)) {
                    IR const* use = m_rg->getIR(i);

                    ASSERT0(use->is_exp());
                    if (use->is_pr()) {
                        if (PR_no(use) == prno) { continue; }

                        //DU manager may be confused and build the redundant
                        //chain because of inexact indirect memory access.
                        //Here, we remove the dependence that confirmed to be
                        //redundant.
                        rmvec->set(cnt++, use);
                        continue;
                    }

                    ASSERT0(!use->isMemoryOpnd());

                    rmvec->set(cnt++, use);
                }
            } else {
                for (INT i = duset->get_first(&di);
                     i >= 0; i = duset->get_next(i, &di)) {
                    IR const* use = m_rg->getIR(i);
                    ASSERT0(use->is_exp());
                    if (use->is_pr()) {
                        if (PR_no(use) == prno) { continue; }

                        //DU manager may be confused and build the redundant
                        //chain because of inexact indirect memory access.
                        //Here, we remove the dependence that confirmed to be
                        //redundant.
                        rmvec->set(cnt++, use);
                        continue;
                    }

                    ASSERT0(use->is_ild() || use->is_array() || use->is_ld());
                    rmvec->set(cnt++, use);
                }
            }

            //Remove redundant du chain.
            for (UINT i = 0; i < cnt; i++) {
                m_du->removeDUChain(ref, rmvec->get(i));
            }
        } else {
            ASSERT0(ref->is_pr());
            UINT prno = PR_no(ref);

            if (PR_ssainfo(ref) != NULL) {
                if (PR_ssainfo(ref)->getDef() != NULL) {
                    //SSA def must be same PR_no with the SSA use.
                    ASSERT0(PR_ssainfo(ref)->getDef()->getPrno() == prno);
                }
            } else {
                for (INT i = duset->get_first(&di);
                     i >= 0; i = duset->get_next(i, &di)) {
                    IR const* d = m_rg->getIR(i);
                    ASSERT0(d->is_stmt());

                    if ((d->isWritePR() || d->isCallHasRetVal()) &&
                        d->getPrno() == prno) {
                        continue;
                    }

                    //DU manager may be confused and build the redundant
                    //chain because of inexact indirect memory access.
                    //Here, we remove the dependence that confirmed to be
                    //redundant.

                    //If the result PR of call does not coincide with ref.
                    //Remove the redundant chain.
                    rmvec->set(cnt++, d);
                }
            }

            //Remove redundant du chain.
            for (UINT i = 0; i < cnt; i++) {
                m_du->removeDUChain(rmvec->get(i), ref);
            }
        }
    }

    delete rmvec;
}


//fixup_list: record the IR that need to fix up duset.
void RegPromot::handleAccessInBody(
    IR * ref,
    IR * delegate,
    IR const* delegate_pr,
    TMap<IR*, SList<IR*>*> const& delegate2has_outside_uses_ir_list,
    OUT TTab<IR*> & restore2mem,
    OUT List<IR*> & fixup_list,
    TMap<IR*, IR*> const& delegate2stpr,
    LI<IRBB> const* li,
    IRIter & ii)
{
    ASSERT0(ref && delegate && delegate_pr && li);
    IR * stmt = NULL;
    if (ref->is_stmt()) {
        stmt = ref;
    } else {
        stmt = ref->getStmt();
        ASSERT0(stmt);
    }

    switch (ref->getCode()) {
    case IR_STARRAY: {
            bool has_use = false;

            //Note, may be some USE of 'ref' has already been promoted to
            //PR, but it doesn't matter, you don't need to check the truely
            //dependence here, since we just want to see whether
            //there exist outer of loop references to this stmt. And all
            //the same group memory references will be promoted to PR after
            //the function return.
            if (hasLoopOutsideUse(ref, li) || mayBeGlobalRef(ref)) {
                has_use = true;
            }

            //Substitute STPR(ARRAY) for STARRAY.
            IR * stpr = m_rg->buildStorePR(PR_no(delegate_pr),
                                           IR_dt(delegate_pr),
                                           STARR_rhs(ref));

            m_rg->allocRefForPR(stpr);

            //New IR has same VN with original one.
            m_gvn->setMapIR2VN(stpr, m_gvn->mapIR2VN(ref));

            if (has_use) {
                restore2mem.append_and_retrieve(delegate);
                SList<IR*> * irlst =
                    delegate2has_outside_uses_ir_list.get(delegate);
                ASSERT0(irlst);
                irlst->append_head(stpr);
            }

            m_du->removeUseFromDefset(ref);

            //Change DU chain.
            m_du->changeDef(stpr, ref, m_rg->getMiscBitSetMgr());
            fixup_list.append_tail(stpr);

            ref->setRHS(NULL);

            IRBB * refbb = ref->getBB();
            ASSERT0(refbb);
            IRListIter ct = NULL;
            BB_irlist(refbb).find(ref, &ct);
            ASSERT0(ct != NULL);

            BB_irlist(refbb).insert_after(stpr, ct);
            BB_irlist(refbb).remove(ct);

            //Do not free stmt here since it will be freed later.
            //m_rg->freeIRTree(ref);
        }
        break;
    case IR_IST:
    case IR_ST: {
            ASSERT0(ref == stmt);
            bool has_use = false;

            //Note, may be some USE of 'ref' has already been promoted to
            //PR, but it doesn't matter, you don't need to check the truely
            //dependence here, since we just want to see whether
            //there exist outer of loop references to this stmt. And all
            //the same group memory reference will be promoted to PR after
            //the function return.
            if (hasLoopOutsideUse(ref, li) || mayBeGlobalRef(ref)) {
                has_use = true;
            }

            //Substitute STPR(exp) for IST(exp) or
            //substitute STPR(exp) for ST(exp).

            IR * stpr = m_rg->buildStorePR(PR_no(delegate_pr),
                                           IR_dt(delegate_pr),
                                           ref->getRHS());
            m_rg->allocRefForPR(stpr);

            //New IR has same VN with original one.
            m_gvn->setMapIR2VN(stpr, m_gvn->mapIR2VN(stmt));

            if (has_use) {
                restore2mem.append_and_retrieve(delegate);
                SList<IR*> * irlst = delegate2has_outside_uses_ir_list.
                    get(delegate);
                ASSERT0(irlst);
                irlst->append_head(stpr);
            }

            if (ref->is_ist()) {
                m_du->removeUseFromDefset(IST_base(ref));
            }

            //Change DU chain.
            m_du->changeDef(stpr, ref, m_rg->getMiscBitSetMgr());
            fixup_list.append_tail(stpr);

            ref->setRHS(NULL);

            IRBB * stmt_bb = ref->getBB();
            ASSERT0(stmt_bb);
            IRListIter ct = NULL;
            BB_irlist(stmt_bb).find(ref, &ct);
            ASSERT0(ct != NULL);

            BB_irlist(stmt_bb).insert_after(stpr, ct);
            BB_irlist(stmt_bb).remove(ct);

            //Do not free stmt here since it will be freed later.
        }
        break;
    default: {
            ASSERT0(ref->is_ild() || ref->is_ld() || ref->is_array());
            IR * pr = m_rg->dupIR(delegate_pr);
            m_rg->allocRefForPR(pr);

            //New IR has same VN with original one.
            m_gvn->setMapIR2VN(pr, m_gvn->mapIR2VN(ref));

            //Find the stpr that correspond to delegate MD,
            //and build DU chain bewteen stpr and new ref PR.
            replaceUseForTree(ref, pr);
            fixup_list.append_tail(pr);

            //Add du chain between new PR and the generated STPR.
            IR * stpr = delegate2stpr.get(delegate);
            ASSERT0(stpr);
            if (m_prssamgr != NULL) {
                m_prssamgr->buildDUChain(stpr, pr);
            } else {
                m_du->buildDUChain(stpr, pr);
            }

            ASSERT0(IR_parent(ref));
            bool r = IR_parent(ref)->replaceKid(ref, pr, false);
            CHECK_DUMMYUSE(r);

            if (stmt->isMayThrow() && !isMayThrow(stmt, ii)) {
                IR_may_throw(stmt) = false;
            }
            //Do not free stmt here since it will be freed later.
        }
    }
}


void RegPromot::handlePrelog(IR * delegate, IR * pr,
                             TMap<IR*, IR*> & delegate2stpr,
                             TMap<IR*, DUSet*> & delegate2def,
                             IRBB * preheader)
{
    IR * rhs = NULL;
    IR * stpr = NULL;
    if (delegate->is_array()) {
        //Load array value into PR.
        rhs = m_rg->dupIRTree(delegate);
        m_du->copyRefAndAddDUChain(ARR_base(rhs), ARR_base(delegate), true);
        m_du->copyRefAndAddDUChain(ARR_sub_list(rhs),
                                ARR_sub_list(delegate), true);
        stpr = m_rg->buildStorePR(PR_no(pr), IR_dt(pr), rhs);
    } else if (delegate->is_starray()) {
        //Load array value into PR.
        rhs = m_rg->buildArray(m_rg->dupIRTree(ARR_base(delegate)),
            m_rg->dupIRTree(ARR_sub_list(delegate)),
            IR_dt(delegate),
            ARR_elemtype(delegate),
            ((CArray*)delegate)->getDimNum(),
            ARR_elem_num_buf(delegate));
        m_du->copyRefAndAddDUChain(ARR_base(rhs), ARR_base(delegate), true);
        m_du->copyRefAndAddDUChain(ARR_sub_list(rhs), ARR_sub_list(delegate), true);
        stpr = m_rg->buildStorePR(PR_no(pr), IR_dt(pr), rhs);
    } else if (delegate->is_ist()) {
        //Load indirect value into PR.
        rhs = m_rg->buildILoad(m_rg->dupIRTree(IST_base(delegate)),
            IST_ofst(delegate), IR_dt(delegate));
        m_du->copyRefAndAddDUChain(ILD_base(rhs), IST_base(delegate), true);
        stpr = m_rg->buildStorePR(PR_no(pr), IR_dt(pr), rhs);
    } else if (delegate->is_ild()) {
        //Load indirect value into PR.
        rhs = m_rg->dupIRTree(delegate);
        m_du->copyRefAndAddDUChain(ILD_base(rhs), ILD_base(delegate), true);
        stpr = m_rg->buildStorePR(PR_no(pr), IR_dt(pr), rhs);
    } else if (delegate->is_ld()) {
        //Load scalar into PR.
        rhs = m_rg->dupIRTree(delegate);
        m_du->copyRefAndAddDUChain(rhs, delegate, false);
        stpr = m_rg->buildStorePR(PR_no(pr), IR_dt(pr), rhs);
    } else if (delegate->is_st()) {
        //Load scalar into PR.
        rhs = m_rg->buildLoad(ST_idinfo(delegate));
        LD_ofst(rhs) = ST_ofst(delegate);
        stpr = m_rg->buildStorePR(PR_no(pr), IR_dt(pr), rhs);
    } else {
        UNREACHABLE(); //unsupport.
    }
    ASSERT0(rhs && stpr);
    rhs->copyRef(delegate, m_rg);

    //Build DU chain if there exist outer loop reach-def.
    DUSet const* set = delegate2def.get(delegate);
    if (set != NULL) {
        m_du->copyDUSet(rhs, set);
        m_du->unionUse(set, rhs);
    }
    m_rg->allocRefForPR(stpr);
    ASSERT0(delegate2stpr.get(pr) == NULL);
    delegate2stpr.set(delegate, stpr);
    BB_irlist(preheader).append_tail_ex(stpr);
}


void RegPromot::computeOuterDefUse(IR * ref,
                                   IR * delegate,
                                   TMap<IR*, DUSet*> & delegate2def,
                                   TMap<IR*, DUSet*> & delegate2use,
                                   DefMiscBitSetMgr * sbs_mgr,
                                   LI<IRBB> const* li)
{
    if (ref->is_ild() || ref->is_ld() || ref->is_array()) {
        //ref is USE.
        ASSERTN(ref->getSSAInfo() == NULL, ("should not have SSA du"));
        DUSet * defset = delegate2def.get(delegate);
        DUSet const* refduset = ref->readDUSet();
        if (defset == NULL && refduset != NULL) {
            defset = (DUSet*)sbs_mgr->allocSBitSetCore();
            delegate2def.set(delegate, defset);
        }
        if (refduset != NULL) {
            DUIter di = NULL;
            for (INT i = refduset->get_first(&di);
                 i >= 0; i = refduset->get_next(i, &di)) {
                IR const* d = m_rg->getIR(i);
                ASSERT0(d->is_stmt());
                if (!li->isInsideLoop(BB_id(d->getBB()))) {
                    defset->bunion(i, *sbs_mgr);
                }
            }
        }
    } else {
        //ref is DEF.
        ASSERT0(ref->is_st() || ref->is_ist() || ref->is_starray());
        DUSet * set = delegate2use.get(delegate);
        DUSet const* refduset = ref->readDUSet();
        if (set == NULL && refduset != NULL) {
            set = (DUSet*)sbs_mgr->allocSBitSetCore();
            delegate2use.set(delegate, set);
        }
        if (refduset != NULL) {
            DUIter di = NULL;
            for (INT i = refduset->get_first(&di);
                 i >= 0; i = refduset->get_next(i, &di)) {
                IR const* u = m_rg->getIR(i);
                ASSERT0(u->is_exp());
                if (!li->isInsideLoop(BB_id(u->getStmt()->getBB()))) {
                    set->bunion(i, *sbs_mgr);
                }
            }
        }
    }
}


void RegPromot::createDelegateInfo(
    IR * delegate,
    TMap<IR*, IR*> & delegate2pr,
    TMap<IR*, SList<IR*>*> & delegate2has_outside_uses_ir_list)
{
    SList<IR*> * irlst = (SList<IR*>*)xmalloc(sizeof(SList<IR*>));
    irlst->init(m_ir_ptr_pool);
    delegate2has_outside_uses_ir_list.set(delegate, irlst);

    //Ref is the delegate of all the semantic equivalent expressions.
    IR * pr = delegate2pr.get(delegate);
    if (pr == NULL) {
        pr = m_rg->buildPR(IR_dt(delegate));
        delegate2pr.set(delegate, pr);
    }
}


//Return true if there is IR be promoted, otherwise return false.
bool RegPromot::promoteInexactAccess(LI<IRBB> const* li,
                                     IRBB * preheader,
                                     IRBB * exit_bb,
                                     TTab<IR*> & inexact_access,
                                     IRIter & ii,
                                     TabIter<IR*> & ti)
{
    ASSERT0(li && exit_bb && preheader);

    //Record a delegate to IR expressions which have same value in
    //array base and subexpression.
    RefTab delegate_tab(getNearestPowerOf2(
                          inexact_access.get_elem_count()));
    delegate_tab.initMem(m_gvn);

    //Map IR expression to STPR which generate the scalar value.
    //Map IR expression to STPR in preheader BB.
    TMap<IR*, IR*> delegate2stpr;

    //Map IR expression to promoted PR.
    TMap<IR*, IR*> delegate2pr;

    //Map delegate to a list of references which use memory outside the loop.
    TMap<IR*, SList<IR*>*> delegate2has_outside_uses_ir_list;

    //Map delegate to USE set.
    TMap<IR*, DUSet*> delegate2use;

    //Map delegate to DEF set.
    TMap<IR*, DUSet*> delegate2def;

    List<IR*> fixup_list; //record the IR that need to fix up duset.

    xcom::BitSet * bbset = LI_bb_set(li);
    CHECK_DUMMYUSE(bbset);
    ASSERT0(!bbset->is_empty()); //loop is empty.

    DefMiscBitSetMgr * sbs_mgr = m_rg->getMiscBitSetMgr();

    //Prepare delegate table and related information.
    for (IR * ref = inexact_access.get_first(ti);
         ref != NULL; ref = inexact_access.get_next(ti)) {
        bool find = false;
        if (!isPromotable(ref)) {
            //Do not promote the reference.
            continue;
        }

        IR * delegate = delegate_tab.append(ref, NULL, &find);
        ASSERT0(delegate);

        if (!find) {
            createDelegateInfo(delegate,
                               delegate2pr,
                               delegate2has_outside_uses_ir_list);
        } else {
            ASSERT0(delegate2has_outside_uses_ir_list.get(delegate));
        }
        computeOuterDefUse(ref, delegate, delegate2def,
                           delegate2use, sbs_mgr, li);
     }

    //dump_delegate_tab(delegate_tab, m_tm);

    if (delegate_tab.get_elem_count() == 0) { return false; }

    INT cur = 0;
    for (IR * delegate = delegate_tab.get_first(cur);
         cur >= 0; delegate = delegate_tab.get_next(cur)) {
        IR * pr = delegate2pr.get(delegate);
        ASSERT0(pr);
        handlePrelog(delegate, pr, delegate2stpr, delegate2def, preheader);
    }

    //Map IR expression which need to restore
    //into memory at epilog of loop.
    TTab<IR*> restore2mem;
    ti.clean();
    for (IR * ref = inexact_access.get_first(ti);
         ref != NULL; ref = inexact_access.get_next(ti)) {
        //Get the unique delegate.
        IR * delegate = NULL;
        delegate_tab.find(ref, &delegate);

        if (delegate == NULL) {
            //If delegate does not exist, the reference can not
            //be promoted.
            continue;
        }

        IR * delegate_pr = delegate2pr.get(delegate);
        ASSERT0(delegate_pr);
        handleAccessInBody(ref, delegate, delegate_pr,
                           delegate2has_outside_uses_ir_list,
                           restore2mem, fixup_list, delegate2stpr, li, ii);
    }

    ASSERT0(verifyIRandBB(m_rg->getBBList(), m_rg));

    handleRestore2Mem(restore2mem, delegate2stpr, delegate2pr, delegate2use,
                      delegate2has_outside_uses_ir_list, ti, exit_bb);

    removeRedundantDUChain(fixup_list);

    ti.clean();
    for (IR * ref = inexact_access.get_first(ti);
         ref != NULL; ref = inexact_access.get_next(ti)) {
        IR * delegate = NULL;
        delegate_tab.find(ref, &delegate);
        if (delegate == NULL) {
            //If delegate does not exist, the reference still in its place.
            continue;
        }
        m_rg->freeIRTree(ref);
    }

    freeLocalStruct(delegate2use, delegate2def, delegate2pr, sbs_mgr);
    return true;
}


void RegPromot::freeLocalStruct(TMap<IR*, DUSet*> & delegate2use,
                                TMap<IR*, DUSet*> & delegate2def,
                                TMap<IR*, IR*> & delegate2pr,
                                DefMiscBitSetMgr * sbs_mgr)
{
    TMapIter<IR*, DUSet*> map_iter2;
    DUSet * duset;
    for (IR * x = delegate2use.get_first(map_iter2, &duset);
         x != NULL; x = delegate2use.get_next(map_iter2, &duset)) {
        if (duset != NULL) {
            sbs_mgr->freeSBitSetCore(duset);
        }
    }

    map_iter2.clean();
    for (IR * x = delegate2def.get_first(map_iter2, &duset);
         x != NULL; x = delegate2def.get_next(map_iter2, &duset)) {
        if (duset != NULL) {
            sbs_mgr->freeSBitSetCore(duset);
        }
    }

    TMapIter<IR*, IR*> map_iter;
    IR * pr;
    for (IR * x = delegate2pr.get_first(map_iter, &pr);
         x != NULL; x = delegate2pr.get_next(map_iter, &pr)) {
        m_rg->freeIRTree(pr);
    }
}


//Determine whether the memory reference is same array or
//definitly different array.
UINT RegPromot::analyzeArrayStatus(IR const* ref1, IR const* ref2)
{
    if (!ref1->isArrayOp() || !ref2->isArrayOp()) {
        return RP_UNKNOWN;
    }

    IR const* base1 = ARR_base(ref1);
    IR const* base2 = ARR_base(ref2);
    if (base1->is_lda() && base2->is_lda()) {
        if (LDA_idinfo(base1) == LDA_idinfo(base2)) {
            if (LDA_ofst(base1) == LDA_ofst(base2)) {
                return RP_SAME_ARRAY;
            }
            return RP_UNKNOWN;
        }
        return RP_DIFFERENT_ARRAY;
    }

    ASSERT0(base1->is_ptr() && base2->is_ptr());
    ASSERT0(m_gvn);

    VN const* vn1 = m_gvn->mapIR2VN(base1);
    VN const* vn2 = m_gvn->mapIR2VN(base2);
    if (vn1 == NULL || vn2 == NULL) { return RP_UNKNOWN; }
    if (vn1 == vn2) { return RP_SAME_ARRAY; }
    return RP_DIFFERENT_ARRAY;
}


//This function perform the rest work of scanBB().
void RegPromot::checkAndRemoveInvalidExactOcc(List<IR*> & exact_occs)
{
    IRListIter ct;
    IRListIter nct;
    for (exact_occs.get_head(&ct), nct = ct; ct != NULL; ct = nct) {
        IR * occ = ct->val();
        exact_occs.get_next(&nct);

        MD const* md = occ->getRefMD();
        ASSERT0(md && md->is_exact());

        //We record all MD that are not suit for promotion, and perform
        //the rest job here that remove all related OCC in exact_list.
        //The MD of promotable candidate must not overlapped each other.
        if (m_dont_promote.is_overlap(md)) {
            exact_occs.remove(ct);
        }
    }
}


//Return true if there are memory locations have been promoted.
bool RegPromot::promote(LI<IRBB> const* li,
                        IRBB * exit_bb,
                        IRIter & ii,
                        TabIter<IR*> & ti,
                        TMap<MD const*, IR*> & exact_access,
                        TTab<IR*> & inexact_access,
                        List<IR*> & exact_occs)
{
    bool change = false;
    IRBB * preheader = NULL;
    if (exact_access.get_elem_count() != 0 ||
        inexact_access.get_elem_count() != 0) {
        preheader = findAndInsertPreheader(li, m_rg, m_is_insert_bb, false);
        ASSERT0(preheader);
        IR const* last = BB_last_ir(preheader);
        if (last != NULL && last->isCallStmt()) {
            preheader = findAndInsertPreheader(li, m_rg, m_is_insert_bb, true);
            ASSERT0(preheader);
            ASSERT0(BB_last_ir(preheader) == NULL);
        }
    }

    if (exact_access.get_elem_count() != 0) {
        #ifdef _DEBUG_
        xcom::BitSet visit;
        for (IR * x = exact_occs.get_head();
            x != NULL; x = exact_occs.get_next()) {
            ASSERT0(!visit.is_contain(IR_id(x)));
            visit.bunion(IR_id(x));
        }
        #endif

        ASSERT0(exact_occs.get_elem_count() != 0);
        checkAndRemoveInvalidExactOcc(exact_occs);
        change |= promoteExactAccess(li, ii, ti, preheader, exit_bb,
            exact_access, exact_occs);
    }

    if (inexact_access.get_elem_count() != 0) {
        change |= promoteInexactAccess(li, preheader, exit_bb,
            inexact_access, ii, ti);
    }
    return change;
}


//Return true if there are memory locations have been promoted.
bool RegPromot::tryPromote(LI<IRBB> const* li,
                           IRBB * exit_bb,
                           IRIter & ii,
                           TabIter<IR*> & ti,
                           TMap<MD const*, IR*> & exact_access,
                           TTab<IR*> & inexact_access,
                           List<IR*> & exact_occs)
{
    ASSERT0(li && exit_bb);
    exact_access.clean();
    inexact_access.clean();
    exact_occs.clean();
    m_dont_promote.clean(*getSBSMgr());    
    for (INT i = li->getBodyBBSet()->get_first();
         i != -1; i = li->getBodyBBSet()->get_next(i)) {
        IRBB * bb = m_cfg->getBB(i);
        ASSERT0(bb && m_cfg->getVertex(BB_id(bb)));
        if (bb->hasReturn()) {
            return false;
        }

        if (!scanBB(bb, li, exact_access, inexact_access,
                    exact_occs, ii)) {
            return false;
        }
    }
    return promote(li, exit_bb, ii, ti,
        exact_access, inexact_access, exact_occs);
}


bool RegPromot::EvaluableScalarReplacement(List<LI<IRBB> const*> & worklst)
{
    //Record the map between MD and ARRAY access expression.
    TMap<MD const*, IR*> access;
    TMap<MD const*, IR*> exact_access;
    TTab<IR*> inexact_access;
    List<IR*> exact_occs;
    IRIter ii;
    TabIter<IR*> ti;
    bool change = false;
    while (worklst.get_elem_count() > 0) {
        LI<IRBB> const* x = worklst.remove_head();
        IRBB * exit_bb = findSingleExitBB(x);
        if (exit_bb != NULL) {
            //If we did not find a single exit bb, this loop is nontrivial.
            change |= tryPromote(x, exit_bb, ii, ti, exact_access,
                                 inexact_access, exact_occs);
        }

        x = x->getInnerList();
        while (x != NULL) {
            worklst.append_tail(x);
            x = x->get_next();
        }
    }
    return change;
}


void RegPromot::init()
{
    m_is_insert_bb = false;
    m_need_rebuild_prssa = false;
}


void RegPromot::dump()
{
    if (g_tfile == NULL) { return; }
    note("\n==---- DUMP RegPromotion ----==");
    //dump_mdlt();
    dumpBBList(m_rg->getBBList(), m_rg);
    if (m_prssamgr != NULL && m_prssamgr->is_valid()) {
        m_prssamgr->dump();
    }
    if (m_mdssamgr != NULL && m_mdssamgr->is_valid()) {
        m_mdssamgr->dump();
    }
    fflush(g_tfile);
}


//Perform scalar replacement of aggregates and array.
bool RegPromot::perform(OptCtx & oc)
{
    BBList * bbl = m_rg->getBBList();
    if (bbl == NULL || bbl->get_elem_count() == 0) { return false; }
    if (!OC_is_ref_valid(oc)) { return false; }
    if (!OC_is_cfg_valid(oc)) { return false; }
    //Check PR DU chain.
    m_prssamgr = (PRSSAMgr*)(m_rg->getPassMgr()->queryPass(PASS_PR_SSA_MGR));
    if (!OC_is_pr_du_chain_valid(oc) && usePRSSADU()) {
        //At least one kind of DU chain should be avaiable.
        return false;
    }
    //Check NONPR DU chain.
    m_mdssamgr = (MDSSAMgr*)(m_rg->getPassMgr()->queryPass(PASS_MD_SSA_MGR));
    if (!OC_is_nonpr_du_chain_valid(oc) && useMDSSADU()) {
        //At least one kind of DU chain should be avaiable.
        return false;
    }

    START_TIMER(t, getPassName());    
    m_rg->checkValidAndRecompute(&oc, PASS_LOOP_INFO, PASS_UNDEF);
    LI<IRBB> const* li = m_cfg->getLoopInfo();
    if (li == NULL) { return false; }
    m_gvn = (GVN*)(m_rg->getPassMgr()->queryPass(PASS_GVN));
    if (m_gvn == NULL) {
        //We dependent on gvn to do critical judgement.
        return false;
    }
    if (!m_gvn->is_valid()) {
        m_gvn->reperform(oc);
    }

    init();    
    List<LI<IRBB> const*> worklst;
    while (li != NULL) {
        worklst.append_tail(li);
        li = LI_next(li);
    }
    bool change = false;
    buildLifeTime();
    change = EvaluableScalarReplacement(worklst);
    if (change) {
        //DU reference and du chain has maintained.
        ASSERT0(m_rg->verifyMDRef());
        ASSERT0(m_du->verifyMDDUChain(DUOPT_COMPUTE_PR_DU|
                                      DUOPT_COMPUTE_NONPR_DU));
        OC_is_reach_def_valid(oc) = false;
        OC_is_avail_reach_def_valid(oc) = false;
        OC_is_live_expr_valid(oc) = false;

        //Enforce following pass to recompute gvn.
        m_gvn->set_valid(false);
    }
    if (m_need_rebuild_prssa) {
        //WORKAROUND|FIXME:For now, we do not support incremental update PRSSA.
        //rebuild PRSSA if DU changed.
        ASSERT0(m_prssamgr);
        m_prssamgr->construction(oc);
        ASSERT0(m_mdssamgr);
        m_mdssamgr->construction(oc);
    }
    if (m_is_insert_bb) {
        OC_is_cdg_valid(oc) = false;
        OC_is_dom_valid(oc) = false;
        OC_is_pdom_valid(oc) = false;
        OC_is_rpo_valid(oc) = false;
        //Loop info is unchanged.
    }
    if (g_is_dump_after_pass && g_dump_opt.isDumpRP()) {
        dump();
    }
    END_TIMER(t, getPassName());
    return change;
}
//END RegPromot

} //namespace xoc
