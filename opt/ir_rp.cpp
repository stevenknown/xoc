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
#include "prdf.h"
#include "prssainfo.h"
#include "ir_ssa.h"
#include "ir_gvn.h"
#include "ir_rp.h"

namespace xoc {

class RefHashFunc {
    IR_GVN * m_gvn;
public:
    void initMem(IR_GVN * gvn)
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

    void initMem(IR_GVN * gvn)
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
//START IR_RP
//
void IR_RP::dump()
{
    if (g_tfile == NULL) { return; }
    g_indent = 0;
    note("\n==---- DUMP IR_RP '%s' ----==\n", m_ru->getRegionName());

    g_indent = 2;
    BBList * bbl = m_ru->getBBList();
    for (IRBB * bb = bbl->get_head(); bb != NULL; bb = bbl->get_next()) {
        note("\n---- BB%d -----", BB_id(bb));
        MDSet * live_in = getLiveInMDSet(bb);
        MDSet * live_out = getLiveOutMDSet(bb);
        MDSet * def = getDefMDSet(bb);
        MDSet * use = getUseMDSet(bb);
        INT i;

        note("\nLIVE-IN: ");
        SEGIter * iter;
        for (i = live_in->get_first(&iter);
             i != -1; i = live_in->get_next(i, &iter)) {
            prt("MD%d, ", i);
        }

        note("\nLIVE-OUT: ");
        for (i = live_out->get_first(&iter);
             i != -1; i = live_out->get_next(i, &iter)) {
            prt("MD%d, ", i);
        }

        note("\nDEF: ");
        for (i = def->get_first(&iter);
             i != -1; i = def->get_next(i, &iter)) {
            prt("MD%d, ", i);
        }

        note("\nUSE: ");
        for (i = use->get_first(&iter);
             i != -1; i = use->get_next(i, &iter)) {
            prt("MD%d, ", i);
        }
    }
    fflush(g_tfile);
}


void IR_RP::dump_occ_list(List<IR*> & occs)
{
    if (g_tfile == NULL) { return; }
    note("\n---- DUMP exact occ list ----");
    for (IR * x = occs.get_head(); x != NULL; x = occs.get_next()) {
        dumpIR(x, m_ru, NULL, IR_DUMP_KID);
        note("\n");
    }
    note("\n");
    fflush(g_tfile);
}


void IR_RP::dump_access2(TTab<IR*> & access)
{
    if (g_tfile == NULL) { return; }
    note("\n---- DUMP inexact access ----");
    TabIter<IR*> iter;
    for (IR * ir = access.get_first(iter);
        ir != NULL; ir = access.get_next(iter)) {
        dumpIR(ir, m_ru);
        note("\n");
    }
    note("\n");
    fflush(g_tfile);
}


void IR_RP::dump_access(TMap<MD const*, IR*> & access)
{
    if (g_tfile == NULL) { return; }
    note("\n---- DUMP exact access ----");
    TMapIter<MD const*, IR*> iter;
    IR * ref;
    for (MD const* md = access.get_first(iter, &ref);
         ref != NULL; md = access.get_next(iter, &ref)) {
        md->dump(m_ru->getTypeMgr());
        dumpIR(ref, m_ru);
        note("\n");
    }
    note("\n");
    fflush(g_tfile);
}


MDSet * IR_RP::getLiveInMDSet(IRBB * bb)
{
    MDSet * set = m_livein_mds_vec.get(BB_id(bb));
    if (set == NULL) {
        set = m_mds_mgr->create();
        m_livein_mds_vec.set(BB_id(bb), set);
    }
    return set;
}


MDSet * IR_RP::getLiveOutMDSet(IRBB * bb)
{
    MDSet * set = m_liveout_mds_vec.get(BB_id(bb));
    if (set == NULL) {
        set = m_mds_mgr->create();
        m_liveout_mds_vec.set(BB_id(bb), set);
    }
    return set;
}


MDSet * IR_RP::getDefMDSet(IRBB * bb)
{
    MDSet * set = m_def_mds_vec.get(BB_id(bb));
    if (set == NULL) {
        set = m_mds_mgr->create();
        m_def_mds_vec.set(BB_id(bb), set);
    }
    return set;
}


MDSet * IR_RP::getUseMDSet(IRBB * bb)
{
    MDSet * set = m_use_mds_vec.get(BB_id(bb));
    if (set == NULL) {
        set = m_mds_mgr->create();
        m_use_mds_vec.set(BB_id(bb), set);
    }
    return set;
}


void IR_RP::computeLocalLiveness(IRBB * bb, IR_DU_MGR & du_mgr)
{
    MDSet * gen = getDefMDSet(bb);
    MDSet * use = getUseMDSet(bb);
    gen->clean(*m_misc_bs_mgr);
    use->clean(*m_misc_bs_mgr);
    MDSet mustuse;
    for (IR * ir = BB_last_ir(bb); ir != NULL; ir = BB_prev_ir(bb)) {
        MD const* def = ir->getExactRef();
        if (def != NULL) {
            gen->bunion(MD_id(def), *m_misc_bs_mgr);
            use->diff(*gen, *m_misc_bs_mgr);
        }
        mustuse.clean(*m_misc_bs_mgr);
        du_mgr.collectMustUsedMDs(ir, mustuse);
        use->bunion(mustuse, *m_misc_bs_mgr);
    }
    mustuse.clean(*m_misc_bs_mgr);
}


MD_LT * IR_RP::getMDLifeTime(MD * md)
{
    MD_LT * lt;
    if ((lt = m_md2lt_map->get(md)) != NULL) {
        return lt;
    }
    lt = (MD_LT*)xmalloc(sizeof(MD_LT));
    MDLT_id(lt) = ++m_mdlt_count;
    MDLT_md(lt) = md;
    MDLT_livebbs(lt) = m_bs_mgr.create();
    m_md2lt_map->set(md, lt);
    return lt;
}


void IR_RP::cleanLiveBBSet()
{
    //Clean.
    Vector<MD_LT*> * bs_vec = m_md2lt_map->get_tgt_elem_vec();
    for (INT i = 0; i <= bs_vec->get_last_idx(); i++) {
        MD_LT * lt = bs_vec->get(i);
        if (lt != NULL) {
            ASSERT0(MDLT_livebbs(lt) != NULL);
            MDLT_livebbs(lt)->clean();
        }
    }
}


void IR_RP::dump_mdlt()
{
    if (g_tfile == NULL) { return; }
    MDSet mdbs;
    BBList * bbl = m_ru->getBBList();
    for (IRBB * bb = bbl->get_head(); bb != NULL; bb = bbl->get_next()) {
        MDSet * livein = getLiveInMDSet(bb);
        MDSet * liveout = getLiveOutMDSet(bb);
        if (livein->is_empty() && liveout->is_empty()) { continue; }
        mdbs.bunion(*livein, *m_misc_bs_mgr);
        mdbs.bunion(*liveout, *m_misc_bs_mgr);
    }
    mdbs.dump(m_md_sys);

    StrBuf buf(32);
    note("\n==---- DUMP MD LIFE TIME ----==");
    SEGIter * iter;
    for (INT i = mdbs.get_first(&iter); i >= 0; i = mdbs.get_next(i, &iter)) {
        MD * md = m_md_sys->getMD(i);
        ASSERT0(md != NULL);
        MD_LT * lt = m_md2lt_map->get(md);
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
    mdbs.clean(*m_misc_bs_mgr);
}


void IR_RP::buildLifeTime()
{
    cleanLiveBBSet();

    //Rebuild life time.
    BBList * bbl = m_ru->getBBList();
    for (IRBB * bb = bbl->get_head(); bb != NULL; bb = bbl->get_next()) {
        MDSet * livein = getLiveInMDSet(bb);
        MDSet * liveout = getLiveOutMDSet(bb);
        if (livein->is_empty() && liveout->is_empty()) { continue; }

        SEGIter * iter;
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


void IR_RP::computeGlobalLiveness()
{
    BBList * bbl = m_ru->getBBList();
    for (IRBB * bb = bbl->get_head(); bb != NULL; bb = bbl->get_next()) {
        getLiveInMDSet(bb)->clean(*m_misc_bs_mgr);
        getLiveOutMDSet(bb)->clean(*m_misc_bs_mgr);
    }
    bool change;
    INT count = 0;
    IR_CFG * cfg = m_ru->getCFG();
    MDSet news;
    List<IRBB*> succs;
    do {
        change = false;
        for (IRBB * bb = bbl->get_tail(); bb != NULL; bb = bbl->get_prev()) {
            MDSet * out = getLiveOutMDSet(bb);
            news.copy(*out, *m_misc_bs_mgr);
            news.diff(*getDefMDSet(bb), *m_misc_bs_mgr);
            news.bunion(*getUseMDSet(bb), *m_misc_bs_mgr);
            MDSet * in = getLiveInMDSet(bb);
            if (!in->is_equal(news)) {
                in->copy(news, *m_misc_bs_mgr);
                change = true;
            }

            cfg->get_succs(succs, bb);
            news.clean(*m_misc_bs_mgr);
            for (IRBB * p = succs.get_head(); p != NULL; p = succs.get_next()) {
                news.bunion(*getLiveInMDSet(p), *m_misc_bs_mgr);
            }
            if (!out->is_equal(news)) {
                out->copy(news, *m_misc_bs_mgr);
                change = true;
            }
        }
        count++;
    } while (change && count < 220);
    ASSERTN(!change, ("result of equation is convergent slowly"));
    news.clean(*m_misc_bs_mgr);
}


void IR_RP::computeLiveness()
{
    IR_DU_MGR * du_mgr = m_ru->getDUMgr();
    BBList * bbl = m_ru->getBBList();
    for (IRBB * bb = bbl->get_head(); bb != NULL; bb = bbl->get_next()) {
        computeLocalLiveness(bb, *du_mgr);
    }
    computeGlobalLiveness();
}


void IR_RP::addExactAccess(
        OUT TMap<MD const*, IR*> & exact_access,
        OUT List<IR*> & exact_occ_list,
        MD const* exact_md,
        IR * ir)
{
    ASSERT0(exact_md && exact_md->is_exact());
    if (!exact_access.find(exact_md)) {
        exact_access.set(exact_md, ir);
    }
    exact_occ_list.append_tail(ir);
}


void IR_RP::addInexactAccess(TTab<IR*> & inexact_access, IR * ir)
{
    inexact_access.append_and_retrieve(ir);
}


bool IR_RP::checkExpressionIsLoopInvariant(IN IR * ir, LI<IRBB> const* li)
{
    if (ir->isMemoryOpnd()) {
        if (ir->isReadPR() && PR_ssainfo(ir) != NULL) {
            SSAInfo * ssainfo = PR_ssainfo(ir);
            if (ssainfo->get_def() != NULL) {
                IRBB * defbb = ssainfo->get_def()->getBB();
                ASSERT0(defbb);

                if (li->isInsideLoop(BB_id(defbb))) {
                    return false;
                }
            }
            return true;
        }

        DUSet const* duset = ir->readDUSet();
        if (duset == NULL) { return true; }

        DUIter dui = NULL;
        for (INT i = duset->get_first(&dui);
             i >= 0; i = duset->get_next(i, &dui)) {
            IR const* defstmt = m_ru->getIR(i);
            ASSERT0(defstmt->is_stmt());
            IRBB * bb = defstmt->getBB();

            if (li->isInsideLoop(BB_id(bb))) { return false; }
        }

        return true;
    }

    for (UINT i = 0; i < IR_MAX_KID_NUM(ir); i++) {
        IR * kid = ir->getKid(i);
        if (kid == NULL) { continue; }
        if (!checkExpressionIsLoopInvariant(kid, li)) {
            return false;
        }
    }

    return true;
}


bool IR_RP::checkArrayIsLoopInvariant(IN IR * ir, LI<IRBB> const* li)
{
    ASSERT0(ir->is_array() && li);
    for (IR * s = ARR_sub_list(ir); s != NULL; s = s->get_next()) {
        if (!checkExpressionIsLoopInvariant(s, li)) {
            return false;
        }
    }
    if (!checkExpressionIsLoopInvariant(ARR_base(ir), li)) {
        return false;
    }
    return true;
}


//Return true if the caller can keep doing the analysis.
//That means there are no memory referrences clobbered the
//candidate in list.
//Or else the analysis for current loop should be terminated.
bool IR_RP::handleArrayRef(
        IN IR * ir,
        LI<IRBB> const* li,
        OUT TMap<MD const*, IR*> & exact_access,
        OUT List<IR*> & exact_occ_list,
        OUT TTab<IR*> & inexact_access)
{
    ASSERT0(ir->isArrayOp());

    if (ARR_ofst(ir) != 0) {
        //The array reference can not be promoted.
        //Check the promotable candidates if current stmt
        //modify the related MD.
        clobberAccessInList(ir, exact_access, exact_occ_list, inexact_access);
        return true;
    }

    MD const* mustuse = ir->getRefMD();
    if (mustuse == NULL || !mustuse->is_effect()) { return false; }

    if (mustuse->is_volatile()) {
        clobberAccessInList(ir, exact_access, exact_occ_list, inexact_access);
        return true;
    }

    if (mustuse->is_exact()) {
        if (!m_dont_promot.is_overlap(mustuse)) {
            //Exact memory access.
            addExactAccess(exact_access, exact_occ_list, mustuse, ir);
        }
        return true;
    }

    if (m_dont_promot.is_overlap(mustuse)) {
        return true;
    }

    //MD is inexact. Check if it is loop invariant.
    if (ir->is_starray() || !checkArrayIsLoopInvariant(ir, li)) {
        //If ir is STARRAY that modify inexact MD.
        //It may clobber all other array with same array base.
        clobberAccessInList(ir, exact_access, exact_occ_list, inexact_access);
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
        clobberAccessInList(ir, exact_access, exact_occ_list, inexact_access);
        return true;
    }
    addInexactAccess(inexact_access, ir);
    return true;
}


bool IR_RP::handleGeneralRef(
        IR * ir,
        LI<IRBB> const* li,
        OUT TMap<MD const*, IR*> & exact_access,
        OUT List<IR*> & exact_occ_list,
        OUT TTab<IR*> & inexact_access)
{
    ASSERT0(ir->isMemoryRef());
    ASSERT0(!ir->is_array());

    if (ir->getOffset() != 0) {
        //TODO:not yet support, x is MC type.
        //clobberAccessInList(ir, exact_access, exact_occ_list,
        //                       inexact_access);
        //return true;
    }

    MD const* mustuse = ir->getRefMD();
    if (mustuse == NULL || !mustuse->is_effect()) {
        //It is dispensable clobbering access if all loop is not analysable.
        //clobberAccessInList(ir, exact_access, exact_occ_list,
        //                       inexact_access);
        return false;
    }

    if (mustuse->is_volatile()) {
        clobberAccessInList(ir, exact_access, exact_occ_list, inexact_access);
        return true;
    }

    if (mustuse->is_exact()) {
        if (!m_dont_promot.is_overlap(mustuse)) {
            //Exact memory access.
            addExactAccess(exact_access, exact_occ_list, mustuse, ir);
        }

        //mustuse already be in the dont_promot table.
        return true;
    }

    if (m_dont_promot.is_overlap(mustuse)) {
        return true;
    }

    if (ir->is_ild() || ir->is_ist()) {
        //MD is inexact. Check if it is loop invariant.
        if (!checkIndirectAccessIsLoopInvariant(ir, li)) {
            clobberAccessInList(ir, exact_access, exact_occ_list, inexact_access);
            return true;
        }
    } else if (ir->is_ld()) {
        if (!checkExpressionIsLoopInvariant(ir, li)) {
            clobberAccessInList(ir, exact_access, exact_occ_list, inexact_access);
            return true;
        }
    } else {
        UNREACHABLE(); //TODO
    }

    TabIter<IR*> ti;
    for (IR * ref = inexact_access.get_first(ti);
         ref != NULL; ref = inexact_access.get_next(ti)) {
        UINT st = analyzeIndirectAccessStatus(ir, ref);
        if (st == RP_SAME_OBJ) { continue; }
        if (st == RP_DIFFERENT_OBJ) { continue; }

        //The ir can not be promoted.
        //Check the promotable candidates if current stmt modify the related MD.
        clobberAccessInList(ir, exact_access, exact_occ_list, inexact_access);
        return true;
    }
    addInexactAccess(inexact_access, ir);
    return true;
}


//The result of 'ir' can not be promoted.
//Check the promotable candidates if current stmt modify the related MD.
void IR_RP::clobberAccessInList(
        IR * ir,
        OUT TMap<MD const*, IR*> & exact_access,
        OUT List<IR*> & exact_occ_list,
        OUT TTab<IR*> & inexact_access)
{
    DUMMYUSE(exact_occ_list);

    MD const* mustdef = ir->getRefMD();
    MDSet const* maydef = ir->getRefMDSet();

    if (mustdef != NULL) { m_dont_promot.bunion(mustdef, *m_misc_bs_mgr); }
    else if (maydef != NULL) { m_dont_promot.bunion(*maydef, *m_misc_bs_mgr); }

    TMapIter<MD const*, IR*> iter;
    Vector<MD const*> need_to_be_removed;
    INT cnt = 0;
    if (mustdef != NULL) {
        for (MD const* md = exact_access.get_first(iter, NULL);
             md != NULL; md = exact_access.get_next(iter, NULL)) {
            if (mustdef == md || mustdef->is_overlap(md)) {
                //Current ir may modify the candidate's md.
                //We think the candidate is not suite to promot any more.
                need_to_be_removed.set(cnt, md);
                cnt++;
            }
        }
    } else if (maydef != NULL && !maydef->is_empty()) {
        for (MD const* md = exact_access.get_first(iter, NULL);
             md != NULL; md = exact_access.get_next(iter, NULL)) {
            if (maydef->is_contain(md)) {
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
    if (mustdef != NULL) {
        for (IR * acc = inexact_access.get_first(iter2);
             acc != NULL; acc = inexact_access.get_next(iter2)) {
            MD const* acc_md = acc->getRefMD();
            MDSet const* acc_mds = acc->getRefMDSet();
            if (acc_md != NULL) {
                if (mustdef == acc_md || mustdef->is_overlap(acc_md)) {
                    //ir is not suite to promot any more, all mds which
                    //overlapped with it are also not promotable.
                    need_to_be_removed2.set(cnt, acc);
                    cnt++;
                }
            } else if (acc_mds != NULL && acc_mds->is_overlap(mustdef, m_ru)) {
                //ir is not suite to promot any more, all mds which
                //overlapped with it are also not promotable.
                need_to_be_removed2.set(cnt, acc);
                cnt++;
            }
        }
    } else if (maydef != NULL && !maydef->is_empty()) {
        for (IR * acc = inexact_access.get_first(iter2);
             acc != NULL; acc = inexact_access.get_next(iter2)) {
            MD const* acc_md = acc->getRefMD();
            MDSet const* acc_mds = acc->getRefMDSet();
            if ((acc_md != NULL && maydef->is_overlap(acc_md, m_ru)) ||
                (acc_mds != NULL &&
                 (acc_mds == maydef || maydef->is_intersect(*acc_mds)))) {
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


bool IR_RP::checkIndirectAccessIsLoopInvariant(IN IR * ir, LI<IRBB> const* li)
{
    ASSERT0(li);
    if (ir->is_ild()) {
        if (!checkExpressionIsLoopInvariant(ILD_base(ir), li)) {
            return false;
        }
    } else if (ir->is_ist()) {
        if (!checkExpressionIsLoopInvariant(IST_base(ir), li)) {
            return false;
        }
    }
    return true;
}


//Determine whether the memory reference is same object or different.
UINT IR_RP::analyzeIndirectAccessStatus(IR const* ref1, IR const* ref2)
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
bool IR_RP::scanOpnd(
        IR * ir,
        LI<IRBB> const* li,
        OUT TMap<MD const*, IR*> & exact_access,
        OUT List<IR*> & exact_occ_list,
        OUT TTab<IR*> & inexact_access,
        IRIter & ii)
{
    ii.clean();
    for (IR * x = iterRhsInit(ir, ii);
         x != NULL; x = iterRhsNext(ii)) {
        if (!x->isMemoryOpnd() || x->is_pr()) { continue; }
        if (x->is_array()) {
            if (!handleArrayRef(x, li, exact_access, exact_occ_list,
                                inexact_access)) {
                return false;
            }
            continue;
        }

        if (!handleGeneralRef(x, li, exact_access, exact_occ_list,
                              inexact_access)) {
            return false;
        }
    }
    return true;
}


//Return true if find promotable memory reference.
bool IR_RP::scanResult(
        IN IR * ir,
        LI<IRBB> const* li,
        OUT TMap<MD const*, IR*> & exact_access,
        OUT List<IR*> & exact_occ_list,
        OUT TTab<IR*> & inexact_access)
{
    switch (ir->getCode()) {
    case IR_ST:
        return handleGeneralRef(ir, li, exact_access, exact_occ_list,
                                inexact_access);
    case IR_STARRAY:
        return handleArrayRef(ir, li, exact_access, exact_occ_list,
                              inexact_access);
    case IR_IST:
        return handleGeneralRef(ir, li, exact_access, exact_occ_list,
                                inexact_access);
    default: break;
    }
    return true;
}


//Scan BB and find promotable memory reference.
//If this function find unpromotable access that with ambiguous
//memory reference, all relative promotable accesses which in list
//will not be scalarized.
//e.g: a[0] = ...
//    a[i] = ...
//    a[0] is promotable, but a[i] is not, then a[0] can not be promoted.
//If there exist memory accessing that we do not know where it access,
//whole loop is unpromotable.
bool IR_RP::scanBB(
        IN IRBB * bb,
        LI<IRBB> const* li,
        OUT TMap<MD const*, IR*> & exact_access,
        OUT TTab<IR*> & inexact_access,
        OUT List<IR*> & exact_occ_list,
        IRIter & ii)
{
    for (IR * ir = BB_last_ir(bb);
         ir != NULL; ir = BB_prev_ir(bb)) {
        if (ir->isCallStmt() && !ir->isReadOnlyCall()) {
            return false;
        }
    }

    for (IR * ir = BB_first_ir(bb);
         ir != NULL; ir = BB_next_ir(bb)) {
        if (!ir->isContainMemRef()) { continue; }
        if (ir->is_region()) { return false; }
        if (!scanResult(ir, li, exact_access, exact_occ_list, inexact_access)) {
            return false;
        }
        if (!scanOpnd(ir, li, exact_access, exact_occ_list,
                      inexact_access, ii)) {
            return false;
        }
    }

    return true;
}


IRBB * IR_RP::findSingleExitBB(LI<IRBB> const* li)
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
void IR_RP::replaceUseForTree(IR * oldir, IR * newir)
{
    ASSERT0(oldir->is_exp() && newir->is_exp());
    if (oldir->is_ld()) {
        m_du->changeUse(newir, oldir, m_ru->getMiscBitSetMgr());
    } else if (oldir->is_ild()) {
        m_du->removeUseOutFromDefset(ILD_base(oldir));
        m_du->changeUse(newir, oldir, m_ru->getMiscBitSetMgr());
    } else if (oldir->is_array()) {
        m_du->removeUseOutFromDefset(ARR_base(oldir));
        m_du->removeUseOutFromDefset(ARR_sub_list(oldir));
        m_du->changeUse(newir, oldir, m_ru->getMiscBitSetMgr());
    } else {
        UNREACHABLE(); //TODO
    }
}


void IR_RP::handleRestore2Mem(
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
        IR * pr = m_ru->dupIRTree(delegate2pr.get(delegate));
        m_ru->allocRefForPR(pr);

        IR * stpr = delegate2stpr.get(delegate);
        if (m_ssamgr != NULL) {
            m_ssamgr->buildDUChain(stpr, pr);
        } else {
            m_du->buildDUChain(stpr, pr);
        }

        IR * stmt = NULL;
        switch (delegate->getCode()) {
        case IR_ARRAY:
        case IR_STARRAY: {
            ASSERTN(delegate->getOffset() == 0, ("TODO: not yet support."));

            //Prepare base and subscript expression list.
            IR * base = m_ru->dupIRTree(ARR_base(delegate));
            IR * sublist = m_ru->dupIRTree(ARR_sub_list(delegate));

            //Copy DU chain and MD reference.
            m_du->copyIRTreeDU(base, ARR_base(delegate), true);
            m_du->copyIRTreeDU(sublist, ARR_sub_list(delegate), true);

            //Build Store Array operation.
            stmt = m_ru->buildStoreArray(
                base, sublist, IR_dt(delegate),
                ARR_elemtype(delegate),
                ((CArray*)delegate)->getDimNum(),
                ARR_elem_num_buf(delegate),
                pr);

            //Copy MD reference for store array operation.
            stmt->copyRef(delegate, m_ru);
            break;
        }
        case IR_IST: {
            IR * lhs = m_ru->dupIRTree(IST_base(delegate));
            m_du->copyIRTreeDU(lhs, IST_base(delegate), true);

            stmt = m_ru->buildIStore(lhs, pr,
                IST_ofst(delegate), IR_dt(delegate));
            break;
        }
        case IR_ST:
            stmt = m_ru->buildStore(ST_idinfo(delegate),
                IR_dt(delegate), ST_ofst(delegate), pr);
            break;
        case IR_ILD: {
            IR * base = m_ru->dupIRTree(ILD_base(delegate));
            stmt = m_ru->buildIStore(base, pr,
                ILD_ofst(delegate), IR_dt(delegate));
            m_du->copyIRTreeDU(base, ILD_base(delegate), true);
            break;
        }
        case IR_LD:
            stmt = m_ru->buildStore(LD_idinfo(delegate),
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

        stmt->copyRef(delegate, m_ru);

        SList<IR*> * irlst = delegate2has_outside_uses_ir_list.get(delegate);
        ASSERT0(irlst);

        for (xcom::SC<IR*> * sc = irlst->get_head();
             sc != irlst->end(); sc = irlst->get_next(sc)) {
            IR * def = sc->val();

            ASSERT0(def && def->is_stpr());
            ASSERT0(STPR_no(def) == PR_no(pr));

            if (m_ssamgr != NULL) {
                m_ssamgr->buildDUChain(def, pr);
            } else {
                m_du->buildDUChain(def, pr);
            }
        }

        BB_irlist(exit_bb).append_head(stmt);
    }
}


//Return true if there is IR be promoted, otherwise return false.
bool IR_RP::promoteExactAccess(
        LI<IRBB> const* li,
        IRIter & ii,
        TabIter<IR*> & ti,
        IRBB * preheader,
        IRBB * exit_bb,
        TMap<MD const*, IR*> & exact_access,
        List<IR*> & exact_occ_list)
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

    DefMiscBitSetMgr * sbs_mgr = m_ru->getMiscBitSetMgr();

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

    for (IR * ref = exact_occ_list.get_head();
         ref != NULL; ref = exact_occ_list.get_next()) {
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
    PromotedTab promoted(m_ru->getMiscBitSetMgr()->getSegMgr());

    for (IR * ref = exact_occ_list.get_head();
         ref != NULL; ref = exact_occ_list.get_next()) {
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

    ASSERT0(verifyIRandBB(m_ru->getBBList(), m_ru));

    handleRestore2Mem(restore2mem, delegate2stpr, delegate2pr, delegate2use,
                      delegate2has_outside_uses_ir_list, ti, exit_bb);

    removeRedundantDUChain(fixup_list);

    freeLocalStruct(delegate2use, delegate2def, delegate2pr, sbs_mgr);

    //Note, during the freeing process, the ref should not be
    //freed if it is IR_UNDEF.
    //This is because the ref is one of the kid of some other
    //stmt/exp which has already been freed.
    for (IR * ref = exact_occ_list.get_head();
         ref != NULL; ref = exact_occ_list.get_next()) {
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

        m_ru->freeIRTree(ref);
    }
    return true;
}


//Return true if some node at IR tree may throw exception.
bool IR_RP::isMayThrow(IR * ir, IRIter & iter)
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


bool IR_RP::hasLoopOutsideUse(IR const* stmt, LI<IRBB> const* li)
{
    ASSERT0(stmt->is_stmt());
    ASSERT0(stmt->getSSAInfo() == NULL);

    DUSet const* useset = stmt->readDUSet();
    if (useset == NULL) { return false; }

    DUIter di = NULL;
    for (INT i = useset->get_first(&di);
         i >= 0; i = useset->get_next(i, &di)) {
        IR const* u = m_ru->getIR(i);
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
void IR_RP::removeRedundantDUChain(List<IR*> & fixup_list)
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
                    IR const* use = m_ru->getIR(i);

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
                    IR const* use = m_ru->getIR(i);
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
                if (PR_ssainfo(ref)->get_def() != NULL) {
                    //SSA def must be same PR_no with the SSA use.
                    ASSERT0(PR_ssainfo(ref)->get_def()->getPrno() == prno);
                }
            } else {
                for (INT i = duset->get_first(&di);
                     i >= 0; i = duset->get_next(i, &di)) {
                    IR const* d = m_ru->getIR(i);
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
void IR_RP::handleAccessInBody(
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
    IR * stmt;
    if (ref->is_stmt()) {
        stmt = ref;
    } else {
        stmt = ref->getStmt();
        ASSERT0(stmt);
    }

    switch (ref->getCode()) {
    case IR_STARRAY:
        {
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
            IR * stpr = m_ru->buildStorePR(PR_no(delegate_pr),
                                           IR_dt(delegate_pr),
                                           STARR_rhs(ref));

            m_ru->allocRefForPR(stpr);

            //New IR has same VN with original one.
            m_gvn->set_mapIR2VN(stpr, m_gvn->mapIR2VN(ref));

            if (has_use) {
                restore2mem.append_and_retrieve(delegate);
                SList<IR*> * irlst =
                    delegate2has_outside_uses_ir_list.get(delegate);
                ASSERT0(irlst);
                irlst->append_head(stpr);
            }

            m_du->removeUseOutFromDefset(ref);

            //Change DU chain.
            m_du->changeDef(stpr, ref, m_ru->getMiscBitSetMgr());
            fixup_list.append_tail(stpr);

            STARR_rhs(ref) = NULL;

            IRBB * refbb = ref->getBB();
            ASSERT0(refbb);
            xcom::C<IR*> * ct = NULL;
            BB_irlist(refbb).find(ref, &ct);
            ASSERT0(ct != NULL);

            BB_irlist(refbb).insert_after(stpr, ct);
            BB_irlist(refbb).remove(ct);

            //Do not free stmt here since it will be freed later.
            //m_ru->freeIRTree(ref);
        }
        break;
    case IR_IST:
    case IR_ST:
        {
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

            IR * stpr = m_ru->buildStorePR(PR_no(delegate_pr),
                                           IR_dt(delegate_pr),
                                           ref->getRHS());
            m_ru->allocRefForPR(stpr);

            //New IR has same VN with original one.
            m_gvn->set_mapIR2VN(stpr, m_gvn->mapIR2VN(stmt));

            if (has_use) {
                restore2mem.append_and_retrieve(delegate);
                SList<IR*> * irlst = delegate2has_outside_uses_ir_list.
                    get(delegate);
                ASSERT0(irlst);
                irlst->append_head(stpr);
            }

            if (ref->is_ist()) {
                m_du->removeUseOutFromDefset(IST_base(ref));
            }

            //Change DU chain.
            m_du->changeDef(stpr, ref, m_ru->getMiscBitSetMgr());
            fixup_list.append_tail(stpr);

            ref->setRHS(NULL);

            IRBB * stmt_bb = ref->getBB();
            ASSERT0(stmt_bb);
            xcom::C<IR*> * ct = NULL;
            BB_irlist(stmt_bb).find(ref, &ct);
            ASSERT0(ct != NULL);

            BB_irlist(stmt_bb).insert_after(stpr, ct);
            BB_irlist(stmt_bb).remove(ct);

            //Do not free stmt here since it will be freed later.
        }
        break;
    default:
        {
            ASSERT0(ref->is_ild() || ref->is_ld() || ref->is_array());
            IR * pr = m_ru->dupIR(delegate_pr);
            m_ru->allocRefForPR(pr);

            //New IR has same VN with original one.
            m_gvn->set_mapIR2VN(pr, m_gvn->mapIR2VN(ref));

            //Find the stpr that correspond to delegate MD,
            //and build DU chain bewteen stpr and new ref PR.
            replaceUseForTree(ref, pr);
            fixup_list.append_tail(pr);

            //Add du chain between new PR and the generated STPR.
            IR * stpr = delegate2stpr.get(delegate);
            ASSERT0(stpr);
            if (m_ssamgr != NULL) {
                m_ssamgr->buildDUChain(stpr, pr);
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


void IR_RP::handlePrelog(
            IR * delegate, IR * pr,
            TMap<IR*, IR*> & delegate2stpr,
            TMap<IR*, DUSet*> & delegate2def,
            IRBB * preheader)
{
    IR * rhs = NULL;
    IR * stpr = NULL;
    if (delegate->is_array()) {
        //Load array value into PR.
        rhs = m_ru->dupIRTree(delegate);
        m_du->copyIRTreeDU(ARR_base(rhs), ARR_base(delegate), true);
        m_du->copyIRTreeDU(ARR_sub_list(rhs),
                                ARR_sub_list(delegate), true);
        stpr = m_ru->buildStorePR(PR_no(pr), IR_dt(pr), rhs);
    } else if (delegate->is_starray()) {
        //Load array value into PR.
        rhs = m_ru->buildArray(m_ru->dupIRTree(ARR_base(delegate)),
            m_ru->dupIRTree(ARR_sub_list(delegate)),
            IR_dt(delegate),
            ARR_elemtype(delegate),
            ((CArray*)delegate)->getDimNum(),
            ARR_elem_num_buf(delegate));

        m_du->copyIRTreeDU(ARR_base(rhs), ARR_base(delegate), true);

        m_du->copyIRTreeDU(ARR_sub_list(rhs), ARR_sub_list(delegate), true);

        stpr = m_ru->buildStorePR(PR_no(pr), IR_dt(pr), rhs);
    } else if (delegate->is_ist()) {
        //Load indirect value into PR.
        rhs = m_ru->buildILoad(m_ru->dupIRTree(IST_base(delegate)),
            IST_ofst(delegate), IR_dt(delegate));
        m_du->copyIRTreeDU(ILD_base(rhs), IST_base(delegate), true);

        stpr = m_ru->buildStorePR(PR_no(pr), IR_dt(pr), rhs);
    } else if (delegate->is_ild()) {
        //Load indirect value into PR.
        rhs = m_ru->dupIRTree(delegate);
        m_du->copyIRTreeDU(ILD_base(rhs), ILD_base(delegate), true);
        stpr = m_ru->buildStorePR(PR_no(pr), IR_dt(pr), rhs);
    } else if (delegate->is_ld()) {
        //Load scalar into PR.
        rhs = m_ru->dupIRTree(delegate);
        m_du->copyIRTreeDU(rhs, delegate, false);
        stpr = m_ru->buildStorePR(PR_no(pr), IR_dt(pr), rhs);
    } else if (delegate->is_st()) {
        //Load scalar into PR.
        rhs = m_ru->buildLoad(ST_idinfo(delegate));
        LD_ofst(rhs) = ST_ofst(delegate);
        stpr = m_ru->buildStorePR(PR_no(pr), IR_dt(pr), rhs);
    } else {
        UNREACHABLE(); //unsupport.
    }

    ASSERT0(rhs && stpr);

    rhs->copyRef(delegate, m_ru);

    //Build DU chain if there exist outer loop reach-def.
    DUSet const* set = delegate2def.get(delegate);
    if (set != NULL) {
        m_du->copyDUSet(rhs, set);
        m_du->unionUse(set, rhs);
    }

    m_ru->allocRefForPR(stpr);

    ASSERT0(delegate2stpr.get(pr) == NULL);
    delegate2stpr.set(delegate, stpr);
    BB_irlist(preheader).append_tail_ex(stpr);
}


void IR_RP::computeOuterDefUse(
        IR * ref,
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
                IR const* d = m_ru->getIR(i);
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
                IR const* u = m_ru->getIR(i);
                ASSERT0(u->is_exp());
                if (!li->isInsideLoop(BB_id(u->getStmt()->getBB()))) {
                    set->bunion(i, *sbs_mgr);
                }
            }
        }
    }
}


void IR_RP::createDelegateInfo(
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
        pr = m_ru->buildPR(IR_dt(delegate));
        delegate2pr.set(delegate, pr);
    }
}


//Return true if there is IR be promoted, otherwise return false.
bool IR_RP::promoteInexactAccess(
        LI<IRBB> const* li,
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

    DefMiscBitSetMgr * sbs_mgr = m_ru->getMiscBitSetMgr();

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

    ASSERT0(verifyIRandBB(m_ru->getBBList(), m_ru));

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
        m_ru->freeIRTree(ref);
    }

    freeLocalStruct(delegate2use, delegate2def, delegate2pr, sbs_mgr);
    return true;
}


void IR_RP::freeLocalStruct(
        TMap<IR*, DUSet*> & delegate2use,
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
        m_ru->freeIRTree(pr);
    }
}


//Determine whether the memory reference is same array or
//definitly different array.
UINT IR_RP::analyzeArrayStatus(IR const* ref1, IR const* ref2)
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
void IR_RP::checkAndRemoveInvalidExactOcc(List<IR*> & exact_occ_list)
{
    xcom::C<IR*> * ct, * nct;
    for (exact_occ_list.get_head(&ct), nct = ct; ct != NULL; ct = nct) {
        IR * occ = ct->val();
        exact_occ_list.get_next(&nct);

        MD const* md = occ->getRefMD();
        ASSERT0(md && md->is_exact());

        //We record all MD that are not suit for promotion, and perform
        //the rest job here that remove all related OCC in exact_list.
        //The MD of promotable candidate must not overlapped each other.
        if (m_dont_promot.is_overlap(md)) {
            exact_occ_list.remove(ct);
        }
    }
}


void IR_RP::buildDepGraph(
        TMap<MD const*, IR*> & exact_access,
        TTab<IR*> & inexact_access,
        List<IR*> & exact_occ_list)
{
    DUMMYUSE(exact_access);
    DUMMYUSE(inexact_access);
    DUMMYUSE(exact_occ_list);
}


bool IR_RP::tryPromote(
        LI<IRBB> const* li,
        IRBB * exit_bb,
        IRIter & ii,
        TabIter<IR*> & ti,
        TMap<MD const*, IR*> & exact_access,
        TTab<IR*> & inexact_access,
        List<IR*> & exact_occ_list)
{
    ASSERT0(li && exit_bb);
    exact_access.clean();
    inexact_access.clean();
    exact_occ_list.clean();
    m_dont_promot.clean(*m_misc_bs_mgr);
    bool change = false;

    for (INT i = LI_bb_set(li)->get_first();
         i != -1; i = LI_bb_set(li)->get_next(i)) {
        IRBB * bb = m_cfg->getBB(i);
        ASSERT0(bb && m_cfg->get_vertex(BB_id(bb)));
        if (bb->hasReturn()) {
            return false;
        }

        if (!scanBB(bb, li, exact_access, inexact_access,
                    exact_occ_list, ii)) {
            return false;
        }
    }

    //m_dont_promot.dump();
    //dump_access(exact_access, m_tm);
    //dump_occ_list(exact_occ_list, m_tm);
    //dump_access2(inexact_access, m_tm);

    IRBB * preheader = NULL;
    if (exact_access.get_elem_count() != 0 ||
        inexact_access.get_elem_count() != 0) {
        preheader = ::findAndInsertPreheader(li, m_ru, m_is_insert_bb, false);
        ASSERT0(preheader);
        IR const* last = BB_last_ir(preheader);
        if (last != NULL && last->isCallStmt()) {
            preheader = ::findAndInsertPreheader(li, m_ru,
                                                m_is_insert_bb, true);
            ASSERT0(preheader);
            ASSERT0(BB_last_ir(preheader) == NULL);
        }
    }

    if (exact_access.get_elem_count() != 0) {
        #ifdef _DEBUG_
        xcom::BitSet visit;
        for (IR * x = exact_occ_list.get_head();
             x != NULL; x = exact_occ_list.get_next()) {
            ASSERT0(!visit.is_contain(IR_id(x)));
            visit.bunion(IR_id(x));
        }
        #endif

        ASSERT0(exact_occ_list.get_elem_count() != 0);
        checkAndRemoveInvalidExactOcc(exact_occ_list);
        change |= promoteExactAccess(li, ii, ti, preheader, exit_bb,
                             exact_access, exact_occ_list);
    }

    if (inexact_access.get_elem_count() != 0) {
        buildDepGraph(exact_access, inexact_access, exact_occ_list);
        change |= promoteInexactAccess(li, preheader, exit_bb,
                                       inexact_access, ii, ti);
    }
    return change;
}


bool IR_RP::EvaluableScalarReplacement(List<LI<IRBB> const*> & worklst)
{
    //Record the map between MD and ARRAY access expression.
    TMap<MD const*, IR*> access;

    TMap<MD const*, IR*> exact_access;
    TTab<IR*> inexact_access;
    List<IR*> exact_occ_list;
    IRIter ii;
    TabIter<IR*> ti;
    bool change = false;
    while (worklst.get_elem_count() > 0) {
        LI<IRBB> const* x = worklst.remove_head();
        IRBB * exit_bb = findSingleExitBB(x);
        if (exit_bb != NULL) {
            //If we did not find a single exit bb, this loop is nontrivial.
            change |= tryPromote(x, exit_bb, ii, ti, exact_access,
                                 inexact_access, exact_occ_list);
        }

        x = LI_inner_list(x);
        while (x != NULL) {
            worklst.append_tail(x);
            x = LI_next(x);
        }
    }
    return change;
}


//Perform scalar replacement of aggregates and array.
bool IR_RP::perform(OptCtx & oc)
{
    START_TIMER(t, getPassName());
    m_ru->checkValidAndRecompute(&oc, PASS_DU_CHAIN, PASS_LOOP_INFO,
                                 PASS_DU_REF, PASS_UNDEF);

    if (!OC_is_du_chain_valid(oc)) {
        END_TIMER(t, getPassName());
        return false;
    }

    //computeLiveness();
    m_ssamgr = NULL;
    PRSSAMgr * ssamgr =
            (PRSSAMgr*)(m_ru->getPassMgr()->queryPass(PASS_PR_SSA_MGR));
    if (ssamgr != NULL && ssamgr->isSSAConstructed()) {
        m_ssamgr = ssamgr;
    }

    ASSERTN(m_ssamgr == NULL,
        ("TODO: Do SSA renaming when after register promotion done"));

    LI<IRBB> const* li = m_cfg->getLoopInfo();
    if (li == NULL) { return false; }

    SMemPool * cspool = smpoolCreate(
        sizeof(xcom::SC<LI<IRBB> const*>), MEM_CONST_SIZE);
    List<LI<IRBB> const*> worklst;
    while (li != NULL) {
        worklst.append_tail(li);
        li = LI_next(li);
    }

    bool change = false;
    if (m_gvn == NULL) {
        //Need gvn.
        goto FIN;
    }

    if (!m_gvn->is_valid()) {
        m_gvn->reperform(oc);
    }

    change = EvaluableScalarReplacement(worklst);
    if (change) {
        //DU reference and du chain has maintained.
        ASSERT0(m_ru->verifyMDRef());
        ASSERT0(m_du->verifyMDDUChain(COMPUTE_PR_DU | COMPUTE_NONPR_DU));

        OC_is_reach_def_valid(oc) = false;
        OC_is_avail_reach_def_valid(oc) = false;
        OC_is_live_expr_valid(oc) = false;

        //Enforce followed pass to recompute gvn.
        m_gvn->set_valid(false);
    }

    if (m_is_insert_bb) {
        OC_is_cdg_valid(oc) = false;
        OC_is_dom_valid(oc) = false;
        OC_is_pdom_valid(oc) = false;
        OC_is_rpo_valid(oc) = false;
        //Loop info is unchanged.
    }

FIN:
    if (g_is_dump_after_pass) {
        dump();
    }
    //buildLifeTime();
    //dump_mdlt();
    smpoolDelete(cspool);
    END_TIMER(t, getPassName());
    return change;
}
//END IR_RP

} //namespace xoc
