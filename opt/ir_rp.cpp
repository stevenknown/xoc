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

//Return true if occ can be regarded as candidate to promoted to PR.
static bool isStmtCand(IR const* occ)
{
    return occ->is_ist() || occ->is_st() || occ->is_starray();
}


//Return true if occ can be regarded as candidate to promoted to PR.
static bool isExpCand(IR const* occ)
{
    return occ->is_ild() || occ->is_ld() || occ->is_array();
}


//Note the delegate is one of references in Occs List of exact delegate
//table. Note, during the IR free processing, the occ should not be
//freed if it is IR_UNDEF. This is because the occ is one of the kid
//of some other stmt|exp which has already been freed.
static void freeExactOccs(IRList & occs, Region * rg, GVN * gvn)
{
    IRListIter irit;
    for (IR * occ = occs.get_head(&irit);
         occ != nullptr; occ = occs.get_next(&irit)) {
        if (occ->is_undef()) {
            //occ is the kid of other stmt/exp, and
            //that stmt/exp has been freed.
            continue;
        }

        gvn->cleanIRTreeVN(occ);
        //Note the delegate is one of references in Occs List.
        rg->freeIRTree(occ);
    }
    occs.clean();
}


//Note the delegate is one of reference in 'inexact_tab'.
static void freeInexactOccs(InexactAccTab & tab, Region * rg, GVN * gvn)
{
    InexactAccTabIter ti;
    for (IR * occ = tab.get_first(ti);
         occ != nullptr; occ = tab.get_next(ti)) {
        if (occ->is_undef()) {
            //occ is the kid of other stmt/exp, and
            //that stmt/exp has been freed.
            continue;
        }

        gvn->cleanIRTreeVN(occ);
        //Note the delegate is one of elements in 'tab'.
        rg->freeIRTree(occ);
    }
}


//
//START ExactAccTab
//
//Collect outside loop DefUse information.
void ExactAccTab::collectOutsideLoopDU(MD const* md, IR const* delegate,
                                       LI<IRBB> const* li,
                                       MOD DelegateMgr & delemgr)
{
    IRListIter it;
    IRList * occs = getOccs(md);
    ASSERT0(occs);
    for (IR * occ = occs->get_head(&it);
         occ != nullptr; occ = occs->get_next(&it)) {
        delemgr.collectOutsideLoopDefUse(occ, delegate, li);
    }
}


void ExactAccTab::dump(Region const* rg) const
{
    if (!rg->isLogMgrInit()) { return; }
    note(rg, "\n==-- DUMP ExactAccessTab --==");
    rg->getLogMgr()->incIndent(2);
    ExactAccTabIter it;
    IR * dele;
    for (MD const* md = get_first(it, &dele);
         md != nullptr; md = get_next(it, &dele)) {
        note(rg, "\nDELEGATE:");
        rg->getLogMgr()->incIndent(2);
        md->dump(rg->getTypeMgr());

        if (dele == nullptr) {
            //There is not delegate corresponding to md.
            note(rg, "\n--");
        } else {
            dumpIR(dele, rg, nullptr, IR_DUMP_DEF);
        }
        note(rg, "\n");
        rg->getLogMgr()->decIndent(2);

        if (dele == nullptr) {
            continue;
        }

        IRList const* occlst = m_md2occlst.get(md->id());
        ASSERT0(occlst);

        rg->getLogMgr()->incIndent(2);

        //Dump occ list.
        note(rg, "\nOCCLIST:");
        rg->getLogMgr()->incIndent(2);
        IRListIter irit;
        for (IR const* occ = occlst->get_head(&irit); occ != nullptr;
             occ = occlst->get_next(&irit)) {
            dumpIR(occ, rg, nullptr, IR_DUMP_DEF);
        }
        rg->getLogMgr()->decIndent(2);

        rg->getLogMgr()->decIndent(2);
        note(rg, "\n");
    }
    note(rg, "\n");
    rg->getLogMgr()->decIndent(2);
}


bool ExactAccTab::verify() const
{
    ExactAccTabIter it;
    IR * dele;
    for (MD const* md = get_first(it, &dele);
         md != nullptr; md = get_next(it, &dele)) {
        ASSERT0(md == dele->getMustRef());
        IRList const* occlst = m_md2occlst.get(md->id());
        ASSERT0(occlst && occlst->get_elem_count() != 0);
        IRListIter irit;
        for (IR const* occ = occlst->get_head(&irit); occ != nullptr;
             occ = occlst->get_next(&irit)) {
            ASSERT0(const_cast<IR*>(occ)->getMustRef() == md);
        }
    }
    return true;
}


//The function will add 'ir' as occurrence for specific MD.
//The first ir will be regarded as a delegate for all those IRs which
//reference the same MD.
//Note the first ir to specific MD is the delegate.
void ExactAccTab::addOcc(IR * ir)
{
    MD const* md = ir->getMustRef();
    ASSERT0(md && md->is_exact());
    IRList * lst = nullptr;
    if (!find(md)) {
        set(md, ir);
        ASSERT0(m_md2occlst.get(md->id()) == nullptr);

        //The first ir that added to list will be the delegate.
        lst = new IRList();
        m_md2occlst.set(md->id(), lst);
    } else {
        lst = m_md2occlst.get(md->id());
    }
    ASSERT0(lst && !lst->find(ir));
    lst->append_tail(ir);
}


//Return true if ir in occs list is unique.
bool ExactAccTab::isOccUnique(DefMiscBitSetMgr * sm) const
{
    DefSBitSet visit(sm->getSegMgr());
    IRList * lst = nullptr;
    MD2OccLstIter it;
    for (MDIdx mdid = m_md2occlst.get_first(it, &lst); mdid != MD_UNDEF;
         mdid = m_md2occlst.get_next(it, &lst)) {
        ASSERT0(lst);
        IRListIter irit;
        for (IR const* x = lst->get_head(&irit);
             x != nullptr; x = lst->get_next(&irit)) {
            ASSERT0(!visit.is_contain(x->id()));
            visit.bunion(x->id());
        }
    }
    //The resource of visit will be freed in destructor of DefSBitSet.
    return true;
}


void ExactAccTab::remove(MD const* md)
{
    TMap<MD const*, IR*>::remove(md);
    IRList * lst = m_md2occlst.remove(md->id());
    ASSERT0(lst);
    delete lst;
}
//END ExactAccTab


//
//START InexactAccTab
//
void InexactAccTab::dump(Region * rg) const
{
    if (!rg->isLogMgrInit()) { return; }
    note(rg, "\n==-- DUMP InexactAccessTab --==");
    rg->getLogMgr()->incIndent(2);
    InexactAccTabIter it;
    for (IR const* ir = get_first(it);
         ir != nullptr; ir = get_next(it)) {
        //InexactAcc always has complex IR tree.
        dumpIR(ir, rg, nullptr, IR_DUMP_DEF|IR_DUMP_KID);
        note(rg, "\n");
    }
    note(rg, "\n");
    rg->getLogMgr()->decIndent(2);
}
//END InexactAccTab


//
//START RefHashFunc
//
void RefHashFunc::initMem(GVN * gvn)
{
    ASSERT0(gvn);
    m_gvn = gvn;
}


//The function will modify m_iter.
UINT RefHashFunc::get_hash_value(IR * t, UINT bucket_size) const
{
    ASSERT0(bucket_size != 0 && isPowerOf2(bucket_size));
    UINT hval = 0;
    ConstIRIter it;
    #define HASHIRCODE(code, x) (UINT)((code << 4) + \
                                ((x->getOffset() + 1) << 10) + \
                                (x->getDType() << 19))
    #define HASHVAR(x) (UINT)(ID_info(x)->id() * 5)
    switch (t->getCode()) {
    case IR_LD:
    case IR_ST:
        hval = HASHIRCODE(IR_LD, t);
        break;
    case IR_ILD:
    case IR_IST:
        hval += HASHIRCODE(IR_ILD, t);
        for (IR const* x = iterInitC(t->getBase(), it, true);
             x != nullptr; x = iterNextC(it, true)) {
            UINT v = HASHIRCODE(x->getCode(), x);
            if (x->is_id()) {
                v += HASHVAR(x);
            }
            hval += v;
        }
        break;
    case IR_ARRAY:
    case IR_STARRAY:
        hval += HASHIRCODE(IR_ARRAY, t);
        for (IR const* x = iterInitC(ARR_base(t), it, true);
             x != nullptr; x = iterNextC(it, true)) {
            UINT v = HASHIRCODE(x->getCode(), x);
            if (x->is_id()) {
                v += HASHVAR(x);
            }
            hval += v;
        }
        for (IR const* x = iterInitC(ARR_sub_list(t), it, true);
             x != nullptr; x = iterNextC(it, true)) {
            UINT v = HASHIRCODE(x->getCode(), x);
            if (x->is_id()) {
                v += HASHVAR(x);
            }
            hval += v;
        }
        break;
    default: UNREACHABLE(); //unsupport.
    }
    return hash32bit(hval) & (bucket_size - 1);
}


bool RefHashFunc::compareArray(IR * t1, IR * t2) const
{
    ASSERT0(m_gvn);
    if (t1 == t2) { return true; }

    VN const* vn1 = m_gvn->mapIR2VNConst(ARR_base(t1));
    VN const* vn2 = m_gvn->mapIR2VNConst(ARR_base(t2));
    if (vn1 != vn2 || vn1 == nullptr) {
        return false;
    }

    if (((CArray*)t1)->getDimNum() != ((CArray*)t2)->getDimNum()) {
        return false;
    }

    IR * s1 = ARR_sub_list(t1);
    IR * s2 = ARR_sub_list(t2);
    for (; s1 != nullptr && s2 != nullptr;
         s1 = IR_next(s1), s2 = IR_next(s2)) {
        VN const* vn1 = m_gvn->mapIR2VNConst(s1);
        VN const* vn2 = m_gvn->mapIR2VNConst(s2);
        if (vn1 == nullptr || vn1 != vn2) {
            return false;
        }
    }

    if (s1 != nullptr || s2 != nullptr) { return false; }
    if (ARR_ofst(t1) != ARR_ofst(t2)) { return false; }
    if (t1->getType() != t2->getType()) { return false; }
    return true;
}


bool RefHashFunc::compareIndirectAccess(IR * t1, IR * t2) const
{
    ASSERT0(m_gvn);
    if (t1 == t2) { return true; }

    IR const* base1 = t1->getBase();
    ASSERT0(base1);

    IR const* base2 = t2->getBase();
    ASSERT0(base2);

    VN const* vn1 = m_gvn->mapIR2VNConst(base1);
    VN const* vn2 = m_gvn->mapIR2VNConst(base2);
    if (vn1 != vn2 || vn1 == nullptr) {
        return false;
    }
    if (t1->getOffset() != t2->getOffset()) { return false; }
    if (t1->getType() != t2->getType()) { return false; }
    return true;
}


bool RefHashFunc::compareDirectAccess(IR * t1, IR * t2) const
{
    ASSERT0(m_gvn);
    if (t1 == t2) { return true; }

    VN const* vn1 = m_gvn->mapIR2VNConst(t1);
    VN const* vn2 = m_gvn->mapIR2VNConst(t2);
    if (vn1 != vn2 || vn1 == nullptr) {
        return false;
    }
    if (t1->getOffset() != t2->getOffset()) { return false; }
    if (t1->getType() != t2->getType()) { return false; }
    return true;
}


bool RefHashFunc::compare(IR * t1, IR * t2) const
{
    if (t1->isArrayOp() && t2->isArrayOp()) {
        return compareArray(t1, t2);
    }
    if (t1->isIndirectMemOp() && t2->isIndirectMemOp()) {
        return compareIndirectAccess(t1, t2);
    }
    if (t1->isDirectMemOp() && t2->isDirectMemOp()) {
        return compareDirectAccess(t1, t2);
    }
    return false;
}
//END RefHashFunc


class PromotedTab : public xcom::TTab<UINT> {
public:
    PromotedTab() {}

    //Add whole ir tree into table.
    void addTree(IR const* ir, IRIter & ii)
    {
        ii.clean();
        for (IR const* x = xoc::iterInit(const_cast<IR*>(ir), ii);
             x != nullptr; x = xoc::iterNext(ii)) {
            append(x->id());
        }
    }
};


//
//START DelegateMgr
//
void DelegateMgr::collectOutsideLoopUse(IR const* delegate, IRSet const& set,
                                        LI<IRBB> const* li)
{
    DUSet * useset = nullptr;
    IRSetIter di = nullptr;
    for (INT i = set.get_first(&di); i >= 0; i = set.get_next(i, &di)) {
        IR * u = m_rg->getIR(i);
        ASSERT0(u->is_exp());
        if (u->is_id()) {
            IRBB * bb = ID_phi(u)->getBB();
            if (!li->isInsideLoop(bb->id())) {
                if (useset == nullptr) {
                    useset = genOutsideUseSet(delegate);
                }
                useset->bunion(i, *getSBSMgr());
            }
            continue;
        }

        if (!li->isInsideLoop(u->getStmt()->getBB()->id())) {
            if (useset == nullptr) {
                useset = genOutsideUseSet(delegate);
            }
            useset->bunion(i, *getSBSMgr());
        }
    }
}


void DelegateMgr::collectOutsideLoopDef(IR const* delegate, IRSet const& set,
                                        LI<IRBB> const* li)
{
    DUSet * defset = nullptr;
    IRSetIter di = nullptr;
    for (INT i = set.get_first(&di); i >= 0; i = set.get_next(i, &di)) {
        IR * d = m_rg->getIR(i);
        ASSERT0(d->is_stmt());
        if (!li->isInsideLoop(d->getBB()->id())) {
            if (defset == nullptr) {
                defset = genOutsideDefSet(delegate);
            }
            defset->bunion(i, *getSBSMgr());
        }
    }
}


//The function collects the outside loop DEF|USE for 'delegate'.
//Note the function does NOT build any DU chain for any IR, it is just
//do collection.
void DelegateMgr::collectOutsideLoopDefUse(IR const* occ, IR const* delegate,
                                           LI<IRBB> const* li)
{
    ASSERT0(occ->isMemoryRefNonPR());
    ASSERTN(occ->getSSAInfo() == nullptr, ("should not have SSA du"));
    IRSet irset(getSegMgr());
    if (occ->is_exp()) {
        //occ is USE.
        xoc::collectDefSet(occ, m_mdssamgr, &irset);
        collectOutsideLoopDef(delegate, irset, li);
        return;
    }

    //occ is DEF.
    ASSERT0(occ->is_stmt());
    xoc::collectUseSet(occ, m_mdssamgr, &irset);
    collectOutsideLoopUse(delegate, irset, li);
}


//The function add delegate using straightforward strategy. Note user must
//ensure the delegate is unique.
void DelegateMgr::createDelegateInfo(IR * delegate)
{
    //Create PR that records the value which is identical to the delegate.
    //throughout the entire life-time of delegate.
    IR * pr = m_dele2pr.get(delegate);
    if (pr == nullptr) {
        pr = m_rg->buildPR(delegate->getType());
        m_rg->allocRefForPR(pr);
        m_dele2pr.set(delegate, pr);
    }
}


//The function generates the initialing stmt for delegated PR.
//rhs: the RHS of initialing stmt.
IR * DelegateMgr::genInitStmt(IR const* delegate, IR * rhs)
{
    ASSERTN(m_dele2init.get(const_cast<IR*>(delegate)) == nullptr,
            ("reproduce init-stmt"));
    IR const* pr = getPR(delegate);
    ASSERT0(pr);
    IR * stpr = m_rg->buildStorePR(PR_no(pr), pr->getType(), rhs);
    m_rg->allocRefForPR(stpr);
    m_dele2init.set(const_cast<IR*>(delegate), stpr);
    return stpr;
}


//The function using RefTab to create delegate for 'ref' to keep the delegate
//is unique. Note the delegate may be 'ref' itself.
IR * DelegateMgr::createUniqueDelegate(IR * ref)
{
    bool find_exist_one = false;
    IR * unique_delegate = m_deletab.append(ref, nullptr, &find_exist_one);
    ASSERT0(unique_delegate);
    if (!find_exist_one) {
        createDelegateInfo(unique_delegate);
    }
    return unique_delegate;
}


void DelegateMgr::clean()
{
    TMapIter<IR const*, DUSet*> it;
    DUSet * duset;
    for (IR const* x = m_dele2outsideuseset.get_first(it, &duset);
         x != nullptr; x = m_dele2outsideuseset.get_next(it, &duset)) {
        if (duset != nullptr) {
            getSBSMgr()->freeSBitSetCore(duset);
        }
    }

    it.clean();
    for (IR const* x = m_dele2outsidedefset.get_first(it, &duset);
         x != nullptr; x = m_dele2outsidedefset.get_next(it, &duset)) {
        if (duset != nullptr) {
            getSBSMgr()->freeSBitSetCore(duset);
        }
    }

    TMapIter<IR*, IR*> mapit;
    IR * pr;
    for (IR * x = m_dele2pr.get_first(mapit, &pr);
         x != nullptr; x = m_dele2pr.get_next(mapit, &pr)) {
        m_rg->freeIRTree(pr);

        //Note the delegate is one of references in Occs List of
        //'exact_tab'. So we do NOT need to free it here because it has
        //been freed during promotion processing.
        //m_rg->freeIRTree(x);
    }
    m_dele2init.clean();
    m_dele2restore.clean();

    smpoolDelete(m_pool);
    m_pool = nullptr;
}


void DelegateMgr::dumpDele2Restore() const
{
    note(m_rg, "\n\n==-- Dele2RestoreStmt --==");
    TMapIter<IR*, IR*> it;
    IR * stmt;
    for (IR * dele = m_dele2restore.get_first(it, &stmt); dele != nullptr;
         dele = m_dele2init.get_next(it, &stmt)) {
        note(m_rg, "\n");
        note(m_rg, "\nDELE:");
        dumpIR(dele, m_rg, nullptr, IR_DUMP_NO_NEWLINE);
        note(m_rg, "\nRestoreStmt:");
        dumpIR(stmt, m_rg, nullptr, IR_DUMP_KID);
    }
}


bool DelegateMgr::dump() const
{
    if (!m_rg->isLogMgrInit()) { return false; }
    note(m_rg, "\n==---- DUMP DelegateMgr '%s' ----==",
         m_rg->getRegionName());
    m_rg->getLogMgr()->incIndent(2);

    note(m_rg, "\n==-- Dele2PR --==");
    TMapIter<IR*, IR*> it;
    IR * pr;
    for (IR * dele = m_dele2pr.get_first(it, &pr); dele != nullptr;
         dele = m_dele2pr.get_next(it, &pr)) {
        note(m_rg, "\n");
        note(m_rg, "\nDELE:");
        dumpIR(dele, m_rg, nullptr, IR_DUMP_NO_NEWLINE);
        note(m_rg, "\nPR:");
        dumpIR(pr, m_rg, nullptr, IR_DUMP_NO_NEWLINE);
    }

    note(m_rg, "\n\n==-- Dele2InitSTPR --==");
    it.clean();
    IR * stpr;
    for (IR * dele = m_dele2init.get_first(it, &stpr); dele != nullptr;
         dele = m_dele2init.get_next(it, &stpr)) {
        note(m_rg, "\n");
        note(m_rg, "\nDELE:");
        dumpIR(dele, m_rg, nullptr, IR_DUMP_NO_NEWLINE);
        note(m_rg, "\nInitSTPR:");
        dumpIR(stpr, m_rg, nullptr, IR_DUMP_KID);
    }

    dumpDele2Restore();

    note(m_rg, "\n\n==-- Dele2OutsideDefSet --==");
    TMapIter<IR const*, DUSet*> it3;
    DUSet * set;
    for (IR const* dele = m_dele2outsidedefset.get_first(it3, &set);
         dele != nullptr;
         dele = m_dele2outsidedefset.get_next(it3, &set)) {
        note(m_rg, "\n");
        note(m_rg, "\nDELE:");
        dumpIR(dele, m_rg, nullptr, IR_DUMP_NO_NEWLINE);

        note(m_rg, "\nOutsideDefSet:");
        m_rg->getLogMgr()->incIndent(2);
        DUSetIter di = nullptr;
        for (INT i = set->get_first(&di); i >= 0;
             i = set->get_next(i, &di)) {
            IR * ir = m_rg->getIR(i);
            ASSERT0(ir);
            dumpIR(ir, m_rg, nullptr, IR_DUMP_NO_NEWLINE);
        }
        m_rg->getLogMgr()->decIndent(2);
    }

    note(m_rg, "\n\n==-- Dele2OutsideUseSet --==");
    TMapIter<IR const*, DUSet*> it4;
    DUSet * set2;
    for (IR const* dele = m_dele2outsideuseset.get_first(it4, &set2);
         dele != nullptr;
         dele = m_dele2outsideuseset.get_next(it4, &set2)) {
        note(m_rg, "\n");
        note(m_rg, "\nDELE:");
        dumpIR(dele, m_rg, nullptr, IR_DUMP_NO_NEWLINE);

        note(m_rg, "\nOutsideUseSet:");
        m_rg->getLogMgr()->incIndent(2);
        DUSetIter di = nullptr;
        for (INT i = set2->get_first(&di); i >= 0;
             i = set2->get_next(i, &di)) {
            IR * ir = m_rg->getIR(i);
            ASSERT0(ir);
            dumpIR(ir, m_rg, nullptr, IR_DUMP_NO_NEWLINE);
        }
        m_rg->getLogMgr()->decIndent(2);
    }
    note(m_rg, "\n");

    m_deletab.dump(m_rg);
    note(m_rg, "\n");

    m_rg->getLogMgr()->decIndent(2);
    return true;
}


//Generate code to fulfill epilog of delegate.
//pr: the PR to be restored to memory.
IR * DelegateMgr::genRestoreStmt(IR const* delegate, IR * pr)
{
    ASSERT0(pr && pr->is_pr());
    IR * stmt = nullptr;
    switch (delegate->getCode()) {
    case IR_ARRAY: {
        stmt = CStArray::dupIRTreeByExp(delegate, pr, m_rg);
        IR * newbase = ARR_base(stmt);
        IR * newsublist = ARR_sub_list(stmt);
        xoc::addUseForTree(newbase, ARR_base(delegate), m_rg);
        xoc::addUseForTree(newsublist, ARR_sub_list(delegate), m_rg);
        break;
    }
    case IR_STARRAY: {
        //Prepare base and subscript expression list.
        IR * newbase = m_rg->dupIRTree(ARR_base(delegate));
        IR * newsublist = m_rg->dupIRTreeList(ARR_sub_list(delegate));
        xoc::addUseForTree(newbase, ARR_base(delegate), m_rg);
        xoc::addUseForTree(newsublist, ARR_sub_list(delegate), m_rg);

        stmt = m_rg->buildStoreArray(newbase, newsublist, delegate->getType(),
                                     ARR_elemtype(delegate),
                                     ((CArray*)delegate)->getDimNum(),
                                     ARR_elem_num_buf(delegate), pr);
        stmt->setOffset(delegate->getOffset());
        stmt->copyRef(delegate, m_rg);
        break;
    }
    case IR_IST: {
        IR * newbase = m_rg->dupIRTree(delegate->getBase());
        xoc::addUseForTree(newbase, delegate->getBase(), m_rg);

        stmt = m_rg->buildIStore(newbase, pr,
                                 delegate->getOffset(), delegate->getType());
        stmt->copyRef(delegate, m_rg);
        break;
    }
    case IR_ST:
        stmt = m_rg->buildStore(ST_idinfo(delegate), delegate->getType(),
                                delegate->getOffset(), pr);
        stmt->copyRef(delegate, m_rg);
        break;
    case IR_ILD: {
        IR * newbase = stmt->getBase();
        xoc::addUseForTree(newbase, delegate->getBase(), m_rg);

        stmt = CISt::dupIRTreeByExp(delegate, pr, m_rg);
        break;
    }
    case IR_LD:
        stmt = CSt::dupIRTreeByExp(delegate, pr, m_rg);
        break;
    default: UNREACHABLE(); //Unsupport.
    }
    ASSERT0(stmt);
    m_dele2restore.set(const_cast<IR*>(delegate), stmt);
    return stmt;
}
//END DelegateMgr


//
//START RegPromot
//
MDLT * RegPromot::getMDLifeTime(MD * md)
{
    MDLT * lt;
    if ((lt = m_md2lt_map->get(md)) != nullptr) {
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
        if (lt != nullptr) {
            ASSERT0(MDLT_livebbs(lt) != nullptr);
            MDLT_livebbs(lt)->clean();
        }
    }
}


void RegPromot::clean()
{
    cleanLiveBBSet();
    m_dont_promote.destroy();
    m_dont_promote.init();
}


void RegPromot::buildLifeTime()
{
    cleanLiveBBSet();

    //Rebuild life time.
    BBList * bbl = m_rg->getBBList();
    MDLivenessMgr * livemgr = getMDLivenessMgr();
    for (IRBB * bb = bbl->get_head(); bb != nullptr; bb = bbl->get_next()) {
        MDSet * livein = livemgr->getLiveInMDSet(bb);
        MDSet * liveout = livemgr->getLiveOutMDSet(bb);
        if (livein->is_empty() && liveout->is_empty()) { continue; }

        MDSetIter it;
        for (INT i = livein->get_first(&it);
             i >= 0; i = livein->get_next(i, &it)) {
            MDLT_livebbs(getMDLifeTime(m_md_sys->getMD(i)))->bunion(bb->id());
        }
        for (INT i = liveout->get_first(&it);
             i >= 0; i = liveout->get_next(i, &it)) {
            MDLT_livebbs(getMDLifeTime(m_md_sys->getMD(i)))->bunion(bb->id());
        }
    }
}


bool RegPromot::checkArrayIsLoopInvariant(IN IR * ir, LI<IRBB> const* li)
{
    ASSERT0(ir->isArrayOp() && li);
    for (IR * s = ARR_sub_list(ir); s != nullptr; s = s->get_next()) {
        if (!xoc::isLoopInvariant(s, li, m_rg, nullptr, true)) {
            return false;
        }
    }
    if (!xoc::isLoopInvariant(ARR_base(ir), li, m_rg, nullptr, true)) {
        return false;
    }
    return true;
}


//Return true if the caller can keep doing the analysis.
//That means there are no memory referrences clobbered the
//candidate in occs of exact_tab.
//Or else the analysis for current loop should be terminated.
//Return false if find unpromotable memory reference, this may
//prevent entire loop to be promoted.
//added: true to indicates given ir has been inserted into either 'exact_tab'
//       or 'inexact_tab'.
bool RegPromot::handleArrayRef(IN IR * ir, LI<IRBB> const* li,
                               OUT ExactAccTab & exact_tab,
                               OUT InexactAccTab & inexact_tab,
                               bool * added)
{
    ASSERT0(ir->isArrayOp());
    if (!checkArrayIsLoopInvariant(ir, li)) {
        //If ir is ARRAY|STARRAY that modify inexact MD.
        //It may clobber all other array with same array base.
        clobberAccess(ir, exact_tab, inexact_tab);
        return true;
    }

    if (ir->is_stmt()) {
        ASSERT0(ir->is_starray());
        InexactAccTabIter ti;
        for (IR * ref = inexact_tab.get_first(ti);
             ref != nullptr; ref = inexact_tab.get_next(ti)) {
            UINT st = analyzeArrayStatus(ir, ref);
            if (st == RP_SAME_ARRAY) { continue; }
            if (st == RP_DIFFERENT_ARRAY) { continue; }
 
            //The result can not be promoted.
            //Check the promotable candidates if current stmt modify
            //the related MD.
            clobberAccess(ir, exact_tab, inexact_tab);
            return true;
        }
    }
    inexact_tab.addOcc(ir);
    *added = true;
    return true;
}


//Return true if the caller can keep doing the analysis.
//That means there are no memory referrences clobbered the
//candidate in of exact_tab.
//Return false if find unpromotable memory reference, this may
//prevent entire loop be promoted.
//ir: stmt or expression to be handled.
//added: true to indicates given ir has been inserted into either 'exact_tab'
//       or 'inexact_tab'.
bool RegPromot::handleGeneralMustRef(IR * ir, LI<IRBB> const* li,
                                     OUT ExactAccTab & exact_tab,
                                     OUT InexactAccTab & inexact_tab,
                                     bool * added)
{
    MD const* mustref = ir->getMustRef();
    ASSERT0(mustref);
    if (mustref->is_volatile()) {
        clobberAccess(ir, exact_tab, inexact_tab);
        return true;
    }

    if (m_dont_promote.is_overlap(ir)) {
        //If ir should not be promoted, then all the others mem-ref
        //that overlapped with it should not be promoted too.
        clobberAccess(ir, exact_tab, inexact_tab);
        return true;
    }

    sweepOutAccess(ir, exact_tab, inexact_tab);

    if (mustref->is_exact()) {
        exact_tab.addOcc(ir);
        *added = true;
        return true;
    }
    return handleInexactOrMayRef(ir, li, exact_tab, inexact_tab, added);
}


//added: true to indicates given ir has been inserted into either 'exact_tab'
//       or 'inexact_tab'.
bool RegPromot::handleIndirect(IR * ir, LI<IRBB> const* li,
                               OUT ExactAccTab & exact_tab,
                               OUT InexactAccTab & inexact_tab,
                               bool * added)
{
    ASSERT0(ir->isIndirectMemOp());
    //Check if ir is loop invariant.
    if (!checkIndirectAccessIsLoopInvariant(ir, li)) {
        clobberAccess(ir, exact_tab, inexact_tab);
        return true;
    }

    if (ir->is_stmt()) {
        ASSERT0(ir->is_ist());
        //Determine wherther current ir clobber elements in access list.
        InexactAccTabIter ti;
        for (IR * ref = inexact_tab.get_first(ti);
             ref != nullptr; ref = inexact_tab.get_next(ti)) {
            if (m_gvn->isSameMemLoc(ir, ref)) { continue; }
            if (m_gvn->isDiffMemLoc(ir, ref)) { continue; }

            //Current ir can not be promoted. Check the promotable candidates
            //if current ir overrided related MDs.
            clobberAccess(ir, exact_tab, inexact_tab);
            return true;
        }
    }
    inexact_tab.addOcc(ir);
    *added = true;
    return true;
}


//The function handle cases either ir does not have MustRef or ir has an
//inexact MustRef.
//added: true to indicates given ir has been inserted into either 'exact_tab'
//       or 'inexact_tab'.
bool RegPromot::handleInexactOrMayRef(IR * ir, LI<IRBB> const* li,
                                      OUT ExactAccTab & exact_tab,
                                      OUT InexactAccTab & inexact_tab,
                                      bool * added)
{
    if (ir->isIndirectMemOp()) {
        return handleIndirect(ir, li, exact_tab, inexact_tab, added);
    }
    if (ir->isArrayOp()) {
        return handleArrayRef(ir, li, exact_tab, inexact_tab, added);
    }
    UNREACHABLE();
    return true;
}


//Return true if the caller can keep doing the analysis.
//That means there are no memory referrences clobbered the
//candidate in of exact_tab.
//Return false if find unpromotable memory reference, this may
//prevent entire loop be promoted.
//ir: stmt or expression to be handled.
//added: true to indicates given ir has been inserted into either 'exact_tab'
//       or 'inexact_tab'.
bool RegPromot::handleGeneralRef(IR * ir, LI<IRBB> const* li,
                                 OUT ExactAccTab & exact_tab,
                                 OUT InexactAccTab & inexact_tab,
                                 bool * added)
{
    ASSERT0(ir->isMemoryRef());
    if (!isPromotable(ir)) { return true; }
    MD const* mustref = ir->getMustRef();
    if (mustref != nullptr) {
        return handleGeneralMustRef(ir, li, exact_tab, inexact_tab, added);
    }
    return handleInexactOrMayRef(ir, li, exact_tab, inexact_tab, added);
}


void RegPromot::addDontPromote(IR const* ir)
{
    m_dont_promote.append(ir);
}


//The function sweep out the Access Expression or Stmt from 'exact_tab' which
//MD reference may or must overlaped with given 'ir'
//except the ones that are exactly covered by 'ir'.
//This function consider both MustRef MD and MayRef MDSet.
void RegPromot::sweepOutExactAccess(IR * ir, MOD ExactAccTab & exact_tab)
{
    MD const* mustref = ir->getMustRef();
    MDSet const* mayref = ir->getMayRef();
    ExactAccTabIter it;
    Vector<MD const*> need_to_be_removed;
    INT cnt = 0;
    if (mustref != nullptr) {
        for (MD const* md = exact_tab.get_first(it, nullptr);
             md != nullptr; md = exact_tab.get_next(it, nullptr)) {
            if (mustref == md || mustref->is_exact_cover(md)) { continue; }
            if (mustref->is_overlap(md)) {
                //occ is not suite to promot any more.
                need_to_be_removed.set(cnt, md);
                cnt++;
            }
        }
    } else if (mayref != nullptr) {
        for (MD const* md = exact_tab.get_first(it, nullptr);
             md != nullptr; md = exact_tab.get_next(it, nullptr)) {
            if (mayref->is_contain(md)) {
                //occ is not suite to promot any more.
                need_to_be_removed.set(cnt, md);
                cnt++;
            }
        }
    }
    for (cnt = cnt - 1; cnt >= 0; cnt--) {
        MD const* md = need_to_be_removed.get(cnt);
        exact_tab.remove(md);
    }
}


//The function sweep out the Access Expression or Stmt from 'inexact_tab' which
//MD reference may or must overlaped with given 'ir'
//except the ones that are exactly covered by 'ir'.
//This function consider both MustRef MD and MayRef MDSet.
void RegPromot::sweepOutInexactAccess(IR * ir, MOD InexactAccTab & inexact_tab)
{
    MD const* mustref = ir->getMustRef();
    MDSet const* mayref = ir->getMayRef();
    InexactAccTabIter iter2;
    Vector<IR*> need_to_be_removed2;
    INT cnt = 0;
    if (mustref != nullptr) {
        for (IR * acc = inexact_tab.get_first(iter2);
             acc != nullptr; acc = inexact_tab.get_next(iter2)) {
            MD const* acc_md = acc->getMustRef();
            MDSet const* acc_mds = acc->getMayRef();
            if (acc_md != nullptr) {
                if (mustref == acc_md || mustref->is_exact_cover(acc_md)) {
                    //Independent MD. There is no need to remove acc.
                    continue;
                }
                if (mustref->is_overlap(acc_md)) {
                    //acc is not suite to promot any more.
                    need_to_be_removed2.set(cnt, acc);
                    cnt++;
                }
                continue;
            }

            if (acc_mds != nullptr && acc_mds->is_overlap(mustref, m_rg)) {
                //acc is not suite to promot any more.
                need_to_be_removed2.set(cnt, acc);
                cnt++;
            }
        }
    }

    if (mayref != nullptr) {
        for (IR * acc = inexact_tab.get_first(iter2);
             acc != nullptr; acc = inexact_tab.get_next(iter2)) {
            MD const* acc_md = acc->getMustRef();
            MDSet const* acc_mds = acc->getMayRef();
            if (acc_md != nullptr && mustref != nullptr) {
                if (mustref == acc_md || mustref->is_exact_cover(acc_md)) {
                    //Independent MD. There is no need to remove acc.
                    continue;
                }
                if (mustref->is_overlap(acc_md)) {
                    //acc is not suite to promot any more.
                    need_to_be_removed2.set(cnt, acc);
                    cnt++;
                }
                //No need to check MayRef.
                continue;
            }

            if ((acc_md != nullptr && mayref->is_overlap(acc_md, m_rg)) ||
                (acc_mds != nullptr &&
                 (acc_mds == mayref || mayref->is_intersect(*acc_mds)))) {
                //acc is not suite to promot any more.
                need_to_be_removed2.set(cnt, acc);
                cnt++;
            }
        }
    }

    for (cnt = cnt - 1; cnt >= 0; cnt--) {
        IR * e = need_to_be_removed2.get(cnt);
        inexact_tab.remove(e);
    }
}


//The function sweep out the Access Expression or Stmt from 'exact_tab' and
//'inexact_tab' which MD reference may or must overlaped with given 'ir'
//except the ones that are exactly covered by 'ir'.
//This function consider both MustRef MD and MayRef MDSet.
void RegPromot::sweepOutAccess(IR * ir, MOD ExactAccTab & exact_tab,
                               MOD InexactAccTab & inexact_tab)
{

    sweepOutExactAccess(ir, exact_tab);
    sweepOutInexactAccess(ir, inexact_tab);
}


void RegPromot::clobberExactAccess(IR const* ir, MOD ExactAccTab & exact_tab)
{
    MD const* mustref = ir->getMustRef();
    MDSet const* mayref = ir->getMayRef();
    ExactAccTabIter it;
    Vector<MD const*> need_to_be_removed;
    INT cnt = 0;
    if (mustref != nullptr) {
        for (MD const* md = exact_tab.get_first(it, nullptr);
             md != nullptr; md = exact_tab.get_next(it, nullptr)) {
            if (mustref == md || mustref->is_overlap(md)) {
                //Current ir may modify the candidate's md.
                //We think the candidate is not suite to promot any more.
                need_to_be_removed.set(cnt, md);
                cnt++;
            }
        }
    }

    if (mayref != nullptr && !mayref->is_empty()) {
        for (MD const* md = exact_tab.get_first(it, nullptr);
             md != nullptr; md = exact_tab.get_next(it, nullptr)) {
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
        exact_tab.remove(md);
    }
}


void RegPromot::clobberInexactAccess(IR const* ir,
                                     MOD InexactAccTab & inexact_tab)
{
    MD const* mustref = ir->getMustRef();
    MDSet const* mayref = ir->getMayRef();
    InexactAccTabIter iter;
    Vector<IR*> need_to_be_removed;
    INT cnt = 0;
    if (mustref != nullptr) {
        for (IR * acc = inexact_tab.get_first(iter);
             acc != nullptr; acc = inexact_tab.get_next(iter)) {
            MD const* acc_md = acc->getMustRef();
            MDSet const* acc_mds = acc->getMayRef();
            if (acc_md != nullptr) {
                if (mustref == acc_md || mustref->is_overlap(acc_md)) {
                    //ir is not suite to promot any more, all mds which
                    //overlapped with it are also not promotable.
                    need_to_be_removed.set(cnt, acc);
                    cnt++;
                }
            } else if (acc_mds != nullptr &&
                       acc_mds->is_overlap(mustref, m_rg)) {
                //ir is not suite to promot any more, all mds which
                //overlapped with it are also not promotable.
                need_to_be_removed.set(cnt, acc);
                cnt++;
            }
        }
    }

    if (mayref != nullptr && !mayref->is_empty()) {
        for (IR * acc = inexact_tab.get_first(iter);
             acc != nullptr; acc = inexact_tab.get_next(iter)) {
            MD const* acc_md = acc->getMustRef();
            MDSet const* acc_mds = acc->getMayRef();
            if (acc_md != nullptr && mustref != nullptr) {
                if (mustref == acc_md || mustref->is_overlap(acc_md)) {
                    //ir is not suite to promot any more, all mds which
                    //overlapped with it are also not promotable.
                    need_to_be_removed.set(cnt, acc);
                    cnt++;
                }
                //No need to check MayRef.
                continue;
            }

            if ((acc_md != nullptr && mayref->is_overlap(acc_md, m_rg)) ||
                (acc_mds != nullptr &&
                 (acc_mds == mayref || mayref->is_intersect(*acc_mds)))) {
                //ir is not suite to promot any more, all mds which
                //overlapped with it are also not promotable.
                need_to_be_removed.set(cnt, acc);
                cnt++;
            }
        }
    }

    for (cnt = cnt - 1; cnt >= 0; cnt--) {
        IR * e = need_to_be_removed.get(cnt);
        inexact_tab.remove(e);
    }
}


//'ir' can not be promoted.
//Check the promotable candidates if ir overlapped with the related
//MD and MDSet.
//This function consider both MustRef MD and MayRef MDSet.
void RegPromot::clobberAccess(IR const* ir, MOD ExactAccTab & exact_tab,
                              MOD InexactAccTab & inexact_tab)
{
    addDontPromote(ir);
    clobberExactAccess(ir, exact_tab);
    clobberInexactAccess(ir, inexact_tab);
}


bool RegPromot::checkIndirectAccessIsLoopInvariant(IR const* ir,
                                                   LI<IRBB> const* li)
{
    ASSERT0(li);
    if (ir->is_ild() || ir->is_ist()) {
        return xoc::isLoopInvariant(ir->getBase(), li, m_rg, nullptr, true);
    }
    return true;
}


//Determine whether the memory reference is same object or different.
UINT RegPromot::analyzeIndirectAccessStatus(IR const* ref1, IR const* ref2)
{
    IR const* base1 = nullptr;
    if (ref1->is_ild()) {
        base1 = ILD_base(ref1);
    } else if (ref1->is_ist()) {
        base1 = IST_base(ref1);
    } else {
        return RP_UNKNOWN;
    }

    IR const* base2 = nullptr;
    if (ref2->is_ild()) {
        base2 = ILD_base(ref2);
    } else if (ref2->is_ist()) {
        base2 = IST_base(ref2);
    } else {
        return RP_UNKNOWN;
    }

    ASSERT0(base1->is_ptr() && base2->is_ptr());

    ASSERT0(m_gvn);

    VN const* vn1 = m_gvn->mapIR2VNConst(base1);
    VN const* vn2 = m_gvn->mapIR2VNConst(base2);
    if (vn1 == nullptr || vn2 == nullptr) { return RP_UNKNOWN; }

    UINT tysz1 = ref1->getTypeSize(m_tm);
    UINT tysz2 = ref2->getTypeSize(m_tm);
    UINT ofst1 = ref1->getOffset();
    UINT ofst2 = ref2->getOffset();
    if ((ofst1 + tysz1) <= ofst2 || (ofst2 + tysz2) <= ofst1) {
        return RP_DIFFERENT_OBJ;
    }
    if (ofst1 == ofst2 && tysz1 == tysz2) {
        return RP_SAME_OBJ;
    }
    return RP_UNKNOWN;
}


bool RegPromot::scanIRTreeList(IR * root, LI<IRBB> const* li,
                               OUT ExactAccTab & exact_tab,
                               OUT InexactAccTab & inexact_tab)
{
    for (IR * ir = root; ir != nullptr; ir = ir->get_next()) {
        bool added = false;
        if (ir->isMemoryRefNonPR() &&
            !handleGeneralRef(ir, li, exact_tab, inexact_tab, &added)) {
            return false;
        }
        if (added) {
            //Do NOT add IR kid tree into table if the root has been added.
            continue;
        }
        for (UINT i = 0; i < IR_MAX_KID_NUM(ir); i++) {
            IR * k = ir->getKid(i);
            if (k == nullptr) { continue; }
            if (!scanIRTreeList(k, li, exact_tab, inexact_tab)) {
                return false;
            }
        }
    }
    return true;
}


//Find promotable memory references.
//Return true if current memory referense does not clobber other
//candidate in list. Or else return false means there are ambiguous
//memory reference.
//Return false if find unpromotable memory reference, this may
//prevent entire loop to be promoted.
bool RegPromot::scanStmt(IR * ir, LI<IRBB> const* li,
                         OUT ExactAccTab & exact_tab,
                         OUT InexactAccTab & inexact_tab)
{
    //Do NOT use IR iterator to avoid adding both parent and kid IR into
    //exact_tab or inexact_tab.
    switch (ir->getCode()) {
    case IR_ST: {
        bool added = false;
        if (!handleGeneralRef(ir, li, exact_tab, inexact_tab, &added)) {
            return false;
        }
        return scanIRTreeList(ir->getRHS(), li, exact_tab, inexact_tab);
    }
    case IR_STPR:
        return scanIRTreeList(ir->getRHS(), li, exact_tab, inexact_tab);
    case IR_STARRAY:
    case IR_IST: {
        bool added = false;
        if (!handleGeneralRef(ir, li, exact_tab, inexact_tab, &added)) {
            return false;
        }
        if (!added) {
            //All kid trees of stmt need to be scanned to collect candidiate.
            for (UINT i = 0; i < IR_MAX_KID_NUM(ir); i++) {
                IR * k = ir->getKid(i);
                if (k != nullptr &&
                    !scanIRTreeList(k, li, exact_tab, inexact_tab)) {
                    return false;
                }
            }
            return true;
        }
        //Only RHS kid tree need to be scanned to collect candidiate.
        return scanIRTreeList(ir->getRHS(), li, exact_tab, inexact_tab);
    }
    case IR_SETELEM:
    case IR_GETELEM:
    case IR_IGOTO:
    case IR_TRUEBR:
    case IR_FALSEBR:
    case IR_RETURN:
    case IR_SWITCH:
        for (UINT i = 0; i < IR_MAX_KID_NUM(ir); i++) {
            IR * k = ir->getKid(i);
            if (k == nullptr) { continue; }
            if (!scanIRTreeList(k, li, exact_tab, inexact_tab)) {
                return false;
            }
        }
        return true;
    case IR_CALL:
        //Do NOT iterate DUMMY expression-list.
        return scanIRTreeList(CALL_param_list(ir), li, exact_tab, inexact_tab);
    case IR_ICALL:
        //Do NOT iterate DUMMY expression-list.
        if (!scanIRTreeList(ICALL_callee(ir), li, exact_tab, inexact_tab)) {
            return false;
        }
        return scanIRTreeList(CALL_param_list(ir), li, exact_tab, inexact_tab);
    case IR_PHI:
    case IR_GOTO:
    case IR_REGION:
        return true;
    default: ASSERTN(0, ("unsupported IR type"));
    }
    return true;
}


//Scan BB and find promotable memory reference.
//If this function will find out unpromotable accessing that with ambiguous
//memory reference. Those related promotable accesses will NOT be promoted.
//e.g:a[0] = ...
//    a[i] = ...
//    a[0] is promotable, but a[i] is not, then a[0] can not be promoted.
//If there exist memory accessing that we do not know where it access,
//whole loop is unpromotable.
//Return false if loop is unpromotable.
bool RegPromot::scanBB(IN IRBB * bb, LI<IRBB> const* li,
                       OUT ExactAccTab & exact_tab,
                       OUT InexactAccTab & inexact_tab)
{
    for (IR * ir = BB_first_ir(bb); ir != nullptr; ir = BB_next_ir(bb)) {
        if (!ir->isContainMemRef()) { continue; }
        if (ir->is_region()) { return false; }
        if (ir->isCallStmt() && !ir->isReadOnly()) {
            clobberAccess(ir, exact_tab, inexact_tab);
        }
        if (!scanStmt(ir, li, exact_tab, inexact_tab)) {
            return false;
        }
    }
    return true;
}


//Generate code to restore value from delegate PR to delegate memory object.
void RegPromot::handleEpilog(ConstIRTab const& restore2mem,
                             DelegateMgr & delemgr, IRBB * exit_bb,
                             MOD RPCtx & ctx)
{
    ConstIRTabIter ti;
    for (IR const* delegate = restore2mem.get_first(ti);
         delegate != nullptr; delegate = restore2mem.get_next(ti)) {
        IR * pr = m_rg->dupIRTree(delemgr.getPR(delegate));
        IR * restore = delemgr.genRestoreStmt(delegate, pr);
        IR const* ir  = BB_irlist(exit_bb).get_head();
        if (ir != nullptr) {
            xoc::copyDbx(restore, ir, m_rg);
        }
        BB_irlist(exit_bb).append_head_ex(restore);
        if (useMDSSADU()) {
            ASSERT0(m_mdssamgr->getMDSSAInfoIfAny(restore) == nullptr);
            m_mdssamgr->insertDefStmt(restore, *ctx.domtree);
            delemgr.setRestoreDUChainBuilt(true);
        }
    }
}


//Return true if 'ir' can be promoted.
//Note ir must be memory reference.
bool RegPromot::isPromotable(IR const* ir) const
{
    ASSERT0(ir->isMemoryRef());
    //CASE:TBD:Should we avoid promotion for following arith-operation for
    //conservative purpose.
    //if (ir->isCallStmt() || ir->is_div() || ir->is_rem() || ir->is_mod()) {
    //    return false;
    //}

    //If IR tree has side-effect, that means exp/stmt can not be removed or
    //changed. RegPromot does not violate no-move attribute.
    //TBD:We are inclined that IR with may-throw and no-move attribute is
    //promotable.
    return !ir->hasSideEffect(true);
}


void RegPromot::handleExactAccOcc(IR const* dele,
                                  MOD DelegateMgr & delemgr,
                                  LI<IRBB> const* li,
                                  OUT ConstIRTab & restore2mem,
                                  OUT Occ2Occ & occ2newocc,
                                  IRIter & ii,
                                  ExactAccTab & exact_tab)
{
    //This table records the IRs which should NOT be processed any more.
    //They may be freed.
    //e.g: a[i], if array referrence is freed, the occurrence of variable i
    //also be freed.
    PromotedTab promoted;
    ASSERT0(dele->getMustRef());
    IRList * occs = exact_tab.getOccs(dele->getMustRef());
    ASSERT0(occs);
    IRListIter irit;
    IRListIter next_irit;
    for (occs->get_head(&irit); irit != occs->end(); irit = next_irit) {
        next_irit = irit;
        occs->get_next(&next_irit);
        IR * occ = irit->val();
        ASSERT0(!occ->is_undef());
        if (promoted.find(occ->id())) { continue; }

        MD const* md = occ->getMustRef();
        ASSERT0(md && md->is_exact());
        //Get the unique delegate.
        IR const* delegate = exact_tab.getDele(md);
        if (delegate == nullptr) {
            //If delegate does not exist, the occurrence can NOT
            //be promoted.
            occs->remove(irit);
            continue;
        }
        handleAccessInBody(occ, delegate, delemgr, restore2mem, occ2newocc);
        //Each memory reference in the tree has been promoted.
        promoted.addTree(occ, ii);
    }
}


//The function promote IR.
//dele: the delegate which indicates an exact-accessing reference.
void RegPromot::promoteExactAccessDelegate(IR const* dele,
                                           DelegateMgr & delemgr,
                                           LI<IRBB> const* li, IRIter & ii,
                                           IRBB * preheader, IRBB * exit_bb,
                                           ExactAccTab & exact_tab,
                                           MOD RPCtx & ctx)
{
    ASSERT0(!dele->isPROp());
    IR const* promoted_pr = delemgr.getPR(dele);
    ASSERT0(promoted_pr);
    handleProlog(dele, promoted_pr, delemgr, preheader);

    //Map delegate MD to its DEF which is in the loop.
    //These MDs need to be restored to memory in epilog BB of loop.
    ConstIRTab restore2mem;
    Occ2Occ occ2newocc;
    handleExactAccOcc(dele, delemgr, li, restore2mem, occ2newocc, ii,
                      exact_tab);
    ASSERT0(verifyIRandBB(m_rg->getBBList(), m_rg));
    //Generate code to fulfill epilog of delegate.
    handleEpilog(restore2mem, delemgr, exit_bb, ctx);
    addDUChainForExactAccDele(dele, occ2newocc, delemgr, exact_tab,
                              ctx, preheader, li);

    if (g_dump_opt.isDumpAfterPass() && g_dump_opt.isDumpRP()) {
        delemgr.dumpDele2Restore();
    }
    removeDUChainForOrgOcc(occ2newocc);

    //CASE: Do not remove outside loop USE, because that will incur
    //the VOpndSet is empty when the USE is and IR_ID. And empty
    //VOpndSet will incur assertion in verify().
    //removeMDPhiDUChain(dele, li, delemgr);

    //Each delegate has an occ-list for it.
    IRList * occs = exact_tab.getOccs(dele->getMustRef());
    ASSERT0(occs);
    freeExactOccs(*occs, m_rg, m_gvn);
}


//Return true if there is IR to be promoted, otherwise return false.
//exact_tab: record delegates for each IR ref.
void RegPromot::promoteExactAccess(LI<IRBB> const* li, IRIter & ii,
                                   IRBB * preheader, IRBB * exit_bb,
                                   ExactAccTab & exact_tab, MOD RPCtx & ctx)
{
    ASSERT0(exact_tab.verify());
    ASSERT0(preheader && exit_bb && li);
    ASSERT0(!li->getBodyBBSet()->is_empty());

    //Create delegate info for each given delegates.
    DelegateMgr delemgr(this, m_rg, m_gvn, exact_tab.get_elem_count());
    ExactAccTabIter mi;
    IR * dele = nullptr;
    MD const* next_md;
    IR * next_dele;
    for (MD const* md = exact_tab.get_first(mi, &dele);
         md != nullptr; md = next_md, dele = next_dele) {
        next_md = exact_tab.get_next(mi, &next_dele);
        ASSERT0(dele);
        if (!isPromotable(dele)) {
            //Do not promote the reference, and remove current 'delegate' from
            //exact_tab.
            exact_tab.remove(md);
            continue;
        }
        delemgr.createDelegateInfo(dele);
        exact_tab.collectOutsideLoopDU(md, dele, li, delemgr);
        ASSERT0(!((md == nullptr) ^ (dele == nullptr)));
    }
    if (g_dump_opt.isDumpAfterPass() && g_dump_opt.isDumpRP()) {
        delemgr.dump();
    }
    mi.clean();
    IR * d = nullptr;
    for (exact_tab.get_first(mi, &d);
         d != nullptr; exact_tab.get_next(mi, &d)) {
        //CASE:If whole IR tree is regards as delegate, the kids in the tree
        //should not be added into exact_tab.
        //if (d->is_undef()) { continue; }
        promoteExactAccessDelegate(d, delemgr, li, ii, preheader,
                                   exit_bb, exact_tab, ctx);
    }
}


void RegPromot::removeMDPhiDUChain(IR const* dele, LI<IRBB> const* li,
                                   DelegateMgr const& delemgr)
{
    if (!useMDSSADU()) { return; }
    for (INT i = li->getBodyBBSet()->get_first();
         i != -1; i = li->getBodyBBSet()->get_next(i)) {
        IRBB * bb = m_cfg->getBB(i);
        MDPhiList * philst = m_mdssamgr->getPhiList(bb);
        if (philst == nullptr) { continue; }
        for (MDPhiListIter it = philst->get_head();
             it != philst->end(); it = philst->get_next(it)) {
            MDPhi * phi = it->val();
            ASSERT0(phi && phi->is_phi());
            if (!phi->isRefSameMDWith(dele)) { continue; }

            //MDPhi of dele's MD is useless, because all occurrences of
            //dele will be replaced to PR. The outside-loop-use will
            //establish DU relation with the restore-stmt of dele rather
            //than MDPHI.
            m_mdssamgr->tryRemoveOutsideLoopUse(phi, li);
        }
    }
}


//Fixup DU chain if there is untrue dependence.
//occ2newocc: record the IR stmt/exp that need to fixup.
void RegPromot::removeDUChainForOrgOcc(Occ2Occ & occ2newocc)
{
    Occ2OccIter it;
    for (IR * occ = occ2newocc.get_first(it, nullptr);
         occ != nullptr; occ = occ2newocc.get_next(it, nullptr)) {
        ASSERT0(occ->isMemoryRefNonPR());
        if (occ->is_exp()) {
            xoc::removeUseForTree(occ, m_rg);
            continue;
        }
        xoc::removeStmt(occ, m_rg);
    }
}


void RegPromot::removeMDPhiForInexactAcc(DelegateMgr const& delemgr,
                                         InexactAccTab & inexact_tab,
                                         LI<IRBB> const* li)
{
    RefTab * deletab = const_cast<DelegateMgr&>(delemgr).getDelegateTab();
    InexactAccTabIter ti;
    TTab<IR*> visited;
    for (IR * occ = inexact_tab.get_first(ti); occ != nullptr;
         occ = inexact_tab.get_next(ti)) {
        //Get the unique delegate.
        IR * dele = nullptr;
        deletab->find(occ, &dele);
        if (visited.find(dele)) { continue; }
        visited.append(dele);

        //If delegate does not exist, the occurrence should be removed.
        ASSERT0(dele);
        removeMDPhiDUChain(dele, li, delemgr);
    }
}


void RegPromot::removeRedundantDUForInexactAcc(Occ2Occ & occ2newocc,
                                               DelegateMgr const& delemgr,
                                               InexactAccTab & inexact_tab,
                                               LI<IRBB> const* li)
{
    removeDUChainForOrgOcc(occ2newocc);

    //CASE: Do not remove outside loop USE, because that will incur
    //the VOpndSet is empty when the USE is and IR_ID. And empty
    //VOpndSet will incur assertion in verify().
    //removeMDPhiForInexactAcc(delemgr, inexact_tab, li);
}


void RegPromot::handleExpInBody(IR * occ, IR const* delegate,
                                DelegateMgr const& delemgr,
                                OUT Occ2Occ & occ2newocc)
{
    ASSERT0(isExpCand(occ));
    xoc::removeUseForTree(occ, m_rg);
    IR * stmt = occ->getStmt();
    ASSERT0(stmt);

    IR * pr = m_rg->dupIR(delemgr.getPR(delegate));
    ASSERT0(occ->getParent());
    bool r = occ->getParent()->replaceKid(occ, pr, false);
    CHECK0_DUMMYUSE(r);

    pr->copyAI(occ, m_rg);
    m_gvn->setMapIR2VN(pr, m_gvn->mapIR2VN(occ));

    //DU chain between init-stmt and delegated-pr will be maintained in
    //addDUChainForInitDef().
    occ2newocc.set(occ, pr);
    //Do not free occ here since it will be freed later.
}


void RegPromot::findAndRecordRestore(IR * occ, IR const* delegate,
                                     MOD DelegateMgr & delemgr,
                                     OUT ConstIRTab & restore2mem)
{
    //Note, may be some USE of 'occ' has already been promoted to
    //PR, but it doesn't matter, you don't need to check the truely
    //dependence here, since we just want to see whether
    //there exist outer loop references to this stmt. And all
    //same group memory references will be promoted to PR after
    //the function return.
    if ((delemgr.hasLoopOutsideUse(delegate) || mayBeGlobalRef(occ)) &&
        !restore2mem.find(const_cast<IR*>(delegate))) { //delegate is unique
        //Record that current delegate have to be restored to memory.
        restore2mem.append(delegate);
    }
}


//restore2mem: record the delegate that need to restore.
void RegPromot::handleStmtInBody(IR * occ, IR const* delegate,
                                 MOD DelegateMgr & delemgr,
                                 OUT ConstIRTab & restore2mem,
                                 OUT Occ2Occ & occ2newocc)
{
    ASSERT0(isStmtCand(occ));
    ASSERTN(occ->getRHS(), ("must be store operation"));
    IR const* delegate_pr = delemgr.getPR(delegate);
    ASSERT0(delegate_pr);
    findAndRecordRestore(occ, delegate, delemgr, restore2mem);
    IR * occrhs = occ->getRHS();
    occ->setRHS(nullptr); //Do NOT remove the DU chain of RHS of occ.
    xoc::removeStmt(occ, m_rg);
    
    //Substitute STPR for writing memory.
    IR * stpr = m_rg->buildStorePR(PR_no(delegate_pr),
                                   delegate_pr->getType(), occrhs);
    m_rg->allocRefForPR(stpr);
    stpr->copyAI(occ, m_rg);
    m_gvn->setMapIR2VN(stpr, m_gvn->mapIR2VN(occ));

    IRBB * refbb = occ->getBB();
    ASSERT0(refbb);
    IRListIter ct = nullptr;
    BB_irlist(refbb).find(occ, &ct);
    ASSERT0(ct != nullptr);
    BB_irlist(refbb).insert_after(stpr, ct);
    BB_irlist(refbb).remove(ct);
    occ2newocc.set(occ, stpr);
    //Do not free occ here since it will be freed later.
}


//restore2mem: record the delegate that need to restore.
void RegPromot::handleAccessInBody(IR * ref, IR const* delegate,
                                   DelegateMgr & delemgr,
                                   OUT ConstIRTab & restore2mem,
                                   OUT Occ2Occ & occ2newocc)
{
    ASSERT0(ref && delegate);
    ASSERT0(ref->isMemoryRefNonPR());
    if (ref->is_stmt()) {
        handleStmtInBody(ref, delegate, delemgr, restore2mem, occ2newocc);
    } else {
        handleExpInBody(ref, delegate, delemgr, occ2newocc);
    }
}


//The function generates iniailization code of promoted PR.
//Note the function leaves the work that to build DU chain of PR and STPR to
//the sebsequent function, it will be done at buildDUChainForDeleRelatedPR().
void RegPromot::handlePrologForStmt(IR const* delegate, IR const* promoted_pr,
                                    DelegateMgr & delemgr, IRBB * preheader)
{
    ASSERT0(delegate->is_stmt());
    IR * rhs = nullptr; //record the initial value of delegate.
    //Load value into PR.
    switch (delegate->getCode()) {
    case IR_STARRAY:
        rhs = CArray::dupIRTreeByStmt(delegate, m_rg);
        xoc::addUseForTree(ARR_base(rhs), ARR_base(delegate), m_rg);
        xoc::addUseForTree(ARR_sub_list(rhs), ARR_sub_list(delegate), m_rg);
        break;
    case IR_IST:
        rhs = CILd::dupIRTreeByStmt(delegate, m_rg);
        xoc::addUseForTree(rhs->getBase(), delegate->getBase(), m_rg);
        break;
    case IR_ST:
        rhs = CLd::dupIRTreeByStmt(delegate, m_rg);
        break;
    default: UNREACHABLE(); //unsupport.
    }
    ASSERT0(rhs);
    if (useMDSSADU()) {
        ASSERT0(m_mdssamgr->getMDSSAInfoIfAny(rhs) == nullptr);
        //We only generate empty MDSSAInfo rather than fulfill versioned MD to
        //avoid the complaint of sebsequent DU chain building.
        //Leave the sanity construction work to the ending of the pass.
        MDSSAInfo const* rhsinfo = m_mdssamgr->genMDSSAInfo(rhs);
        ASSERT0(rhsinfo->readVOpndSet().is_empty());
    }
 
    IR * stpr = delemgr.genInitStmt(delegate, rhs);
    BBIRList & irs = preheader->getIRList();
    if (irs.get_head() != nullptr) {
        stpr->copyAI(irs.get_head(), m_rg);
    }

    //TODO: There is no need to insert init-stmt for delegate if the delegate
    //does not have a live-in DEF stmt to loop-header.
    //e.g:int m;
    //    while (...) {
    //      m=...
    //      ...=m;
    //    }
    //    USE(m);
    irs.append_tail_ex(stpr);
}


//The function generates iniailization code of promoted PR.
//Note the function leaves the work that to build DU chain of PR and STPR to
//the sebsequent function, it will be done at buildDUChainForDeleRelatedPR().
void RegPromot::handlePrologForExp(IR const* delegate, IR const* promoted_pr,
                                   DelegateMgr & delemgr, IRBB * preheader)
{
    ASSERT0(delegate->is_exp());
    IR * rhs = nullptr; //record the initial value of delegate.
    //Load value into PR.
    switch (delegate->getCode()) {
    case IR_ARRAY:
    case IR_ILD:
    case IR_LD:
        rhs = m_rg->dupIRTree(delegate);
        xoc::addUseForTree(rhs, delegate, m_rg);
        break;
    default: UNREACHABLE(); //unsupport.
    }
    ASSERT0(rhs);
    IR * stpr = delemgr.genInitStmt(delegate, rhs);
    BBIRList & irs = preheader->getIRList();
    if (irs.get_head() != nullptr) {
        stpr->copyAI(irs.get_head(), m_rg);
    }

    //TODO: There is no need to insert init-stmt for delegate if the delegate
    //does not have a live-in DEF stmt to loop-header.
    //e.g:int m;
    //    while (...) {
    //      m=...
    //      ...=m;
    //    }
    //    USE(m);
    irs.append_tail_ex(stpr);
}


//The function generates iniailization code of promoted PR.
//Note the function leaves the work that to build DU chain of PR and STPR to
//the sebsequent function, it will be done at buildDUChainForDeleRelatedPR().
void RegPromot::handleProlog(IR const* delegate, IR const* promoted_pr,
                             DelegateMgr & delemgr, IRBB * preheader)
{
    //Load value into PR.
    switch (delegate->getCode()) {
    case IR_STARRAY:
    case IR_IST:
    case IR_ST:
        handlePrologForStmt(delegate, promoted_pr, delemgr, preheader);
        return;
    case IR_ARRAY:
    case IR_ILD:
    case IR_LD:
        handlePrologForExp(delegate, promoted_pr, delemgr, preheader);
        return;
    default: UNREACHABLE(); //unsupport.
    }
}


//The function insert Phi for init-stmt PR or reconstruct SSA overall.
//Return true if PRSSA changed.
bool RegPromot::buildPRSSADUChainForInexactAcc(Occ2Occ const& occ2newocc,
                                               DelegateMgr const& delemgr,
                                               LI<IRBB> const* li,
                                               IRBB * preheader,
                                               MOD RPCtx & ctx)
{
    if (!usePRSSADU()) { return false; }
    PRSSARegion ssarg(getSBSMgr());
    ssarg.add(*li->getBodyBBSet());
    ssarg.setRootBB(preheader);
    RefTab const* deletab = const_cast<DelegateMgr&>(delemgr).getDelegateTab();
    RefTabIter it;
    for (IR const* dele = deletab->get_first(it);
         it >= 0; dele = deletab->get_next(it)) {
        IR * init = delemgr.getInitStmt(dele);
        if (init == nullptr) { continue; }

        IR const* pr = delemgr.getPR(dele);
        ASSERT0(pr);
        UINT deleprno = pr->getPrno();
        ssarg.add(deleprno, init);

        IR * rest = delemgr.getRestoreStmt(dele);
        if (rest != nullptr) {
            ASSERT0(rest->getRHS()->isPROp());
            ssarg.add(deleprno, rest);
        }
    }
    Occ2OccIter occit;
    IR * newocc = nullptr;
    for (IR * occ = occ2newocc.get_first(occit, &newocc);
         occ != nullptr; occ = occ2newocc.get_next(occit, &newocc)) {
        ASSERT0(newocc->isPROp());
        ssarg.add(newocc);
    }

    if (m_cfg->read_dom_set(preheader->id()) == nullptr) {
        //preheader is just inserted, SSA needs its domset.
        ctx.oc->setDomValid(false);
        m_rg->getPassMgr()->checkValidAndRecompute(ctx.oc, PASS_DOM,
                                                   PASS_UNDEF);
    }
    ASSERT0(ctx.domtree);
    m_prssamgr->constructDesignatedRegion(ssarg, *ctx.domtree);
    OC_is_pr_du_chain_valid(*ctx.oc) = false;
    return true;
}


//The function insert Phi for init-stmt PR or reconstruct SSA overall.
//Return true if PRSSA changed.
bool RegPromot::buildPRSSADUChainForExactAcc(IR const* dele,
                                             Occ2Occ const& occ2newocc,
                                             DelegateMgr const& delemgr,
                                             LI<IRBB> const* li,
                                             IRBB * preheader,
                                             MOD RPCtx & ctx)
{
    if (!usePRSSADU()) { return false; }
    IR const* pr = delemgr.getPR(dele);
    ASSERT0(pr && m_prssamgr);
    UINT deleprno = pr->getPrno();
    PRSSARegion ssarg(getSBSMgr());
    ssarg.add(*li->getBodyBBSet());
    IR * init = delemgr.getInitStmt(dele);
    ASSERT0(init);
    ssarg.add(deleprno, init);
    ssarg.setRootBB(init->getBB());

    IR * rest = delemgr.getRestoreStmt(dele);
    if (rest != nullptr) {
        ASSERT0(rest->getRHS()->isPROp());
        ssarg.add(deleprno, rest);
    }

    Occ2OccIter it;
    IR * newocc = nullptr;
    for (IR * occ = occ2newocc.get_first(it, &newocc);
         occ != nullptr; occ = occ2newocc.get_next(it, &newocc)) {
        ASSERT0(newocc->isPROp());
        ssarg.add(newocc);
    }
    ASSERTN(m_cfg->read_dom_set(preheader->id()),
            ("dominfo of preheader must have been maintained"));
    ASSERT0(ctx.domtree);
    m_prssamgr->constructDesignatedRegion(ssarg, *ctx.domtree);
    OC_is_pr_du_chain_valid(*ctx.oc) = false;
    return true;
}


void RegPromot::addDUChainForInexactAcc(DelegateMgr const& delemgr,
                                        Occ2Occ const& occ2newocc,
                                        InexactAccTab const& inexact_tab,
                                        LI<IRBB> const* li,
                                        IRBB * preheader,
                                        MOD RPCtx & ctx)
{
    RefTab const* deletab = const_cast<DelegateMgr&>(delemgr).getDelegateTab();
    if (deletab->get_elem_count() == 0) { return; }
    RefTabIter it;
    for (IR * dele = deletab->get_first(it);
         it >= 0; dele = deletab->get_next(it)) {
        if (delemgr.getInitStmt(dele) != nullptr) {
            addDUChainForInexactAccDele(dele, delemgr, occ2newocc,
                                        inexact_tab, ctx);
        }
    }
    buildPRSSADUChainForInexactAcc(occ2newocc, delemgr, li, preheader, ctx);
}


void RegPromot::addDUChainForInexactAccDele(IR const* dele,
                                            DelegateMgr const& delemgr,
                                            Occ2Occ const& occ2newocc,
                                            InexactAccTab const& inexact_tab,
                                            MOD RPCtx & ctx)
{
    ASSERT0(delemgr.isDelegate(dele));
    //dele may not have exact-must-MD.
    IR * init_stmt = delemgr.getInitStmt(dele);
    ASSERT0(init_stmt && init_stmt->is_stpr());
    addDUChainForInitDefAndExposedUse(dele, init_stmt, delemgr);
    addDUChainForRHSOfInitDef(dele, init_stmt);

    //Build DU chain for initialization-def and intra-loop-use.
    IRList deflst;
    addDUChainForInitDef(dele, init_stmt, occ2newocc, inexact_tab, deflst);

    //Build DU chain for intra-loop-def and its USE.
    addDUChainForIntraDef(occ2newocc, deflst);
    addSSADUChainForExpOfRestore(dele, delemgr);
    addDUChainForRestoreToOutsideUse(dele, delemgr);
}


void RegPromot::addDUChainForExpTree(IR * root, IR * startir, IRBB * startbb)
{
    ASSERT0(root->is_exp());
    bool use_prssa = usePRSSADU();
    bool use_mdssa = useMDSSADU();
    if (!use_prssa && !use_mdssa) { return; }
    IRIter it;
    for (IR * x = iterInit(root, it);
         x != nullptr; x = iterNext(it)) {
        if (use_mdssa && x->isMemoryRefNonPR()) {
            m_mdssamgr->findAndSetDomLiveInDef(x, startir, startbb);
        } else if (use_prssa && x->isReadPR()) {
            m_prssamgr->findAndSetLiveinDef(x);
        }
    }
}


void RegPromot::addDUChainForRHSOfInitDef(IR const* dele, IR * init_stmt)
{
    ASSERT0(dele->isIRIsomo(init_stmt->getRHS(), true));
    IR * startir = init_stmt->getBB()->getPrevIR(init_stmt);
    IRBB * startbb = init_stmt->getBB();
    addDUChainForExpTree(init_stmt->getRHS(), startir, startbb);
}


void RegPromot::addSSADUChainForExpOfRestore(IR const* dele,
                                             DelegateMgr const& delemgr)
{
    bool use_prssa = usePRSSADU();
    bool use_mdssa = useMDSSADU();
    if (!use_prssa && !use_mdssa) { return; }
    IR * restore = delemgr.getRestoreStmt(dele);
    if (restore == nullptr) { return; }
    ASSERT0(restore->is_stmt());
    ASSERT0(restore->isMemoryRefNonPR() && restore->getRHS()->isPROp());
    IRIter it;
    IR * startir = restore->getBB()->getPrevIR(restore);
    IRBB * startbb = restore->getBB();
    for (IR * x = iterExpOfStmtInit(restore, it);
         x != nullptr; x = iterExpOfStmtNext(it)) {
        if (use_mdssa && x->isMemoryRefNonPR()) {
            m_mdssamgr->findAndSetDomLiveInDef(x, startir, startbb);
        } else if (use_prssa && x->isReadPR()) {
            m_prssamgr->findAndSetLiveinDef(x);
        }
    }
}


//Build DU chain for initialization-def and outside loop exposed-use.
void RegPromot::addDUChainForInitDefAndExposedUse(IR const* dele,
                                                  IR * init_stmt,
                                                  DelegateMgr const& delemgr)
{
    IR * restore_stmt = delemgr.getRestoreStmt(dele);
    if (restore_stmt == nullptr) { return; }

    ASSERT0(restore_stmt->is_stmt());
    IR * exposed_use = restore_stmt->getRHS();
    ASSERT0(exposed_use->is_pr());
    ASSERT0(init_stmt->getPrno() == exposed_use->getPrno());

    //Build DU chain for initialization-def and exposed-use.
    buildDUChainOnDemand(init_stmt, exposed_use);
}


//Process inexact access tab.
//Build DU chain for initialization-def and intra-loop-use.
void RegPromot::addDUChainForInitDef(IR const* dele, IR * init_stmt,
                                     Occ2Occ const& occ2newocc,
                                     InexactAccTab const& inexact_tab,
                                     OUT IRList & deflst)
{
    //Find the PR corresponding to 'dele' and build the DU chain.
    ASSERT0(init_stmt->is_stpr());
    InexactAccTabIter iter;
    for (IR * occ = inexact_tab.get_first(iter);
         occ != nullptr; occ = inexact_tab.get_next(iter)) {
        if (occ->is_stmt()) {
            deflst.append_tail(occ);
            continue;
        }
        IR * newocc = const_cast<Occ2Occ&>(occ2newocc).get(occ);
        ASSERT0(newocc && newocc->is_pr());
        if (newocc->getPrno() == init_stmt->getPrno()) {
            buildDUChainOnDemand(init_stmt, newocc);
        }
    }
}


//Process exact access tab.
//Build DU chain for initialization-def and intra-loop-use.
void RegPromot::addDUChainForInitDef(IR const* dele, IR * init_stmt,
                                     Occ2Occ const& occ2newocc,
                                     ExactAccTab const& exact_tab,
                                     OUT IRList & deflst)
{
    MD const* md = const_cast<IR*>(dele)->getMustRef();
    IRList * occlst = exact_tab.getOccs(md);
    ASSERT0(occlst);
    IRListIter irit;
    for (IR * occ = occlst->get_head(&irit); occ != nullptr;
         occ = occlst->get_next(&irit)) {
        ASSERT0(const_cast<IR*>(occ)->getMustRef() == md);
        if (occ->is_stmt()) {
            deflst.append_tail(occ);
            continue;
        }
        IR * newocc = const_cast<Occ2Occ&>(occ2newocc).get(occ);
        ASSERT0(newocc && newocc->is_pr());
        if (newocc->getPrno() == init_stmt->getPrno()) {
            buildDUChainOnDemand(init_stmt, newocc);
        }
    }
}


//Build DU chain for intra-loop-def and its USE.
void RegPromot::addDUChainForIntraDefAndUseSet(Occ2Occ const& occ2newocc,
                                               IRSet const& useset,
                                               IR * newocc_def)
{
    ASSERT0(newocc_def->is_stpr());
    IRSetIter di = nullptr;
    for (INT i = useset.get_first(&di); i >= 0; i = useset.get_next(i, &di)) {
        IR * use = m_rg->getIR(i);
        ASSERT0(use->is_exp() && use->isMemoryRefNonPR());
        IR * newocc_use = const_cast<Occ2Occ&>(occ2newocc).get(use);
        if (newocc_use == nullptr) { continue; }
        ASSERT0(newocc_use->is_pr());
        if (newocc_def->getPrno() == newocc_use->getPrno()) {
            buildDUChainOnDemand(newocc_def, newocc_use);
        }
    }
}


void RegPromot::buildDUChainOnDemandForPROp(IR * def, IR * use)
{
    ASSERT0(def->isPROp() && use->isPROp());
    if (usePRSSADU()) {
        //Nothing to do, the DU chain will be built by
        //buildPRSSADUChainForExactAcc().
        return;
    }
    xoc::buildDUChain(def, use, m_rg);
}


void RegPromot::buildDUChainOnDemand(IR * def, IR * use)
{
    if (def->isPROp()) {
        buildDUChainOnDemandForPROp(def, use);
        return;
    }
    xoc::buildDUChain(def, use, m_rg);
}


void RegPromot::setNewDefForExpOfRestore(IR * restore)
{
    ASSERT0(restore->is_stmt() && restore->getRHS()->isPROp());
    IRIter it;
    IRBB * startbb = restore->getBB();
    IR * startir = startbb->getPrevIR(restore);
    for (IR * x = xoc::iterInit(restore, it);
         x != nullptr; x = xoc::iterNext(it)) {
        if (x->is_stmt() || !x->isMemoryRefNonPR()) { continue; }
        m_mdssamgr->findAndSetDomLiveInDef(x, startir, startbb);
    }
}


void RegPromot::addDUChainForRestoreToOutsideUse(IR const* dele,
                                                 DelegateMgr const& delemgr)
{
    if (delemgr.isRestoreDUChainBuilt()) { return; }
    IR * restore = delemgr.getRestoreStmt(dele);
    if (restore == nullptr) { return; }
    ASSERT0(restore->is_stmt());
    ASSERT0(restore->isMemoryRefNonPR() && restore->getRHS()->isPROp());

    //The USE is an outside-loop USE, that should establish
    //DU chain with the restore.
    DUSet const* useset = delemgr.getOutsideUseSet(dele);
    if (useset == nullptr) { return; }
    DUSetIter it;
    for (INT i = useset->get_first(&it); i >= 0; i = useset->get_next(i, &it)) {
        IR * u = m_rg->getIR(i);
        ASSERT0(u->is_exp());
        xoc::removeUseForTree(u, m_rg);
        xoc::buildDUChain(restore, u, m_rg);
    }
}


//Build DU chain for intra-loop-def and its USE.
void RegPromot::addDUChainForIntraDef(Occ2Occ const& occ2newocc,
                                      IRList const& deflst)
{
    IRListIter irit;
    //Build DU chain for intra-loop-def and its USE.
    IRSet useset(getSegMgr());
    for (IR * def = deflst.get_head(&irit); def != nullptr;
         def = deflst.get_next(&irit)) {
        useset.clean();
        xoc::collectUseSet(def, m_mdssamgr, &useset);

        IR * newocc_def = const_cast<Occ2Occ&>(occ2newocc).get(def);
        ASSERT0(newocc_def && newocc_def->is_stpr());
        addDUChainForIntraDefAndUseSet(occ2newocc, useset, newocc_def);
    }
}


void RegPromot::addDUChainForExactAccDele(IR const* dele,
                                          Occ2Occ const& occ2newocc,
                                          DelegateMgr const& delemgr,
                                          ExactAccTab const& exact_tab,
                                          MOD RPCtx & ctx,
                                          IRBB * preheader,
                                          LI<IRBB> const* li)
{
    ASSERTN(const_cast<IR*>(dele)->getMustRef() &&
            const_cast<IR*>(dele)->getMustRef()->is_exact(),
            ("delegate must be exact MD"));
    IR * init_stmt = delemgr.getInitStmt(dele);
    ASSERT0(init_stmt->is_stpr());
    addDUChainForInitDefAndExposedUse(dele, init_stmt, delemgr);
    addDUChainForRHSOfInitDef(dele, init_stmt);

    //Build DU chain for initialization-def and intra-loop-use.
    IRList deflst;
    addDUChainForInitDef(dele, init_stmt, occ2newocc, exact_tab, deflst);

    //Build DU chain for intra-loop-def and its USE.
    addDUChainForIntraDef(occ2newocc, deflst);
    addSSADUChainForExpOfRestore(dele, delemgr);
    addDUChainForRestoreToOutsideUse(dele, delemgr);
    buildPRSSADUChainForExactAcc(dele, occ2newocc, delemgr, li, preheader, ctx);
}


void RegPromot::handleInexactAccOcc(MOD DelegateMgr & delemgr,
                                    InexactAccTab & inexact_tab,
                                    OUT ConstIRTab & restore2mem,
                                    OUT Occ2Occ & occ2newocc,
                                    InexactAccTabIter & ti)
{
    ti.clean();
    IR * nextocc = nullptr;
    RefTab * deletab = delemgr.getDelegateTab();
    for (IR * occ = inexact_tab.get_first(ti); occ != nullptr; occ = nextocc) {
        nextocc = inexact_tab.get_next(ti);
        //Get the unique delegate.
        IR * dele = nullptr;
        deletab->find(occ, &dele);
        if (dele == nullptr) {
            //If delegate does not exist, the reference can not be promoted.
            inexact_tab.remove(occ);
            continue;
        }
        handleAccessInBody(occ, dele, delemgr, restore2mem, occ2newocc);
    }
}


//Return true if IR is promoted, otherwise false.
bool RegPromot::promoteInexactAccessDelegate(DelegateMgr & delemgr,
                                             LI<IRBB> const* li,
                                             IRBB * preheader,
                                             IRBB * exit_bb,
                                             InexactAccTab & inexact_tab,
                                             IRIter & ii,
                                             MOD RPCtx & ctx)
{
    ASSERT0(li && exit_bb && preheader);
    ASSERT0(m_gvn && m_gvn->is_valid());
    ASSERT0(!li->getBodyBBSet()->is_empty());
    RefTab * deletab = delemgr.getDelegateTab();
    if (deletab->get_elem_count() == 0) { return false; }

    InexactAccTabIter ti;
    for (IR * occ = inexact_tab.get_first(ti);
         occ != nullptr; occ = inexact_tab.get_next(ti)) {
        ASSERT0(!occ->is_undef());
        ASSERT0(!occ->isPROp());
        IR * dele = nullptr;
        deletab->find(const_cast<IR*>(occ), &dele);
        if (dele == nullptr || delemgr.getInitStmt(dele) != nullptr) {
            //If delegate does not exist, the reference can not be promoted.
            continue;
        }
        IR const* pr = delemgr.getPR(dele);
        ASSERT0(pr);
        handleProlog(dele, pr, delemgr, preheader);
    }
    //Generate code to fulfill initialization of delegate.

    //Map IR expression which need to restore
    //into memory at epilog of loop.
    ConstIRTab restore2mem; //record the delegate that need to restore.
    Occ2Occ occ2newocc;
    handleInexactAccOcc(delemgr, inexact_tab, restore2mem, occ2newocc, ti);
    ASSERT0(verifyIRandBB(m_rg->getBBList(), m_rg));

    //Generate code to fulfill epilog of delegate.
    handleEpilog(restore2mem, delemgr, exit_bb, ctx);
    addDUChainForInexactAcc(delemgr, occ2newocc, inexact_tab, li,
                            preheader, ctx);
    removeRedundantDUForInexactAcc(occ2newocc, delemgr, inexact_tab, li);
    return true;
}


//Return true if there is IR being promoted, otherwise return false.
void RegPromot::promoteInexactAccess(LI<IRBB> const* li,
                                     IRBB * preheader,
                                     IRBB * exit_bb,
                                     InexactAccTab & inexact_tab,
                                     IRIter & ii, MOD RPCtx & ctx)
{
    DelegateMgr delemgr(this, m_rg, m_gvn,
        xcom::getNearestPowerOf2(inexact_tab.get_elem_count()));
    InexactAccTabIter ti;
    //Prepare delegate table and related information.
    for (IR * occ = inexact_tab.get_first(ti);
         occ != nullptr; occ = inexact_tab.get_next(ti)) {
        if (!isPromotable(occ)) {
            //Do not promote the reference.
            continue;
        }
        IR * delegate = delemgr.createUniqueDelegate(occ);
        ASSERT0(delegate);
        delemgr.collectOutsideLoopDefUse(occ, delegate, li);
    }
    if (g_dump_opt.isDumpAfterPass() && g_dump_opt.isDumpRP()) {
        delemgr.dump();
    }
    promoteInexactAccessDelegate(delemgr, li, preheader, exit_bb,
                                 inexact_tab, ii, ctx);
    //Note the delegate is one of reference in 'inexact_tab'.
    //All delegates are recorded in one table.
    freeInexactOccs(inexact_tab, m_rg, m_gvn);
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

    VN const* vn1 = m_gvn->mapIR2VNConst(base1);
    VN const* vn2 = m_gvn->mapIR2VNConst(base2);
    if (vn1 == nullptr || vn2 == nullptr) { return RP_UNKNOWN; }
    if (vn1 == vn2) { return RP_SAME_ARRAY; }
    return RP_DIFFERENT_ARRAY;
}


//This function performs the rest work of scanBB().
//The function picks up the insuitable IR to be promoted.
void RegPromot::checkAndRemoveInvalidExactOcc(ExactAccTab & acctab)
{
    ExactAccTabIter it;
    IR * dele;
    for (MD const* md = acctab.get_first(it, &dele); md != nullptr;
         md = acctab.get_next(it, &dele)) {
        IRListIter ct;
        IRListIter nct;
        IRList * occs = acctab.getOccs(md);
        ASSERT0(occs);
        for (occs->get_head(&ct), nct = ct; ct != nullptr; ct = nct) {
            IR * occ = ct->val();
            occs->get_next(&nct);
 
            MD const* md = occ->getMustRef();
            ASSERT0(md && md->is_exact());
 
            //We record all MD that are not suitable for promotion in
            //m_dont_promote, and remove all related OCC in exact_list.
            //The MD of promotable candidate must not overlapped each other.
            ASSERT0(dele);
            if (m_dont_promote.is_overlap(dele)) {
                occs->remove(ct);
            }
        }
    }
}


//The function will promote occ in exact_tab or inexact_tab.
void RegPromot::promote(LI<IRBB> const* li, IRBB * exit_bb, IRBB * preheader,
                        IRIter & ii, ExactAccTab & exact_tab,
                        InexactAccTab & inexact_tab, MOD RPCtx & ctx)
{
    ASSERT0(exact_tab.get_elem_count() != 0 ||
            inexact_tab.get_elem_count() != 0);
    if (exact_tab.get_elem_count() != 0) {
        ASSERT0(exact_tab.isOccUnique(getSBSMgr()));
        checkAndRemoveInvalidExactOcc(exact_tab);
        promoteExactAccess(li, ii, preheader, exit_bb, exact_tab, ctx);
    }
    if (inexact_tab.get_elem_count() != 0) {
        promoteInexactAccess(li, preheader, exit_bb, inexact_tab, ii, ctx);
    }
}


//Return true if there are memory locations have been promoted.
bool RegPromot::tryPromoteLoop(LI<IRBB> const* li, IRIter & ii,
                               MOD RPCtx & ctx)
{
    //Record the mapping between MD and ARRAY access expression.
    ExactAccTab exact_tab;
    InexactAccTab inexact_tab;
    m_dont_promote.clean();
    if (!analyszLoop(li, exact_tab, inexact_tab)) {
        return false;
    }
    if (exact_tab.get_elem_count() == 0 && inexact_tab.get_elem_count() == 0) {
        return false;
    }
    xcom::Edge const* exitedge = nullptr;
    IRBB * exit_bb = m_cfg->findSingleExitBB(li, &exitedge);
    if (exit_bb == nullptr) {
        //If we did not find a single exit bb, the loop is nontrivial
        //to promote.
        return false;
    }

    if (g_dump_opt.isDumpAfterPass() && g_dump_opt.isDumpRP()) {
        note(m_rg, "\n==-- DUMP LoopInfo --==");
        li->dump(m_rg);
        exact_tab.dump(m_rg);
        inexact_tab.dump(m_rg);
    }
    exit_bb = tryInsertStubExitBB(exit_bb, exitedge, ctx);
    ASSERT0(exit_bb);

    //Insert a preheader BB before Loop.
    IRBB * preheader = nullptr;
    bool change = xoc::insertPreheader(li, m_rg, &preheader, *ctx.oc, false);
    if ((change || ctx.domtree == nullptr || ctx.need_rebuild_domtree) &&
        (useMDSSADU() || usePRSSADU())) {
        ctx.buildDomTree(m_cfg);
    }
    promote(li, exit_bb, preheader, ii, exact_tab, inexact_tab, ctx);
    //promote() has maintaind PRSSA and MDSSA.
    ASSERT0(!usePRSSADU() || PRSSAMgr::verifyPRSSAInfo(m_rg));
    ASSERT0(!useMDSSADU() || MDSSAMgr::verifyMDSSAInfo(m_rg));
    return true;
}


//The function try to insert stub-BB before 'exit_bb' if there is MDPhi in
//the BB.
IRBB * RegPromot::tryInsertStubExitBB(IRBB * exit_bb,
                                      xcom::Edge const* exitedge,
                                      MOD RegPromot::RPCtx & ctx)
{
    if (m_mdssamgr == nullptr || m_mdssamgr->getPhiList(exit_bb) == nullptr) {
        return exit_bb;
    }
    BBListIter exit_bb_it;
    m_cfg->getBBList()->find(exit_bb, &exit_bb_it);
    ASSERT0(exit_bb_it);
    ASSERT0(exitedge);
    IRBB * pred = m_cfg->getBB(exitedge->from()->id());
    ASSERT0(pred);
    BBListIter pred_it;
    m_cfg->getBBList()->find(pred, &pred_it);
    ASSERT0(pred_it);

    IRBB * stub = m_rg->allocBB();
    m_cfg->addBB(stub);
    m_cfg->insertBBbetween(pred, pred_it, exit_bb, exit_bb_it, stub);
    if (!m_cfg->tryUpdateRPO(stub, exit_bb, true)) {
        OC_is_rpo_valid(*ctx.oc) = false;
        OC_is_cdg_valid(*ctx.oc) = false;
    }
    ctx.oc->setDomValid(false);
    ctx.need_rebuild_domtree = true;
    m_cfg->getRegion()->getPassMgr()->checkValidAndRecompute(ctx.oc, PASS_DOM,
                                                             PASS_UNDEF);
    return stub;
}


//Return true if the loop is promotable.
bool RegPromot::analyszLoop(LI<IRBB> const* li, ExactAccTab & exact_tab,
                            InexactAccTab & inexact_tab)
{
    ASSERT0(li);
    for (INT i = li->getBodyBBSet()->get_first();
         i != -1; i = li->getBodyBBSet()->get_next(i)) {
        IRBB * bb = m_cfg->getBB(i);
        ASSERT0(bb && m_cfg->getVertex(bb->id()));
        if (bb->hasReturn()) {
            return false;
        }

        if (!scanBB(bb, li, exact_tab, inexact_tab)) {
            return false;
        }
    }
    return true;
}


bool RegPromot::EvaluableScalarReplacement(List<LI<IRBB> const*> & worklst,
                                           MOD RPCtx & ctx)
{
    IRIter ii;
    bool changed = false;
    if (g_dump_opt.isDumpAfterPass() && g_dump_opt.isDumpRP()) {
        note(m_rg, "\n==---- DUMP Register Promotion '%s' ----==",
             m_rg->getRegionName());
    }
    while (worklst.get_elem_count() > 0) {
        LI<IRBB> const* x = worklst.remove_head();
        changed |= tryPromoteLoop(x, ii, ctx);
        x = x->getInnerList();
        while (x != nullptr) {
            worklst.append_tail(x);
            x = x->get_next();
        }
    }
    return changed;
}


bool RegPromot::dumpBeforePass() const
{
    if (!getRegion()->isLogMgrInit()) { return false; }
    START_TIMER_FMT(t, ("DUMP BEFORE %s", getPassName()));
    note(getRegion(), "\n==---- DUMP BEFORE %s '%s' ----==",
         getPassName(), m_rg->getRegionName());
    m_rg->getLogMgr()->incIndent(2);
    dumpBBList(m_rg->getBBList(), m_rg);
    m_rg->getLogMgr()->decIndent(2);
    Pass::dumpBeforePass();
    END_TIMER_FMT(t, ("DUMP BEFORE %s", getPassName()));
    return true;
}


bool RegPromot::dump() const
{
    if (!m_rg->isLogMgrInit()) { return false; }
    note(getRegion(), "\n==---- DUMP %s '%s' ----==",
         getPassName(), m_rg->getRegionName());
    m_rg->getLogMgr()->incIndent(2);
    m_dont_promote.dump();
    Pass::dump();
    m_rg->getLogMgr()->decIndent(2);
    note(getRegion(), "\n");
    return Pass::dump();
}


//Perform scalar replacement of aggregates and array.
bool RegPromot::perform(OptCtx & oc)
{
    BBList * bbl = m_rg->getBBList();
    if (bbl == nullptr || bbl->get_elem_count() == 0) { return false; }
    if (!oc.is_ref_valid()) { return false; }
    if (!oc.is_cfg_valid()) { return false; }
    //Check PR DU chain.
    m_prssamgr = m_rg->getPRSSAMgr();
    if (!oc.is_pr_du_chain_valid() && !usePRSSADU()) {
        //At least one kind of DU chain should be available.
        return false;
    }
    //Check NONPR DU chain.
    m_mdssamgr = m_rg->getMDSSAMgr();
    if (!oc.is_nonpr_du_chain_valid() && !useMDSSADU()) {
        //At least one kind of DU chain should be available.
        return false;
    }

    START_TIMER(t, getPassName());
    m_rg->getPassMgr()->checkValidAndRecompute(&oc, PASS_LOOP_INFO,
                                               PASS_UNDEF);
    LI<IRBB> const* li = m_cfg->getLoopInfo();
    if (li == nullptr) { return false; }

    m_gvn = (GVN*)(m_rg->getPassMgr()->queryPass(PASS_GVN));
    if (m_gvn == nullptr) {
        //We dependent on gvn to do critical judgement.
        return false;
    }
    if (!m_gvn->is_valid()) {
        m_gvn->perform(oc);
    }
    if (!m_gvn->is_valid()) { return false; }

    if (g_dump_opt.isDumpBeforePass() && g_dump_opt.isDumpRP()) {
        dumpBeforePass();
    }

    m_rg->getLogMgr()->startBuffer();
    init();
    List<LI<IRBB> const*> worklst;
    while (li != nullptr) {
        worklst.append_tail(li);
        li = LI_next(li);
    }
    //buildLifeTime();
    RPCtx ctx(&oc);
    bool prssa_valid = usePRSSADU();
    bool mdssa_valid = useMDSSADU();
    bool change = EvaluableScalarReplacement(worklst, ctx);
    if (change) {
        //DU reference and du chain has maintained.
        ASSERT0(m_rg->verifyMDRef());
        ASSERT0(verifyMDDUChain(m_rg, oc));

        //Enforce following pass to recompute gvn.
        m_gvn->set_valid(false);
        oc.setInvalidIfDUMgrLiveChanged();
        ASSERT0(!prssa_valid || PRSSAMgr::verifyPRSSAInfo(m_rg));
        ASSERT0(!mdssa_valid || MDSSAMgr::verifyMDSSAInfo(m_rg));
        if (g_dump_opt.isDumpAfterPass() && g_dump_opt.isDumpRP()) {
            //Exact and Inexact Acc info has been dumpped during promotion.
            dump();
        }
    } else {
        m_rg->getLogMgr()->cleanBuffer();
    }
    m_rg->getLogMgr()->endBuffer();
    clean();
    END_TIMER(t, getPassName());
    return change;
}
//END RegPromot

} //namespace xoc
