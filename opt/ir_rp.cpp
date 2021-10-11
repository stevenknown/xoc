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


//Note the delegate is one of reference in 'inexact_acc'.
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

        IRList const* occlst = m_md2occlst.get(md);
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
        IRList const* occlst = m_md2occlst.get(md);
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
        ASSERT0(m_md2occlst.get(md) == nullptr);

        //The first ir that added to list will be the delegate.
        lst = new IRList();
        m_md2occlst.set(md, lst);
    } else {
        lst = m_md2occlst.get(md);
    }
    ASSERT0(lst && !lst->find(ir));
    lst->append_tail(ir);
}


//Return true if ir in occs list is unique.
bool ExactAccTab::isOccUnique(DefMiscBitSetMgr * sm) const
{
    DefSBitSet visit(sm->getSegMgr());
    IRList * lst = nullptr;
    TMapIter<MD const*, List<IR*>*> it;
    for (MD const* md = m_md2occlst.get_first(it, &lst); md != nullptr;
         md = m_md2occlst.get_next(it, &lst)) {
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
    IRList * lst = m_md2occlst.remove(md);
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
        dumpIR(ir, rg);
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
    switch (t->getCode()) {
    case IR_LD:
        hval = t->getCode() + (t->getOffset() + 1) +
               (UINT)(size_t)t->getType();
        break;
    case IR_ILD:
        for (IR const* x = iterInitC(t, it);
             x != nullptr; x = iterNextC(it)) {
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
        for (IR const* x = iterInitC(IST_base(t), it);
             x != nullptr; x = iterNextC(it)) {
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
        for (IR const* x = iterInitC(t, it);
             x != nullptr; x = iterNextC(it)) {
            UINT v = x->getCode() + (x->getOffset() + 1) +
                    (UINT)(size_t)x->getType();
            if (x->is_id()) {
                v += ((UINT)(size_t)ID_info(x)) * 5;
            }
            hval += v;
        }
        break;
    case IR_STARRAY:
        hval += IR_ARRAY + (t->getOffset() + 1) +
                (UINT)(size_t)t->getType();
        for (IR const* x = iterInitC(ARR_base(t), it);
             x != nullptr; x = iterNextC(it)) {
            UINT v = x->getCode() + (x->getOffset() + 1) +
                    (UINT)(size_t)x->getType();
            if (x->is_id()) {
                v += ((UINT)(size_t)ID_info(x)) * 5;
            }
            hval += v;
        }
        for (IR const* x = iterInitC(ARR_sub_list(t), it);
             x != nullptr; x = iterNextC(it)) {
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
        for (IR const* x = iterInit(const_cast<IR*>(ir), ii);
             x != nullptr; x = iterNext(ii)) {
            append(x->id());
        }
    }
};


//
//START DelegateMgr
//
//The function collects the outside loop DEF|USE for 'delegate'.
//Note the function does NOT build any DU chain for any IR, it is just
//do collection.
void DelegateMgr::collectOutsideLoopDefUse(IR * occ, IR * delegate,
                                           LI<IRBB> const* li)
{
    ASSERT0(occ->isMemoryRefNonPR());
    ASSERTN(occ->getSSAInfo() == nullptr, ("should not have SSA du"));
    IRSet irset(getSegMgr());
    if (occ->is_exp()) {
        //occ is USE.
        DUSet * defset = m_dele2outsidedefset.get(delegate);
        xoc::collectDefSet(occ, m_mdssamgr, true, &irset);
        if (irset.is_empty()) { return; }

        if (defset == nullptr) {
            defset = (DUSet*)getSBSMgr()->allocSBitSetCore();
            m_dele2outsidedefset.set(delegate, defset);
        }

        IRSetIter di = nullptr;
        for (INT i = irset.get_first(&di); i >= 0; i = irset.get_next(i, &di)) {
            IR * d = m_rg->getIR(i);
            ASSERT0(d->is_stmt());
            if (!li->isInsideLoop(d->getBB()->id())) {
                defset->bunion(i, *getSBSMgr());
            }
        }
        return;
    }

    //occ is DEF.
    ASSERT0(occ->is_stmt());
    xoc::collectUseSet(occ, m_mdssamgr, &irset);

    DUSet * useset = m_dele2outsideuseset.get(delegate);
    if (useset == nullptr) {
        useset = (DUSet*)getSBSMgr()->allocSBitSetCore();
        m_dele2outsideuseset.set(delegate, useset);
    }

    IRSetIter di = nullptr;
    for (INT i = irset.get_first(&di); i >= 0; i = irset.get_next(i, &di)) {
        IR * u = m_rg->getIR(i);
        ASSERT0(u->is_exp());
        if (u->is_id()) {
            IRBB * bb = ID_phi(u)->getBB();
            if (!li->isInsideLoop(bb->id())) {
                useset->bunion(i, *getSBSMgr());
            }
            continue;
        }

        if (!li->isInsideLoop(u->getStmt()->getBB()->id())) {
            useset->bunion(i, *getSBSMgr());
        }
    }
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
IR * DelegateMgr::genInitStmt(IR const* delegate, IN IR * rhs)
{
    IR const* pr = getPR(delegate);
    ASSERT0(pr);
    IR * stpr = m_rg->buildStorePR(PR_no(pr), pr->getType(), rhs);
    m_rg->allocRefForPR(stpr);
    ASSERT0(m_dele2init.get(const_cast<IR*>(delegate)) == nullptr);
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
        //'exact_acc'. So we do NOT need to free it here because it has
        //been freed during promotion processing.
        //m_rg->freeIRTree(x);
    }
    m_dele2init.clean();
    m_dele2restore.clean();

    smpoolDelete(m_pool);
    m_pool = nullptr;
}


bool DelegateMgr::dump() const
{
    if (!m_rg->isLogMgrInit()) { return false; }
    note(m_rg, "\n==---- DUMP DelegateMgr '%s' ----==",
         m_rg->getRegionName());

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

    note(m_rg, "\n\n==-- Dele2RestoreStmt --==");
    it.clean();
    IR * stmt;
    for (IR * dele = m_dele2restore.get_first(it, &stmt); dele != nullptr;
         dele = m_dele2init.get_next(it, &stmt)) {
        note(m_rg, "\n");
        note(m_rg, "\nDELE:");
        dumpIR(dele, m_rg, nullptr, IR_DUMP_NO_NEWLINE);
        note(m_rg, "\nRestoreStmt:");
        dumpIR(stmt, m_rg, nullptr, IR_DUMP_KID);
    }

    note(m_rg, "\n\n==-- Dele2DefSet --==");
    TMapIter<IR const*, DUSet*> it3;
    DUSet * set;
    for (IR const* dele = m_dele2outsidedefset.get_first(it3, &set);
         dele != nullptr;
         dele = m_dele2outsidedefset.get_next(it3, &set)) {
        note(m_rg, "\n");
        note(m_rg, "\nDELE:");
        dumpIR(dele, m_rg, nullptr, IR_DUMP_NO_NEWLINE);

        note(m_rg, "\nDEFSET:");
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

    note(m_rg, "\n\n==-- Dele2UseSet --==");
    TMapIter<IR const*, DUSet*> it4;
    DUSet * set2;
    for (IR const* dele = m_dele2outsideuseset.get_first(it4, &set2);
         dele != nullptr;
         dele = m_dele2outsideuseset.get_next(it4, &set2)) {
        note(m_rg, "\n");
        note(m_rg, "\nDELE:");
        dumpIR(dele, m_rg, nullptr, IR_DUMP_NO_NEWLINE);

        note(m_rg, "\nUSESET:");
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
    return true;
}


//Generate code to fulfill epilog of delegate.
//pr: the PR to be restored to memory.
IR * DelegateMgr::genRestoreStmt(IR const* delegate, IR * pr)
{
    ASSERT0(pr && pr->is_pr());
    IR * stmt = nullptr;
    switch (delegate->getCode()) {
    case IR_ARRAY:
        stmt = CStArray::dupIRTreeByExp(delegate, pr, m_rg);
        break;
    case IR_STARRAY:
        ASSERTN(delegate->getOffset() == 0, ("TODO: not yet support."));
        //Prepare base and subscript expression list.
        stmt = m_rg->buildStoreArray(m_rg->dupIRTree(ARR_base(delegate)),
            m_rg->dupIRTreeList(ARR_sub_list(delegate)),
            delegate->getType(),
            ARR_elemtype(delegate),
            ((CArray*)delegate)->getDimNum(),
            ARR_elem_num_buf(delegate), pr);
        stmt->setOffset(delegate->getOffset());
        stmt->copyRef(delegate, m_rg);
        break;
    case IR_IST:
        stmt = m_rg->buildIStore(m_rg->dupIRTree(delegate->getBase()), pr,
                                 delegate->getOffset(), delegate->getType());
        stmt->copyRef(delegate, m_rg);
        break;
    case IR_ST:
        stmt = m_rg->buildStore(ST_idinfo(delegate), delegate->getType(),
                                delegate->getOffset(), pr);
        stmt->copyRef(delegate, m_rg);
        break;
    case IR_ILD:
        stmt = CISt::dupIRTreeByExp(delegate, pr, m_rg);
        break;
    case IR_LD:
        stmt = CSt::dupIRTreeByExp(delegate, pr, m_rg);
        break;
    default: UNREACHABLE(); //Unsupport.
    }
    ASSERT0(stmt);
    if (useMDSSADU()) {
        ASSERT0(m_mdssamgr->getMDSSAInfoIfAny(stmt) == nullptr);
        //We only generate empty MDSSAInfo rather than versioned MD info to
        //avoid the complaint of sebsequent DU chain building.
        //Leave the sanity construction work to the ending of the pass.
        m_mdssamgr->genMDSSAInfo(stmt);
    }
    m_dele2restore.set(const_cast<IR*>(delegate), stmt);
    return stmt;
}
//END DelegateMgr


//
//START RegPromot
//
void RegPromot::dumpInexact(InexactAccTab & acc)
{
    if (!m_rg->isLogMgrInit()) { return; }
    note(getRegion(), "\n---- DUMP inexact access ----");
    InexactAccTabIter it;
    for (IR * ir = acc.get_first(it); ir != nullptr; ir = acc.get_next(it)) {
        dumpIR(ir, m_rg, nullptr, IR_DUMP_SRC_LINE);
        note(getRegion(), "\n");
    }
    note(getRegion(), "\n");
}


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


void RegPromot::dump_mdlt()
{
    if (!m_rg->isLogMgrInit()) { return; }
    MDSet mdbs;
    xcom::DefMiscBitSetMgr * sbsmgr = getSBSMgr();
    MDLivenessMgr * livemgr = getMDLivenessMgr();
    BBList * bbl = m_rg->getBBList();
    for (IRBB * bb = bbl->get_head(); bb != nullptr; bb = bbl->get_next()) {
        MDSet * livein = livemgr->getLiveInMDSet(bb);
        MDSet * liveout = livemgr->getLiveOutMDSet(bb);
        if (livein->is_empty() && liveout->is_empty()) { continue; }

        mdbs.bunion(*livein, *sbsmgr);
        mdbs.bunion(*liveout, *sbsmgr);
    }
    mdbs.dump(m_md_sys);

    StrBuf buf(32);
    note(getRegion(), "\n==---- DUMP MD LIFE TIME ----==");
    MDSetIter it;
    for (INT i = mdbs.get_first(&it); i >= 0; i = mdbs.get_next(i, &it)) {
        MD * md = m_md_sys->getMD(i);
        ASSERT0(md != nullptr);
        MDLT * lt = m_md2lt_map->get(md);
        ASSERT0(lt != nullptr);
        xcom::BitSet * livebbs = MDLT_livebbs(lt);
        ASSERT0(livebbs != nullptr);

        //Print MD name.
        note(getRegion(), "\nMD%d", MD_id(md));
        prt(getRegion(), ":");

        //Print live BB.
        if (livebbs == nullptr || livebbs->is_empty()) { continue; }
        INT start = 0;
        for (INT u = livebbs->get_first(); u >= 0; u = livebbs->get_next(u)) {
            for (INT j = start; j < u; j++) {
                buf.sprint("%d,", j);
                for (UINT k = 0; k < buf.strlen(); k++) {
                    prt(getRegion(), " ");
                }
            }
            prt(getRegion(), "%d,", u);
            start = u + 1;
        }
    }
    mdbs.clean(*sbsmgr);
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

    //dump_mdlt();
}


bool RegPromot::checkArrayIsLoopInvariant(IN IR * ir, LI<IRBB> const* li)
{
    ASSERT0(ir->isArrayOp() && li);
    for (IR * s = ARR_sub_list(ir); s != nullptr; s = s->get_next()) {
        if (!isLoopInvariant(s, li, m_rg, nullptr, true)) {
            return false;
        }
    }
    if (!isLoopInvariant(ARR_base(ir), li, m_rg, nullptr, true)) {
        return false;
    }
    return true;
}


//Return true if the caller can keep doing the analysis.
//That means there are no memory referrences clobbered the
//candidate in occs of exact_acc.
//Or else the analysis for current loop should be terminated.
//Return false if find unpromotable memory reference, this may
//prevent entire loop to be promoted.
//added: true to indicates given ir has been inserted into either 'exact_acc'
//       or 'inexact_acc'.
bool RegPromot::handleArrayRef(IN IR * ir, LI<IRBB> const* li,
                               OUT ExactAccTab & exact_acc,
                               OUT InexactAccTab & inexact_acc,
                               bool * added)
{
    ASSERT0(ir->isArrayOp());
    if (!checkArrayIsLoopInvariant(ir, li)) {
        //If ir is ARRAY|STARRAY that modify inexact MD.
        //It may clobber all other array with same array base.
        clobberAccess(ir, exact_acc, inexact_acc);
        return true;
    }

    if (ir->is_stmt()) {
        ASSERT0(ir->is_starray());
        InexactAccTabIter ti;
        for (IR * ref = inexact_acc.get_first(ti);
             ref != nullptr; ref = inexact_acc.get_next(ti)) {
            UINT st = analyzeArrayStatus(ir, ref);
            if (st == RP_SAME_ARRAY) { continue; }
            if (st == RP_DIFFERENT_ARRAY) { continue; }
 
            //The result can not be promoted.
            //Check the promotable candidates if current stmt modify
            //the related MD.
            clobberAccess(ir, exact_acc, inexact_acc);
            return true;
        }
    }
    inexact_acc.addOcc(ir);
    *added = true;
    return true;
}


//Return true if the caller can keep doing the analysis.
//That means there are no memory referrences clobbered the
//candidate in of exact_acc.
//Return false if find unpromotable memory reference, this may
//prevent entire loop be promoted.
//ir: stmt or expression to be handled.
//added: true to indicates given ir has been inserted into either 'exact_acc'
//       or 'inexact_acc'.
bool RegPromot::handleGeneralMustRef(IR * ir, LI<IRBB> const* li,
                                     OUT ExactAccTab & exact_acc,
                                     OUT InexactAccTab & inexact_acc,
                                     bool * added)
{
    MD const* mustref = ir->getMustRef();
    ASSERT0(mustref);
    if (mustref->is_volatile()) {
        clobberAccess(ir, exact_acc, inexact_acc);
        return true;
    }

    if (m_dont_promote.is_overlap(ir)) {
        //If ir should not be promoted, then all the others mem-ref
        //that overlapped with it should not be promoted too.
        clobberAccess(ir, exact_acc, inexact_acc);
        return true;
    }

    sweepOutAccess(ir, exact_acc, inexact_acc);

    if (mustref->is_exact()) {
        exact_acc.addOcc(ir);
        *added = true;
        return true;
    }
    return handleInexactOrMayRef(ir, li, exact_acc, inexact_acc, added);
}


//added: true to indicates given ir has been inserted into either 'exact_acc'
//       or 'inexact_acc'.
bool RegPromot::handleIndirect(IR * ir, LI<IRBB> const* li,
                               OUT ExactAccTab & exact_acc,
                               OUT InexactAccTab & inexact_acc,
                               bool * added)
{
    ASSERT0(ir->isIndirectMemOp());
    //Check if ir is loop invariant.
    if (!checkIndirectAccessIsLoopInvariant(ir, li)) {
        clobberAccess(ir, exact_acc, inexact_acc);
        return true;
    }

    if (ir->is_stmt()) {
        ASSERT0(ir->is_ist());
        //Determine wherther current ir clobber elements in access list.
        InexactAccTabIter ti;
        for (IR * ref = inexact_acc.get_first(ti);
             ref != nullptr; ref = inexact_acc.get_next(ti)) {
            if (m_gvn->isSameMemLoc(ir, ref)) { continue; }
            if (m_gvn->isDiffMemLoc(ir, ref)) { continue; }

            //Current ir can not be promoted. Check the promotable candidates
            //if current ir overrided related MDs.
            clobberAccess(ir, exact_acc, inexact_acc);
            return true;
        }
    }
    inexact_acc.addOcc(ir);
    *added = true;
    return true;
}


//The function handle cases either ir does not have MustRef or ir has an
//inexact MustRef.
//added: true to indicates given ir has been inserted into either 'exact_acc'
//       or 'inexact_acc'.
bool RegPromot::handleInexactOrMayRef(IR * ir, LI<IRBB> const* li,
                                      OUT ExactAccTab & exact_acc,
                                      OUT InexactAccTab & inexact_acc,
                                      bool * added)
{
    if (ir->isIndirectMemOp()) {
        return handleIndirect(ir, li, exact_acc, inexact_acc, added);
    }
    if (ir->isArrayOp()) {
        return handleArrayRef(ir, li, exact_acc, inexact_acc, added);
    }
    UNREACHABLE();
    return true;
}


//Return true if the caller can keep doing the analysis.
//That means there are no memory referrences clobbered the
//candidate in of exact_acc.
//Return false if find unpromotable memory reference, this may
//prevent entire loop be promoted.
//ir: stmt or expression to be handled.
//added: true to indicates given ir has been inserted into either 'exact_acc'
//       or 'inexact_acc'.
bool RegPromot::handleGeneralRef(IR * ir, LI<IRBB> const* li,
                                 OUT ExactAccTab & exact_acc,
                                 OUT InexactAccTab & inexact_acc,
                                 bool * added)
{
    ASSERT0(ir->isMemoryRef());
    MD const* mustref = ir->getMustRef();
    if (mustref != nullptr) {
        return handleGeneralMustRef(ir, li, exact_acc, inexact_acc, added);
    }
    return handleInexactOrMayRef(ir, li, exact_acc, inexact_acc, added);
}


void RegPromot::addDontPromote(IR const* ir)
{
    m_dont_promote.append(ir);
}


//The function sweep out the Access Expression or Stmt from 'exact_acc' which
//MD reference may or must overlaped with given 'ir'
//except the ones that are exactly covered by 'ir'.
//This function consider both MustRef MD and MayRef MDSet.
void RegPromot::sweepOutExactAccess(IR * ir, MOD ExactAccTab & exact_acc)
{
    MD const* mustref = ir->getMustRef();
    MDSet const* mayref = ir->getMayRef();
    ExactAccTabIter it;
    Vector<MD const*> need_to_be_removed;
    INT cnt = 0;
    if (mustref != nullptr) {
        for (MD const* md = exact_acc.get_first(it, nullptr);
             md != nullptr; md = exact_acc.get_next(it, nullptr)) {
            if (mustref == md || mustref->is_exact_cover(md)) { continue; }
            if (mustref->is_overlap(md)) {
                //occ is not suite to promot any more.
                need_to_be_removed.set(cnt, md);
                cnt++;
            }
        }
    } else if (mayref != nullptr) {
        for (MD const* md = exact_acc.get_first(it, nullptr);
             md != nullptr; md = exact_acc.get_next(it, nullptr)) {
            if (mayref->is_contain(md)) {
                //occ is not suite to promot any more.
                need_to_be_removed.set(cnt, md);
                cnt++;
            }
        }
    }

    for (cnt = cnt - 1; cnt >= 0; cnt--) {
        MD const* md = need_to_be_removed.get(cnt);
        exact_acc.remove(md);
    }
}


//The function sweep out the Access Expression or Stmt from 'inexact_acc' which
//MD reference may or must overlaped with given 'ir'
//except the ones that are exactly covered by 'ir'.
//This function consider both MustRef MD and MayRef MDSet.
void RegPromot::sweepOutInexactAccess(IR * ir, MOD InexactAccTab & inexact_acc)
{
    MD const* mustref = ir->getMustRef();
    MDSet const* mayref = ir->getMayRef();
    InexactAccTabIter iter2;
    Vector<IR*> need_to_be_removed2;
    INT cnt = 0;
    if (mustref != nullptr) {
        for (IR * acc = inexact_acc.get_first(iter2);
             acc != nullptr; acc = inexact_acc.get_next(iter2)) {
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
        for (IR * acc = inexact_acc.get_first(iter2);
             acc != nullptr; acc = inexact_acc.get_next(iter2)) {
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
        inexact_acc.remove(e);
    }
}


//The function sweep out the Access Expression or Stmt from 'exact_acc' and
//'inexact_acc' which MD reference may or must overlaped with given 'ir'
//except the ones that are exactly covered by 'ir'.
//This function consider both MustRef MD and MayRef MDSet.
void RegPromot::sweepOutAccess(IR * ir, MOD ExactAccTab & exact_acc,
                               MOD InexactAccTab & inexact_acc)
{

    sweepOutExactAccess(ir, exact_acc);
    sweepOutInexactAccess(ir, inexact_acc);
}


void RegPromot::clobberExactAccess(IR const* ir, MOD ExactAccTab & exact_acc)
{
    MD const* mustref = ir->getMustRef();
    MDSet const* mayref = ir->getMayRef();
    ExactAccTabIter it;
    Vector<MD const*> need_to_be_removed;
    INT cnt = 0;
    if (mustref != nullptr) {
        for (MD const* md = exact_acc.get_first(it, nullptr);
             md != nullptr; md = exact_acc.get_next(it, nullptr)) {
            if (mustref == md || mustref->is_overlap(md)) {
                //Current ir may modify the candidate's md.
                //We think the candidate is not suite to promot any more.
                need_to_be_removed.set(cnt, md);
                cnt++;
            }
        }
    }

    if (mayref != nullptr && !mayref->is_empty()) {
        for (MD const* md = exact_acc.get_first(it, nullptr);
             md != nullptr; md = exact_acc.get_next(it, nullptr)) {
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
        exact_acc.remove(md);
    }
}


void RegPromot::clobberInexactAccess(IR const* ir,
                                     MOD InexactAccTab & inexact_acc)
{
    MD const* mustref = ir->getMustRef();
    MDSet const* mayref = ir->getMayRef();
    InexactAccTabIter iter;
    Vector<IR*> need_to_be_removed;
    INT cnt = 0;
    if (mustref != nullptr) {
        for (IR * acc = inexact_acc.get_first(iter);
             acc != nullptr; acc = inexact_acc.get_next(iter)) {
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
        for (IR * acc = inexact_acc.get_first(iter);
             acc != nullptr; acc = inexact_acc.get_next(iter)) {
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
        inexact_acc.remove(e);
    }
}


//'ir' can not be promoted.
//Check the promotable candidates if ir overlapped with the related
//MD and MDSet.
//This function consider both MustRef MD and MayRef MDSet.
void RegPromot::clobberAccess(IR const* ir, MOD ExactAccTab & exact_acc,
                              MOD InexactAccTab & inexact_acc)
{
    addDontPromote(ir);
    clobberExactAccess(ir, exact_acc);
    clobberInexactAccess(ir, inexact_acc);
}


bool RegPromot::checkIndirectAccessIsLoopInvariant(IR const* ir,
                                                   LI<IRBB> const* li)
{
    ASSERT0(li);
    if (ir->is_ild() || ir->is_ist()) {
        return isLoopInvariant(ir->getBase(), li, m_rg, nullptr, true);
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
                               OUT ExactAccTab & exact_acc,
                               OUT InexactAccTab & inexact_acc)
{
    for (IR * ir = root; ir != nullptr; ir = ir->get_next()) {
        bool added = false;
        if (ir->isMemoryRefNonPR() &&
            !handleGeneralRef(ir, li, exact_acc, inexact_acc, &added)) {
            return false;
        }
        if (added) {
            //Do NOT add IR kid tree into table if the root has been added.
            continue;
        }
        for (UINT i = 0; i < IR_MAX_KID_NUM(ir); i++) {
            IR * k = ir->getKid(i);
            if (k == nullptr) { continue; }
            if (!scanIRTreeList(k, li, exact_acc, inexact_acc)) {
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
                         OUT ExactAccTab & exact_acc,
                         OUT InexactAccTab & inexact_acc)
{
    //Do NOT use IR iterator to avoid adding both parent and kid IR into
    //exact_acc or inexact_acc.
    switch (ir->getCode()) {
    case IR_ST: {
        bool added;
        if (!handleGeneralRef(ir, li, exact_acc, inexact_acc, &added)) {
            return false;
        }
        return scanIRTreeList(ir->getRHS(), li, exact_acc, inexact_acc);
    }
    case IR_STPR:
        return scanIRTreeList(ir->getRHS(), li, exact_acc, inexact_acc);
    case IR_STARRAY:
    case IR_IST: {
        bool added;
        if (!handleGeneralRef(ir, li, exact_acc, inexact_acc, &added)) {
            return false;
        }
        if (added) {
            return scanIRTreeList(ir->getRHS(), li, exact_acc, inexact_acc);
        }
        for (UINT i = 0; i < IR_MAX_KID_NUM(ir); i++) {
            IR * k = ir->getKid(i);
            if (k != nullptr &&
                !scanIRTreeList(k, li, exact_acc, inexact_acc)) {
                return false;
            }
        }
        return true;
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
            if (!scanIRTreeList(k, li, exact_acc, inexact_acc)) {
                return false;
            }
        }
        return true;
    case IR_CALL:
        //Do NOT iterate DUMMY expression-list.
        return scanIRTreeList(CALL_param_list(ir), li, exact_acc, inexact_acc);
    case IR_ICALL:
        //Do NOT iterate DUMMY expression-list.
        if (!scanIRTreeList(ICALL_callee(ir), li, exact_acc, inexact_acc)) {
            return false;
        }
        return scanIRTreeList(CALL_param_list(ir), li, exact_acc, inexact_acc);
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
                       OUT ExactAccTab & exact_acc,
                       OUT InexactAccTab & inexact_acc)
{
    for (IR * ir = BB_first_ir(bb); ir != nullptr; ir = BB_next_ir(bb)) {
        if (!ir->isContainMemRef()) { continue; }
        if (ir->is_region()) { return false; }
        if (ir->isCallStmt() && !ir->isReadOnly()) {
            clobberAccess(ir, exact_acc, inexact_acc);
        }
        if (!scanStmt(ir, li, exact_acc, inexact_acc)) {
            return false;
        }
    }
    return true;
}


//Generate code to restore value from delegate PR to delegate memory object.
void RegPromot::handleEpilog(ConstIRTab const& restore2mem,
                             DelegateMgr & delemgr, IRBB * exit_bb)
{
    ConstIRTabIter ti;
    for (IR const* delegate = restore2mem.get_first(ti);
         delegate != nullptr; delegate = restore2mem.get_next(ti)) {
        IR * pr = m_rg->dupIRTree(delemgr.getPR(delegate));
        IR * stmt = delemgr.genRestoreStmt(delegate, pr);
        IR const* ir  = BB_irlist(exit_bb).get_head();
        if (ir != nullptr) {
            xoc::copyDbx(stmt, ir, m_rg);
        }
        BB_irlist(exit_bb).append_head_ex(stmt);
    }
}


//Return true if 'ir' can be promoted.
//Note ir must be memory reference.
bool RegPromot::isPromotable(IR const* ir) const
{
    ASSERT0(ir->isMemoryRef());
    //I think that the IR with may-throw attribute is promotable.
    if (ir->hasSideEffect() && ir->isNoMove()) { return false; }
    return true;
}


//Return true if there is IR to be promoted, otherwise return false.
//dele: the delegate which indicates an exact-accessing reference.
bool RegPromot::promoteExactAccessDelegate(IR const* dele,
                                           DelegateMgr & delemgr,
                                           LI<IRBB> const* li, IRIter & ii,
                                           IRBB * preheader, IRBB * exit_bb,
                                           ExactAccTab & exact_tab)
{
    ASSERT0(!dele->isPROp());
    IR const* promoted_pr = delemgr.getPR(dele);
    ASSERT0(promoted_pr);
    handleProlog(dele, promoted_pr, delemgr, preheader);

    //Map delegate MD to its DEF which is in the loop.
    //These MDs need to be restored to memory in epilog BB of loop.
    ConstIRTab restore2mem;

    //This table records the IRs which should NOT be processed any more.
    //They may be freed.
    //e.g: a[i], if array referrence is freed, the occurrence of variable i
    //also be freed.
    PromotedTab promoted;
    ASSERT0(dele->getMustRef());
    IRList * occs = exact_tab.getOccs(dele->getMustRef());
    ASSERT0(occs);
    Occ2Occ occ2newocc;
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

        handleAccessInBody(occ, delegate, delemgr,
                           restore2mem, occ2newocc, li, ii);

        //Each memory reference in the tree has been promoted.
        promoted.addTree(occ, ii);
    }

    ASSERT0(verifyIRandBB(m_rg->getBBList(), m_rg));
    //Generate code to fulfill epilog of delegate.
    handleEpilog(restore2mem, delemgr, exit_bb);
    addDUChainForExactAccDele(dele, occ2newocc, delemgr, exact_tab, li);
    removeRedundantDUChain(occ2newocc);
    freeExactOccs(*occs, m_rg, m_gvn);
    return true;
}


static void collectOutsideLoopDefUseInfo(ExactAccTab const& exact_tab,
                                         LI<IRBB> const* li,
                                         MOD DelegateMgr & delemgr)
{
    ExactAccTabIter it;
    for (MD const* md = exact_tab.get_first(it);  md != nullptr;
         md = exact_tab.get_next(it)) {
        IRListIter it;
        IRList * occs = const_cast<ExactAccTab&>(exact_tab).getOccs(md);
        ASSERT0(occs);
        for (IR * occ = occs->get_head(&it);
             occ != nullptr; occ = occs->get_next(&it)) {
             MD const* md = occ->getMustRef();
            ASSERT0(md && md->is_exact());
 
            //Get the unique delegate.
            IR * delegate = const_cast<ExactAccTab&>(exact_tab).getDele(md);
            if (delegate == nullptr) {
                continue;
            }
            delemgr.collectOutsideLoopDefUse(occ, delegate, li);
        }
    }
}


//Return true if there is IR to be promoted, otherwise return false.
//exact_tab: record delegates for each IR ref.
bool RegPromot::promoteExactAccess(LI<IRBB> const* li, IRIter & ii,
                                   IRBB * preheader, IRBB * exit_bb,
                                   ExactAccTab & exact_tab)
{
    ASSERT0(exact_tab.verify());
    ASSERT0(preheader && exit_bb && li);
    ASSERT0(!li->getBodyBBSet()->is_empty());

    //Create delegate info for each given delegates.
    DelegateMgr delemgr(m_rg, m_gvn, exact_tab.get_elem_count());
    ExactAccTabIter mi;
    IR * delegate = nullptr;
    for (MD const* md = exact_tab.get_first(mi, &delegate);
         md != nullptr;) {
        ASSERT0(delegate);
        if (!isPromotable(delegate)) {
            //Do not promote the reference, and remove current 'delegate' from
            //exact_tab.
            MD const* next_md = exact_tab.get_next(mi, &delegate);
            exact_tab.remove(md);
            md = next_md;
            continue;
        }

        delemgr.createDelegateInfo(delegate);

        //Collect outside loop DefUse information.
        IRListIter it;
        IRList * occs = const_cast<ExactAccTab&>(exact_tab).getOccs(md);
        ASSERT0(occs);
        for (IR * occ = occs->get_head(&it);
             occ != nullptr; occ = occs->get_next(&it)) {
            delemgr.collectOutsideLoopDefUse(occ, delegate, li);
        }

        md = exact_tab.get_next(mi, &delegate);
        ASSERT0(!((md == nullptr) ^ (delegate == nullptr)));
    }
    //collectOutsideLoopDefUseInfo(exact_tab, li, delemgr);

    mi.clean();
    for (exact_tab.get_first(mi, &delegate);
         delegate != nullptr; exact_tab.get_next(mi, &delegate)) {
        //CASE:If whole IR tree is regards as delegate, the kids in the tree
        //should not be added into exact_tab.
        //if (delegate->is_undef()) { continue; }

        promoteExactAccessDelegate(delegate, delemgr, li, ii, preheader,
                                   exit_bb, exact_tab);
    }
    return true;
}


//Return true if some node at IR tree may throw exception.
bool RegPromot::isMayThrow(IR * ir, IRIter & it)
{
    it.clean();
    IR const* k = iterInit(ir, it);
    for (; k != nullptr; k = iterNext(it)) {
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


static bool hasLoopOutsideUse(IR const* delegate, DelegateMgr const& mgr)
{
    ASSERT0(delegate);
    DUSet const* useset = mgr.getOutsideUseSet(delegate);
    ASSERT0(useset);
    return !useset->is_empty();
}


//Fixup DU chain if there is untrue dependence.
//occ2newocc: record the IR stmt/exp that need to fixup.
void RegPromot::removeRedundantDUChain(Occ2Occ & occ2newocc)
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


void RegPromot::handleExpInBody(IR * occ, IR const* delegate,
                                DelegateMgr const& delemgr,
                                OUT Occ2Occ & occ2newocc, IRIter & ii)
{
    ASSERT0(isExpCand(occ));
    IR * stmt = occ->getStmt();
    ASSERT0(stmt);

    IR * pr = m_rg->dupIR(delemgr.getPR(delegate));

    ASSERT0(occ->getParent());
    bool r = occ->getParent()->replaceKid(occ, pr, false);
    CHECK0_DUMMYUSE(r);
    pr->copyAI(occ, m_rg);
    m_gvn->setMapIR2VN(pr, m_gvn->mapIR2VN(occ));

    if (stmt->isMayThrow() && !isMayThrow(stmt, ii)) {
        IR_may_throw(stmt) = false;
    }

    occ2newocc.set(occ, pr);
    //Do not free occ here since it will be freed later.
}


//restore2mem: record the delegate that need to restore.
void RegPromot::handleStmtInBody(IR * occ, IR const* delegate,
                                 MOD DelegateMgr & delemgr,
                                 OUT ConstIRTab & restore2mem,
                                 OUT Occ2Occ & occ2newocc,
                                 LI<IRBB> const* li)
{
    ASSERT0(isStmtCand(occ));
    ASSERTN(occ->getRHS(), ("must be store operation"));
    IR const* delegate_pr = delemgr.getPR(delegate);
    ASSERT0(delegate_pr);

    //Substitute STPR for writing memory.
    IR * stpr = m_rg->buildStorePR(PR_no(delegate_pr),
                                   delegate_pr->getType(), occ->getRHS());
    m_rg->allocRefForPR(stpr);
    occ->setRHS(nullptr);
    stpr->copyAI(occ, m_rg);
    m_gvn->setMapIR2VN(stpr, m_gvn->mapIR2VN(occ));

    //Note, may be some USE of 'occ' has already been promoted to
    //PR, but it doesn't matter, you don't need to check the truely
    //dependence here, since we just want to see whether
    //there exist outer loop references to this stmt. And all
    //same group memory references will be promoted to PR after
    //the function return.
    if ((hasLoopOutsideUse(delegate, delemgr) || mayBeGlobalRef(occ)) &&
        !restore2mem.find(const_cast<IR*>(delegate))) { //delegate is unique
        //Record that current delegate have to be restored to memory.
        restore2mem.append(delegate);
    }

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
                                   OUT Occ2Occ & occ2newocc,
                                   LI<IRBB> const* li, IRIter & ii)
{
    ASSERT0(ref && delegate && li);
    ASSERT0(ref->isMemoryRefNonPR());
    if (ref->is_stmt()) {
        handleStmtInBody(ref, delegate, delemgr, restore2mem, occ2newocc, li);
        return;
    }
    handleExpInBody(ref, delegate, delemgr, occ2newocc, ii);
}


//The function generates iniailization code of promoted PR.
//Note the function leaves the work that to build DU chain of PR and STPR to
//the sebsequent function, it will be done at buildDUChainForDeleRelatedPR().
void RegPromot::handleProlog(IR const* delegate, IR const* promoted_pr,
                             DelegateMgr & delemgr, IRBB * preheader)
{
    IR * rhs = nullptr; //record the initial value of delegate.
    //Load value into PR.
    switch (delegate->getCode()) {
    case IR_STARRAY:
        rhs = CArray::dupIRTreeByStmt(delegate, m_rg);
        break;
    case IR_IST:
        rhs = CILd::dupIRTreeByStmt(delegate, m_rg);
        break;
    case IR_ST:
        rhs = CLd::dupIRTreeByStmt(delegate, m_rg);
        break;
    case IR_ARRAY:
    case IR_ILD:
    case IR_LD:
        rhs = m_rg->dupIRTree(delegate);
        break;
    default: UNREACHABLE(); //unsupport.
    }
    ASSERT0(rhs);
    IR * stpr = delemgr.genInitStmt(delegate, rhs);
    if (BB_irlist(preheader).get_head() != nullptr) {
        stpr->copyAI(BB_irlist(preheader).get_head(), m_rg);
    }

    //TODO: There is no need to insert init-stmt for delegate if the delegate
    //does not have a live-in DEF stmt to loop-header.
    //e.g:int m;
    //    while (...) {
    //      m=...
    //      ...=m;
    //    }
    //    USE(m);
    BB_irlist(preheader).append_tail_ex(stpr);
}


void RegPromot::addDUChainForInexactAccDele(IR const* dele,
                                            DelegateMgr const& delemgr,
                                            Occ2Occ const& occ2newocc)
{
    IR * init_stmt = delemgr.getInitStmt(dele);
    ASSERT0(init_stmt->is_stpr());

    IR * restore_stmt = delemgr.getRestoreStmt(dele);
    if (restore_stmt != nullptr) {
        ASSERT0(restore_stmt->is_stmt());
        IR * exposed_use = restore_stmt->getRHS();
        ASSERT0(exposed_use->is_pr());

        //Build DU chain for initialization-def and exposed-use.
        xoc::buildDUChain(init_stmt, exposed_use, m_rg);
    }

    IRList deflst;
    //Build DU chain for initialization-def and intra-loop-use.
    //addDUChainForInitDef(dele, init_stmt, occ2newocc, exact_tab, deflst);

    //Build DU chain for intra-loop-def and its USE.
    //addDUChainForIntraDef(occ2newocc, restore_stmt, deflst, li);

    if (usePRSSADU()) {
        //TODO: hack
        //insertPhi(dele, init_stmt, occ2newocc, exact_tab, deflst);
    }
}


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
        xoc::buildDUChain(init_stmt, newocc, m_rg);
    }
}


//Build DU chain for intra-loop-def and its USE.
void RegPromot::addDUChainForIntraDefAndUseSet(Occ2Occ const& occ2newocc,
                                               IRSet const& useset,
                                               LI<IRBB> const* li,
                                               IR * restore_stmt,
                                               IR * newocc_def)
{
    IRSetIter di = nullptr;
    for (INT i = useset.get_first(&di); i >= 0;
         i = useset.get_next(i, &di)) {
        IR * use = m_rg->getIR(i);
        ASSERT0(use->is_exp() && use->isMemoryRefNonPR());

        IR * newocc_use = const_cast<Occ2Occ&>(occ2newocc).get(use);
        if (newocc_use != nullptr) {
            xoc::buildDUChain(newocc_def, newocc_use, m_rg);
            continue;
        }

        if (restore_stmt == nullptr) { continue; }

        //The USE is an outside-loop USE, that should establish
        //DU chain with the restore-stmt.
        IRBB * usebb = nullptr;
        if (use->is_id()) {
            //Build DU chain from restore_stmt to id, if the id is NOT
            //belong to current LoopInfo.
            usebb = ID_phi(use)->getBB();
        } else {
            ASSERT0(use->getStmt());
            usebb = use->getStmt()->getBB();
        }
        ASSERT0(usebb);
        if (!li->isInsideLoop(usebb->id()) &&
            !m_gvn->isDiffMemLoc(restore_stmt, use)) {
            xoc::buildDUChain(restore_stmt, use, m_rg);
        }
    }
}


//Build DU chain for intra-loop-def and its USE.
void RegPromot::addDUChainForIntraDef(Occ2Occ const& occ2newocc,
                                      IR * restore_stmt, IRList const& deflst,
                                      LI<IRBB> const* li)
{
    IR * exposed_use = nullptr;
    if (restore_stmt != nullptr) {
        exposed_use = restore_stmt->getRHS();
        ASSERT0(exposed_use->is_pr());
    }
    IRListIter irit;
    //Build DU chain for intra-loop-def and its USE.
    IRSet useset(getSegMgr());
    for (IR * def = deflst.get_head(&irit); def != nullptr;
         def = deflst.get_next(&irit)) {
        useset.clean();
        xoc::collectUseSet(def, m_mdssamgr, &useset);

        IR * newocc_def = const_cast<Occ2Occ&>(occ2newocc).get(def);
        ASSERT0(newocc_def && newocc_def->is_stpr());

        addDUChainForIntraDefAndUseSet(occ2newocc, useset, li, restore_stmt,
                                       newocc_def);

        if (exposed_use != nullptr) {
            //Build DU chain for intra-loop-def and exposed-use.
            xoc::buildDUChain(newocc_def, exposed_use, m_rg);
        }
    }
}


void RegPromot::addDUChainForExactAccDele(IR const* dele,
                                          Occ2Occ const& occ2newocc,
                                          DelegateMgr const& delemgr,
                                          ExactAccTab const& exact_tab,
                                          LI<IRBB> const* li)
{
    MD const* md = const_cast<IR*>(dele)->getMustRef();
    ASSERT0(md && md->is_exact());
    IR * init_stmt = delemgr.getInitStmt(dele);
    ASSERT0(init_stmt->is_stpr());

    IR * restore_stmt = delemgr.getRestoreStmt(dele);
    if (restore_stmt != nullptr) {
        ASSERT0(restore_stmt->is_stmt());
        IR * exposed_use = restore_stmt->getRHS();
        ASSERT0(exposed_use->is_pr());

        //Build DU chain for initialization-def and exposed-use.
        xoc::buildDUChain(init_stmt, exposed_use, m_rg);
    }

    IRList deflst;
    //Build DU chain for initialization-def and intra-loop-use.
    addDUChainForInitDef(dele, init_stmt, occ2newocc, exact_tab, deflst);

    //Build DU chain for intra-loop-def and its USE.
    addDUChainForIntraDef(occ2newocc, restore_stmt, deflst, li);

    if (usePRSSADU()) {
        //TODO: hack
        //insertPhi(dele, init_stmt, occ2newocc, exact_tab, deflst);
    }
}


//Return true if there is IR being promoted, otherwise return false.
bool RegPromot::promoteInexactAccessDelegate(IR const* dele,
                                             DelegateMgr & delemgr,
                                             LI<IRBB> const* li,
                                             IRBB * preheader,
                                             IRBB * exit_bb,
                                             InexactAccTab & inexact_acc,
                                             IRIter & ii)
{
    ASSERT0(!dele->isPROp());
    ASSERT0(li && exit_bb && preheader);
    ASSERT0(m_gvn && m_gvn->is_valid());
    ASSERT0(!li->getBodyBBSet()->is_empty());
    RefTab * deletab = delemgr.getDelegateTab();
    if (deletab->get_elem_count() == 0) { return false; }

    //Generate code to fulfill initialization of delegate.
    INT cur = 0;
    for (IR * delegate = deletab->get_first(cur);
         cur >= 0; delegate = deletab->get_next(cur)) {
        IR const* pr = delemgr.getPR(delegate);
        ASSERT0(pr);
        handleProlog(delegate, pr, delemgr, preheader);
    }

    //Map IR expression which need to restore
    //into memory at epilog of loop.
    ConstIRTab restore2mem; //record the delegate that need to restore.
    InexactAccTabIter ti;
    Occ2Occ occ2newocc;
    IR * nextref = nullptr;
    for (IR * ref = inexact_acc.get_first(ti); ref != nullptr; ref = nextref) {
        nextref = inexact_acc.get_next(ti);
        //Get the unique delegate.
        IR * delegate = nullptr;
        deletab->find(ref, &delegate);
        if (delegate == nullptr) {
            //If delegate does not exist, the reference can not be promoted.
            inexact_acc.remove(ref);
            continue;
        }
        handleAccessInBody(ref, delegate, delemgr, restore2mem,
                           occ2newocc, li, ii);
    }
    ASSERT0(verifyIRandBB(m_rg->getBBList(), m_rg));

    //Generate code to fulfill epilog of delegate.
    handleEpilog(restore2mem, delemgr, exit_bb);
    addDUChainForInexactAccDele(dele, delemgr, occ2newocc);
    removeRedundantDUChain(occ2newocc);

    //Note the delegate is one of reference in 'inexact_acc'.
    freeInexactOccs(inexact_acc, m_rg, m_gvn);
    return true;
}


//Return true if there is IR being promoted, otherwise return false.
bool RegPromot::promoteInexactAccess(LI<IRBB> const* li,
                                     IRBB * preheader,
                                     IRBB * exit_bb,
                                     InexactAccTab & inexact_acc,
                                     IRIter & ii)
{
    DelegateMgr delemgr(m_rg, m_gvn,
        xcom::getNearestPowerOf2(inexact_acc.get_elem_count()));
    InexactAccTabIter ti;
    //Prepare delegate table and related information.
    for (IR * occ = inexact_acc.get_first(ti);
         occ != nullptr; occ = inexact_acc.get_next(ti)) {
        if (!isPromotable(occ)) {
            //Do not promote the reference.
            continue;
        }
        IR * delegate = delemgr.createUniqueDelegate(occ);
        ASSERT0(delegate);
        delemgr.collectOutsideLoopDefUse(occ, delegate, li);
    }

    ti.clean();
    for (IR * occ = inexact_acc.get_first(ti);
         occ != nullptr; occ = inexact_acc.get_next(ti)) {
        if (occ->is_undef()) {
            //occ is the kid of other stmt/exp, and
            //that stmt/exp has been freed.
            continue;
        }
        promoteInexactAccessDelegate(occ, delemgr, li, preheader, exit_bb,
                                     inexact_acc, ii);
    }
    return true;
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


bool RegPromot::tryInsertPreheader(LI<IRBB> const* li, OUT IRBB ** preheader,
                                   MOD RPCtx & ctx)
{
    bool insert_bb = false;
    *preheader = findAndInsertPreheader(li, m_rg, insert_bb, false);
    if (*preheader == nullptr ||
        ((*preheader)->getLastIR() != nullptr &&
         (*preheader)->getLastIR()->isCallStmt() &&
         CALL_is_intrinsic((*preheader)->getLastIR()))) {
        //Insert new BB as preheader.
        *preheader = findAndInsertPreheader(li, m_rg, insert_bb, true);
    }
    ASSERT0(*preheader);

    if (insert_bb) {
        ctx.oc->setDomValid(false);
        ASSERT0(li->getLoopHead());
        if (!m_cfg->tryUpdateRPO(*preheader, li->getLoopHead(), true)) {
            OC_is_rpo_valid(*ctx.oc) = false;
            OC_is_cdg_valid(*ctx.oc) = false;
        }
    }
    return insert_bb;
}


//Return true if there are memory locations have been promoted.
bool RegPromot::promote(LI<IRBB> const* li, IRBB * exit_bb, IRIter & ii,
                        ExactAccTab & exact_acc, InexactAccTab & inexact_acc,
                        MOD RPCtx & ctx)
{
    if (exact_acc.get_elem_count() == 0 && inexact_acc.get_elem_count() == 0) {
        return false;
    }
    //Insert a preheader BB before Loop.
    IRBB * preheader = nullptr;
    bool change = tryInsertPreheader(li, &preheader, ctx);

    if (exact_acc.get_elem_count() != 0) {
        ASSERT0(exact_acc.isOccUnique(getSBSMgr()));
        checkAndRemoveInvalidExactOcc(exact_acc);
        change |= promoteExactAccess(li, ii, preheader, exit_bb, exact_acc);
    }
    if (inexact_acc.get_elem_count() != 0) {
        change |= promoteInexactAccess(li, preheader, exit_bb,
                                       inexact_acc, ii);
    }
    return change;
}


//Return true if there are memory locations have been promoted.
bool RegPromot::tryPromote(LI<IRBB> const* li, IRBB * exit_bb, IRIter & ii,
                           ExactAccTab & exact_acc,
                           InexactAccTab & inexact_acc, MOD RPCtx & ctx)
{
    ASSERT0(li && exit_bb);
    exact_acc.clean();
    inexact_acc.clean();
    m_dont_promote.clean();
    for (INT i = li->getBodyBBSet()->get_first();
         i != -1; i = li->getBodyBBSet()->get_next(i)) {
        IRBB * bb = m_cfg->getBB(i);
        ASSERT0(bb && m_cfg->getVertex(bb->id()));
        if (bb->hasReturn()) {
            return false;
        }

        if (!scanBB(bb, li, exact_acc, inexact_acc)) {
            return false;
        }
    }
    return promote(li, exit_bb, ii, exact_acc, inexact_acc, ctx);
}


bool RegPromot::EvaluableScalarReplacement(List<LI<IRBB> const*> & worklst,
                                           MOD RPCtx & ctx)
{
    //Record the map between MD and ARRAY access expression.
    ExactAccTab access;
    ExactAccTab exact_acc;
    InexactAccTab inexact_acc;
    IRIter ii;
    bool change = false;
    while (worklst.get_elem_count() > 0) {
        LI<IRBB> const* x = worklst.remove_head();
        IRBB * exit_bb = m_cfg->findSingleExitBB(x);
        if (exit_bb != nullptr) {
            //If we did not find a single exit bb, this loop is nontrivial.
            change |= tryPromote(x, exit_bb, ii, exact_acc,
                                 inexact_acc, ctx);
        }

        x = x->getInnerList();
        while (x != nullptr) {
            worklst.append_tail(x);
            x = x->get_next();
        }
    }
    return change;
}


bool RegPromot::dump() const
{
    if (!m_rg->isLogMgrInit()) { return false; }
    note(getRegion(), "\n==---- DUMP %s '%s' ----==",
         getPassName(), m_rg->getRegionName());
    //dump_mdlt();
    m_dont_promote.dump();
    dumpBBList(m_rg->getBBList(), m_rg);
    if (usePRSSADU()) {
        m_prssamgr->dump();
    }
    if (useMDSSADU()) {
        m_mdssamgr->dump();
    }
    note(getRegion(), "\n");
    return true;
}


//Perform scalar replacement of aggregates and array.
bool RegPromot::perform(OptCtx & oc)
{
    BBList * bbl = m_rg->getBBList();
    if (bbl == nullptr || bbl->get_elem_count() == 0) { return false; }
    if (!oc.is_ref_valid()) { return false; }
    if (!oc.is_cfg_valid()) { return false; }
    //Check PR DU chain.
    m_prssamgr = (PRSSAMgr*)(m_rg->getPassMgr()->queryPass(PASS_PR_SSA_MGR));
    if (!oc.is_pr_du_chain_valid() && usePRSSADU()) {
        //At least one kind of DU chain should be avaiable.
        return false;
    }
    //Check NONPR DU chain.
    m_mdssamgr = (MDSSAMgr*)(m_rg->getPassMgr()->queryPass(PASS_MD_SSA_MGR));
    if (!oc.is_nonpr_du_chain_valid() && useMDSSADU()) {
        //At least one kind of DU chain should be avaiable.
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

    init();
    List<LI<IRBB> const*> worklst;
    while (li != nullptr) {
        worklst.append_tail(li);
        li = LI_next(li);
    }
    buildLifeTime();
    RPCtx ctx(&oc);
    bool change = EvaluableScalarReplacement(worklst, ctx);
    if (change) {
        //DU reference and du chain has maintained.
        ASSERT0(m_rg->verifyMDRef());
        ASSERT0(verifyMDDUChain(m_rg));
        ASSERT0(m_rg->verifyMDRef());
        OC_is_reach_def_valid(oc) = false;
        OC_is_avail_reach_def_valid(oc) = false;
        OC_is_live_expr_valid(oc) = false;

        //Enforce following pass to recompute gvn.
        m_gvn->set_valid(false);

        //WORKAROUND|FIXME:For now, we do not support incremental update SSA.
        if (usePRSSADU()) {
            //Rebuild PRSSA if DU changed.
            ASSERT0(m_prssamgr);
            m_prssamgr->construction(oc);        
            if (m_du != nullptr && !oc.is_du_chain_valid()) {
                //PRSSAMgr will destruct classic DU-chain.
                m_du->cleanDUSet();
                oc.setInvalidDUChain();
            }
        }
        if (useMDSSADU()) {
            //Rebuild MDSSA if DU changed.
            ASSERT0(m_mdssamgr);
            m_mdssamgr->construction(oc);
        }
    }

    clean();

    if (g_is_dump_after_pass && g_dump_opt.isDumpRP()) {
        dump();
    }
    END_TIMER(t, getPassName());
    return change;
}
//END RegPromot

} //namespace xoc
