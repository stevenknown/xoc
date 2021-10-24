/*@
Copyright (c) 2013-2021, Su Zhenyu steven.known@gmail.com
All rights reserved.

Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions are met:

    * Redistributions of source code must retain the above copyright
      notice, m_rg list of conditions and the following disclaimer.
    * Redistributions in binary form must reproduce the above copyright
      notice, m_rg list of conditions and the following disclaimer in the
      documentation and/or other materials provided with the distribution.
    * Neither the name of the Su Zhenyu nor the names of its contributors
      may be used to endorse or promote products derived from m_rg software
      without specific prior written permission.

m_rg SOFTWARE IS PROVIDED "AS IS" AND ANY
EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE
FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,
OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE
USE OF m_rg SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
@*/
#include "cominc.h"
#include "comopt.h"

namespace xoc {

MDMgr::MDMgr(Region * rg) :
    m_rg(rg), m_rm(rg->getRegionMgr()), m_mdsys(rg->getMDSystem()),
    m_tm(rg->getTypeMgr()), m_vm(rg->getVarMgr())
{
}


//The function generates new MD for given PR.
//It should be called if new PR generated in optimzations.
MD const* MDMgr::allocPRMD(IR * pr)
{
    ASSERT0(pr->is_pr());
    MD const* md = genMDForPR(pr);
    pr->setMustRef(md, m_rg);
    pr->cleanRefMDSet();
    return md;
}


//The function generates new MD for given PR.
//It should be called if new PR generated in optimzations.
MD const* MDMgr::allocPhiMD(IR * phi)
{
    ASSERT0(phi->is_phi());
    MD const* md = genMDForPR(phi);
    phi->setMustRef(md, m_rg);
    phi->cleanRefMDSet();
    return md;
}


MD const* MDMgr::allocIdMD(IR * ir)
{
    ASSERT0(ir->is_id());
    MD const* t = genMDForId(ir);
    ir->setMustRef(t, m_rg);
    ir->cleanRefMDSet();
    return t;
}


MD const* MDMgr::allocLoadMD(IR * ir)
{
    ASSERT0(ir->is_ld());
    MD const* t = genMDForLoad(ir);
    ASSERT0(t);
    ir->cleanRefMDSet();

    //TO BE REMOVED: genMDForLoad has consider the Offset of ir.
    //if (LD_ofst(ir) != 0) {
    //    MD t2(*t);
    //    ASSERT0(t2.is_exact());
    //    MD_ofst(&t2) += LD_ofst(ir);
    //    MD_size(&t2) = ir->getTypeSize(m_tm);
    //    MD const* entry = m_mdsys->registerMD(t2);
    //    ASSERTN(MD_id(entry) > 0, ("Not yet registered"));
    //    t = entry; //regard MD with offset as return result.
    //}

    ir->setMustRef(t, m_rg);
    return t;
}


MD const* MDMgr::allocStorePRMD(IR * ir)
{
    ASSERT0(ir->is_stpr());
    MD const* md = genMDForPR(ir);
    ir->setMustRef(md, m_rg);
    ir->cleanRefMDSet();
    return md;
}


MD const* MDMgr::allocCallResultPRMD(IR * ir)
{
    ASSERT0(ir->isCallStmt());
    MD const* md = genMDForPR(ir);
    ir->setMustRef(md, m_rg);
    ir->cleanRefMDSet();
    return md;
}


MD const* MDMgr::allocSetelemMD(IR * ir)
{
    ASSERT0(ir->is_setelem());
    MD const* md = genMDForPR(ir);
    IR const* ofst = SETELEM_ofst(ir);
    ASSERT0(ofst);
    if (md->is_exact()) {
        if (ofst->is_const()) {
            ASSERTN(ofst->is_int(), ("offset of SETELEM must be integer."));

            //Accumulating offset of identifier.
            //e.g: struct {int a,b; } s; s.a = 10
            //generate: st s:offset(4) = 10;
            MD t(*md);
            ASSERT0(ir->getTypeSize(m_tm) > 0);
            MD_ofst(&t) += (UINT)CONST_int_val(ofst);
            MD_size(&t) = ir->getTypeSize(m_tm);
            MD const* entry = m_mdsys->registerMD(t);
            ASSERTN(MD_id(entry) > 0, ("Not yet registered"));
            md = entry; //regard MD with offset as return result.
        } else {
            //Offset is variable.
            //e.g: vector<4xi32> v; v[i] = 34;
            //will generate:
            //    st $1 = ld v;
            //    setelem $1 = 34, ld i;
            //    st v = $1;

            MD t(*md);
            ASSERT0(ir->getTypeSize(m_tm) > 0);
            MD_ty(&t) = MD_RANGE;
            MD_ofst(&t) = 0;
            MD_size(&t) = ir->getTypeSize(m_tm);
            MD const* entry = m_mdsys->registerMD(t);
            ASSERTN(MD_id(entry) > 0, ("Not yet registered"));
            md = entry; //regard MD with range as return result.
        }
    }

    ir->setMustRef(md, m_rg);
    ir->cleanRefMDSet();
    return md;
}


//Generate MD corresponding to PR load or write.
MD const* MDMgr::genMDForPR(UINT prno, Type const* type)
{
    ASSERT0(type);
    Var * pr_var = m_rg->mapPR2Var(prno);
    if (pr_var == nullptr) {
        pr_var = m_rg->genVarForPR(prno, type);
    }

    MD md;
    MD_base(&md) = pr_var; //correspond to Var
    MD_ofst(&md) = 0;
    if (type->is_any()) {
        MD_ty(&md) = MD_UNBOUND;
    } else {
        MD_ty(&md) = MD_EXACT;
        MD_size(&md) = m_tm->getByteSize(type);
    }
    MD const* e = m_mdsys->registerMD(md);
    ASSERT0(MD_id(e) > 0);
    return e;
}


MD const* MDMgr::allocGetelemMD(IR * ir)
{
    ASSERT0(ir->is_getelem());
    MD const* md = genMDForPR(ir);
    ir->setMustRef(md, m_rg);
    ir->cleanRefMDSet();
    return md;
}


MD const* MDMgr::allocStoreMD(IR * ir)
{
    ASSERT0(ir->is_st());
    MD const* md = genMDForStore(ir);
    ASSERT0(md);
    ir->cleanRefMDSet();

    //TO BE REMOVED: genMDForStore has considered the Offset of ir.
    //if (ST_ofst(ir) != 0) {
    //    //Accumulating offset of identifier.
    //    //e.g: struct {int a,b; } s; s.a = 10
    //    //generate: st('s', ofst:4) = 10
    //    MD t(*md);
    //    ASSERT0(t.is_exact());
    //    ASSERT0(ir->getTypeSize(m_tm) > 0);
    //    MD_ofst(&t) += ST_ofst(ir);
    //    MD_size(&t) = ir->getTypeSize(m_tm);
    //    MD const* entry = m_mdsys->registerMD(t);
    //    ASSERTN(MD_id(entry) > 0, ("Not yet registered"));
    //    md = entry; //regard MD with offset as return result.
    //}

    ir->setMustRef(md, m_rg);
    return md;
}


//Alloc MD for const string.
MD const* MDMgr::allocStringMD(Sym const* string)
{
    ASSERT0(string);
    MD const* strmd = m_rm->genDedicateStrMD();
    if (strmd != nullptr) { return strmd; }

    Var * v = m_vm->registerStringVar(nullptr, string, MEMORY_ALIGNMENT);
    //Set string address to be taken only if it is base of LDA.
    //VAR_is_addr_taken(v) = true;
    MD md;
    MD_base(&md) = v;
    MD_size(&md) = (UINT)strlen(SYM_name(string)) + 1;
    MD_ofst(&md) = 0;
    MD_ty(&md) = MD_EXACT;
    ASSERT0(v->is_string());

    MD const* e = m_mdsys->registerMD(md);
    ASSERT0(MD_id(e) > 0);
    return e;
}


void MDMgr::assignMDImpl(IR * x, bool assign_pr, bool assign_nonpr)
{
    ASSERT0(x);
    switch (x->getCode()) {
    case IR_PR:
        if (assign_pr) {
            allocPRMD(x);
        }
        break;
    case IR_STPR:
        if (assign_pr) {
            allocStorePRMD(x);
        }
        break;
    case IR_GETELEM:
        if (assign_pr) {
            allocGetelemMD(x);
        }
        break;
    case IR_SETELEM:
        if (assign_pr) {
            allocSetelemMD(x);
        }
        break;
    case IR_PHI:
        if (assign_pr) {
            allocPhiMD(x);
        }
        break;
    case IR_CALL:
    case IR_ICALL:
        if (assign_pr && x->hasReturnValue()) {
            allocCallResultPRMD(x);
        }
        break;
    case IR_ST:
        if (assign_nonpr) {
            allocStoreMD(x);
        }
        break;
    case IR_LD:
        if (assign_nonpr) {
            allocLoadMD(x);
        }
        break;
    case IR_ID:
        if (assign_nonpr) {
            if (ID_info(x)->is_string()) {
                allocStringMD(ID_info(x)->get_name());
            } else {
                allocIdMD(x);
            }
        }
        break;
    default: ASSERT0(!x->isReadPR() && !x->isWritePR());
    }
}


//Assign MD for ST/LD/ReadPR/WritePR operations.
//is_only_assign_pr: true if assign MD for each ReadPR/WritePR operations.
void MDMgr::assignMD(bool assign_pr, bool assign_nonpr)
{
    if (m_rg->getIRList() != nullptr) {
        assignMDForIRList(m_rg->getIRList(), assign_pr, assign_nonpr);
        return;
    }
    if (m_rg->getBBList() != nullptr) {
        assignMDForBBList(m_rg->getBBList(), assign_pr, assign_nonpr);
    }
}


//Assign MD for ST/LD/ReadPR/WritePR operations.
//is_only_assign_pr: true if assign MD for each ReadPR/WritePR operations.
void MDMgr::assignMD(IR * irlist, bool assign_pr, bool assign_nonpr)
{
    assignMDForIRList(irlist, assign_pr, assign_nonpr);
}


void MDMgr::assignMDForBBList(BBList * lst, bool assign_pr, bool assign_nonpr)
{
    ASSERT0(lst);
    IRIter ii;
    for (IRBB * bb = lst->get_head(); bb != nullptr; bb = lst->get_next()) {
        assignMDForBB(bb, ii, assign_pr, assign_nonpr);
    }
}


void MDMgr::assignMDForBB(IRBB * bb, IRIter & ii,
                           bool assign_pr, bool assign_nonpr)
{
    BBIRListIter ct;
    for (xoc::IR * ir = bb->getIRList()->get_head(&ct);
         ir != nullptr; ir = bb->getIRList()->get_next(&ct)) {
        for (IR * x = iterInit(ir, ii);
           x != nullptr; x = iterNext(ii)) {
           assignMDImpl(x, assign_pr, assign_nonpr);
        }
    }
}


void MDMgr::assignMDForIRList(IR * lst, bool assign_pr, bool assign_nonpr)
{
    for (IR * ir = lst; ir != nullptr; ir = ir->get_next()) {
        switch (ir->getCode()) {
        case IR_PR:
        case IR_LD:
        case IR_ID:
            assignMDImpl(ir, assign_pr, assign_nonpr);
            break;
        default: {
            //Iterate rest of ir in lst.
            IRIter ii;
            for (IR * x = iterInit(lst, ii);
                 x != nullptr; x = iterNext(ii)) {
                assignMDImpl(x, assign_pr, assign_nonpr);
            }
            return;
        }
        }
    }
}

} //namespace xoc
