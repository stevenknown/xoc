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
    ASSERT0(m_rg && m_rm && m_tm);
    ASSERTN(m_mdsys, ("MD system is necessary, "
                      "initialize VarMgr and MDSystem first"));
}


//Generate MD for Var.
MD const* MDMgr::genMDForVar(Var * var, Type const* type, TMWORD offset)
{
    ASSERT0(var && type);
    MD md;
    MD_base(&md) = var;
    if (type->is_any()) {
        MD_ty(&md) = MD_UNBOUND;
    } else {
        MD_size(&md) = m_tm->getByteSize(type);
        MD_ty(&md) = MD_EXACT;
        MD_ofst(&md) = offset;
    }
    MD const* e = m_mdsys->registerMD(md);
    ASSERT0(MD_id(e) > 0);
    return e;
}


//The function generates new MD for given PR.
//It should be called if new PR generated in optimzations.
MD const* MDMgr::allocMDForPROp(IR * pr)
{
    ASSERT0(pr->isPROp());
    if (pr->is_setelem()) { return allocSetElemMD(pr); }
    MD const* md = genMDForPR(pr);
    pr->setMustRef(md, m_rg);
    pr->cleanRefMDSet();
    return md;
}


MD const* MDMgr::allocIdMD(IR * ir)
{
    ASSERT0(ir->is_id());
    MD const* t = genMDForVar(ir->getIdinfo(), ir->getType(), ir->getOffset());
    ir->setMustRef(t, m_rg);
    ir->cleanRefMDSet();
    return t;
}


MD const* MDMgr::allocMDForDirectMemOp(IR * ir, bool clean_mayset)
{
    ASSERT0(ir->isDirectMemOp());
    MD const* t = genMDForDirectMemOp(ir);
    ASSERT0(t);
    //Note genMDForLoad has consider the Offset of ir.
    ir->setMustRef(t, m_rg);
    if (clean_mayset) {
        ir->cleanRefMDSet();
    }
    return t;
}


MD const* MDMgr::allocSetElemMD(IR * ir)
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
MD const* MDMgr::genMDForPR(PRNO prno, Type const* type)
{
    ASSERT0(type);
    Var * pr_var = m_rg->getVarByPRNO(prno);
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


//Alloc MD for const string.
MD const* MDMgr::allocStringMD(Sym const* string)
{
    ASSERT0(string);
    MD const* strmd = m_rm->genDedicateStrMD();
    if (strmd != nullptr) { return strmd; }

    Var * v = m_vm->registerStringVar(nullptr, string, MEMORY_ALIGNMENT);
    //Set string address to be taken only if it is base of LDA.
    //v->setFlag(VAR_ADDR_TAKEN);

    MD md;
    MD_base(&md) = v;
    MD_size(&md) = string->getLen() + 1;
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
    SWITCH_CASE_PR_OP:
        if (assign_pr) {
            allocMDForPROp(x);
        }
        break;
    SWITCH_CASE_CALL:
        if (assign_pr && x->hasReturnValue()) {
            allocMDForPROp(x);
        }
        break;
    SWITCH_CASE_DIRECT_MEM_OP:
        if (assign_nonpr) {
            allocMDForDirectMemOp(x, true);
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
    SWITCH_CASE_INDIRECT_MEM_OP:
    SWITCH_CASE_ARRAY_OP:
        break;
    default: ASSERTN(!x->isMemRef(), ("TODO:need to support"));
    }
}


//Assign MD for ST/LD/ReadPR/WritePR operations.
//is_only_assign_pr: true if assign MD for each ReadPR/WritePR operations.
void MDMgr::assignMD(bool assign_pr, bool assign_nonpr)
{
    if (m_rg->getIRList() != nullptr) {
        assignMD(m_rg->getIRList(), assign_pr, assign_nonpr);
        return;
    }
    if (m_rg->getBBList() != nullptr) {
        assignMD(m_rg->getBBList(), assign_pr, assign_nonpr);
    }
}


void MDMgr::assignMD(IR * irlist, bool assign_pr, bool assign_nonpr,
                     MOD IRIter & ii)
{
    ii.clean();
    for (IR * x = xoc::iterInit(irlist, ii, true);
         x != nullptr; x = xoc::iterNext(ii, true)) {
        assignMDImpl(x, assign_pr, assign_nonpr);
    }
}


void MDMgr::assignMD(IR * irlist, bool assign_pr, bool assign_nonpr)
{
    IRIter ii;
    assignMD(irlist, assign_pr, assign_nonpr, ii);
}


void MDMgr::assignMD(xcom::List<IR*> const& irlist, bool assign_pr,
                     bool assign_nonpr)
{
    IRIter ii;
    xcom::List<IR*>::Iter ct;
    for (xoc::IR * ir = irlist.get_head(&ct);
         ir != nullptr; ir = irlist.get_next(&ct)) {
        assignMD(ir, assign_pr, assign_nonpr, ii);
    }
}


void MDMgr::assignMD(BBList * lst, bool assign_pr, bool assign_nonpr)
{
    ASSERT0(lst);
    IRIter ii;
    for (IRBB * bb = lst->get_head(); bb != nullptr; bb = lst->get_next()) {
        assignMD(bb, assign_pr, assign_nonpr, ii);
    }
}


void MDMgr::assignMD(IRBB * bb, bool assign_pr, bool assign_nonpr,
                     MOD IRIter & ii)
{
    BBIRListIter ct;
    for (xoc::IR * ir = bb->getIRList().get_head(&ct);
         ir != nullptr; ir = bb->getIRList().get_next(&ct)) {
        ii.clean();
        for (IR * x = xoc::iterInit(ir, ii, true);
             x != nullptr; x = xoc::iterNext(ii, true)) {
           assignMDImpl(x, assign_pr, assign_nonpr);
        }
    }
}


//The function generates new MD for the IR tree that rooted by 'root'.
//sibling: true if the function have to walk the sibling node of 'root'.
//It should be called if new IR tree generated in optimzations.
void MDMgr::allocRefForIRTree(IR * root, bool sibling)
{
    IR * ir = root;
    switch (ir->getCode()) {
    SWITCH_CASE_PR_OP:
        allocMDForPROp(ir);
        break;
    SWITCH_CASE_DIRECT_MEM_OP:
        allocMDForDirectMemOp(ir, true);
        break;
    default:;
    }
    for (UINT i = 0; i < IR_MAX_KID_NUM(ir); i++) {
        IR * k = ir->getKid(i);
        if (k != nullptr) {
            allocRefForIRTree(k, true);
        }
    }
    if (!sibling) { return; }
    for (IR * s = ir->get_next(); s != nullptr; s = s->get_next()) {
        allocRefForIRTree(s, false);
    }
}


MD const* MDMgr::allocRef(IR * ir)
{
    switch (ir->getCode()) {
    SWITCH_CASE_MAY_PR_OP:
        return allocMDForPROp(ir);
    SWITCH_CASE_DIRECT_MEM_OP:
        //Do NOT clean MaySet because transformation may combine ILD(LDA)
        //into LD and carry MDSet from ILD.
        return allocMDForDirectMemOp(ir, false);
    case IR_ID: return allocIdMD(ir);
    default: UNREACHABLE();
    }
    return nullptr;
}

} //namespace xoc
